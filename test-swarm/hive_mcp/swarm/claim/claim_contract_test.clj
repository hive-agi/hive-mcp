(ns hive-mcp.swarm.claim.claim-contract-test
  "Contract facets for the claim registry, the claim negotiation and the carto
   claim graph shim. Every test owns its state: the swarm DataScript store,
   the logic db, the yield ledger and the callers cache are reset around each
   test, and the directed-message port is an injected recorder, so nothing
   here reaches a real agent.

   Also the coverage gate for src/hive_mcp/swarm/claim."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.swarm.claim.graph :as graph]
            [hive-mcp.swarm.claim.negotiate :as neg]
            [hive-mcp.swarm.claim.registry :as creg]
            [hive-mcp.swarm.claim.span :as span]
            [hive-mcp.swarm.datascript.connection :as conn]
            [hive-mcp.swarm.datascript.lings :as lings]
            [hive-mcp.swarm.logic :as logic]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :as hst]
            [hive-test.golden :as golden]
            [hive-test.mutation :as mut]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- reset-all! []
  (conn/reset-conn!)
  (logic/reset-db!)
  (neg/reset-yield-ledger!)
  (graph/reset-cache!))

(defn- fresh [f]
  (reset-all!)
  (try (f) (finally (reset-all!))))

(use-fixtures :each fresh)

(defn- with-lings
  "A fresh store holding the named lings, then f."
  [ids f]
  (reset-all!)
  (doseq [id ids] (lings/add-slave! id {:status :working}))
  (f))

;;; ---------------------------------------------------------------------------
;;; registry: row->span is pure
;;; ---------------------------------------------------------------------------

(def ^:private RowIn
  "Rows in every shape row->span reads; the key and qn are drawn so a stored
   mode is usually believed (a parsed or stored qn is present)."
  [:map
   [:claim/file {:optional true} [:enum "src/a.clj" "src/a.clj#a/f" "src/b.clj#x/y#z" "src/c.clj#c/h" ""]]
   [:claim/qn {:optional true} [:maybe [:enum "a/g"]]]
   [:claim/mode [:maybe [:enum :file :body :signature :signature]]]
   [:claim/slave {:optional true} [:maybe [:enum "ling-a"]]]])

(hst/deftrifecta-from-schema row->span-believes-its-own-fields
  hive-mcp.swarm.claim.registry/row->span
  {:in RowIn
   :out creg/HeldSpan
   :rel (fn [row out]
          (let [k (or (:claim/file row) "")
                i (clojure.string/last-index-of k "#")
                qn (or (:claim/qn row) (when i (subs k (inc i))))]
            (and (= k (:claim/key out))
                 (= (:claim/slave row) (:claim/slave out))
                 (= (if i (subs k 0 i) k) (:span/file out))
                 (if qn
                   (and (= qn (:span/qn out))
                        (= (or (:claim/mode row) :body)
                           (if (contains? #{:body :signature :file} (:claim/mode row))
                             (:span/mode out)
                             :body)))
                   (= :file (:span/mode out))))))
   :classify (fn [_ out] (:span/mode out))
   :classify-domain #{:file :body :signature}
   :classify-floor 5
   :num-tests 200
   :golden-path "test/golden/hive-mcp/swarm/claim/row-span.edn"})

;;; ---------------------------------------------------------------------------
;;; registry: acquire and release over the real store
;;; ---------------------------------------------------------------------------

(defn- acquire-scenario
  "Two lings, one held file, one form span: what each step answers."
  []
  (with-lings ["ling-a" "ling-b"]
    (fn []
      (let [a1 (creg/acquire! ["src/x.clj" {:span/file "src/w.clj" :span/qn "w/f"}] "ling-a")
            a2 (creg/acquire! #{"src/x.clj"} "ling-b" {:task-id nil})
            a3 (creg/acquire! [{:span/file "src/w.clj" :span/qn "w/g"}] "ling-b")
            a4 (creg/acquire! [] "ling-b")
            held (creg/held-spans)
            cf   (creg/conflicts-for "src/x.clj" "ling-b")
            av   [(creg/available? "src/x.clj" "ling-b") (creg/available? "src/x.clj" "ling-a")]
            _    (creg/release-span! "src/x.clj")
            av2  (creg/available? "src/x.clj" "ling-b")]
        {:a1 a1 :a2 (update a2 :conflicts #(mapv (fn [c] (dissoc c :message)) %))
         :a3 a3 :a4 a4
         :held (sort-by :claim/key held)
         :conflicts-for cf :available av :available-after-release av2}))))

(golden/deftest-golden acquire-and-release-golden
  "test/golden/hive-mcp/swarm/claim/acquire-release.edn"
  (acquire-scenario))

(deftest acquire-contracts-hold-test
  (let [{:keys [a1 a2 a3 a4 held]} (acquire-scenario)]
    (doseq [r [a1 a2 a3 a4]] (is (m/validate creg/AcquireResult r)))
    (is (every? #(m/validate creg/HeldSpan %) held))
    (testing "all or nothing"
      (is (:acquired? a1))
      (is (= 2 (:spans-claimed a1)))
      (is (not (:acquired? a2)))
      (is (zero? (:spans-claimed a2))))
    (testing "two forms in one file do not collide"
      (is (:acquired? a3)))))

(mut/deftest-mutations acquire!-mutants-caught
  hive-mcp.swarm.claim.registry/acquire!
  [["always-acquires" (fn [w _ & _] {:acquired? true :conflicts [] :warnings [] :spans-claimed (count w)})]
   ["always-refuses"  (fn [_ _ & _] {:acquired? false :conflicts [] :warnings [] :spans-claimed 0})]]
  (fn []
    (let [{:keys [a1 a2 a3]} (acquire-scenario)]
      (is (:acquired? a1))
      (is (not (:acquired? a2)))
      (is (:acquired? a3)))))

(deftest held-spans-drops-stale-unless-asked-test
  (with-lings ["ling-a"]
    (fn []
      (creg/claim-span! "src/old.clj" "ling-a")
      (let [future-now (+ (System/currentTimeMillis) (* 24 60 60 1000))]
        (is (= [] (creg/held-spans {:now-ms future-now :threshold-ms 1000})))
        (is (= ["src/old.clj"]
               (mapv :claim/key (creg/held-spans {:now-ms future-now :threshold-ms 1000
                                                  :include-stale? true}))))))))

;;; ---------------------------------------------------------------------------
;;; negotiate
;;; ---------------------------------------------------------------------------

(hst/deftrifecta-from-schema held-key-matches-the-span-key
  hive-mcp.swarm.claim.negotiate/held-key
  {:in [:map [:file [:enum "src/a.clj" "src/b.clj"]]
        [:qn {:optional true} [:maybe [:enum "a/f" "a/g"]]]
        [:mode {:optional true} [:maybe [:enum :file :body :signature]]]]
   :out :string
   :rel (fn [{:keys [file qn mode]} out]
          (= out (if (and qn (not= :file mode)) (str file "#" qn) file)))
   :golden-path "test/golden/hive-mcp/swarm/claim/held-key.edn"
   :n-cases 10})

(defn- negotiate-scenario
  "ling-a holds a file; ling-b is refused, parked and asks once; a retry does
   not ask again; then ling-b acquires a free file."
  []
  (with-lings ["ling-a" "ling-b"]
    (fn []
      (let [sent    (atom [])
            notify! (fn [from to et data] (swap! sent conj [from to et (:file data)]) true)
            a  (neg/acquire-for-ling! "ling-a" ["src/held.clj"] {:notify! notify! :now-ms 1})
            b1 (neg/acquire-for-ling! "ling-b" ["src/held.clj"] {:notify! notify! :now-ms 2 :task-id "t-b"})
            b2 (neg/acquire-for-ling! "ling-b" ["src/held.clj"] {:notify! notify! :now-ms 3 :task-id "t-b"})
            b3 (neg/acquire-for-ling! "ling-b" ["src/free.clj"] {:notify! notify! :now-ms 4})
            b4 (neg/acquire-for-ling! "ling-b" nil)]
        {:a a
         :b1 (update b1 :conflicts #(mapv (fn [c] (dissoc c :message)) %))
         :b2 (select-keys b2 [:acquired? :parked :yield-requested])
         :b3 b3 :b4 b4
         :sent @sent
         :deadlock (neg/foresee-deadlock "ling-b" ["src/held.clj"])}))))

(golden/deftest-golden negotiate-golden
  "test/golden/hive-mcp/swarm/claim/negotiate.edn"
  (negotiate-scenario))

(deftest negotiate-contracts-hold-test
  (let [{:keys [a b1 b2 b3 b4 sent deadlock]} (negotiate-scenario)]
    (doseq [r [a b1 b3 b4]] (is (m/validate neg/AcquireForLing r)))
    (is (:acquired? a))
    (is (= ["src/held.clj"] (:parked b1)))
    (is (= [{:holder "ling-a" :file "src/held.clj"}] (:yield-requested b1)))
    (testing "a retry is deduplicated"
      (is (= [] (:yield-requested b2)))
      (is (= 1 (count sent))))
    (is (:acquired? b3))
    (is (= {:acquired? true :conflicts [] :warnings [] :spans-claimed 0} b4))
    (is (nil? deadlock))))

(mut/deftest-mutations acquire-for-ling!-mutants-caught
  hive-mcp.swarm.claim.negotiate/acquire-for-ling!
  [["no-park" (fn [_ _ & _] {:acquired? false :conflicts [] :warnings [] :spans-claimed 0
                             :parked [] :yield-requested []})]
   ["acquire-blindly" (fn [_ files & _] {:acquired? true :conflicts [] :warnings []
                                         :spans-claimed (count files)})]]
  (fn []
    (let [{:keys [a b1]} (negotiate-scenario)]
      (is (:acquired? a))
      (is (= ["src/held.clj"] (:parked b1))))))

(deftest deadlock-is-foreseen-test
  (with-lings ["ling-a" "ling-b"]
    (fn []
      (let [quiet (fn [& _] true)]
        (neg/acquire-for-ling! "ling-a" ["src/x.clj"] {:notify! quiet :now-ms 1})
        (neg/acquire-for-ling! "ling-b" ["src/y.clj"] {:notify! quiet :now-ms 1})
        (neg/acquire-for-ling! "ling-a" ["src/y.clj"] {:notify! quiet :now-ms 2})
        (let [d (neg/foresee-deadlock "ling-b" ["src/x.clj"])]
          (when d ;; nil only on a hive-agent without a wait graph
            (is (m/validate neg/Deadlock d))
            (is (= #{"ling-a" "ling-b"} (set (:cycle d))))))))))

(deftest reset-yield-ledger-answers-empty-test
  (is (= {} (neg/reset-yield-ledger!))))

;;; ---------------------------------------------------------------------------
;;; graph
;;; ---------------------------------------------------------------------------

(deftest graph-shim-contracts-test
  (is (boolean? (graph/available?)))
  (is (= {} (graph/reset-cache!)))
  (let [cf (graph/callers-fn nil)]
    (is (= (graph/available?) (some? cf)))
    (when cf (is (m/validate graph/Callers (cf "no.such/qn")))))
  (is (m/validate graph/Callers (graph/callers nil "no.such/qn"))))

;;; ---------------------------------------------------------------------------
;;; schemas and the coverage gate
;;; ---------------------------------------------------------------------------

(deftest claim-schemas-generate-test
  (doseq [s [creg/ClaimRow creg/HeldSpan creg/Wanted creg/AcquireResult creg/TxReport
             neg/NotifyFn neg/Deadlock neg/AcquireForLing graph/Callers graph/CallersFn]]
    (is (seq (mg/sample s {:size 3})) (pr-str (m/form s)))))

(deftest every-claim-contract-generates-both-sides-test
  (doseq [ns-sym '[hive-mcp.swarm.claim.span hive-mcp.swarm.claim.graph
                   hive-mcp.swarm.claim.registry hive-mcp.swarm.claim.negotiate]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          :let [s (m/schema schema)]
          arity (if (= :function (m/type s)) (m/children s) [s])
          side (take 2 (m/children arity))]
    (is (seq (mg/sample side {:size 3 :seed 7})) (str ns-sym "/" fn-sym))))

(deftest-contract-coverage swarm-claim-contracts-gate
  {:paths ["src/hive_mcp/swarm/claim"]
   :exempt {}})

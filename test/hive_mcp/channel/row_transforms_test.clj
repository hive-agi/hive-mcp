(ns hive-mcp.channel.row-transforms-test
  "The hivemind.rows seam: registered transforms reshape the rows a reader
   earned, core's :progress fold is just the first of them, and nothing a
   transform does can move a cursor, leak an internal field, or grow a read."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.audience :as audience]
            [hive-mcp.channel.piggyback :as pb]
            [hive-mcp.channel.row-transforms :as rt]
            [hive-addon.registry.commands :as addon-cmds]
            [hive-mcp.addons.core :as addon-core]
            [hive-mcp.config.core :as config]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.server.init :as init]
            [hive-mcp.system.layer3]
            [integrant.core :as ig]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- k
  "A hivemind.rows key. Built, not written literally: the reader refuses a
   keyword whose name starts with a digit, and the seam's keys sort by one."
  [n]
  (keyword rt/key-namespace n))

(def ^:private test-config
  "The config a test reads through hive-mcp.config.core/get-in-config."
  (atom {}))

(defn- clean-state
  "Isolate the piggyback state, the config and the hivemind.rows registry.

   Every hivemind.rows key registered before the test is saved and removed,
   and on the way out the registry is put back exactly as it was found, so no
   transform a test registers outlives it. Core's default fold is NOT
   registered here: it is not a registry entry, and a fixture that put it back
   would hide a lifecycle bug."
  [f]
  (let [original-source @pb/message-source-fn
        saved (into {} (map (juxt identity ext/get-extension)) (rt/transform-keys))]
    (doseq [rk (keys saved)] (ext/deregister! rk))
    (reset! test-config {})
    (pb/reset-all-cursors!)
    (pb/clear-backbone-buffer!)
    (pb/register-message-source! (constantly []))
    (try
      (with-redefs [config/get-in-config (fn [path] (get-in @test-config path))]
        (f))
      (finally
        (doseq [rk (rt/transform-keys)] (ext/deregister! rk))
        (doseq [[rk v] saved] (ext/register! rk v))
        (pb/reset-all-cursors!)
        (pb/clear-backbone-buffer!)
        (pb/register-message-source! original-source)))))

(use-fixtures :each clean-state)

;; =============================================================================
;; Seed + oracle
;; =============================================================================

(def ^:private seeded
  "A read with two agents' :progress bursts, a deliberate shout inside one of
   them, and every optional rendered field (:t :ctx :ref)."
  [{:agent-id "ling-a" :event-type :progress :message "turn 1"
    :timestamp 1000 :project-id "global"}
   {:agent-id "ling-b" :event-type :started :message "go" :task "T1"
    :timestamp 1001 :project-id "global"}
   {:agent-id "ling-a" :event-type :progress :message "turn 2"
    :timestamp 1002 :project-id "global" :context-id "c1"}
   {:agent-id "ling-a" :event-type :progress :message "hello"
    :timestamp 1003 :project-id "global" :deliberate? true}
   {:agent-id "ling-b" :event-type "progress" :message "b1" :ref "r1"
    :timestamp 1004 :project-id "global"}
   {:agent-id "ling-a" :event-type :progress :message "turn 3" :task "T2"
    :timestamp 1005 :project-id "global"}
   {:agent-id "ling-b" :event-type :completed :message "done"
    :timestamp 1006 :project-id "global"}])

(defn- pre-seam-rows
  "The row stage of get-messages exactly as it was before the seam existed:
   format, fold when enabled, strip :deliberate?. The oracle for (a)."
  [msgs fold?]
  (let [formatted (mapv (fn [{:keys [agent-id event-type message task deliberate?
                                     context-id ref]}]
                          (cond-> {:a agent-id
                                   :e (if (keyword? event-type) (name event-type) event-type)
                                   :m message}
                            task (assoc :t task)
                            context-id (assoc :ctx context-id)
                            ref (assoc :ref ref)
                            deliberate? (assoc :deliberate? true)))
                        msgs)]
    (mapv #(dissoc % :deliberate?)
          (if fold? (audience/digest formatted) formatted))))

(defn- read-seeded
  []
  (pb/register-message-source! (constantly seeded))
  (pb/get-messages "coordinator" :project-id "global"))

(defn- register-capture!
  "Register a pass-through transform under `k` that records what it saw."
  [k]
  (let [seen (atom nil)]
    (ext/register! k (fn [rows ctx] (reset! seen {:rows rows :ctx ctx}) rows))
    seen))

;; =============================================================================
;; (a) default behaviour is unchanged
;; =============================================================================

(deftest default-config-output-is-byte-identical-test
  (let [out (read-seeded)]
    (testing "the fold is on by default and matches the pre-seam pipeline byte for byte"
      (is (= (pr-str (pre-seam-rows seeded true)) (pr-str out)))
      (is (= (pr-str [{:a "ling-b" :e "started" :m "go" :t "T1"}
                      {:a "ling-a" :e "progress" :m "hello"}
                      {:a "ling-b" :e "progress" :m "b1" :ref "r1"}
                      {:a "ling-a" :e "progress" :m "turn 3" :t "T2" :n 3}
                      {:a "ling-b" :e "completed" :m "done"}])
             (pr-str out))))))

(deftest progress-digest-false-disables-the-fold-test
  (swap! test-config assoc-in [:hivemind :progress-digest] false)
  (let [out (read-seeded)]
    (is (= 7 (count out)))
    (is (not-any? :n out))
    (is (= (pr-str (pre-seam-rows seeded false)) (pr-str out))))
  (testing "the flag is read per read: flipping it back folds again"
    (pb/reset-all-cursors!)
    (swap! test-config assoc-in [:hivemind :progress-digest] true)
    (is (= (pr-str (pre-seam-rows seeded true)) (pr-str (read-seeded))))))

;; =============================================================================
;; (b) internal fields never reach the wire
;; =============================================================================

(deftest no-ts-and-no-deliberate-on-any-output-row-test
  (register-capture! (k "50-capture"))
  (let [out (read-seeded)]
    (is (seq out))
    (is (not-any? #(contains? % :ts) out))
    (is (not-any? #(contains? % :deliberate?) out))))

;; =============================================================================
;; (c) a registered transform runs and sees the internal fields
;; =============================================================================

(deftest a-registered-transform-is-applied-and-sees-ts-and-deliberate-test
  (let [seen (register-capture! (k "50-capture"))]
    (ext/register! (k "60-shout")
                   (fn [rows _] (mapv #(update % :m str "!") rows)))
    (let [out (read-seeded)
          {:keys [rows ctx]} @seen]
      (testing "applied"
        (is (every? #(.endsWith ^String (:m %) "!") out)))
      (testing "it sees :ts on every row and :deliberate? on the deliberate one"
        (is (every? #(integer? (:ts %)) rows))
        (is (= [true] (keep :deliberate? rows))))
      (testing "it runs after the default fold"
        (is (= 3 (:n (first (filter :n rows))))))
      (testing "the read ctx"
        (is (= {:reader "coordinator" :project-id "global"
                :session-id nil :context-id nil}
               ctx))))))

;; =============================================================================
;; (d) contract guards
;; =============================================================================

(deftest a-transform-outside-the-contract-is-skipped-test
  (let [baseline (do (pb/register-message-source! (constantly seeded))
                     (pb/get-messages "coordinator" :project-id "global"))]
    (doseq [[label f] {"throws"         (fn [_ _] (throw (ex-info "boom" {})))
                       "non-vector"     (fn [rows _] (seq rows))
                       "nil"            (fn [_ _] nil)
                       "malformed :a"   (fn [rows _] (assoc-in rows [0 :a] 1))
                       "missing :e"     (fn [rows _] (update rows 0 dissoc :e))
                       "not a map"      (fn [rows _] (assoc rows 0 "row"))
                       "over-producing" (fn [rows _] (conj rows (first rows)))}]
      (testing label
        (pb/reset-all-cursors!)
        (ext/register! (k "40-bad") f)
        (ext/register! (k "90-later")
                       (fn [rows _] (mapv #(assoc % :m (str "later:" (:m %))) rows)))
        (let [out (pb/get-messages "coordinator" :project-id "global")]
          (is (= (mapv #(str "later:" (:m %)) baseline) (mapv :m out))
              "the bad step passes its input on and the later transform still runs")
          (is (= (count baseline) (count out))))
        (ext/deregister! (k "40-bad"))
        (ext/deregister! (k "90-later"))))))

(deftest a-shrinking-transform-is-accepted-test
  (ext/register! (k "50-last-only") (fn [rows _] [(peek rows)]))
  (is (= [{:a "ling-b" :e "completed" :m "done"}] (read-seeded))))

;; =============================================================================
;; (e) composition order
;; =============================================================================

(deftest transforms-compose-in-sorted-key-order-test
  ;; registered in REVERSE order on purpose
  (ext/register! (k "20-b") (fn [rows _] (mapv #(update % :m str "b") rows)))
  (ext/register! (k "10-a") (fn [rows _] (mapv #(update % :m str "a") rows)))
  (is (= [(keyword "hivemind.rows" "10-a")
          (keyword "hivemind.rows" "20-b")]
         (rt/transform-keys)))
  (is (every? #(.endsWith ^String (:m %) "ab") (read-seeded))))

(deftest the-core-fold-runs-before-keys-that-sort-before-it-test
  (testing "the \"00-\" prefix is not what puts the fold first"
    (let [seen (mapv #(register-capture! (k %)) ["0" "!first" "+plus" "00-a"])
          out  (read-seeded)]
      (is (= (pr-str (pre-seam-rows seeded true)) (pr-str out)))
      (doseq [s seen]
        (is (= 3 (:n (first (filter :n (:rows @s))))))))))

;; =============================================================================
;; (e') core's default is not a registry entry
;; =============================================================================

(deftest the-core-fold-survives-an-extensions-halt-and-reinit-test
  (let [saved-ext     (into {} (map (juxt identity ext/get-extension)) (ext/registered-keys))
        saved-tools   (vec (ext/get-registered-tools))
        schema-atom   @#'ext/schema-ledger
        saved-schemas @schema-atom]
    (try
      (with-redefs [init/load-extensions!    (constantly {:registered 0 :total 0 :sources []})
                    addon-core/shutdown-all! (constantly {})
                    addon-cmds/clear!        (constantly nil)]
        (ig/halt-key! :hive/extensions {:status :running})
        (ig/init-key :hive/extensions {}))
      (testing "no manual re-registration: the fold still collapses bursts"
        (is (= (pr-str (pre-seam-rows seeded true)) (pr-str (read-seeded)))))
      (finally
        (ext/clear-all!)
        (ext/register-many! saved-ext)
        (doseq [t saved-tools] (ext/register-tool! t))
        (reset! schema-atom saved-schemas)))))

(deftest an-addon-cannot-override-or-remove-the-core-fold-test
  (testing "an addon registering the reserved key does not replace the fold"
    (ext/register! pb/progress-fold-key (fn [rows _] (subvec rows 0 1)))
    (is (= (pr-str (pre-seam-rows seeded true)) (pr-str (read-seeded)))))
  (testing "its shutdown, which deregisters every key it listed, does not remove the fold"
    (ext/deregister! pb/progress-fold-key)
    (pb/reset-all-cursors!)
    (is (= (pr-str (pre-seam-rows seeded true)) (pr-str (read-seeded)))))
  (testing "the reserved key is not listed as an addon transform"
    (ext/register! pb/progress-fold-key (fn [rows _] rows))
    (is (not (some #{pb/progress-fold-key} (rt/transform-keys))))
    (ext/deregister! pb/progress-fold-key)))

;; =============================================================================
;; (e'') errors, interrupts, fatal errors
;; =============================================================================

(deftest a-non-fatal-error-from-a-transform-is-skipped-test
  (doseq [[label e] {"AssertionError"     (AssertionError. "pre failed")
                     "NoClassDefFoundError" (NoClassDefFoundError. "addon/Missing")}]
    (testing label
      (pb/reset-all-cursors!)
      (ext/register! (k "40-err") (fn [_ _] (throw e)))
      (ext/register! (k "90-later") (fn [rows _] (mapv #(update % :m str "!") rows)))
      (is (= (mapv #(str (:m %) "!") (pre-seam-rows seeded true)) (mapv :m (read-seeded)))
          "the erroring step is skipped and the later transform still runs")
      (ext/deregister! (k "40-err"))
      (ext/deregister! (k "90-later")))))

(deftest listing-registered-transforms-failing-keeps-the-core-fold-test
  (ext/register! (k "50-shout") (fn [rows _] (mapv #(update % :m str "!") rows)))
  (with-redefs [rt/transform-keys (fn [] (throw (ex-info "registry down" {})))]
    (is (= (pr-str (pre-seam-rows seeded true)) (pr-str (read-seeded)))
        "the core-folded rows are delivered, not the raw input")))

(deftest an-interrupted-reader-keeps-its-interrupt-flag-test
  (ext/register! (k "50-slow") (fn [rows _] (Thread/sleep 200) rows))
  (let [rows [{:a "a" :e "x" :m "1" :ts 1}]
        out  (do (.interrupt (Thread/currentThread))
                 (rt/apply-transforms rows {}))
        flag (Thread/interrupted)]
    (is (= rows out) "the step is skipped and the rows pass on")
    (is (true? flag) "the interrupt is restored, not swallowed")))

(deftest a-fatal-vm-error-is-not-swallowed-test
  (ext/register! (k "50-oom") (fn [_ _] (throw (OutOfMemoryError. "simulated"))))
  (is (thrown? OutOfMemoryError
               (rt/apply-transforms [{:a "a" :e "x" :m "1"}] {}))))

(deftest only-hivemind-rows-keys-are-transforms-test
  (ext/register! :hivemind.other/x (fn [_ _] []))
  (try
    (is (not (some #{:hivemind.other/x} (rt/transform-keys))))
    (is (= 5 (count (read-seeded))))
    (finally (ext/deregister! :hivemind.other/x))))

;; =============================================================================
;; (f) peer-traffic rows bypass the chain
;; =============================================================================

(def ^:private directed
  [{:agent-id "ling-x" :event-type :progress :message "to y" :to "ling-y"
    :context-id "conv-1" :timestamp 2000 :project-id "global"}
   {:agent-id "ling-y" :event-type :progress :message "to x" :to "ling-x"
    :context-id "conv-1" :timestamp 2001 :project-id "global"}
   {:agent-id "ling-z" :event-type :completed :message "root" :timestamp 2002
    :project-id "global"}])

(deftest peer-traffic-rows-never-reach-the-chain-test
  (let [seen (register-capture! (k "50-capture"))]
    (ext/register! (k "60-drop-all") (fn [_ _] []))
    (pb/register-message-source! (constantly directed))
    (let [out (pb/get-messages "coordinator" :project-id "global")]
      (testing "the chain saw only the addressed row"
        (is (= ["root"] (mapv :m (:rows @seen))))
        (is (not-any? #(= "peer-traffic" (:e %)) (:rows @seen))))
      (testing "a transform that drops everything cannot drop the peer summary"
        (is (= ["peer-traffic"] (mapv :e out)))
        (is (= "conv-1" (:ctx (first out))))))))

;; =============================================================================
;; (g) cursors do not depend on the chain
;; =============================================================================

(deftest cursors-advance-identically-when-a-transform-throws-test
  (pb/register-message-source! (constantly (into seeded directed)))
  (pb/get-messages "coordinator" :project-id "global" :session-id "s1")
  (let [clean-cursors @pb/agent-read-cursors]
    (pb/reset-all-cursors!)
    (ext/register! (k "50-boom") (fn [_ _] (throw (ex-info "boom" {}))))
    (let [out (pb/get-messages "coordinator" :project-id "global" :session-id "s1")]
      (is (seq out))
      (is (= clean-cursors @pb/agent-read-cursors))
      (is (= {["s1" "global"] 2002} @pb/agent-read-cursors)))
    (testing "and the next read is empty: nothing is redelivered"
      (is (nil? (pb/get-messages "coordinator" :project-id "global" :session-id "s1"))))))

;; =============================================================================
;; (h) :since belongs to the transform
;; =============================================================================

(deftest since-set-by-a-transform-survives-test
  (ext/register! (k "50-since")
                 (fn [rows _] (mapv #(assoc % :since (:ts %)) rows)))
  (let [out (read-seeded)]
    (is (not-any? #(contains? % :ts) out))
    (testing "the folded row points at the start of its burst"
      (is (= 1000 (:since (first (filter :n out))))))
    (is (= [1001 1003 1004 1000 1006] (mapv :since out)))))

;; =============================================================================
;; (i) the fold keeps the burst's earliest :ts
;; =============================================================================

(deftest digest-keeps-the-minimum-ts-of-a-collapsed-burst-test
  (let [rows [{:a "a" :e "progress" :m "1" :ts 30}
              {:a "b" :e "progress" :m "b" :ts 5}
              {:a "a" :e "progress" :m "2" :ts 10}
              {:a "a" :e "progress" :m "3" :ts 20}
              {:a "a" :e "completed" :m "done" :ts 40}]
        out (audience/digest rows)]
    (is (= [{:a "b" :e "progress" :m "b" :ts 5}
            {:a "a" :e "progress" :m "3" :ts 10 :n 3}
            {:a "a" :e "completed" :m "done" :ts 40}]
           out))
    (testing "rows without :ts gain none"
      (is (not-any? #(contains? % :ts)
                    (audience/digest (mapv #(dissoc % :ts) rows)))))))

;; =============================================================================
;; apply-transforms directly
;; =============================================================================

(deftest apply-transforms-on-an-empty-read-runs-nothing-test
  (let [called (atom false)]
    (ext/register! (k "50-spy") (fn [rows _] (reset! called true) rows))
    (is (= [] (rt/apply-transforms [] {})))
    (is (false? @called))))

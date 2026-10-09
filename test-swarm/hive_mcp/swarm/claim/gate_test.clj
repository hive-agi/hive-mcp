(ns hive-mcp.swarm.claim.gate-test
  "The carto write gate through the REAL claim store (`:swarm-ds` isolation,
   as `roundtrip-test`). Yield requests go to a recording notify fn through
   `gate/*yield-notify!*`; the conflict explainer is installed through
   `gate/register-reason-fn!`. No var is redefined except `registry/acquire!`
   in the fail-open test, which needs an internal error to happen."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.swarm.claim.gate :as gate]
            [hive-mcp.swarm.claim.negotiate :as negotiate]
            [hive-mcp.swarm.claim.registry :as registry]
            [hive-mcp.swarm.claim.span :as span]
            [hive-mcp.swarm.datascript :as ds]
            [hive-test.isolation :as iso]
            hive-mcp.isolation-methods
            [hive-mcp.swarm.datascript.lings :as lings]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- clean-seams [t]
  (negotiate/reset-yield-ledger!)
  (let [prev (gate/register-reason-fn! nil)]
    (try (t)
         (finally
           (gate/register-reason-fn! prev)
           (negotiate/reset-yield-ledger!)))))

(use-fixtures :each (iso/with-isolations :swarm-ds) clean-seams)

(def ^:private f "/repo/src/app/core.clj")

(defn- ling! [id]
  (ds/add-slave! id {:name id :presets [] :cwd "/tmp"})
  id)

(defn- write! [agent-id qn]
  (gate/check-write! {:agent-id agent-id
                      :op       :write-form
                      :spans    [{:file f :qn qn :mode :body}]}))

(defn- backdate-creation!
  "Move the claim on `qn` back `ms` in creation time, leaving any heartbeat."
  [qn ms]
  (ds/transact! [{:claim/file       (span/key-of (span/form-span f qn))
                  :claim/created-at (java.util.Date. (- (System/currentTimeMillis) ms))}]))

(def ^:private eleven-min (* 11 60 1000))

;; =============================================================================
;; Contract
;; =============================================================================

(deftest two-agents-two-forms-of-one-file-are-both-allowed
  (ling! "ling-1")
  (ling! "ling-2")
  (let [a (write! "ling-1" "app.core/alpha")
        b (write! "ling-2" "app.core/delta")]
    (is (= {:allowed? true :acquired 1 :warnings [] :lease-ms 600000} a))
    (is (:allowed? b) (pr-str b))
    (is (= 2 (count (registry/held-spans))))))

(deftest the-same-form-is-refused-and-names-the-holder
  (ling! "ling-1")
  (ling! "ling-2")
  (write! "ling-1" "app.core/alpha")
  (let [sent (atom [])
        r    (binding [gate/*yield-notify!* (fn [& args] (swap! sent conj (vec args)))]
               (write! "ling-2" "app.core/alpha"))
        c    (first (:conflicts r))]
    (is (false? (:allowed? r)))
    (is (= :same-form (:reason c)))
    (is (= "ling-1" (:held-by c)))
    (is (= {:file f :qn "app.core/alpha" :mode :body}
           (select-keys c [:file :qn :mode])))
    (is (string? (:message c)))
    (is (contains? c :why))
    (is (nil? (:why c)) "no explainer registered and none on this classpath")
    (is (re-find #"ling-1" (:hint r)))
    (testing "the holder is asked to yield, once, and the caller is not parked"
      (is (= [["ling-2" "ling-1" :claim/yield-request]]
             (mapv #(subvec % 0 3) @sent)))
      (binding [gate/*yield-notify!* (fn [& args] (swap! sent conj (vec args)))]
        (write! "ling-2" "app.core/alpha"))
      (is (= 1 (count @sent)) "a retry does not repeat the request")
      (is (empty? (ds/q '[:find ?w :where [?w :wait-queue/ling-id "ling-2"]]))))))

(deftest one-agent-across-sessions-is-one-holder
  (ling! "ling-x")
  (is (:allowed? (write! "ling-x:12345" "app.core/alpha")))
  (let [r (write! "ling-x:67890" "app.core/alpha")]
    (is (:allowed? r) (pr-str r)))
  (is (= ["ling-x"] (mapv :claim/slave (registry/held-spans)))))

(deftest a-nil-or-blank-agent-is-skipped
  (is (= {:allowed? true :skipped :no-agent}
         (gate/check-write! {:agent-id nil :spans [{:file f :qn "app.core/alpha"}]})))
  (is (= {:allowed? true :skipped :no-agent}
         (gate/check-write! {:agent-id "  " :spans [{:file f :qn "app.core/alpha"}]})))
  (is (empty? (registry/held-spans))))

(deftest an-internal-error-fails-open-as-degraded
  (ling! "ling-1")
  (with-redefs [registry/acquire! (fn [& _] (throw (ex-info "store down" {})))]
    (let [r (write! "ling-1" "app.core/alpha")]
      (is (true? (:allowed? r)))
      (is (re-find #"store down" (:degraded r))))))

;; =============================================================================
;; Lease and liveness
;; =============================================================================

(deftest an-idle-claim-old-by-creation-stops-blocking
  (ling! "ling-1")
  (ling! "ling-2")
  (write! "ling-1" "app.core/alpha")
  (backdate-creation! "app.core/alpha" eleven-min)
  (is (:allowed? (write! "ling-2" "app.core/alpha"))
      "the control: with no activity since creation the lease has lapsed"))

(deftest a-rewrite-refreshes-the-lease
  (ling! "ling-1")
  (ling! "ling-2")
  (write! "ling-1" "app.core/alpha")
  (is (:allowed? (write! "ling-1" "app.core/alpha")) "rewriting one's own form")
  (backdate-creation! "app.core/alpha" eleven-min)
  (let [r (write! "ling-2" "app.core/alpha")]
    (is (false? (:allowed? r))
        "created 11 min ago but touched just now: the lease is live")
    (is (= "ling-1" (:held-by (first (:conflicts r))))))
  (testing "the rewrite was a refresh: creation kept, activity stamped"
    (let [k   (span/key-of (span/form-span f "app.core/alpha"))
          row (ds/pull '[:claim/created-at :claim/heartbeat-at] [:claim/file k])]
      (is (> (- (System/currentTimeMillis) (inst-ms (:claim/created-at row)))
             (* 10 60 1000)))
      (is (some? (:claim/heartbeat-at row))))))

(deftest a-refreshed-lease-also-fences-the-unchecked-claim-path
  (testing "claim-file! (the store upsert behind every claim) honours the lease too"
    (ling! "ling-1")
    (ling! "ling-2")
    (write! "ling-1" "app.core/alpha")
    (write! "ling-1" "app.core/alpha")
    (backdate-creation! "app.core/alpha" eleven-min)
    (let [k (span/key-of (span/form-span f "app.core/alpha"))
          r (lings/claim-file! k "ling-2")]
      (is (= :held-by-live-slave (:refused r)) (pr-str r)))))

(deftest a-legacy-row-without-a-heartbeat-ages-by-creation
  (is (= 5 (registry/last-active-ms {:created-at 5})))
  (is (= 9 (registry/last-active-ms {:created-at 5 :touched-at 9})))
  (is (= 9 (registry/last-active-ms {:claim/created-at 9 :heartbeat-at 5})))
  (is (nil? (registry/last-active-ms {}))))

(deftest a-dead-holder-never-blocks
  (ling! "ling-1")
  (ling! "ling-2")
  (write! "ling-1" "app.core/alpha")
  (ds/update-slave! "ling-1" {:slave/status :killed})
  (let [r (write! "ling-2" "app.core/alpha")]
    (is (:allowed? r) (pr-str r))
    (is (= ["ling-2"] (mapv :claim/slave (registry/held-spans))))))

;; =============================================================================
;; Explainer seam
;; =============================================================================

(deftest the-reason-fn-is-used-when-resolvable
  (ling! "ling-1")
  (ling! "ling-2")
  (write! "ling-1" "app.core/alpha")
  (let [seen (atom nil)]
    (gate/register-reason-fn!
     (fn [_callers held wanted agent]
       (reset! seen {:agent agent :qn (:span/qn wanted) :held (count held)})
       [{:file f :qn "app.core/alpha" :held-by "ling-1"
         :why {:rule :same-form :because "both edit alpha"}}]))
    (let [r (write! "ling-2" "app.core/alpha")]
      (is (= {:agent "ling-2" :qn "app.core/alpha" :held 1} @seen))
      (is (= {:rule :same-form :because "both edit alpha"}
             (:why (first (:conflicts r))))))))

(deftest a-throwing-reason-fn-leaves-why-nil
  (ling! "ling-1")
  (ling! "ling-2")
  (write! "ling-1" "app.core/alpha")
  (gate/register-reason-fn! (fn [& _] (throw (ex-info "boom" {}))))
  (let [r (write! "ling-2" "app.core/alpha")]
    (is (false? (:allowed? r)))
    (is (nil? (:why (first (:conflicts r)))))))

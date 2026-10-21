(ns hive-mcp.tools.memory.handlers-contract-test
  "Contract facets for the store-backed memory handlers (analytics, decay,
   promotion, lifecycle, reconcile, review) and the pure plan helpers. The
   memory store is the port: a fresh stub store
   (hive-mcp.test.stub.memory-store) is installed for every test."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.test.stub.memory-store :as stub]
            [hive-mcp.tools.core :as tcore]
            [hive-mcp.tools.memory.analytics :as analytics]
            [hive-mcp.tools.memory.decay :as decay]
            [hive-mcp.tools.memory.lifecycle :as lifecycle]
            [hive-mcp.tools.memory.promotion :as promo]
            [hive-mcp.tools.memory.reconcile :as reconcile]
            [hive-mcp.tools.memory.review :as review]
            [hive-mcp.tools.memory.schema :as ms]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.memory.registry :as registry]
            [malli.core :as m]
            [malli.generator :as mg]))


;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private seed-entries
  [{:id "e1" :type "note" :duration "short" :tags ["scope:project:hive-mcp"]
    :content "hello" :project-id "hive-mcp" :access-count 4}
   {:id "e2" :type "decision" :duration "long"
    :tags ["scope:global" "xpoll:project:a" "xpoll:project:b"]
    :content "x" :project-id "p2"}])

(defn- stub-store-fixture
  "Install a fresh stub store seeded with `seed-entries`, then restore the
   registry. KG edge removal is the other port these handlers touch; the
   expire/cleanup paths get a no-op so they never reach a KG store."
  [f]
  (let [prior (registry/registered-stores)]
    (try
      (stub/install! (stub/->stub seed-entries))
      (with-redefs [kg-edges/remove-edges-for-node! (fn [_] 0)]
        (f))
      (finally
        (registry/reset-registry!)
        (doseq [[k s] prior] (registry/register-store! k s))))))

(use-fixtures :each stub-store-fixture)

(defn- body [resp] (json/read-str (:text resp) :key-fn keyword))

;; =============================================================================
;; Pure helpers
;; =============================================================================

(deftrifecta-from-schema walk-tiers-climbs-at-most-max-tiers
  #'promo/walk-tiers
  {:in [:cat ms/DurationName [:int {:min 0 :max 6}]]
   :out ms/TierWalk
   :rel (fn [[d n] {:keys [final-duration tiers-promoted]}]
          (let [order ["ephemeral" "short" "medium" "long" "permanent"]
                i (.indexOf ^java.util.List order d)
                j (min (+ i n) 4)]
            (and (= final-duration (nth order j))
                 (= tiers-promoted (- j i)))))
   :golden-path "test/golden/hive-mcp/tools/memory/walk-tiers.edn"
   :num-tests 200})

(deftrifecta-from-schema normalize-cleanup-result-reads-every-store-shape
  #'lifecycle/normalize-cleanup-result
  {:in [:cat [:or :nil [:int {:min 0 :max 9}]
              [:map {:closed true}
               [:count {:optional true} [:int {:min 0 :max 9}]]
               [:deleted-ids {:optional true} [:vector {:max 3} [:enum "a" "b" "c"]]]
               [:repaired {:optional true} [:int {:min 0 :max 3}]]]]]
   :out ms/CleanupResult
   :rel (fn [[r] out]
          (cond (integer? r) (= {:count r :deleted-ids [] :repaired 0} out)
                (map? r) (= (or (:count r) (count (:deleted-ids r))) (:count out))
                :else (= {:count 0 :deleted-ids [] :repaired 0} out)))
   :classify (fn [[r] _] (cond (integer? r) :int (map? r) :map :else :nil))
   :classify-domain #{:int :map :nil}
   :num-tests 200})

(deftrifecta-from-schema reconcile-not-wired-without-sources
  #'reconcile/reconcile-response
  {:in [:cat [:maybe ms/ReconcileSources] [:or :nil [:int {:min -1 :max 5}] [:enum "3" "x"]]]
   :out ms/ReconcileResponse
   :rel (fn [[sources _] out]
          (= (if sources "ok" "not-wired") (:status out)))
   :classify (fn [[s] _] (if s :wired :not-wired))
   :classify-domain #{:wired :not-wired}
   :num-tests 100})

(deftrifecta-from-schema group-queues-keeps-every-queue
  #'review/group-queues
  {:in [:cat [:vector {:max 3} [:tuple [:enum "axiom-candidate" "friction-candidate"]
                                [:vector {:max 2} [:map [:id [:enum "a" "b"]]]]]]]
   :out ms/QueueGroups
   :rel (fn [[rows] out] (= (set (map first rows)) (set (keys out))))
   :golden-path "test/golden/hive-mcp/tools/memory/group-queues.edn"
   :num-tests 100})

;; =============================================================================
;; Store-backed handlers
;; =============================================================================

(deftrifecta-from-schema feedback-counts-or-refuses
  #'analytics/handle-feedback
  {:in [:cat [:map {:closed true}
              [:id [:enum "e1" "e2" "nope"]]
              [:feedback [:enum "helpful" "unhelpful" "bogus"]]]]
   :out tcore/McpResponse
   :rel (fn [[{:keys [id feedback]}] out]
          (cond (= "nope" id) (= {:error "Entry not found"} (body out))
                (= "bogus" feedback) (true? (:isError out))
                :else (= id (:id (body out)))))
   :num-tests 60})

(deftrifecta-from-schema helpfulness-ratio-is-a-fraction
  #'analytics/handle-helpfulness-ratio
  {:in [:cat [:map {:closed true} [:id [:enum "e1" "e2" "nope"]]]]
   :out tcore/McpResponse
   :rel (fn [[{:keys [id]}] out]
          (let [b (body out)]
            (if (= "nope" id) (= "Entry not found" (:error b)) (contains? b :ratio))))
   :num-tests 40})

(deftrifecta-from-schema promote-entry-by-tiers-reports-the-move
  #'promo/promote-entry-by-tiers!
  {:in [:cat [:map {:closed true} [:id [:enum "e1" "e2"]] [:duration ms/DurationName]]
        [:int {:min 0 :max 3}]]
   :out ms/Promotion
   :rel (fn [[{:keys [duration]} n] out]
          (= (:promoted out) (and (pos? n) (not= "permanent" duration))))
   :classify (fn [_ out] (if (:promoted out) :promoted :held))
   :classify-domain #{:promoted :held}
   :num-tests 150 :classify-floor 11})

(deftest decay-and-promotion-reports
  (is (m/validate ms/DecayCycle (decay/run-decay-cycle! {})))
  (is (m/validate ms/XpollCycle (promo/run-xpoll-cycle! {})))
  (is (m/validate ms/DecayCycle (lifecycle/run-decay-cycle! {})))
  (is (m/validate ms/XpollCycle (lifecycle/run-xpoll-cycle! {})))
  (is (every? #(m/validate ms/Entry %) (decay/query-scoped-entries nil 10)))
  (is (m/validate ms/DecayPlan (decay/build-decay-plan (second seed-entries)
                                                       {:access-threshold 3 :recency-days 7})))
  (is (m/validate [:maybe ms/DecayApplied]
                  (decay/apply-decay! (second seed-entries) {:access-threshold 3 :recency-days 7})))
  (is (= ["a" "b"] (:cross_projects (promo/build-xpoll-plan (second seed-entries)))))
  (doseq [r [(decay/handle-decay {:dry_run true}) (lifecycle/handle-decay {:dry_run true})
             (promo/handle-xpoll-promote {:dry_run true}) (lifecycle/handle-xpoll-promote {:dry_run true})]]
    (is (true? (:dry_run (body r))))))

(deftest lifecycle-handlers-answer-mcp-responses
  ;; One store per test: each step sees the previous step's write.
  (is (= 5 (:access-count (body (analytics/handle-log-access {:id "e1" :directory "/tmp/zz"})))))
  (is (= "medium" (:duration (body (lifecycle/handle-set-duration {:id "e1" :duration "medium"})))))
  (is (:isError (lifecycle/handle-set-duration {:id "zz" :duration "medium"})))
  (is (= "long" (:duration (body (lifecycle/handle-promote {:id "e1"})))))
  (is (= "medium" (:duration (body (lifecycle/handle-demote {:id "e1"})))))
  (is (:isError (lifecycle/handle-demote {:id "zz"})))
  (is (= [] (body (lifecycle/handle-expiring-soon {:directory "/tmp/x"}))))
  (is (= 0 (:deleted (body (lifecycle/handle-cleanup-expired {})))))
  (is (= "e1" (:expired (body (lifecycle/handle-expire {:id "e1"}))))))


(deftest reconcile-and-review-handlers
  (reconcile/clear-sources!)
  (is (= "not-wired" (:status (body (reconcile/handle-reconcile {})))))
  (let [src (mg/generate ms/ReconcileSources)]
    (try
      (is (identical? src (reconcile/set-sources! src)))
      (is (= "ok" (:status (body (reconcile/handle-reconcile {:limit 5})))))
      (finally (reconcile/clear-sources!))))

  (is (set? (review/queue-types)))
  (is (:isError (review/handle-review {:id "e1" :verdict "x"})))
  (is (:isError (review/handle-review {:id "e1" :verdict "approve"}))))

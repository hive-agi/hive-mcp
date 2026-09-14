(ns hive-mcp.tools.consolidated.workflow.execution-routing-test
  "Trifecta for execution routing under an unroutable spawn mode.

   Property and schema-mutation facets are SYNTHESIZED from the malli shapes by
   hive-schemas.test. Hand-written below only what a schema cannot state: the
   partition is a stable split of the input, and a plausible regression (the
   old whole-batch behaviour, a lost task, a reordered report) is caught."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dsl.adt :as adt]
            [hive-schemas.test :as hst]
            [hive-test.mutation :as mut]
            [hive-mcp.tools.consolidated.workflow.execution-routing :as er]))

;; ============================================================================
;; Schema-synthesized
;; ============================================================================

;; THE THEOREM: every task lands in exactly one side, in input order; accepted
;; tasks carry no routing and every rejection names a routed task.
(hst/deftrifecta-from-schema partition-is-a-stable-split
  hive-mcp.tools.consolidated.workflow.execution-routing/reject-execution-routed
  ;; Short vectors: the relation is length-independent, and an all-routed batch
  ;; of 20 random tasks is too rare for :classify to see it reached.
  {:in [:cat er/UnroutableMode [:vector {:max 4} er/ForgeTask]]
   :out er/Partition
   :rel (fn [[route tasks] {:keys [accepted rejected]}]
          (and (= (mapv :id (remove er/execution-routed? tasks)) (mapv :id accepted))
               (= (mapv :id (filter er/execution-routed? tasks)) (mapv :task-id rejected))
               (not-any? er/execution-routed? accepted)
               (every? #(= route (:route %)) rejected)))
   :classify (fn [[_ tasks] {:keys [accepted rejected]}]
               (cond (empty? tasks)    :empty
                     (empty? rejected) :all-dispatched
                     (empty? accepted) :all-rejected
                     :else             :mixed))
   :classify-domain #{:empty :all-dispatched :all-rejected :mixed}
   :classify-floor 5
   :num-tests 300})

;; A routed task is exactly what `execution-routed?` must recognize. The schema
;; states only what the predicate decides on: a corruption of a field it never
;; reads (say :id) would rightly leave the answer unchanged.
(hst/deftrifecta-predicate execution-routed-detection
  hive-mcp.tools.consolidated.workflow.execution-routing/execution-routed?
  {:schema [:map
            [:context [:map [:execution [:map-of {:min 1} :keyword :any]]]]]})

;; ============================================================================
;; Hand-written: the sum type and the regressions a schema cannot name
;; ============================================================================

(def ^:private routed {:id "t-routed" :title "Routed"
                       :context {:execution {:provider "venice" :model "m"}}})
(def ^:private plain  {:id "t-plain" :title "Plain"})
(def ^:private untitled-routed {:id "t-untitled" :context {:execution {:provider "p"}}})

(deftest disposition-is-a-closed-sum
  (is (= #{:disposition/dispatch :disposition/reject}
         (set (adt/type-variants :ForgeTaskDisposition))))
  (is (= :disposition/reject (adt/adt-variant (er/disposition routed))))
  (is (= :disposition/dispatch (adt/adt-variant (er/disposition plain))))
  (is (= :disposition/dispatch
         (adt/adt-variant (er/disposition {:id "t" :context {:execution {}}})))
      "empty execution settings route nothing"))

(mut/deftest-mutations partition-regressions-are-caught
  hive-mcp.tools.consolidated.workflow.execution-routing/reject-execution-routed
  [["the old behaviour: reject the whole batch when any task is routed"
    (fn [route tasks]
      (if (some er/execution-routed? tasks)
        {:accepted [] :rejected (mapv #(hash-map :task-id (:id %) :route route) tasks)}
        {:accepted (vec tasks) :rejected []}))]
   ["drops routed tasks silently instead of reporting them"
    (fn [_ tasks] {:accepted (vec (remove er/execution-routed? tasks)) :rejected []})]
   ["dispatches routed tasks anyway"
    (fn [_ tasks] {:accepted (vec tasks) :rejected []})]
   ["reports rejections in reverse order"
    (fn [route tasks]
      (let [{:keys [accepted rejected]}
            (reduce (fn [acc t]
                      (if (er/execution-routed? t)
                        (update acc :rejected conj {:task-id (:id t) :route route})
                        (update acc :accepted conj t)))
                    {:accepted [] :rejected []} tasks)]
        {:accepted accepted :rejected (vec (reverse rejected))}))]]
  (fn []
    (let [{:keys [accepted rejected]}
          (er/reject-execution-routed :orchestrator [routed plain untitled-routed])]
      (testing "the plain task is dispatched, alone"
        (is (= [plain] accepted)))
      (testing "both routed tasks are reported, in task order"
        (is (= ["t-routed" "t-untitled"] (mapv :task-id rejected)))
        (is (every? #(= :execution/unsupported-mode (:type %)) rejected))))))

(deftest rejection-title-falls-back-to-the-id
  (let [{:keys [rejected]} (er/reject-execution-routed :orchestrator [untitled-routed routed])]
    (is (= ["t-untitled" "Routed"] (mapv :task-title rejected)))
    (is (every? false? (map :spawned rejected)))))

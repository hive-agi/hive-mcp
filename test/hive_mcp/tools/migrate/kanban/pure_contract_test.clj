(ns hive-mcp.tools.migrate.kanban.pure-contract-test
  "Schema-driven facets for the kanban migrator's pure core, generated from
   the contracts in hive-mcp.tools.migrate.kanban.pure: the writer answer
   fold, the classifier, the cursor hold and the batch slicer. Each subject
   is the contracted var itself."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.migrate.kanban.pure :as pure]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Answer
  "Every add-entry! answer shape write-result folds, one arm per outcome."
  [:or
   [:enum "id-1" "id-2"]
   [:map [:queued? [:= true]]]
   [:map [:error [:enum "refused" "boom"]]]
   [:enum nil "" 42 :kw]])

(deftrifecta-from-schema write-result-folds-every-answer
  #'pure/write-result
  {:in [:cat [:enum "id-1" "id-2"] Answer]
   :out pure/WriteRow
   :rel (fn [[id answer] out]
          (and (= id (:id out))
               (cond
                 (and (string? answer) (seq answer)) (true? (:ok? out))
                 (and (map? answer) (:queued? answer)) (true? (:queued? out))
                 :else (and (false? (:ok? out)) (not (:queued? out))))))
   :classify (fn [_ out] (cond (:ok? out) :landed (:queued? out) :queued :else :failed))
   :classify-domain #{:landed :queued :failed}
   :num-tests 200})

(def ^:private Card
  "Target rows: an opened card (kanban or not), a sealed envelope, an empty
   stub, or no row."
  [:maybe [:or
           [:map [:id [:= "x"]] [:content [:map [:task-type [:enum "kanban" "note"]]
                                           [:title {:optional true} [:= "t"]]]]]
           [:map [:id [:= "x"]] [:content [:enum "sealed-envelope"]]]
           [:map [:id [:= "x"]] [:content [:map-of {:max 0} :keyword :any]]]]])

(def ^:private Source
  "Source rows: mostly kanban cards so every target branch is reached."
  [:or
   [:map [:id [:= "x"]] [:content [:map [:task-type [:= "kanban"]] [:title [:= "t"]]]]]
   [:maybe [:map [:id [:= "x"]] [:content [:map [:task-type [:= "note"]]]]]]])

(deftrifecta-from-schema classify-outcome-is-the-decision-table
  #'pure/classify-outcome
  {:in [:cat Source Card]
   :out pure/ClassifierOutcome
   :rel (fn [[src tgt] out]
          (= out (cond
                   (nil? src) :missing-from-source
                   (not (pure/task-type-kanban? src)) :not-task
                   (and tgt (pure/task-type-kanban? tgt) (pure/full-payload? tgt)) :already-full
                   (or (nil? tgt) (pure/stub-row? tgt)) :ready-to-write
                   :else :unreadable)))
   :classify (fn [_ out] out)
   :classify-domain pure/ClassifierOutcome
   :classify-floor 4
   :num-tests 400})

(def ^:private Outcomes
  [:vector {:max 5} [:map [:outcome [:enum :written :queued :unconfirmed :failed :already-full]]]])

(deftrifecta-from-schema hold-cursor-holds-on-unconfirmed-then-queued
  #'pure/hold-cursor
  {:in [:cat [:int {:min 0 :max 20}] Outcomes [:int {:min 0 :max 30}] :boolean]
   :out [:tuple pure/Cursor :boolean]
   :rel (fn [[start outcomes end done?] out]
          (let [q (first (keep-indexed (fn [i o] (when (= :queued (:outcome o)) i)) outcomes))]
            (= out (cond
                     (some #(= :unconfirmed (:outcome %)) outcomes) [start false]
                     q [(+ start q) false]
                     :else [end done?]))))
   :classify (fn [[_ outcomes] _]
               (cond (some #(= :unconfirmed (:outcome %)) outcomes) :unconfirmed
                     (some #(= :queued (:outcome %)) outcomes) :queued
                     :else :clear))
   :classify-domain #{:unconfirmed :queued :clear}
   :num-tests 300})

(deftrifecta-from-schema slice-batch-never-reads-past-the-end
  #'pure/slice-batch
  {:in [:cat [:vector {:max 12} [:enum "a" "b" "c" "d"]] [:int {:min -2 :max 15}] [:int {:min 1 :max 5}]]
   :out [:tuple [:vector pure/Id] pure/Cursor :boolean]
   :rel (fn [[ids cursor size] [batch new-cursor done?]]
          (let [start (max 0 (min cursor (count ids)))]
            (and (= batch (subvec ids start (min (count ids) (+ start size))))
                 (= new-cursor (min (count ids) (+ start size)))
                 (= done? (>= new-cursor (count ids))))))
   :num-tests 200})

(golden/deftest-golden migrate-pure-golden
  "test/golden/hive-mcp/tools/migrate/kanban-pure.edn"
  (sorted-map
   :write-landed   (pure/write-result "id" "id")
   :write-queued   (pure/write-result "id" {:queued? true})
   :write-refused  (pure/write-result "id" {:success? false :error "full"})
   :write-odd      (pure/write-result "id" 42)
   :reason-nil     (pure/unreadable-reason nil)
   :reason-sealed  (pure/unreadable-reason {:content "envelope"})
   :reason-card    (pure/unreadable-reason {:content {:task-type "note"}})
   :tally          (pure/tally-outcomes [{:outcome :written} {:outcome :written} {:outcome :failed}])
   :dedup          (pure/dedup-sorted-ids [["b" "a"] ["a" "c"]])
   :settled        (pure/settled-outcomes 4 6 [{:id "a"} {:id "b"} {:id "c"}])
   :fold           (pure/fold-batch {} {:start 0 :cursor 2 :done? false
                                         :outcomes [{:id "a" :outcome :written}
                                                    {:id "b" :outcome :failed :error "x"}]}
                                    "2026-10-20T00:00:00Z")))

(deftest tally-always-answers-every-outcome
  (is (= (set (keys pure/empty-tally)) (set (keys (pure/tally-outcomes []))))))

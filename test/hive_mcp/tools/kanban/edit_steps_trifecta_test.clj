(ns hive-mcp.tools.kanban.edit-steps-trifecta-test
  "Schema-driven trifectas for the pure edit steps of the transitions layer
   (`kt/apply-tag-deltas`, `kt/tag-diff`, `kt/edit-changed`, `kt/edit-plan`)
   and the effect builders (`fxb/*`) that `events/edit-fx` and
   `update/update-fx` are now thin pipelines over.

   The :rel of each trifecta is the behaviour the inline let of edit-fx had,
   so the steps cannot drift from it."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.kanban.events :as events]
            [hive-mcp.tools.kanban.fx-builders :as fxb]
            [hive-mcp.tools.kanban.transitions :as kt]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Tag [:enum "kanban" "todo" "priority-medium" "scope:project:p" "run:R1"
                    "verdict:pass" "cleanup:pending"])

(def ^:private Tags [:vector {:max 5} Tag])

(def ^:private Deltas
  [:map
   [:add-tags {:optional true} [:maybe Tags]]
   [:remove-tags {:optional true} [:maybe Tags]]])

(def ^:private Entry
  [:map {:closed true}
   [:tags Tags]
   [:content [:map {:closed true}
              [:task-type [:= "kanban"]]
              [:title [:enum "t" "u"]]
              [:status [:= "todo"]]
              [:priority [:enum "high" "medium" "low"]]]]])

(def ^:private EditsIn
  [:map {:closed true}
   [:title {:optional true} [:maybe [:enum "t" "u" " "]]]
   [:priority {:optional true} [:maybe [:enum "high" "medium" "low"]]]])

;; -----------------------------------------------------------------------------
;; apply-tag-deltas: removal wins, distinct, base order kept
;; -----------------------------------------------------------------------------

(deftrifecta-from-schema apply-tag-deltas-contract
  #'kt/apply-tag-deltas
  {:in  [:cat Tags Deltas]
   :out [:vector :string]
   :rel (fn [[tags {:keys [add-tags remove-tags]}] out]
          (and (= out (distinct out))
               (not-any? (set remove-tags) out)
               (every? (set out) (remove (set remove-tags) (concat tags add-tags)))
               (every? (set (concat tags add-tags)) out)))
   :num-tests 100})

(deftest retag-transition-shares-the-delta-algorithm
  (let [entry {:tags ["kanban" "todo" "scope:project:p"] :content {:task-type "kanban"}}
        d     {:add-tags ["run:R1" "kanban"] :remove-tags ["todo"]}]
    (is (= (kt/apply-tag-deltas (:tags entry) d)
           (:new-tags (kt/retag-transition entry nil d))))))

;; -----------------------------------------------------------------------------
;; tag-diff: the audit diff reconstructs new from old
;; -----------------------------------------------------------------------------

(deftrifecta-from-schema tag-diff-contract
  #'kt/tag-diff
  {:in  [:cat Tags Tags]
   :out [:map {:closed true} [:added-tags [:vector :string]] [:removed-tags [:vector :string]]]
   :rel (fn [[old new] {:keys [added-tags removed-tags]}]
          (= (set new)
             (into (apply disj (set old) removed-tags) added-tags)))
   :num-tests 100})

;; -----------------------------------------------------------------------------
;; edit-changed: :tags only when a delta actually moved the vector
;; -----------------------------------------------------------------------------

(deftrifecta-from-schema edit-changed-contract
  #'kt/edit-changed
  {:in  [:cat Entry [:set [:enum :title :priority]] [:maybe Tags] Deltas]
   :out [:set [:enum :title :description :priority :tags]]
   :rel (fn [[entry fc new-tags deltas] out]
          (= out (cond-> (set fc)
                   (and (kt/tag-deltas? deltas) (not= new-tags (:tags entry))) (conj :tags))))
   :num-tests 100})

;; -----------------------------------------------------------------------------
;; edit-plan: the pipeline the old inline let computed
;; -----------------------------------------------------------------------------

(defn- legacy-edit
  "The edit-fx let before the refactor, verbatim in substance: the oracle."
  [entry edits {:keys [add-tags remove-tags]}]
  (let [{:keys [changed new-content new-tags old-title new-title old-priority new-priority]}
        (kt/edit-transition entry edits)
        deltas?   (or (seq add-tags) (seq remove-tags))
        base-tags (or new-tags (vec (:tags entry)))
        new-tags  (if deltas?
                    (:new-tags (kt/retag-transition (assoc entry :tags base-tags) nil
                                                    {:add-tags add-tags :remove-tags remove-tags}))
                    new-tags)
        changed   (cond-> (set changed)
                    (and deltas? (not= new-tags (vec (:tags entry)))) (conj :tags))]
    {:new-content new-content
     :new-tags    new-tags
     :changed     changed
     :audit       (when (seq changed)
                    (cond-> {:changed      (vec (sort (map name changed)))
                             :old-title    old-title :new-title new-title
                             :old-priority old-priority :new-priority new-priority}
                      (contains? changed :tags)
                      (assoc :added-tags   (vec (remove (set (:tags entry)) new-tags))
                             :removed-tags (vec (remove (set new-tags) (:tags entry))))))}))

(deftrifecta-from-schema edit-plan-matches-the-legacy-handler
  #'kt/edit-plan
  {:in  [:cat Entry EditsIn Deltas]
   :out [:map {:closed true}
         [:new-content :map]
         [:new-tags [:maybe [:vector :string]]]
         [:changed [:set [:enum :title :description :priority :tags]]]
         [:audit [:maybe [:map [:changed [:vector :string]]]]]]
   :rel (fn [[entry edits deltas] out] (= out (legacy-edit entry edits deltas)))
   :classify (fn [_ {:keys [changed]}]
               (cond (empty? changed) :no-op (changed :tags) :tags :else :fields))
   :classify-domain #{:no-op :tags :fields}
   :num-tests 200})

;; -----------------------------------------------------------------------------
;; effect builders
;; -----------------------------------------------------------------------------

(deftrifecta-from-schema edit-payload-carries-tags-only-when-written
  #'fxb/edit-payload
  {:in  [:cat [:map {:closed true} [:title [:enum "t"]]] [:maybe Tags]]
   :out [:map {:closed true} [:content :map] [:tags {:optional true} [:vector :string]]]
   :rel (fn [[content tags] out]
          (and (= content (:content out)) (= (some? tags) (contains? out :tags))))
   :num-tests 50})

(deftest with-temporal-record-is-a-no-op-on-nil-data
  (is (= {:a 1} (fxb/with-temporal-record {:a 1} "id" :kanban-edit nil "p")))
  (is (= {:entry-id "id" :op :kanban-edit :data {:changed ["title"]} :project-id "p"}
         (:x (fxb/with-temporal-record {} :x "id" :kanban-edit {:changed ["title"]} "p")))))

(deftest edit-fx-is-the-plan-assembled
  (testing "edit-fx = facade-update of the plan + its audit, nothing else"
    (let [entry {:id "e" :tags ["kanban" "todo" "priority-medium" "scope:project:p" "cleanup:pending"]
                 :content {:task-type "kanban" :title "t" :status "todo" :priority "medium"}}
          ev    {:task-id "e" :priority "high" :add-tags ["verdict:pass"]
                 :remove-tags ["cleanup:pending"]}
          plan  (kt/edit-plan entry {:priority "high"} (select-keys ev [:add-tags :remove-tags]))
          fx    (events/edit-fx {:kanban/entry entry} [:kanban/edit ev])]
      (is (= #{:kanban/facade-update :kanban/temporal-record} (set (keys fx))))
      (is (= {:content (:new-content plan) :tags (:new-tags plan)}
             (get-in fx [:kanban/facade-update :payload])))
      (is (= (:audit plan) (get-in fx [:kanban/temporal-record :data])))
      (testing "the diff covers the priority move and the deltas of the same commit"
        (is (= ["priority-high" "verdict:pass"]
               (get-in fx [:kanban/temporal-record :data :added-tags])))
        (is (= ["priority-medium" "cleanup:pending"]
               (get-in fx [:kanban/temporal-record :data :removed-tags])))))))

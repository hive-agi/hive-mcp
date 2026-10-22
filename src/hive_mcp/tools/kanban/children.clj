(ns hive-mcp.tools.kanban.children
  "Open KG children of a kanban card, surfaced when the card is completed.

   Read-only: the caller decides whether to close or re-parent each child;
   nothing here writes to the store or the KG.

   A child is a kanban entry linked to the card by one of:
   - card  --contains-->     child
   - child --derived-from--> card
   - child --depends-on-->   card
   - child --implements-->   card
   and whose status is not done."
  (:require [hive-mcp.tools.kanban.predicates :as kp]
            [hive-mcp.tools.kanban.transitions :as kt]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def child-links
  "[relation direction] pairs that make the far end a child of the card."
  #{[:contains :outgoing]
    [:derived-from :incoming]
    [:depends-on :incoming]
    [:implements :incoming]})

(defn- relation-kw [r]
  (cond (keyword? r) r
        (string? r)  (keyword r)
        :else        nil))

(defn- linked-ids
  "Ordered, de-duplicated [child-id relation direction] triples for `card-id`."
  [card-id edges-from edges-to]
  (let [pick    (fn [edges direction far-key]
                  (for [e edges
                        :let [rel (relation-kw (:kg-edge/relation e))
                              far (get e far-key)]
                        :when (and far (not= far card-id)
                                   (contains? child-links [rel direction]))]
                    [far rel direction]))
        triples (concat (pick edges-from :outgoing :kg-edge/to)
                        (pick edges-to :incoming :kg-edge/from))]
    (second
     (reduce (fn [[seen acc] [id :as t]]
               (if (seen id) [seen acc] [(conj seen id) (conj acc t)]))
             [#{} []]
             triples))))

(defn open-children
  "Open kanban children of `card-id`, as
   [{:id :title :status :priority :relation :direction} ...].

   `deps` injects the reads so the function runs on stubs:
     :edges-from (fn [id] -> seq of KG edge maps leaving id)
     :edges-to   (fn [id] -> seq of KG edge maps arriving at id)
     :get-entry  (fn [id] -> memory entry or nil)
   Non-kanban neighbours, missing entries and done cards are skipped."
  [card-id {:keys [edges-from edges-to get-entry]}]
  (vec
   (for [[id rel direction] (linked-ids card-id (edges-from card-id) (edges-to card-id))
         :let [entry (get-entry id)]
         :when (and entry (kp/kanban-entry? entry))
         :let [slim (kt/task->slim (assoc entry :id id))]
         :when (not (kp/done? (:status slim)))]
     (assoc slim :relation (name rel) :direction (name direction)))))

(m/=> open-children [:=> [:cat :string [:map
                                        [:edges-from ifn?]
                                        [:edges-to ifn?]
                                        [:get-entry ifn?]]]
                     [:vector :map]])

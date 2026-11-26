(ns hive-mcp.tools.kanban.diff
  "Pure core of `kanban diff` (RUN-LEDGER-5, CPPB Promote stratum).

   Two agentic runs are two tag sets (run:AGT-1125R3 vs run:AGT-1125R4). Each
   step card carries scenario:<card> and persona:<p>, and a closed one carries
   one verdict:<v> (convention 20261125100357-6730324c). This ns keys every
   card of each side by scenario + persona, lines the two sides up and
   classifies each key:

     flipped      both runs hold a verdict and they differ
     same         both runs hold the same verdict
     only-a       the key is in run A only
     only-b       the key is in run B only
     unverdicted  the key is in both runs and at least one holds no verdict

   `request` coerces the boundary params; `diff` is the whole promotion. The
   boards arrive as data: the boundary (memory-kanban.query/diff*) reads them
   through the same board source `kanban list` uses."
  (:require [clojure.string :as str]
            [hive-mcp.tools.kanban.diff.schema :as s]
            [hive-mcp.tools.kanban.list.plan :as plan]
            [hive-mcp.tools.kanban.predicates :as kp]
            [hive-mcp.tools.kanban.schema :as ks]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def scenario-prefix "scenario:")
(def persona-prefix "persona:")
(def verdict-prefix "verdict:")

(defn request
  "Coerce the boundary params of `kanban diff`: tags_a and tags_b, each a tag
   or a collection of tags, both required and non-empty. Answers {:ok
   DiffRequest} or {:error message}."
  [{:keys [tags_a tags_b]}]
  (let [a (kp/coerce-tags tags_a)
        b (kp/coerce-tags tags_b)
        bad (fn [k v] (cond (nil? v)        (str k " is required: the tags every card of that run carries, e.g. [\"run:AGT-1125R3\"]")
                            (= :invalid v)  (str k " must be a tag or a list of tags")))]
    (if-let [msg (or (bad "tags_a" a) (bad "tags_b" b))]
      {:error msg}
      {:ok {:tags-a a :tags-b b}})))

(defn tag-values
  "The non-blank values of every string tag in `tags` that starts with
   `prefix`, in tag order."
  [prefix tags]
  (into []
        (comp (filter string?)
              (filter #(str/starts-with? % prefix))
              (map #(subs % (count prefix)))
              (remove str/blank?))
        tags))

(defn card-key
  "What a card is matched on across runs: its first scenario:<x> and
   persona:<p> values. A card with neither is keyed by its own id, so it
   never matches a card of the other run."
  [{:keys [id tags]}]
  (let [scenario (first (tag-values scenario-prefix tags))
        persona  (first (tag-values persona-prefix tags))
        k        (if (or scenario persona)
                   (str/join " " (remove nil? [(some->> scenario (str scenario-prefix))
                                               (some->> persona (str persona-prefix))]))
                   (str "id:" id))]
    {:key k :scenario scenario :persona persona}))

(defn verdict
  "The verdict of a group of cards: the distinct verdict:<v> values they
   carry, sorted and joined with |; nil when none carries one."
  [entries]
  (let [vs (into (sorted-set) (mapcat #(tag-values verdict-prefix (:tags %))) entries)]
    (when (seq vs) (str/join "|" vs))))

(defn index-side
  "One run keyed: card-key -> SideCell. Only kanban entries carrying every
   tag in `tags` count, the same selection `kanban list` applies."
  [tags entries]
  (let [selected (plan/select-tagged entries (into ["kanban"] tags))]
    (into {}
          (map (fn [[{:keys [key scenario persona]} group]]
                 [key {:key      key
                       :scenario scenario
                       :persona  persona
                       :ids      (vec (sort (keep #(some-> (:id %) str) group)))
                       :verdict  (verdict group)}]))
          (group-by card-key selected))))

(defn classify
  "The DiffClass of one key, from the cell each run holds (nil when absent)."
  [a b]
  (cond
    (and (nil? a) (nil? b))                     "unverdicted"
    (nil? a)                                    "only-b"
    (nil? b)                                    "only-a"
    (or (nil? (:verdict a)) (nil? (:verdict b))) "unverdicted"
    (= (:verdict a) (:verdict b))               "same"
    :else                                       "flipped"))

(defn diff-row
  "One key across both runs."
  [a b]
  (let [cell (or a b)]
    {:key       (or (:key cell) "id:")
     :scenario  (:scenario cell)
     :persona   (:persona cell)
     :class     (classify a b)
     :verdict_a (:verdict a)
     :verdict_b (:verdict b)
     :ids_a     (or (:ids a) [])
     :ids_b     (or (:ids b) [])}))

(def ^:private class-rank (zipmap s/classes (range)))

(defn summarize
  "Rows counted per class (every class present, zero when unseen) and in total."
  [rows]
  (merge (zipmap (map keyword s/classes) (repeat 0))
         (update-vals (group-by (comp keyword :class) rows) count)
         {:total (count rows)}))

(defn diff
  "The whole promotion: a DiffRequest and the two fetched boards -> DiffReport.
   Rows are ordered flipped, only-a, only-b, unverdicted, same, then by key."
  [{:keys [tags-a tags-b]} entries-a entries-b]
  (let [ia   (index-side tags-a entries-a)
        ib   (index-side tags-b entries-b)
        rows (->> (into (sorted-set) (concat (keys ia) (keys ib)))
                  (map #(diff-row (get ia %) (get ib %)))
                  (sort-by (juxt (comp class-rank :class) :key))
                  vec)]
    {:tags_a  tags-a
     :tags_b  tags-b
     :summary (summarize rows)
     :rows    rows}))

(def ^:private Entries [:maybe [:sequential ks/EntryIn]])

(m/=> request    [:=> [:cat [:maybe [:map [:tags_a {:optional true} :any]
                                          [:tags_b {:optional true} :any]]]]
                  s/RequestResult])
(m/=> tag-values [:=> [:cat :string ks/TagsIn] [:vector :string]])
(m/=> card-key   [:=> [:cat ks/EntryIn] s/CardKey])
(m/=> verdict    [:=> [:cat Entries] [:maybe s/Verdict]])
(m/=> index-side [:=> [:cat s/Tags Entries] s/SideIndex])
(m/=> classify   [:=> [:cat s/MaybeCell s/MaybeCell] s/DiffClass])
(m/=> diff-row   [:=> [:cat s/MaybeCell s/MaybeCell] s/DiffRow])
(m/=> summarize  [:=> [:cat s/Rows] s/Summary])
(m/=> diff       [:=> [:cat s/DiffRequest Entries Entries] s/DiffReport])

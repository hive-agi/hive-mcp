(ns hive-mcp.test.stub.memory-query
  "Query semantics of the atom-backed IMemoryStore stub
   (hive-mcp.test.stub.memory-store): the query-entries filters, ordering and
   :output-fields projection, as pure functions over entry maps.

   Filters follow hive-spi.memory.entry/matches? (:type, :project-id,
   :project-ids OR, :tags AND, :exclude-tags, :include-expired?) plus the
   Milvus range clauses :created-after / :updated-after (strict string `>`).
   Projection maps backend column names to kebab entry keys, as the real
   stores answer them.

   Ordering: the store applies `apply-order-by` BEFORE it cuts to :limit, so
   a limited query answers the first N rows in :order-by order. Milvus has
   no server-side ORDER BY and answers N rows in scan order; the stub does
   not model that fetch window, so limit-saturation behaviour cannot be
   reproduced with it."
  (:require [hive-spi.memory.entry :as entry]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn ->name
  "Keyword or string to its string name; nil stays nil."
  [x]
  (cond (nil? x) nil (keyword? x) (name x) :else (str x)))

(defn- parse-ts
  "Epoch millis for an ISO-8601 timestamp (`entry/parse-instant` forms); nil
   when unparseable."
  [s]
  (some-> (entry/parse-instant s) .toEpochMilli))

(defn expiry-ms
  "Epoch millis of the entry's expiry. Accepts :expires (backend field) or
   :expires-at (contract-test field); nil when neither parses."
  [entry]
  (or (parse-ts (:expires entry)) (parse-ts (:expires-at entry))))

(defn expired?
  "True when the entry's expiry is strictly before NOW-MS."
  [entry now-ms]
  (when-let [t (expiry-ms entry)]
    (< t now-ms)))

(defn- tag-set [entry] (into #{} (map str) (:tags entry)))

(defn- after?
  "True when ENTRY's FIELD sorts strictly after THRESHOLD as a string, the
   comparison Milvus pushes server-side for :created-after / :updated-after."
  [entry field threshold]
  (pos? (compare (str (get entry field)) (str threshold))))

(defn matches?
  "Does ENTRY satisfy the query-entries OPTS filters at NOW-MS?"
  [entry {:keys [type project-id project-ids tags exclude-tags include-expired?
                 created-after updated-after]} now-ms]
  (let [ts (tag-set entry)]
    (and (or (nil? type) (= (->name type) (->name (:type entry))))
         (or (nil? project-id) (= project-id (:project-id entry)))
         (or (empty? project-ids) (contains? (set project-ids) (:project-id entry)))
         (every? #(contains? ts (str %)) (or tags []))
         (not-any? #(contains? ts (str %)) (or exclude-tags []))
         (or (nil? created-after) (after? entry :created created-after))
         (or (nil? updated-after) (after? entry :updated updated-after))
         (or include-expired? (not (expired? entry now-ms))))))

(defn apply-order-by
  "Sort ROWS by [field direction]; identity when ORDER-BY is nil."
  [order-by rows]
  (if-let [[field direction] order-by]
    (vec (sort-by field (if (= direction :desc) #(compare %2 %1) compare) rows))
    (vec rows)))

(defn field-key
  "Entry key for an output-field name: backend column names (`project_id`)
   map to kebab entry keys (:project-id)."
  [f]
  (keyword (.replace ^String (->name f) "_" "-")))

(defn project-fields
  "ROWS trimmed to OUTPUT-FIELDS (column or entry-key names); :id always survives."
  [rows output-fields]
  (if (seq output-fields)
    (let [ks (conj (set (map field-key output-fields)) :id)]
      (mapv #(select-keys % ks) rows))
    rows))

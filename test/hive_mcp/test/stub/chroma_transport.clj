(ns hive-mcp.test.stub.chroma-transport
  "In-memory `IChromaTransport`, so the Chroma adapter (hive-mcp.chroma.* and
   ChromaMemoryStore) runs with no Chroma server.

   It stands in for the vendor client at the transport seam, below every line
   of adapter code, so a test through it exercises the adapter exactly as
   production does. It mirrors the vendor's observable behaviour:

   - every op answers a deref-able value, as the client's futures do;
   - metadata values are stored the way a JSON round trip leaves them
     (keywords become their names);
   - `:where` understands equality, `$in`, `$contains`, `$not_contains` and
     `$and`; `:where-document` understands `$contains`, `$not_contains` and
     `$and` over the document text;
   - `query` answers records nearest first with an ascending `:distance`;
   - `get-collection` of an unknown name answers nil."
  (:require [clojure.string :as str]
            [hive-mcp.chroma.client :as client]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- realized
  "V as an already-delivered deref-able, the shape the vendor client answers."
  [v]
  (doto (promise) (deliver v)))

(defn- json-value [v] (if (keyword? v) (name v) v))

(defn- json-metadata [m]
  (into {} (map (fn [[k v]] [k (json-value v)])) m))

(defn- coll-name [coll] (if (map? coll) (:name coll) coll))

(defn- clause-holds?
  "Does metadata value V satisfy the operand OP of one where clause?"
  [v op]
  (if (map? op)
    (every? (fn [[o x]]
              (let [x (json-value x)]
                (case o
                  :$in           (contains? (set (map json-value x)) v)
                  :$contains     (and (string? v) (str/includes? v (str x)))
                  :$not_contains (not (and (string? v) (str/includes? v (str x))))
                  :$eq           (= v x)
                  :$ne           (not= v x)
                  (throw (ex-info "stub transport: unsupported where operator" {:op o})))))
            op)
    (= v (json-value op))))

(defn- where-holds? [where metadata]
  (cond
    (nil? where)          true
    (contains? where :$and) (every? #(where-holds? % metadata) (:$and where))
    :else (every? (fn [[k op]] (clause-holds? (get metadata k) op)) where)))

(defn- document-holds? [clause document]
  (cond
    (nil? clause)                      true
    (contains? clause :$and)           (every? #(document-holds? % document) (:$and clause))
    (contains? clause :$contains)      (str/includes? (str document) (:$contains clause))
    (contains? clause :$not_contains)  (not (str/includes? (str document) (:$not_contains clause)))
    :else (throw (ex-info "stub transport: unsupported where-document clause" {:clause clause}))))

(defn- select-records [records {:keys [ids where where-document limit]}]
  (cond->> (vals records)
    (seq ids)      (filter #(contains? (set ids) (:id %)))
    true           (filter #(where-holds? where (:metadata %)))
    where-document (filter #(document-holds? where-document (:document %)))
    limit          (take limit)
    true           vec))

(defn- l2 [a b]
  (Math/sqrt (reduce + 0.0 (map (fn [x y] (let [d (- (double x) (double y))] (* d d))) a b))))

(defrecord InMemoryChromaTransport [state]
  client/IChromaTransport
  (-configure [_ _opts] (realized nil))

  (-get-collection [_ n]
    (realized (when-let [c (get @state n)] {:name n :metadata (:metadata c)})))

  (-create-collection [_ n opts]
    (swap! state (fn [s] (if (contains? s n) s (assoc s n {:metadata (:metadata opts) :records {}}))))
    (realized {:name n :metadata (get-in @state [n :metadata])}))

  (-delete-collection [_ coll]
    (swap! state dissoc (coll-name coll))
    (realized nil))

  (-add [_ coll records _opts]
    (swap! state update-in [(coll-name coll) :records]
           (fn [existing]
             (reduce (fn [acc r] (assoc acc (:id r) (update r :metadata json-metadata)))
                     (or existing {}) records)))
    (realized nil))

  (-get [_ coll opts]
    (realized (select-records (get-in @state [(coll-name coll) :records]) opts)))

  (-query [_ coll embedding opts]
    (realized
     (->> (select-records (get-in @state [(coll-name coll) :records]) (dissoc opts :limit))
          (keep (fn [r] (when-let [e (:embedding r)] (assoc r :distance (l2 embedding e)))))
          (sort-by :distance)
          (take (or (:num-results opts) 10))
          vec)))

  (-delete [_ coll opts]
    (let [n      (coll-name coll)
          doomed (set (map :id (select-records (get-in @state [n :records]) opts)))]
      (swap! state update-in [n :records] #(apply dissoc % doomed)))
    (realized nil))

  (-update [_ coll records]
    (swap! state update-in [(coll-name coll) :records]
           (fn [existing]
             (reduce (fn [acc r]
                       (if (contains? acc (:id r))
                         (update acc (:id r) merge (update r :metadata json-metadata))
                         acc))
                     (or existing {}) records)))
    (realized nil)))

(defn ->transport
  "A fresh, empty in-memory transport."
  []
  (->InMemoryChromaTransport (atom {})))

(defn record-count
  "How many records TRANSPORT holds across every collection."
  [transport]
  (reduce + 0 (map (comp count :records) (vals @(:state transport)))))

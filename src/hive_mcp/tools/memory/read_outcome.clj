(ns hive-mcp.tools.memory.read-outcome
  "What a store read answered, as data a handler can report honestly.

   A read through the IMemoryStore port answers one of: a sequence of
   entries; an error Result `{:error kw ...}` (hive-milvus answers
   `{:error :milvus/read-incomplete :failed [..] :partial [..]}` when a
   collection cannot answer); or a hive-spi Failure map
   `{:success? false :errors [..]}`. `classify` folds the three into one
   ReadOutcome, `kept-rows` drops rows with no id, and `warnings` names every
   reason an answer may be short."
  (:require [clojure.string :as str]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ReadOutcome
  [:map
   [:rows [:vector :map]]
   [:status [:enum :complete :incomplete :failed]]
   [:failed {:optional true} [:vector :any]]
   [:cause {:optional true} [:maybe :string]]])

(defn- failure-map? [x]
  (and (map? x) (false? (:success? x))))

(defn- error-result? [x]
  (and (map? x) (contains? x :error)))

(defn classify
  "One ReadOutcome for RAW, the answer of a store read."
  [raw]
  (cond
    (nil? raw)
    {:rows [] :status :complete}

    (error-result? raw)
    (let [partial (vec (filter map? (:partial raw)))]
      {:rows   partial
       :status (if (seq partial) :incomplete :failed)
       :failed (vec (:failed raw))
       :cause  (str (:error raw) (when-let [msg (:message raw)] (str ": " msg)))})

    (failure-map? raw)
    {:rows   []
     :status :failed
     :failed []
     :cause  (str/join "; " (map str (or (seq (:errors raw)) [(:error raw)])))}

    (map? raw)
    {:rows   []
     :status :failed
     :failed []
     :cause  (str "store answered a map, not a sequence of entries (keys "
                  (pr-str (vec (take 6 (keys raw)))) ")")}

    (sequential? raw)
    {:rows (vec (filter map? raw)) :status :complete}

    :else
    {:rows [] :status :failed :failed []
     :cause (str "store answered " (type raw) ", not a sequence of entries")}))

(defn entry-read-failure
  "The cause when RAW, the answer of a single-entry read, says the store could
   not read: an error Result or a Failure map. nil for an entry or for absence."
  [raw]
  (when (or (error-result? raw) (failure-map? raw))
    (:cause (classify raw))))

(defn kept-rows
  "ROWS with an id, and how many were dropped for having none."
  [rows]
  (let [kept (filterv #(some? (:id %)) rows)]
    {:rows kept :dropped-null-ids (- (count rows) (count kept))}))

(defn- collections [failed]
  (str/join ", " (map #(or (:collection %) (str %)) failed)))

(defn warnings
  "Every reason the answer built from OUTCOME may be short. OPTS:
   :dropped-null-ids, and :window-full? with :fetch-limit when the store
   filled its fetch window before a filter it does not apply ran."
  [outcome {:keys [dropped-null-ids window-full? fetch-limit]}]
  (cond-> []
    (= :failed (:status outcome))
    (conj (str "the store read FAILED (" (:cause outcome) "); an empty result is not evidence of absence"
               (when (seq (:failed outcome)) (str "; failed collections: " (collections (:failed outcome))))))

    (= :incomplete (:status outcome))
    (conj (str "the store read was INCOMPLETE (" (:cause outcome) "); only the partial rows are shown"
               (when (seq (:failed outcome)) (str "; failed collections: " (collections (:failed outcome))))))

    (pos? (or dropped-null-ids 0))
    (conj (str dropped-null-ids " row(s) without an id were dropped"))

    window-full?
    (conj (str "the store filled its fetch window of " fetch-limit " rows before filters it "
               "does not apply ran, so matching entries outside that window are missing; "
               "narrow with :type, :tags or :created_after"))))

(m/=> classify [:=> [:cat :any] ReadOutcome])
(m/=> kept-rows [:=> [:cat [:sequential :map]] [:map [:rows [:vector :map]] [:dropped-null-ids nat-int?]]])
(m/=> warnings [:=> [:cat ReadOutcome :map] [:vector :string]])

(m/=> entry-read-failure [:=> [:cat :any] [:maybe :string]])

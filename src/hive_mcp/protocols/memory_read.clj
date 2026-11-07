(ns hive-mcp.protocols.memory-read
  "The read answer a hive-mcp caller of query-entries / search-similar sees.

   A store answers a read with a sequential of entries. hive-milvus (from
   11895fb) answers a read that timed out or reached only some collections
   with an error Result instead: a map such as

     {:error :milvus/read-incomplete :failed [{:collection :message}] :partial [..]}

   A caller that maps, counts or seqs that map walks its key/value pairs as if
   they were entries. `read-answer` is the one place hive-mcp turns such an
   answer into an explicit degraded value: a vector of the partial rows the
   store did return (empty when it returned none) whose metadata carries the
   failure under `::incomplete`, logged once at warn. Callers keep iterating
   a vector, a partial read keeps its rows, and `incomplete` tells a failed or
   partial read from a complete one. Pure apart from the log line."
  (:require [malli.core :as m]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Failure
  "What `incomplete` answers for a failed read."
  [:map [:op :keyword] [:error :any]])

(defn error-answer?
  "True when ANSWER is a store error Result (a map carrying :error), not a
   sequential of entries."
  [answer]
  (and (map? answer) (contains? answer :error)))

(m/=> error-answer? [:=> [:cat :any] :boolean])

(defn read-answer
  "ANSWER as a value safe to iterate. Entries (any sequential, or nil) pass
   through unchanged. A store error Result becomes a vector of its partial
   rows (the maps under :partial, empty when it has none) with
   `{::incomplete {:op OP :error .. :failed .. :partial-count n}}` metadata,
   logged at warn. A partial read therefore keeps the rows it did get and is
   still marked incomplete. Any other map is a contract breach of the store
   and is degraded the same way."
  [op answer]
  (if (map? answer)
    (let [rows    (vec (filter map? (:partial answer)))
          failure (cond-> {:op op :error (get answer :error :store/non-sequential-answer)}
                    (:failed answer)  (assoc :failed (:failed answer))
                    (:partial answer) (assoc :partial-count (count rows)))]
      (log/warn "memory" (name op) "answered a failure, not entries; keeping"
                (count rows) "partial row(s):" (pr-str (dissoc failure :op)))
      (with-meta rows {::incomplete failure}))
    answer))

(m/=> read-answer [:=> [:cat :keyword :any] [:maybe [:sequential :any]]])

(defn incomplete
  "The failure a `read-answer` value carries, or nil for a complete read.
   When the read kept partial rows they are under :partial, so the failure
   reads like the store's own error Result (see
   `hive-mcp.tools.memory.read-outcome/classify`)."
  [answer]
  (when-let [failure (::incomplete (meta answer))]
    (cond-> failure
      (seq answer) (assoc :partial (vec answer)))))

(m/=> incomplete [:=> [:cat :any] [:maybe Failure]])

(ns hive-mcp.channel.memory-piggyback
  "Memory piggyback channel for incremental delivery of axioms and conventions via cursor+budget drain."
  (:require [taoensso.timbre :as log]
            [hive-dsl.bounded-atom :refer [bounded-atom bput! bget bounded-swap!
                                           bclear! register-sweepable!]]
            [hive-dsl.context.identity :as ctx-id]
            [hive-spi.swarm.guards :as guards]
            [hive-mcp.channel.drain-rank :as rank]
            [hive-mcp.channel.drain-telemetry :as telemetry]
            [clojure.string :as str]
            [hive-mcp.channel.drain-projection :as proj]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const drain-char-budget
  "Max chars per drain batch. Kept modest so each tool response
   stays readable — full content absorbed over several calls."
  12000)

;; gc-fix-3: buffers — LRU eviction, 200 entries, 30min TTL
;; Piggyback buffers are ephemeral per-agent state. Orphaned buffers waste memory.
(defonce ^{:doc "Map of caller-id-key -> buffer state (session-scoped)."}
  buffers
  (bounded-atom {:max-entries 200
                 :ttl-ms 1800000    ;; 30 minutes
                 :eviction-policy :lru}))
(register-sweepable! buffers :piggyback-buffers)

(defn- format-entry
  "Convert a catchup entry to compact piggyback format.

   :A and :F carry the stored access count and the net helpful-minus-unhelpful
   feedback forward so `drain-rank/score` can read them without a store hit.
   Both are omitted when the source entry has neither."
  [entry]
  (let [access (:access-count entry)
        net (- (or (:helpful-count entry) 0) (or (:unhelpful-count entry) 0))]
    (cond-> {:id (:id entry)
             :T (or (:type entry) "note")
             :C (or (:content entry) (:preview entry) "")}
      (:severity entry) (assoc :S (:severity entry))
      (seq (:tags entry)) (assoc :tags (vec (:tags entry)))
      (and access (pos? access)) (assoc :A access)
      (not (zero? net)) (assoc :F net))))

(def CallerId
  "Caller identity as the route layer passes it (string or keyword). nil is
   accepted: ctx-id/parse-caller-id falls back to the default caller."
  [:maybe [:or :string :keyword]])

(def DrainCtx
  "Optional drain ctx: task tokens, pins, floor cap and wire policy override."
  [:maybe [:map
           [:tokens {:optional true} [:maybe [:or [:set :string] [:sequential :string]]]]
           [:pins {:optional true} :any]
           [:floor-cap {:optional true} [:maybe :int]]
           [:policy {:optional true} :any]]])

(def DrainResult
  "One drained batch plus the buffer position after it."
  [:map
   [:batch [:vector :map]]
   [:remaining [:int {:min 0}]]
   [:total [:int {:min 0}]]
   [:delivered [:int {:min 0}]]
   [:seq pos-int?]
   [:done {:optional true} [:= true]]
   [:context-refs {:optional true} :any]
   [:pull {:optional true} :any]])

(defn enqueue!
  "Enqueue entries into the memory piggyback buffer.

   MERGES with any existing buffer for the same key: entries already buffered or
   already delivered this session (matched by :id) are skipped, genuinely new
   entries are appended, and the drain cursor is preserved so nothing is re-sent
   and nothing queued is discarded.

   Supplying context-refs marks them undelivered so the next drain carries them.

   Session-scoped: keyed by caller-id only (no project dimension)."
  ([caller-id entries]
   (enqueue! caller-id entries nil))
  ([caller-id entries context-refs]
   (let [buffer-key (ctx-id/caller-id-key (ctx-id/parse-caller-id caller-id))
         existing   (bget buffers buffer-key)
         formatted  (mapv format-entry entries)
         prior      (vec (:entries existing))
         known      (into (set (:sent existing)) (keep :id) prior)
         fresh      (filterv #(not (and (:id %) (contains? known (:id %)))) formatted)
         merged     (into prior fresh)
         cursor     (min (:cursor existing 0) (count merged))
         refs       (or context-refs (:context-refs existing))
         refs-delivered? (if (some? context-refs)
                           false (boolean (:refs-delivered? existing)))]
     (bput! buffers buffer-key
            (cond-> {:entries merged
                     :cursor cursor
                     :done (and (empty? (drop cursor merged))
                                (or (nil? refs) refs-delivered?))
                     :seq-num (:seq-num existing 0)
                     :sent (set (:sent existing))}
              (some? refs)
              (assoc :context-refs refs :refs-delivered? refs-delivered?)))
     (log/info "memory-piggyback: enqueued" (count fresh) "new of" (count formatted)
               "offered for" buffer-key
               "- buffer now" (count merged) "entries, cursor" cursor
               (when (seq (:sent existing)) (str ", " (count (:sent existing)) " already delivered"))
               (when context-refs (str ", " (count context-refs) " context-refs"))))))

(m/=> enqueue! [:function
                [:=> [:cat CallerId [:maybe [:sequential :map]]] :nil]
                [:=> [:cat CallerId [:maybe [:sequential :map]] :any] :nil]])

(defn drain!
  "Drain next batch of entries within char budget for a caller session.

   Carries :context-refs forward across batches and attaches them to the first
   response after they are enqueued, marking them delivered.

   On exhaustion the buffer collapses to a content-free tombstone retaining the
   delivered ids under :sent, so a later enqueue! for the same caller can skip
   what this session already received.

   The 2-arity takes a drain ctx. A non-empty (:tokens ctx) selects the batch
   two-lane via drain-rank/select-batch, rewrites the undrained tail of the
   buffer into that order, and ages every offered-but-not-taken entry under
   :offers. ctx :pins and :floor-cap are forwarded to the ranker. ctx nil, or
   ctx without :tokens, is FIFO — identical to the 1-arity in both the returned
   batch and the buffer written back.

   The WIRE POLICY is resolved first and applied to the whole buffer, so the
   char budget is spent on what is actually sent. Under :index every pool entry
   becomes an id-and-title pointer while floor entries (axioms, pins) pass
   through whole; the projection is idempotent, so writing a projected buffer
   back is safe. ctx :policy overrides the configured one. When the batch
   carries pointers the result gains :pull, the instruction for turning them
   back into entries.

   Every drain folds its offered / delivered ids into `drain-telemetry`."
  ([caller-id] (drain! caller-id nil))
  ([caller-id ctx]
   (let [budget (max 0 (min drain-char-budget (or (:char-budget ctx) drain-char-budget)))
         strict? (some? (:char-budget ctx))
         buffer-key (ctx-id/caller-id-key (ctx-id/parse-caller-id caller-id))
         buf (when (pos? budget) (bget buffers buffer-key))]
     (when (and buf (not (:done buf)))
       (let [{:keys [cursor seq-num context-refs refs-delivered? sent offers]} buf
             policy (proj/resolve-policy (:policy ctx))
             entries (proj/project (:entries buf) {:policy policy :pins (:pins ctx)})
             total (count entries)
             tokens (:tokens ctx)
             pending (when (seq tokens) (subvec (vec entries) (min cursor total)))
             ranked (when (seq pending)
                      (rank/select-batch pending {:tokens tokens :offers offers
                                                  :pins (:pins ctx)
                                                  :floor-cap (:floor-cap ctx)
                                                  :char-budget budget :strict-budget? strict?}))
             entries* (if ranked
                        (into (subvec (vec entries) 0 (min cursor total)) (:ordered ranked))
                        entries)
             [candidate _]
             (if ranked
               [(:batch ranked) (+ cursor (:taken ranked))]
               (loop [batch [] chars 0 idx cursor]
                 (if (>= idx total)
                   [batch idx]
                   (let [entry (nth entries idx)
                         new-chars (+ chars (count (pr-str entry)))]
                     (if (and (> new-chars budget) (or strict? (seq batch)))
                       [batch idx]
                       (recur (conj batch entry) new-chars (inc idx)))))))
             new-seq (inc seq-num)
             refs-pending? (and (some? context-refs) (not refs-delivered?))
             ;; Charge the actual serialized result, including refs, pull hint,
             ;; metadata and the MEMORY delimiters (the caller reserves 200).
             result-for (fn [batch refs?]
                          (let [new-cursor (+ cursor (count batch))
                                done? (>= new-cursor total)]
                            (cond-> {:batch batch :remaining (- total new-cursor)
                                     :total total :delivered new-cursor :seq new-seq}
                              done? (assoc :done true)
                              refs? (assoc :context-refs context-refs)
                              (proj/pull-hint batch) (assoc :pull (proj/pull-hint batch)))))
             fits? (fn [batch refs?]
                     (or (not strict?)
                         (<= (+ 30 (count (pr-str (result-for batch refs?)))) budget)))
             ;; Refs have priority over entries, but must never be marked sent
             ;; unless they really fit. An oversized ref waits in the buffer.
             send-refs? (and refs-pending? (fits? [] true))
             batch (if strict?
                     (loop [remaining candidate acc []]
                       (if (and (seq remaining)
                                (fits? (conj acc (first remaining)) send-refs?))
                         (recur (rest remaining) (conj acc (first remaining)))
                         acc))
                     candidate)
             new-cursor (+ cursor (count batch))
             is-done (and (>= new-cursor total) (or (not refs-pending?) send-refs?))
             taken-ids (when ranked (into #{} (keep :id) batch))
             next-offers (if ranked
                           (reduce (fn [m e]
                                     (let [eid (:id e)]
                                       (if (and eid (not (contains? taken-ids eid)))
                                         (update m eid (fnil inc 0)) m)))
                                   (or offers {}) pending)
                           offers)]
         (when (or (seq batch) send-refs?)
           (telemetry/record! {:seq-num new-seq :batch batch
                               :delivered-ids (into [] (keep :id) batch)
                               :offered-ids (when ranked
                                              (into [] (comp (keep :id) (remove taken-ids)) pending))})
           (bput! buffers buffer-key
                  (cond-> (if is-done
                            {:entries [] :cursor 0 :done true :seq-num new-seq
                             :sent (into (set sent) (keep :id) entries*)}
                            {:entries entries* :cursor new-cursor :done false
                             :seq-num new-seq :sent (set sent)})
                    (some? context-refs)
                    (assoc :context-refs context-refs
                           :refs-delivered? (or (boolean refs-delivered?) send-refs?))
                    (some? next-offers) (assoc :offers next-offers)))
           (result-for batch send-refs?)))))))

(m/=> drain! [:function
              [:=> [:cat CallerId] [:maybe DrainResult]]
              [:=> [:cat CallerId DrainCtx] [:maybe DrainResult]]])

(defn has-pending?
  "Check if a caller session has undrained memory entries."
  [caller-id]
  (let [buffer-key (ctx-id/caller-id-key (ctx-id/parse-caller-id caller-id))
        buf (bget buffers buffer-key)]
    (and (some? buf) (not (:done buf)))))

(m/=> has-pending? [:=> [:cat CallerId] :boolean])

(defn clear-buffer!
  "Clear buffer for a specific caller session. For testing."
  [caller-id]
  (let [buffer-key (ctx-id/caller-id-key (ctx-id/parse-caller-id caller-id))]
    (bounded-swap! buffers dissoc buffer-key)))

(m/=> clear-buffer! [:=> [:cat CallerId] :any])

(defn reset-all!
  "Reset all buffers. For testing.

   Guarded by `when-not-coordinator` — no-op when the live coordinator
   is running so test fixtures cannot wipe live piggyback buffers."
  []
  (guards/when-not-coordinator
   "channel.memory-piggyback/reset-all! blocked"
   (bclear! buffers)))

(m/=> reset-all! [:=> [:cat] :any])

(defn adopt-buffer!
  "Adopt an orphaned buffer from a previous coordinator instance.
   When a coordinator restarts (new instance-id), the old buffer is orphaned.
   This function finds a matching buffer from any coordinator instance
   and re-keys it to the new caller.

   Session-scoped: matches by caller prefix only, no project dimension.

   Returns true if a buffer was adopted, false otherwise."
  [new-caller-id]
  (let [new-key (ctx-id/caller-id-key (ctx-id/parse-caller-id new-caller-id))
        ;; Find orphaned coordinator buffer (any coordinator instance)
        donor (->> @(:atom buffers)
                   (filter (fn [[aid entry]]
                             (let [buf (:data entry)]
                               (and (not= aid new-key)
                                    (clojure.string/starts-with? (str aid) "coordinator:")
                                    (not (:done buf))))))
                   first)]
    (when-let [[donor-key donor-entry] donor]
      (let [donor-buf (:data donor-entry)]
        (bounded-swap! buffers dissoc donor-key)
        (bput! buffers new-key donor-buf)
        (log/info "memory-piggyback: adopted buffer from" donor-key "->" new-key
                  "(" (- (count (:entries donor-buf)) (:cursor donor-buf)) "entries remaining)")
        true))))

(m/=> adopt-buffer! [:=> [:cat CallerId] [:maybe [:= true]]])

(defn evict-orphaned-buffers!
  "Evict coordinator buffers that have no matching active caller.
   Called during catchup to clean up buffers from dead bb-mcp processes.

   Takes an active caller-id (with instance suffix) and evicts
   coordinator buffers whose key doesn't match the active caller prefix.

   Returns count of evicted buffers."
  [active-caller-id]
  (let [orphaned (->> @(:atom buffers)
                      (filter (fn [[aid _entry]]
                                (and (clojure.string/starts-with? (str aid) "coordinator:")
                                     (not (clojure.string/starts-with? (str aid) (str active-caller-id "-"))))))
                      (map first)
                      vec)]
    (when (seq orphaned)
      (bounded-swap! buffers #(apply dissoc % orphaned))
      (log/info "memory-piggyback: evicted" (count orphaned) "orphaned buffers:" orphaned))
    (count orphaned)))

(m/=> evict-orphaned-buffers! [:=> [:cat :any] [:int {:min 0}]])

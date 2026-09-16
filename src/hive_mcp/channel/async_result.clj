(ns hive-mcp.channel.async-result
  "Async tool result buffer for piggyback delivery with cursor+budget drain."
  (:require [taoensso.timbre :as log]
            [hive-dsl.context.identity :as ctx-id]
            [hive-mcp.channel.async-result-journal :as journal])
  (:import [java.time Instant]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Constants

(def ^:const drain-char-budget "Max chars per drain batch (~8K tokens)." 32000)

(def ^:const ttl-seconds
  "TTL for DELIVERED async results in seconds.

   A delivered entry is a receipt: the caller already holds the payload, so the
   copy left in the buffer exists only for dedup and may be reclaimed promptly."
  300)

(def ^:const undelivered-ttl-seconds
  "Grace period for results the caller has NEVER been given, in seconds.

   Deliberately far longer than `ttl-seconds`. An undelivered entry is the only
   copy of a result whose acknowledgement already went out as `{:queued true}`,
   so reclaiming one is data loss rather than housekeeping, and the
   `:status :error` entries are exactly the ones a caller cannot reconstruct.
   It still has a bound, because a caller that never drains must not grow its
   buffer forever, but the bound is a day rather than five minutes and passing
   it is never silent."
  86400)

;; Buffer State

(defonce ^{:doc "Map of caller-id-key -> {:entries [...] :cursor 0} (session-scoped)."}
  buffers
  (atom {}))

;; Internal Helpers

(defn- now-epoch-seconds
  "Current epoch seconds for TTL comparison."
  []
  (.getEpochSecond (Instant/now)))

(defn- content-hash
  "Generate content hash for dedup. Uses Clojure hash for speed."
  [result-map]
  (hash (select-keys result-map [:task-id :tool :status :result])))

(defn- expired?
  "True when `entry` is past the TTL that applies to it.

   Which TTL applies depends on whether the caller has already been given the
   entry: `delivered?` selects `ttl-seconds` over `undelivered-ttl-seconds`.
   Collection status, not age alone, is what makes an entry safe to drop."
  [entry now-secs delivered?]
  (> (- now-secs (:timestamp entry 0))
     (if delivered? ttl-seconds undelivered-ttl-seconds)))

;; Garbage Collection

(defn- gc-buffer
  "One buffer with its expired entries removed. PURE.

   Returns `[buf' dropped-undelivered]`. `buf'` is nil when nothing survives, so
   the caller can drop the key. `dropped-undelivered` is the entries reclaimed
   WITHOUT ever having been handed to the caller, which are the ones whose loss
   has to be reported rather than merely counted."
  [{:keys [entries cursor] :as buf} now-secs]
  (let [tagged     (map-indexed vector entries)
        delivered? (fn [[idx _]] (< idx cursor))
        keep?      (fn [[idx e]] (not (expired? e now-secs (< idx cursor))))
        kept       (filterv keep? tagged)
        live       (mapv second kept)
        ;; The cursor must still point just past the last DELIVERED survivor, or
        ;; a survivor is re-delivered or skipped.
        new-cursor (count (filter delivered? kept))]
    [(when (seq live) (assoc buf :entries live :cursor new-cursor))
     (mapv second (remove delivered? (remove keep? tagged)))]))

(defn gc-expired!
  "Reclaim expired entries from every buffer; returns the count removed.

   A DELIVERED entry (one before its buffer's cursor) expires at `ttl-seconds`.
   An UNDELIVERED entry expires only at `undelivered-ttl-seconds`, and its
   removal is logged at WARN with the task-ids, because that is the one path
   where a caller loses a result it was promised and would otherwise never be
   told.

   This used to expire every entry at `ttl-seconds` regardless of whether it had
   been collected, which destroyed results no drain had yet reached. It also
   counted removals by `swap!`ing a side-effecting atom from INSIDE the `swap!`
   update function, so a contended retry double-counted; the count now comes
   from the before/after values `swap-vals!` returns.

   Compaction rides along here rather than on a sweep of its own, because this
   is the moment the set of owed results shrinks: reclaiming the heap copy and
   reclaiming the disk copy are the same event seen twice, and giving them
   separate timers would let the journal describe a state the buffers had
   already left."
  []
  (let [now-secs  (now-epoch-seconds)
        lost      (volatile! [])
        [old new] (swap-vals!
                   buffers
                   (fn [bufs]
                     (vreset! lost [])
                     (reduce-kv
                      (fn [acc buffer-key buf]
                        (let [[buf' dropped] (gc-buffer buf now-secs)]
                          (when (seq dropped)
                            (vswap! lost into dropped))
                          (cond-> acc buf' (assoc buffer-key buf'))))
                      {}
                      bufs)))
        n-entries (fn [bufs] (reduce + 0 (map (comp count :entries val) bufs)))
        total     (- (n-entries old) (n-entries new))
        orphaned  @lost]
    (when (seq orphaned)
      (log/warn "async-result: GC dropped" (count orphaned)
                "UNDELIVERED result(s) past the" undelivered-ttl-seconds
                "second grace period; task-ids:" (mapv :task-id orphaned)))
    (when (pos? total)
      (log/info "async-result: GC removed" total "expired entries"))
    (journal/compact! new)
    total))

;; Public API

(defn enqueue-result!
  "Enqueue a completed async result into the buffer with content-hash dedup.
   Session-scoped: keyed by caller-id only (no project dimension).

   The JOURNAL WRITE HAPPENS FIRST, before the atom is touched, so the result
   is on disk before anything claims to hold it. A crash in between loses
   nothing: replay restores the entry and the caller is served from the
   journal instead. The other order would leave a window in which the buffer
   is the only copy, which is the entire bug this is closing.

   Dedup is by content-hash while the journal is keyed by task-id, so a
   result deduped away still leaves a journal record behind, and replay may
   restore one extra copy of an identical payload after a crash. Handing a
   caller the same content twice is a far smaller fault than dropping it, and
   that is the trade this ordering buys."
  [caller-id result-map]
  (let [buffer-key (ctx-id/caller-id-key (ctx-id/parse-caller-id caller-id))
        entry (assoc result-map
                     :timestamp (now-epoch-seconds)
                     :content-hash (content-hash result-map))]
    (journal/record-result! buffer-key (:task-id result-map) entry)
    (swap! buffers update buffer-key
           (fn [buf]
             (let [buf (or buf {:entries [] :cursor 0})
                   ;; Dedup: check if content-hash already exists
                   existing-hashes (set (map :content-hash (:entries buf)))]
               (if (contains? existing-hashes (:content-hash entry))
                 (do
                   (log/debug "async-result: dedup skip for task" (:task-id result-map))
                   buf)
                 (update buf :entries conj entry)))))
    (log/info "async-result: enqueued result for task" (:task-id result-map)
              "tool:" (:tool result-map) "status:" (:status result-map)
              "buffer:" buffer-key)))

(defn- next-batch
  "The batch one drain would take from `buf`, as [entries new-cursor]. PURE.

   Kept pure and total so it can be applied twice on the same value: once
   inside the `swap!` that advances the cursor, and once outside it to build
   the caller's payload. Both applications see the same buffer value, so the
   payload cannot disagree with the cursor that was committed.

   At least one entry is always taken, even when that single entry is over
   budget, or an oversized result would wedge the buffer forever."
  [{:keys [entries cursor]}]
  (let [total (count entries)]
    (loop [batch [] chars 0 idx cursor]
      (if (>= idx total)
        [batch idx]
        (let [output-entry (dissoc (nth entries idx) :timestamp :content-hash)
              new-chars    (+ chars (count (pr-str output-entry)))]
          (if (and (seq batch) (> new-chars drain-char-budget))
            [batch idx]
            (recur (conj batch output-entry) new-chars (inc idx))))))))

(defn- drainable?
  "True when `buf` exists and holds an entry the caller has not been given."
  [buf]
  (boolean (and buf (< (:cursor buf) (count (:entries buf))))))

(defn drain!
  "Drain next batch of async results within char budget for a caller session.

   The cursor advance is ONE atomic step. It used to be a read-modify-write:
   the buffer was snapshotted with `@buffers`, the batch computed from that
   snapshot, and the snapshot then written back with `assoc` (or the whole key
   `dissoc`ed when the snapshot looked fully drained). A result enqueued in
   that window was destroyed -- clobbered by the stale snapshot, or deleted
   with the buffer -- and since the ack had already gone out as
   `{:queued true}`, the caller was never told. A batch of concurrent async
   calls is exactly the case that loses results this way, including the
   `:status :error` results that are the only report a failed write ever makes.

   `swap-vals!` gives the value the winning attempt actually saw, so the
   payload is rebuilt from that same value with the same pure `next-batch`.
   The buffer is dropped only when it is empty AT THE INSTANT of the swap, not
   when a stale copy looked empty.

   The delivery is journalled AFTER the cursor has moved, so a crash between
   the two re-delivers rather than drops. At-least-once is the correct
   direction to fail for something the caller has already been promised."
  [caller-id]
  (let [buffer-key (ctx-id/caller-id-key (ctx-id/parse-caller-id caller-id))
        [old _new] (swap-vals!
                    buffers
                    (fn [bufs]
                      (let [buf (get bufs buffer-key)]
                        (if-not (drainable? buf)
                          bufs
                          (let [[_ new-cursor] (next-batch buf)]
                            (if (>= new-cursor (count (:entries buf)))
                              (dissoc bufs buffer-key)
                              (assoc bufs buffer-key (assoc buf :cursor new-cursor))))))))
        buf (get old buffer-key)]
    (when (drainable? buf)
      (let [[batch new-cursor] (next-batch buf)
            total              (count (:entries buf))]
        (journal/record-delivered! buffer-key (keep :task-id batch))
        (cond-> {:results   batch
                 :remaining (- total new-cursor)
                 :total     total
                 :delivered new-cursor}
          (>= new-cursor total) (assoc :done true))))))

(defn has-pending?
  "Check if a caller session has undrained async results."
  [caller-id]
  (let [buffer-key (ctx-id/caller-id-key (ctx-id/parse-caller-id caller-id))
        buf (get @buffers buffer-key)]
    (and (some? buf)
         (< (:cursor buf) (count (:entries buf))))))

(defn poll-task
  "Get status of a specific async task by task-id across all buffers."
  [task-id]
  (some (fn [[_buffer-key {:keys [entries]}]]
          (some #(when (= task-id (:task-id %))
                   (dissoc % :timestamp :content-hash))
                entries))
        @buffers))

(defn clear-buffer!
  "Clear buffer for a specific caller session. For testing."
  [caller-id]
  (let [buffer-key (ctx-id/caller-id-key (ctx-id/parse-caller-id caller-id))]
    (swap! buffers dissoc buffer-key)))

(defn reset-all!
  "Reset all buffers. For testing."
  []
  (reset! buffers {}))

(defn restore!
  "Restore undelivered results from the journal into `buffers`.

   Called once at startup, BEFORE anything can enqueue. Everything the
   journal still carries is by definition undelivered, so it is restored at
   `:cursor 0` and the caller is served on its next drain exactly as if the
   process had never died.

   Anything already in memory wins over the journal, so calling this twice,
   or late, cannot clobber a live buffer with a stale disk copy.

   Returns the number of entries restored. A positive count is logged at WARN
   because it means the previous process did not shut down cleanly, which is
   worth noticing even though it was survived."
  []
  (let [restored (journal/restore)
        n (reduce + 0 (map (comp count :entries val) restored))]
    (when (pos? n)
      (log/warn "async-result: restored" n
                "undelivered result(s) from the journal; the previous process"
                "did not shut down cleanly"
                {:callers (vec (keys restored))}))
    (swap! buffers #(merge restored %))
    n))

(defn record-submission!
  "Journal that an async task was accepted for `caller-id`, before its ack.

   Exists so the caller id is NORMALISED through the same
   `ctx-id/caller-id-key` that `enqueue-result!` and `drain!` use. The
   journal itself knows nothing about caller-id parsing, and the middleware
   holds the raw id; if either recorded the raw form, a replayed submission
   would land under a key the caller never drains, and an `:interrupted`
   result would be restored into a buffer nobody reads.

   Returns true when the record reached disk, which is what the ack reports
   as `:durable`."
  [caller-id task-id tool]
  (journal/record-submitted!
   (ctx-id/caller-id-key (ctx-id/parse-caller-id caller-id))
   task-id
   tool))

(defn stats
  "Get buffer statistics. For monitoring/debugging."
  []
  (let [bufs @buffers]
    {:buffer-count (count bufs)
     :total-entries (reduce + 0 (map (comp count :entries val) bufs))
     :pending-entries (reduce + 0
                              (map (fn [[_ {:keys [entries cursor]}]]
                                     (- (count entries) cursor))
                                   bufs))
     :buffers (into {}
                    (map (fn [[k {:keys [entries cursor]}]]
                           [k {:entries (count entries)
                               :cursor cursor
                               :pending (- (count entries) cursor)}])
                         bufs))}))

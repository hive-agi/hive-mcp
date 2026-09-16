(ns hive-mcp.channel.async-result
  "Async tool result buffer for piggyback delivery with cursor+budget drain."
  (:require [taoensso.timbre :as log]
            [hive-dsl.context.identity :as ctx-id])
  (:import [java.time Instant]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Constants

(def ^:const drain-char-budget "Max chars per drain batch (~8K tokens)." 32000)

(def ^:const ttl-seconds "TTL for async results in seconds." 300)

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
  "Check if an entry has expired based on TTL."
  [entry now-secs]
  (> (- now-secs (:timestamp entry 0)) ttl-seconds))

;; Garbage Collection

(defn gc-expired!
  "Remove expired entries from all buffers, returns count removed."
  []
  (let [now-secs (now-epoch-seconds)
        removed (atom 0)]
    (swap! buffers
           (fn [bufs]
             (reduce-kv
              (fn [acc buffer-key {:keys [entries cursor] :as buf}]
                (let [live-entries (vec (remove #(expired? % now-secs) entries))
                      removed-count (- (count entries) (count live-entries))
                      ;; Adjust cursor: count how many removed entries were before cursor
                      removed-before-cursor (count (filter
                                                    (fn [idx]
                                                      (expired? (nth entries idx) now-secs))
                                                    (range (min cursor (count entries)))))
                      new-cursor (max 0 (- cursor removed-before-cursor))]
                  (swap! removed + removed-count)
                  (if (empty? live-entries)
                    acc ;; Remove empty buffer entirely
                    (assoc acc buffer-key
                           (assoc buf
                                  :entries live-entries
                                  :cursor new-cursor)))))
              {}
              bufs)))
    (let [total @removed]
      (when (pos? total)
        (log/info "async-result: GC removed" total "expired entries"))
      total)))

;; Public API

(defn enqueue-result!
  "Enqueue a completed async result into the buffer with content-hash dedup.
   Session-scoped: keyed by caller-id only (no project dimension)."
  [caller-id result-map]
  (let [buffer-key (ctx-id/caller-id-key (ctx-id/parse-caller-id caller-id))
        entry (assoc result-map
                     :timestamp (now-epoch-seconds)
                     :content-hash (content-hash result-map))]
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
   when a stale copy looked empty."
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

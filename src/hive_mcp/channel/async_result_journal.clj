(ns hive-mcp.channel.async-result-journal
  "An append-only journal so an async result outlives the process that made it.

   `hive-mcp.channel.async-result/buffers` is an atom. A caller is told
   `{:queued true}` and the only copy of what it was promised then lives in
   heap. SIGKILL runs no shutdown hook, so the kernel OOM killer taking the
   coordinator (twice on 2026-09-16) destroys every result nobody had drained
   yet, and the caller is never told. That is the whole of kanban
   20260721013318-1a9d46a7.

   THREE EVENTS, because two of them are not enough:

     :submitted  the task was accepted and the ack went out
     :result     the work finished and produced something to hand back
     :delivered  the caller actually took it

   Journalling only `:result` would still lose the case the card is named
   for: accepted, acknowledged, and then the process dies BEFORE any result
   exists. Replay turns such a task into an `:interrupted` entry, so the
   caller learns its work died instead of waiting on an ack forever.

   ON FSYNC, deliberately absent. The threat is a process kill, not a power
   cut. Data handed to `write(2)` lives in the kernel page cache and survives
   SIGKILL, the OOM killer and a JVM crash; only the machine losing power
   loses it. fsync on every result would buy protection against a failure
   mode nobody has observed here, at a cost paid on every async call.

   A TORN LAST RECORD IS EXPECTED, for exactly the same reason: the process
   can die mid-append. `read-records` therefore drops an unreadable trailing
   line rather than refusing the whole journal. A journal that cannot be read
   at all yields no records, never an exception: losing replay is bad, and
   failing to start is worse."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn default-path
  "Where the journal lives when nothing overrides it.

   Beside the other hive-mcp on-disk state, which already sits under
   ~/.config/hive-mcp/data (see the cartography sentinel path)."
  []
  (str (System/getProperty "user.home")
       "/.config/hive-mcp/data/async-result/journal.edn"))

(def ^:dynamic *journal-path*
  "The journal file. Rebound by tests; there is one per coordinator process."
  nil)

(defn journal-path [] (or *journal-path* (default-path)))

(defonce ^:private degraded
  ;; Set when an append fails. Durability that has silently stopped working
  ;; is worse than none, because the promise keeps being made. `status`
  ;; surfaces this so a caller or an operator can see it.
  (atom nil))

(defn status
  "What the journal is currently able to promise."
  []
  (let [f (io/file (journal-path))]
    {:path (str f)
     :exists? (.exists f)
     :size-bytes (if (.exists f) (.length f) 0)
     :degraded (deref degraded)}))

;; ---------------------------------------------------------------- write

(defn append!
  "Append one record. Returns true when it reached the file.

   NEVER throws. A journal failure must not break the call it is journalling:
   the result still belongs in the buffer, and a caller that loses durability
   is better off than a caller that loses the result outright. The failure is
   logged once and latched into `status`, so it is visible rather than
   merely survived."
  [record]
  (try
    (let [f (io/file (journal-path))]
      (io/make-parents f)
      (spit f (str (pr-str record) "\n") :append true))
    (when @degraded
      (log/info "async-result journal: writes are working again")
      (reset! degraded nil))
    true
    (catch Throwable t
      (when-not @degraded
        (log/error t "async-result journal: append failed; results are NO LONGER durable"
                   {:path (journal-path)}))
      (reset! degraded (str (.getName (class t)) ": " (.getMessage t)))
      false)))

(defn record-submitted!
  "Journal that `task-id` was accepted for `caller` and acknowledged."
  [caller task-id tool]
  (append! {:op :submitted :caller caller :task-id task-id :tool tool
            :at (System/currentTimeMillis)}))

(defn record-result!
  "Journal the result now owed to `caller`."
  [caller task-id entry]
  (append! {:op :result :caller caller :task-id task-id :entry entry
            :at (System/currentTimeMillis)}))

(defn record-delivered!
  "Journal that `caller` has been handed `task-ids`, so replay skips them."
  [caller task-ids]
  (if (seq task-ids)
    (append! {:op :delivered :caller caller :task-ids (vec task-ids)
              :at (System/currentTimeMillis)})
    true))

;; ---------------------------------------------------------------- read

(defn- parse-line
  "One journal line as a record, or nil when it cannot be read."
  [line]
  (try
    (let [r (edn/read-string line)]
      (when (and (map? r) (:op r)) r))
    (catch Throwable _ nil)))

(defn read-records
  "Every readable record in the journal, in order. Never throws.

   A trailing record the process died halfway through writing is dropped
   silently, because that is the normal shape of a journal after the very
   crash it exists to survive. A record dropped anywhere EARLIER is logged:
   mid-file corruption is not expected and means something else is wrong."
  []
  (let [f (io/file (journal-path))]
    (if-not (.exists f)
      []
      (try
        (let [lines (->> (slurp f) str/split-lines (remove str/blank?))
              n (count lines)
              parsed (map-indexed (fn [idx line] [idx (parse-line line)]) lines)
              bad-early (seq (for [[idx r] parsed :when (and (nil? r) (< idx (dec n)))] idx))]
          (when bad-early
            (log/warn "async-result journal: unreadable record(s) mid-file"
                      {:lines bad-early :total n}))
          (vec (keep second parsed)))
        (catch Throwable t
          (log/error t "async-result journal: unreadable; replay will restore nothing")
          [])))))

;; ---------------------------------------------------------------- replay

(def ^:const interrupted-error
  "What a caller is told about a task that never produced a result."
  "the coordinator died before this task produced a result")

(defn replay
  "Buffers reconstructed from journal `records`. PURE.

   Shaped exactly like `async-result/buffers`, with every restored entry
   UNDELIVERED (`:cursor 0`), because an entry the journal still carries is
   by definition one the caller was never given.

   A task with a `:result` is restored as that result. A task left at
   `:submitted` is restored as `:status :interrupted`: its work died with the
   process, and saying so is the entire point, since the alternative is the
   caller waiting forever on an ack it already received."
  [records]
  (let [delivered (into #{}
                        (mapcat (fn [{:keys [op caller task-ids]}]
                                  (when (= :delivered op)
                                    (map (fn [t] [caller t]) task-ids))))
                        records)
        kept (remove (fn [{:keys [op caller task-id]}]
                       (or (= :delivered op) (delivered [caller task-id])))
                     records)
        ;; A later record for the same task supersedes an earlier one, so a
        ;; :result replaces the :submitted that preceded it.
        latest (reduce (fn [m r] (assoc m [(:caller r) (:task-id r)] r)) {} kept)
        order (distinct (map (juxt :caller :task-id) kept))]
    (reduce
     (fn [acc k]
       (let [{:keys [op caller task-id tool entry at]} (latest k)
             e (if (= :result op)
                 entry
                 {:task-id task-id :tool tool :status :interrupted
                  :error interrupted-error
                  :timestamp (long (/ (or at 0) 1000))})]
         (update acc caller
                 (fn [buf] (update (or buf {:entries [] :cursor 0}) :entries conj e)))))
     {}
     order)))

(defn restore
  "The buffers the journal on disk implies. Never throws."
  []
  (replay (read-records)))

;; ---------------------------------------------------------------- compact

(defn compact!
  "Rewrite the journal so it carries only what `bufs` still owes.

   Without this the file grows without bound: every delivered result stays on
   disk forever behind its own tombstone. Written to a sibling and renamed,
   so a crash mid-compaction leaves either the old journal or the new one and
   never a half-written one.

   Writes NOTHING when there is nothing owed and no journal already exists,
   so a process that never enqueues an async result never creates the file.
   Returns the number of records written, or nil when compaction failed."
  [bufs]
  (try
    (let [f (io/file (journal-path))
          records (vec (for [[caller {:keys [entries cursor]}] bufs
                             e (drop cursor entries)]
                         {:op :result :caller caller :task-id (:task-id e) :entry e
                          :at (System/currentTimeMillis)}))]
      (if (and (empty? records) (not (.exists f)))
        0
        (let [tmp (io/file (str (journal-path) ".compacting"))]
          (io/make-parents tmp)
          (spit tmp (str/join (map #(str (pr-str %) "\n") records)))
          (.renameTo tmp f)
          (count records))))
    (catch Throwable t
      (log/error t "async-result journal: compaction failed; the journal keeps growing")
      nil)))

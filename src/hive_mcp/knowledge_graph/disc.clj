(ns hive-mcp.knowledge-graph.disc
  "Disc entity management for L1 (file) abstraction level.

   Disc entities track the actual state of files on disk, enabling:
   - Grounding verification without re-reading files
   - Change detection via content hash comparison
   - Git commit tracking for provenance

   CLARITY-Y: Graceful failure with status codes instead of exceptions."
  (:require [hive-mcp.knowledge-graph.connection :as conn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.security MessageDigest]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Hash Utilities
;; =============================================================================

(defn compute-hash
  "Compute SHA-256 hash of content string.
   Returns hex string."
  [content]
  (let [md (MessageDigest/getInstance "SHA-256")
        hash-bytes (.digest md (.getBytes (str content) "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) hash-bytes))))

(defn file-content-hash
  "Read file and compute content hash.
   Returns {:hash \"..\" :exists? true} or {:exists? false}."
  [path]
  (try
    (let [file (io/file path)]
      (if (.exists file)
        {:hash (compute-hash (slurp file)) :exists? true}
        {:exists? false}))
    (catch Exception e
      (log/warn "Failed to hash file" {:path path :error (.getMessage e)})
      {:exists? false :error (.getMessage e)})))

;; =============================================================================
;; Disc Entity CRUD
;; =============================================================================

(defn add-disc!
  "Create or update a disc entity for a file path.

   Arguments:
     opts - Map with:
       :path         - File path (required, unique identity)
       :content-hash - SHA256 of file content
       :analyzed-at  - Timestamp of analysis (defaults to now)
       :git-commit   - Git commit hash when analyzed
       :project-id   - Project scope

   Returns the entity ID."
  [{:keys [path content-hash analyzed-at git-commit project-id]}]
  {:pre [(string? path) (seq path)]}
  (let [now (java.util.Date.)
        tx-data [{:disc/path path
                  :disc/content-hash (or content-hash "")
                  :disc/analyzed-at (or analyzed-at now)
                  :disc/git-commit (or git-commit "")
                  :disc/project-id (or project-id "global")}]
        result (conn/transact! tx-data)]
    (log/debug "Added/updated disc entity" {:path path})
    ;; Return the entity ID
    (-> result :tx-data first :e)))

(defn get-disc
  "Get disc entity by file path.
   Returns entity map or nil if not found."
  [path]
  {:pre [(string? path)]}
  (let [result (conn/query '[:find (pull ?e [*])
                             :in $ ?path
                             :where [?e :disc/path ?path]]
                           path)]
    (ffirst result)))

(defn get-disc-by-id
  "Get disc entity by entity ID.
   Returns entity map or nil if not found."
  [eid]
  (when-let [e (conn/entity eid)]
    (when (:disc/path e)
      (into {} e))))

(defn disc-exists?
  "Check if a disc entity exists for the given path."
  [path]
  (some? (get-disc path)))

(defn update-disc!
  "Update a disc entity.
   Path is used to find the entity; other fields are updated."
  [path updates]
  {:pre [(string? path)]}
  (when-let [_existing (get-disc path)]
    (let [tx-data [(merge {:disc/path path} updates)]
          _ (conn/transact! tx-data)]
      (log/debug "Updated disc entity" {:path path :updates (keys updates)})
      (get-disc path))))

(defn remove-disc!
  "Remove a disc entity by path.
   Returns true if removed, nil if not found."
  [path]
  {:pre [(string? path)]}
  (when-let [disc (get-disc path)]
    (let [eid (:db/id disc)]
      (conn/transact! [[:db.fn/retractEntity eid]])
      (log/debug "Removed disc entity" {:path path})
      true)))

;; =============================================================================
;; Disc Queries
;; =============================================================================

(defn get-all-discs
  "Get all disc entities.
   Optional project-id filter."
  [& {:keys [project-id]}]
  (let [results (if project-id
                  (conn/query '[:find (pull ?e [*])
                                :in $ ?pid
                                :where
                                [?e :disc/path _]
                                [?e :disc/project-id ?pid]]
                              project-id)
                  (conn/query '[:find (pull ?e [*])
                                :where [?e :disc/path _]]))]
    (map first results)))

(defn get-stale-discs
  "Get disc entities with content hash mismatch.
   Computes current file hash and compares with stored hash.
   Returns seq of {:disc ... :current-hash ... :stale? true/false}."
  [& {:keys [project-id]}]
  (let [discs (get-all-discs :project-id project-id)]
    (->> discs
         (map (fn [disc]
                (let [path (:disc/path disc)
                      stored-hash (:disc/content-hash disc)
                      {:keys [hash exists?]} (file-content-hash path)
                      stale? (or (not exists?)
                                 (and hash stored-hash (not= hash stored-hash)))]
                  {:disc disc
                   :current-hash hash
                   :exists? exists?
                   :stale? stale?})))
         (filter :stale?)
         vec)))

(defn refresh-disc!
  "Refresh a disc entity by re-reading the file.
   Updates content-hash and analyzed-at.
   Returns {:status :refreshed|:not-found|:file-missing :disc ...}."
  [path & {:keys [git-commit]}]
  {:pre [(string? path)]}
  (let [{:keys [hash exists?]} (file-content-hash path)]
    (cond
      (not exists?)
      {:status :file-missing :path path}

      :else
      (let [updates {:disc/content-hash hash
                     :disc/analyzed-at (java.util.Date.)}
            updates (if git-commit
                      (assoc updates :disc/git-commit git-commit)
                      updates)]
        (if (disc-exists? path)
          (do (update-disc! path updates)
              {:status :refreshed :disc (get-disc path)})
          (do (add-disc! (merge {:path path :content-hash hash} updates))
              {:status :created :disc (get-disc path)}))))))

;; =============================================================================
;; Read Tracking
;; =============================================================================

(defn touch-disc!
  "Record that a file was read by an agent.
   Creates the disc entity if it doesn't exist, updates last-read-at and
   increments read-count. Returns the updated disc entity.

   Arguments:
     path       - File path (required)
     project-id - Project scope (optional, defaults to 'global')"
  [path & {:keys [project-id]}]
  {:pre [(string? path) (seq path)]}
  (let [now (java.util.Date.)]
    (if (disc-exists? path)
      ;; Update existing: bump last-read-at and read-count
      (let [existing (get-disc path)
            current-count (or (:disc/read-count existing) 0)]
        (update-disc! path {:disc/last-read-at now
                            :disc/read-count (inc current-count)})
        (get-disc path))
      ;; Create new: also compute content hash for fresh tracking
      (let [{:keys [hash]} (file-content-hash path)]
        (add-disc! {:path path
                    :content-hash (or hash "")
                    :project-id (or project-id "global")})
        (update-disc! path {:disc/last-read-at now
                            :disc/read-count 1})
        (get-disc path)))))

(defn staleness-score
  "Compute staleness score for a disc entity.
   Score ranges from 0.0 (fresh) to 1.0 (very stale).

   Factors:
   - Hash mismatch (content changed since last analysis): +0.5
   - Time since last read (>7 days: +0.3, >30 days: +0.5)
   - Never analyzed: +0.2

   Arguments:
     disc - Disc entity map

   Returns:
     Float score 0.0-1.0"
  [disc]
  (let [now-ms (System/currentTimeMillis)
        day-ms (* 24 60 60 1000)
        ;; Check hash staleness
        path (:disc/path disc)
        {:keys [hash exists?]} (when path (file-content-hash path))
        hash-stale? (and exists? hash (:disc/content-hash disc)
                         (not= hash (:disc/content-hash disc)))
        ;; Check time since last read
        last-read (:disc/last-read-at disc)
        days-since-read (when last-read
                          (/ (- now-ms (.getTime ^java.util.Date last-read)) day-ms))
        ;; Check if ever analyzed
        never-analyzed? (nil? (:disc/analyzed-at disc))]
    (min 1.0
         (+ (if hash-stale? 0.5 0.0)
            (cond
              (nil? days-since-read) 0.3
              (> days-since-read 30) 0.5
              (> days-since-read 7) 0.3
              :else 0.0)
            (if never-analyzed? 0.2 0.0)))))

;; =============================================================================
;; L1 Disc Surfacing — Proactive Staleness Reporting
;; =============================================================================

(defn staleness-report
  "Compute staleness score and diagnostic info for a disc entity in one pass.
   Avoids redundant file I/O compared to calling staleness-score separately.

   Returns {:score :days-since-read :hash-mismatch? :never-analyzed?}"
  [disc]
  (let [now-ms (System/currentTimeMillis)
        day-ms (* 24 60 60 1000)
        path (:disc/path disc)
        {:keys [hash exists?]} (when path (file-content-hash path))
        hash-mismatch? (boolean
                        (and exists? hash (:disc/content-hash disc)
                             (not= hash (:disc/content-hash disc))))
        last-read (:disc/last-read-at disc)
        days-since-read (when last-read
                          (long (/ (- now-ms (.getTime ^java.util.Date last-read))
                                   day-ms)))
        never-analyzed? (nil? (:disc/analyzed-at disc))
        score (min 1.0
                   (+ (if hash-mismatch? 0.5 0.0)
                      (cond
                        (nil? days-since-read) 0.3
                        (> days-since-read 30) 0.5
                        (> days-since-read 7) 0.3
                        :else 0.0)
                      (if never-analyzed? 0.2 0.0)))]
    {:score score
     :days-since-read days-since-read
     :hash-mismatch? hash-mismatch?
     :never-analyzed? never-analyzed?}))

(defn staleness-warnings
  "Generate staleness warnings for a collection of file paths.
   Only returns warnings for files with existing disc entities that are stale.
   Fresh files and files without disc entities produce no output (zero noise).

   Arguments:
     paths - Collection of file path strings

   Returns:
     Vector of {:path :staleness :message} for stale files only (staleness > 0.3)"
  [paths]
  (when (seq paths)
    (->> paths
         (keep (fn [path]
                 (when-let [disc (get-disc path)]
                   (let [{:keys [score days-since-read hash-mismatch?]}
                         (staleness-report disc)]
                     (when (> score 0.3)
                       {:path path
                        :staleness score
                        :message (format "NOTE: file %s is stale (staleness: %.1f%s%s). Re-read carefully."
                                         path
                                         (float score)
                                         (if days-since-read
                                           (format ", last read %dd ago" days-since-read)
                                           ", never read")
                                         (if hash-mismatch?
                                           ", hash mismatch"
                                           ""))})))))
         vec)))

(defn format-staleness-warnings
  "Format staleness warnings as a text block for injection into task prompts.
   Returns nil if no warnings (zero noise for fresh files)."
  [warnings]
  (when (seq warnings)
    (str "## L1 Disc Staleness Warnings\n"
         (str/join "\n" (map :message warnings))
         "\n\n")))

(defn top-stale-files
  "Query top-N most stale disc entities.
   Returns vector of {:path :score :days-since-read :hash-mismatch?}
   sorted by staleness score descending.

   Arguments:
     n          - Max entries to return (default: 5)
     project-id - Optional project filter
     threshold  - Minimum staleness score (default: 0.5)"
  [& {:keys [n project-id threshold] :or {n 5 threshold 0.5}}]
  (let [discs (get-all-discs :project-id project-id)]
    (->> discs
         (map (fn [disc]
                (let [report (staleness-report disc)]
                  (assoc report :path (:disc/path disc)))))
         (filter #(> (:score %) threshold))
         (sort-by :score >)
         (take n)
         vec)))

;; =============================================================================
;; KG-First Context — Read the Map Before the Territory
;; =============================================================================

(defn- classify-disc
  "Classify a single file path's KG status.
   Returns a map with :path, :disc, :status (:fresh | :stale | :missing),
   and diagnostic fields.

   Staleness threshold: files with score <= threshold are considered fresh.
   Default threshold: 0.3 (matches staleness-warnings threshold)."
  [path staleness-threshold]
  (try
    (if-let [disc (get-disc path)]
      (let [{:keys [score days-since-read hash-mismatch? never-analyzed?]}
            (staleness-report disc)]
        {:path path
         :disc disc
         :status (if (<= score staleness-threshold) :fresh :stale)
         :staleness-score score
         :days-since-read days-since-read
         :hash-mismatch? hash-mismatch?
         :never-analyzed? never-analyzed?
         :read-count (or (:disc/read-count disc) 0)
         :last-read-at (:disc/last-read-at disc)})
      ;; No disc entity — file has never been tracked
      {:path path
       :disc nil
       :status :missing
       :staleness-score 1.0
       :days-since-read nil
       :hash-mismatch? false
       :never-analyzed? true
       :read-count 0
       :last-read-at nil})
    (catch Exception e
      (log/warn "KG classify-disc failed, treating as missing"
                {:path path :error (.getMessage e)})
      {:path path
       :disc nil
       :status :missing
       :staleness-score 1.0
       :days-since-read nil
       :hash-mismatch? false
       :never-analyzed? true
       :read-count 0
       :last-read-at nil})))

(defn kg-first-context
  "Consult the KG before file reads — the Structural Differential principle.
   Labels (KG) before disc (files).

   Takes a collection of file paths and classifies each based on KG freshness:
   - :kg-known  — Files with fresh KG data (skip file read, use KG knowledge)
   - :needs-read — Files with no KG data (must read from disc)
   - :stale     — Files with stale KG data (should re-read from disc)

   Arguments:
     paths - Collection of file path strings
     opts  - Optional map:
       :staleness-threshold - Score cutoff for fresh vs stale (default: 0.3)
                              0.0 = only perfectly fresh, 1.0 = everything is fresh

   Returns:
     {:kg-known   {path {:disc ... :staleness-score ... :read-count ...} ...}
      :needs-read [path ...]
      :stale      [path ...]
      :summary    {:total N :known N :needs-read N :stale N}}

   CLARITY-Y: Individual file failures are logged and treated as :needs-read.
   CLARITY-A: Single-pass classification, no redundant file I/O."
  [paths & [{:keys [staleness-threshold] :or {staleness-threshold 0.3}}]]
  (let [unique-paths (distinct (filter (every-pred string? seq) paths))
        classified (mapv #(classify-disc % staleness-threshold) unique-paths)
        grouped (group-by :status classified)
        ;; Build the result maps
        kg-known (->> (:fresh grouped)
                      (reduce (fn [m entry]
                                (assoc m (:path entry)
                                       (dissoc entry :path :status)))
                              {}))
        needs-read (mapv :path (:missing grouped))
        stale (mapv :path (:stale grouped))]
    {:kg-known kg-known
     :needs-read needs-read
     :stale stale
     :summary {:total (count unique-paths)
               :known (count kg-known)
               :needs-read (count needs-read)
               :stale (count stale)}}))

;; =============================================================================
;; Disc Statistics
;; =============================================================================

(defn disc-stats
  "Get statistics about disc entities.
   Returns {:total int :by-project {...} :stale-count int}."
  []
  (let [all-discs (get-all-discs)
        stale (get-stale-discs)]
    {:total (count all-discs)
     :by-project (frequencies (map :disc/project-id all-discs))
     :stale-count (count stale)}))

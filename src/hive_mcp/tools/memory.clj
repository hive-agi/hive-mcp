(ns hive-mcp.tools.memory
  "MCP tool handlers for memory operations.

   Chroma-only storage: All memory is stored in Chroma vector database.
   Elisp is only used for getting current project context (not storage).

   Architecture (post-migration):
   - Chroma: Single source of truth for all memory data
   - Clojure: All CRUD operations via chroma.clj
   - Elisp: Thin API layer (optional, for backward compatibility)"
  (:require [hive-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.elisp :as el]
            [hive-mcp.chroma :as chroma]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; ============================================================
;; Configuration
;; ============================================================

(def ^:private duration-days
  "Duration category to days mapping. nil = permanent (never expires)."
  {"ephemeral" 1
   "short" 7
   "medium" 30
   "long" 90
   "permanent" nil})

;; ============================================================
;; Helpers
;; ============================================================

(defn- get-current-project-id
  "Get current project ID from Emacs, or 'global' if not in a project."
  []
  (try
    (let [{:keys [success result]} (ec/eval-elisp "(hive-mcp-memory--project-id)")]
      (if (and success result (not= result "nil"))
        (str/replace result #"\"" "")
        "global"))
    (catch Exception _
      "global")))

(defn- calculate-expires
  "Calculate expiration timestamp for given duration."
  [duration]
  (when-let [days (get duration-days (or duration "long"))]
    (str (.plusDays (java.time.ZonedDateTime/now) days))))

(defn- inject-project-scope
  "Add project scope tag if not already present."
  [tags project-id]
  (let [has-scope? (some #(str/starts-with? % "scope:") tags)]
    (if has-scope?
      tags
      (if (= project-id "global")
        (conj (vec tags) "scope:global")
        (conj (vec tags) (str "scope:project:" project-id))))))

(defn- matches-scope?
  "Check if entry matches the given scope filter."
  [entry scope-filter]
  (let [tags (or (:tags entry) [])]
    (cond
      ;; No filter or "all" - match everything
      (or (nil? scope-filter) (= scope-filter "all"))
      true

      ;; "global" - only global scope
      (= scope-filter "global")
      (some #(= % "scope:global") tags)

      ;; Specific scope tag
      :else
      (or (some #(= % scope-filter) tags)
          (some #(= % "scope:global") tags)))))

(defn- entry->json-alist
  "Convert entry map to JSON-serializable format."
  [entry]
  (-> entry
      (update :tags #(or % []))
      (dissoc :document)))  ; Remove internal field

(defn handle-mcp-memory-add
  "Add an entry to project memory (Chroma-only storage).
   Stores full entry in Chroma with content, metadata, and embedding."
  [{:keys [type content tags duration]}]
  (log/info "mcp-memory-add:" type)
  (if-not (chroma/embedding-configured?)
    {:type "text"
     :text (json/write-str {:error "Chroma not configured"
                            :message "Memory storage requires Chroma with embedding provider"})
     :isError true}
    (try
      (let [project-id (get-current-project-id)
            tags-with-scope (inject-project-scope (or tags []) project-id)
            content-hash (chroma/content-hash content)
            duration-str (or duration "long")
            expires (calculate-expires duration-str)
            ;; Check for duplicate
            existing (chroma/find-duplicate type content-hash :project-id project-id)]
        (if existing
          ;; Duplicate found - merge tags and return existing
          (let [merged-tags (distinct (concat (:tags existing) tags-with-scope))
                updated (chroma/update-entry! (:id existing) {:tags merged-tags})]
            (log/info "Duplicate found, merged tags:" (:id existing))
            {:type "text" :text (json/write-str (entry->json-alist updated))})
          ;; Create new entry
          (let [entry-id (chroma/index-memory-entry!
                          {:type type
                           :content content
                           :tags tags-with-scope
                           :content-hash content-hash
                           :duration duration-str
                           :expires (or expires "")
                           :project-id project-id})
                created (chroma/get-entry-by-id entry-id)]
            (log/info "Created memory entry:" entry-id)
            {:type "text" :text (json/write-str (entry->json-alist created))})))
      (catch Exception e
        (log/error "Memory add failed:" (ex-message e))
        {:type "text"
         :text (json/write-str {:error (str "Memory add failed: " (ex-message e))})
         :isError true}))))

(defn handle-mcp-memory-query
  "Query project memory by type with scope filtering (Chroma-only).
  SCOPE controls which memories are returned:
    - nil/omitted: auto-filter by current project + global
    - \"all\": return all entries regardless of scope
    - \"global\": return only scope:global entries
    - specific scope tag: filter by that scope"
  [{:keys [type tags limit duration scope]}]
  (log/info "mcp-memory-query:" type "scope:" scope)
  (if-not (chroma/embedding-configured?)
    {:type "text"
     :text (json/write-str {:error "Chroma not configured"
                            :message "Memory query requires Chroma with embedding provider"})
     :isError true}
    (try
      (let [project-id (get-current-project-id)
            limit-val (or limit 20)
            ;; Query from Chroma
            entries (chroma/query-entries :type type
                                          :project-id (when (nil? scope) project-id)
                                          :limit (* limit-val 5))  ; Over-fetch for filtering
            ;; Apply scope filter
            scope-filter (cond
                           (nil? scope) (str "scope:project:" project-id)
                           (= scope "all") nil
                           :else scope)
            filtered (if scope-filter
                       (filter #(matches-scope? % scope-filter) entries)
                       entries)
            ;; Apply tag filter
            tag-filtered (if (seq tags)
                           (filter (fn [entry]
                                     (let [entry-tags (set (:tags entry))]
                                       (every? #(contains? entry-tags %) tags)))
                                   filtered)
                           filtered)
            ;; Apply duration filter
            dur-filtered (if duration
                           (filter #(= (:duration %) duration) tag-filtered)
                           tag-filtered)
            ;; Apply limit
            results (take limit-val dur-filtered)]
        {:type "text" :text (json/write-str (mapv entry->json-alist results))})
      (catch Exception e
        (log/error "Memory query failed:" (ex-message e))
        {:type "text"
         :text (json/write-str {:error (str "Memory query failed: " (ex-message e))})
         :isError true}))))

(defn- entry->metadata
  "Convert entry to metadata-only format (id, type, preview, tags, created)."
  [entry]
  (let [content (:content entry)
        preview (cond
                  (string? content)
                  (subs content 0 (min 100 (count content)))

                  (map? content)
                  (or (:description content)
                      (:title content)
                      (:name content)
                      (subs (json/write-str content) 0 (min 100 (count (json/write-str content)))))

                  :else
                  (str content))]
    {:id (:id entry)
     :type (:type entry)
     :preview (if (> (count (str preview)) 97)
                (str (subs (str preview) 0 97) "...")
                preview)
     :tags (or (:tags entry) [])
     :created (:created entry)}))

(defn handle-mcp-memory-query-metadata
  "Query project memory by type, returning only metadata (Chroma-only).
  Returns id, type, preview, tags, created - ~10x fewer tokens than full query.
  Follow up with mcp_memory_get_full to fetch specific entries."
  [{:keys [type tags limit scope]}]
  (log/info "mcp-memory-query-metadata:" type "scope:" scope)
  (if-not (chroma/embedding-configured?)
    {:type "text"
     :text (json/write-str {:error "Chroma not configured"})
     :isError true}
    (try
      (let [;; Reuse query logic
            {:keys [text isError]} (handle-mcp-memory-query
                                    {:type type :tags tags :limit limit :scope scope})]
        (if isError
          {:type "text" :text text :isError true}
          (let [entries (json/read-str text :key-fn keyword)
                metadata (mapv entry->metadata entries)]
            {:type "text" :text (json/write-str metadata)})))
      (catch Exception e
        (log/error "Memory query-metadata failed:" (ex-message e))
        {:type "text"
         :text (json/write-str {:error (str "Query failed: " (ex-message e))})
         :isError true}))))

(defn handle-mcp-memory-get-full
  "Get full content of a memory entry by ID (Chroma-only).
  Use after mcp_memory_query_metadata to fetch specific entries."
  [{:keys [id]}]
  (log/info "mcp-memory-get-full:" id)
  (if-not (chroma/embedding-configured?)
    {:type "text"
     :text (json/write-str {:error "Chroma not configured"})
     :isError true}
    (try
      (if-let [entry (chroma/get-entry-by-id id)]
        {:type "text" :text (json/write-str (entry->json-alist entry))}
        {:type "text" :text (json/write-str {:error "Entry not found" :id id}) :isError true})
      (catch Exception e
        {:type "text"
         :text (json/write-str {:error (str "Get failed: " (ex-message e))})
         :isError true}))))

(defn handle-mcp-memory-search-semantic
  "Search project memory using semantic similarity (vector search).
   Requires Chroma to be configured with an embedding provider."
  [{:keys [query limit type]}]
  (log/info "mcp-memory-search-semantic:" query)
  (let [status (chroma/status)]
    (if-not (:configured? status)
      {:type "text"
       :text (json/write-str
              {:error "Chroma semantic search not configured"
               :message "To enable semantic search, configure Chroma with an embedding provider. See hive-mcp.chroma namespace."
               :status status})
       :isError true}
      (try
        (let [results (chroma/search-similar query
                                             :limit (or limit 10)
                                             :type type)
              ;; Format results for user-friendly output
              formatted (mapv (fn [{:keys [id document metadata distance]}]
                                {:id id
                                 :type (get metadata :type)
                                 :tags (when-let [t (get metadata :tags)]
                                         (when (not= t "")
                                           (str/split t #",")))
                                 :distance distance
                                 :preview (when document
                                            (subs document 0 (min 200 (count document))))})
                              results)]
          {:type "text"
           :text (json/write-str {:results formatted
                                  :count (count formatted)
                                  :query query})})
        (catch Exception e
          {:type "text"
           :text (json/write-str {:error (str "Semantic search failed: " (.getMessage e))
                                  :status status})
           :isError true})))))

(def ^:private duration-order
  "Duration categories in order from shortest to longest."
  ["ephemeral" "short" "medium" "long" "permanent"])

(defn handle-mcp-memory-set-duration
  "Set duration category for a memory entry (Chroma-only)."
  [{:keys [id duration]}]
  (log/info "mcp-memory-set-duration:" id duration)
  (if-not (chroma/embedding-configured?)
    {:type "text" :text (json/write-str {:error "Chroma not configured"}) :isError true}
    (try
      (let [expires (calculate-expires duration)
            updated (chroma/update-entry! id {:duration duration
                                              :expires (or expires "")})]
        (if updated
          {:type "text" :text (json/write-str (entry->json-alist updated))}
          {:type "text" :text (json/write-str {:error "Entry not found"}) :isError true}))
      (catch Exception e
        {:type "text" :text (json/write-str {:error (ex-message e)}) :isError true}))))

(defn handle-mcp-memory-promote
  "Promote memory entry to longer duration (Chroma-only)."
  [{:keys [id]}]
  (log/info "mcp-memory-promote:" id)
  (if-not (chroma/embedding-configured?)
    {:type "text" :text (json/write-str {:error "Chroma not configured"}) :isError true}
    (try
      (if-let [entry (chroma/get-entry-by-id id)]
        (let [current-dur (or (:duration entry) "long")
              idx (.indexOf duration-order current-dur)
              next-idx (min (inc idx) (dec (count duration-order)))
              new-dur (nth duration-order next-idx)]
          (if (= new-dur current-dur)
            {:type "text" :text (json/write-str {:message "Already at maximum duration"
                                                 :duration current-dur})}
            (let [expires (calculate-expires new-dur)
                  updated (chroma/update-entry! id {:duration new-dur
                                                    :expires (or expires "")})]
              {:type "text" :text (json/write-str (entry->json-alist updated))})))
        {:type "text" :text (json/write-str {:error "Entry not found"}) :isError true})
      (catch Exception e
        {:type "text" :text (json/write-str {:error (ex-message e)}) :isError true}))))

(defn handle-mcp-memory-demote
  "Demote memory entry to shorter duration (Chroma-only)."
  [{:keys [id]}]
  (log/info "mcp-memory-demote:" id)
  (if-not (chroma/embedding-configured?)
    {:type "text" :text (json/write-str {:error "Chroma not configured"}) :isError true}
    (try
      (if-let [entry (chroma/get-entry-by-id id)]
        (let [current-dur (or (:duration entry) "long")
              idx (.indexOf duration-order current-dur)
              prev-idx (max (dec idx) 0)
              new-dur (nth duration-order prev-idx)]
          (if (= new-dur current-dur)
            {:type "text" :text (json/write-str {:message "Already at minimum duration"
                                                 :duration current-dur})}
            (let [expires (calculate-expires new-dur)
                  updated (chroma/update-entry! id {:duration new-dur
                                                    :expires (or expires "")})]
              {:type "text" :text (json/write-str (entry->json-alist updated))})))
        {:type "text" :text (json/write-str {:error "Entry not found"}) :isError true})
      (catch Exception e
        {:type "text" :text (json/write-str {:error (ex-message e)}) :isError true}))))

(defn handle-mcp-memory-cleanup-expired
  "Remove all expired memory entries (Chroma-only)."
  [_]
  (log/info "mcp-memory-cleanup-expired")
  (if-not (chroma/embedding-configured?)
    {:type "text" :text (json/write-str {:error "Chroma not configured"}) :isError true}
    (try
      (let [count (chroma/cleanup-expired!)]
        {:type "text" :text (json/write-str {:deleted count})})
      (catch Exception e
        {:type "text" :text (json/write-str {:error (ex-message e)}) :isError true}))))

(defn handle-mcp-memory-expiring-soon
  "List memory entries expiring within N days (Chroma-only)."
  [{:keys [days]}]
  (log/info "mcp-memory-expiring-soon:" (or days 7))
  (if-not (chroma/embedding-configured?)
    {:type "text" :text (json/write-str {:error "Chroma not configured"}) :isError true}
    (try
      (let [project-id (get-current-project-id)
            entries (chroma/entries-expiring-soon (or days 7) :project-id project-id)]
        {:type "text" :text (json/write-str (mapv entry->json-alist entries))})
      (catch Exception e
        {:type "text" :text (json/write-str {:error (ex-message e)}) :isError true}))))

(defn handle-mcp-memory-log-access
  "Log access to a memory entry (Chroma-only).
   Increments access-count and updates last-accessed timestamp."
  [{:keys [id]}]
  (log/info "mcp-memory-log-access:" id)
  (if-not (chroma/embedding-configured?)
    {:type "text" :text (json/write-str {:error "Chroma not configured"}) :isError true}
    (try
      (if-let [entry (chroma/get-entry-by-id id)]
        (let [new-count (inc (or (:access-count entry) 0))
              updated (chroma/update-entry! id {:access-count new-count
                                                :last-accessed (str (java.time.ZonedDateTime/now))})]
          {:type "text" :text (json/write-str (entry->json-alist updated))})
        {:type "text" :text (json/write-str {:error "Entry not found"}) :isError true})
      (catch Exception e
        {:type "text" :text (json/write-str {:error (ex-message e)}) :isError true}))))

(defn handle-mcp-memory-feedback
  "Submit helpfulness feedback for a memory entry (Chroma-only).
   feedback should be 'helpful' or 'unhelpful'."
  [{:keys [id feedback]}]
  (log/info "mcp-memory-feedback:" id feedback)
  (if-not (chroma/embedding-configured?)
    {:type "text" :text (json/write-str {:error "Chroma not configured"}) :isError true}
    (try
      (if-let [entry (chroma/get-entry-by-id id)]
        (let [updates (case feedback
                        "helpful" {:helpful-count (inc (or (:helpful-count entry) 0))}
                        "unhelpful" {:unhelpful-count (inc (or (:unhelpful-count entry) 0))}
                        (throw (ex-info "Invalid feedback type" {:feedback feedback})))
              updated (chroma/update-entry! id updates)]
          {:type "text" :text (json/write-str (entry->json-alist updated))})
        {:type "text" :text (json/write-str {:error "Entry not found"}) :isError true})
      (catch Exception e
        {:type "text" :text (json/write-str {:error (ex-message e)}) :isError true}))))

(defn handle-mcp-memory-helpfulness-ratio
  "Get helpfulness ratio for a memory entry (Chroma-only).
   Returns helpful/(helpful+unhelpful) or null if no feedback."
  [{:keys [id]}]
  (log/info "mcp-memory-helpfulness-ratio:" id)
  (if-not (chroma/embedding-configured?)
    {:type "text" :text (json/write-str {:error "Chroma not configured"}) :isError true}
    (try
      (if-let [entry (chroma/get-entry-by-id id)]
        (let [helpful (or (:helpful-count entry) 0)
              unhelpful (or (:unhelpful-count entry) 0)
              total (+ helpful unhelpful)
              ratio (when (pos? total) (/ (double helpful) total))]
          {:type "text" :text (json/write-str {:ratio ratio
                                               :helpful helpful
                                               :unhelpful unhelpful})})
        {:type "text" :text (json/write-str {:error "Entry not found"}) :isError true})
      (catch Exception e
        {:type "text" :text (json/write-str {:error (ex-message e)}) :isError true}))))

(defn handle-mcp-memory-check-duplicate
  "Check if content already exists in memory (Chroma-only)."
  [{:keys [type content]}]
  (log/info "mcp-memory-check-duplicate:" type)
  (if-not (chroma/embedding-configured?)
    {:type "text" :text (json/write-str {:error "Chroma not configured"}) :isError true}
    (try
      (let [project-id (get-current-project-id)
            hash (chroma/content-hash content)
            existing (chroma/find-duplicate type hash :project-id project-id)]
        {:type "text" :text (json/write-str {:exists (some? existing)
                                             :entry (when existing (entry->json-alist existing))
                                             :content_hash hash})})
      (catch Exception e
        {:type "text" :text (json/write-str {:error (ex-message e)}) :isError true}))))

(defn handle-mcp-memory-migrate-project
  "Migrate memory from one project-id to another (Chroma-only).
   Updates project-id and optionally scope tags for all matching entries."
  [{:keys [old-project-id new-project-id update-scopes]}]
  (log/info "mcp-memory-migrate-project:" old-project-id "->" new-project-id)
  (if-not (chroma/embedding-configured?)
    {:type "text" :text (json/write-str {:error "Chroma not configured"}) :isError true}
    (try
      (let [entries (chroma/query-entries :project-id old-project-id :limit 10000)
            migrated (atom 0)
            updated-scopes (atom 0)]
        (doseq [entry entries]
          (let [new-tags (if update-scopes
                           (mapv (fn [tag]
                                   (if (= tag (str "scope:project:" old-project-id))
                                     (do (swap! updated-scopes inc)
                                         (str "scope:project:" new-project-id))
                                     tag))
                                 (:tags entry))
                           (:tags entry))]
            (chroma/update-entry! (:id entry) {:project-id new-project-id
                                               :tags new-tags})
            (swap! migrated inc)))
        {:type "text" :text (json/write-str {:migrated @migrated
                                             :updated-scopes @updated-scopes
                                             :old-project-id old-project-id
                                             :new-project-id new-project-id})})
      (catch Exception e
        {:type "text" :text (json/write-str {:error (ex-message e)}) :isError true}))))

;; =============================================================================
;; Migration Tool (JSON to Chroma)
;; =============================================================================

(defn handle-mcp-memory-import-json
  "Import memory entries from JSON (for migrating from elisp JSON storage to Chroma).
   Reads existing JSON files from Emacs hive-mcp directory and imports to Chroma."
  [{:keys [project-id dry-run]}]
  (log/info "mcp-memory-import-json:" project-id "dry-run:" dry-run)
  (if-not (chroma/embedding-configured?)
    {:type "text" :text (json/write-str {:error "Chroma not configured"}) :isError true}
    (try
      ;; Get JSON data from elisp
      (let [pid (or project-id (get-current-project-id))
            elisp (format "(json-encode (list :notes (hive-mcp-memory-query 'note nil %s 1000 nil t)
                                              :snippets (hive-mcp-memory-query 'snippet nil %s 1000 nil t)
                                              :conventions (hive-mcp-memory-query 'convention nil %s 1000 nil t)
                                              :decisions (hive-mcp-memory-query 'decision nil %s 1000 nil t)))"
                          (pr-str pid) (pr-str pid) (pr-str pid) (pr-str pid))
            {:keys [success result error]} (ec/eval-elisp elisp)]
        (if-not success
          {:type "text" :text (json/write-str {:error (str "Failed to read JSON: " error)}) :isError true}
          (let [data (json/read-str result :key-fn keyword)
                all-entries (concat (:notes data) (:snippets data)
                                    (:conventions data) (:decisions data))
                imported (atom 0)
                skipped (atom 0)]
            (if dry-run
              {:type "text" :text (json/write-str {:dry-run true
                                                   :would-import (count all-entries)
                                                   :by-type {:notes (count (:notes data))
                                                             :snippets (count (:snippets data))
                                                             :conventions (count (:conventions data))
                                                             :decisions (count (:decisions data))}})}
              (do
                (doseq [entry all-entries]
                  ;; Check for existing entry
                  (if (chroma/get-entry-by-id (:id entry))
                    (swap! skipped inc)
                    (do
                      (chroma/index-memory-entry!
                       {:id (:id entry)
                        :type (:type entry)
                        :content (:content entry)
                        :tags (if (vector? (:tags entry))
                                (vec (:tags entry))
                                (:tags entry))
                        :content-hash (or (:content-hash entry)
                                          (chroma/content-hash (:content entry)))
                        :created (:created entry)
                        :updated (:updated entry)
                        :duration (or (:duration entry) "long")
                        :expires (or (:expires entry) "")
                        :access-count (or (:access-count entry) 0)
                        :helpful-count (or (:helpful-count entry) 0)
                        :unhelpful-count (or (:unhelpful-count entry) 0)
                        :project-id pid})
                      (swap! imported inc))))
                {:type "text" :text (json/write-str {:imported @imported
                                                     :skipped @skipped
                                                     :project-id pid})})))))
      (catch Exception e
        (log/error "Import failed:" (ex-message e))
        {:type "text" :text (json/write-str {:error (ex-message e)}) :isError true}))))

;; =============================================================================
;; Tool Definitions
;; =============================================================================

(def tools
  [{:name "mcp_memory_add"
    :description "Add an entry to project memory (Chroma storage). Types: note, snippet, convention, decision. Optionally specify duration for TTL: ephemeral (1 day), short (7 days), medium (30 days), long (90 days), permanent (never expires)."
    :inputSchema {:type "object"
                  :properties {"type" {:type "string"
                                       :enum ["note" "snippet" "convention" "decision"]
                                       :description "Type of memory entry"}
                               "content" {:type "string"
                                          :description "Content of the memory entry"}
                               "tags" {:type "array"
                                       :items {:type "string"}
                                       :description "Optional tags for categorization"}
                               "duration" {:type "string"
                                           :enum ["ephemeral" "short" "medium" "long" "permanent"]
                                           :description "Duration/TTL category (default: long)"}}
                  :required ["type" "content"]}
    :handler handle-mcp-memory-add}

   {:name "mcp_memory_query"
    :description "Query project memory by type with scope filtering (Chroma storage). Returns stored notes, snippets, conventions, or decisions filtered by scope (auto-filters by current project + global unless specified)."
    :inputSchema {:type "object"
                  :properties {"type" {:type "string"
                                       :enum ["note" "snippet" "convention" "decision" "conversation"]
                                       :description "Type of memory entries to query"}
                               "tags" {:type "array"
                                       :items {:type "string"}
                                       :description "Optional tags to filter by"}
                               "limit" {:type "integer"
                                        :description "Maximum number of results (default: 20)"}
                               "duration" {:type "string"
                                           :enum ["ephemeral" "short" "medium" "long" "permanent"]
                                           :description "Filter by duration category"}
                               "scope" {:type "string"
                                        :description "Scope filter: nil=auto (project+global), 'all'=no filter, 'global'=only global, or specific scope tag"}}
                  :required ["type"]}
    :handler handle-mcp-memory-query}

   {:name "mcp_memory_query_metadata"
    :description "Query project memory by type, returning only metadata (id, type, preview, tags, created). Use this for efficient browsing - returns ~10x fewer tokens than full query. Follow up with mcp_memory_get_full to fetch specific entries."
    :inputSchema {:type "object"
                  :properties {"type" {:type "string"
                                       :enum ["note" "snippet" "convention" "decision" "conversation"]
                                       :description "Type of memory entries to query"}
                               "tags" {:type "array"
                                       :items {:type "string"}
                                       :description "Optional tags to filter by"}
                               "limit" {:type "integer"
                                        :description "Maximum number of results (default: 20)"}
                               "scope" {:type "string"
                                        :description "Scope filter: nil=auto (project+global), 'all'=no filter, 'global'=only global, or specific scope tag"}}
                  :required ["type"]}
    :handler handle-mcp-memory-query-metadata}

   {:name "mcp_memory_get_full"
    :description "Get full content of a memory entry by ID. Use after mcp_memory_query_metadata to fetch specific entries when you need the full content."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "ID of the memory entry to retrieve"}}
                  :required ["id"]}
    :handler handle-mcp-memory-get-full}

   {:name "mcp_memory_search_semantic"
    :description "Search project memory using semantic similarity (vector search). Finds conceptually related entries even without exact keyword matches. Requires Chroma to be configured with an embedding provider."
    :inputSchema {:type "object"
                  :properties {"query" {:type "string"
                                        :description "Natural language query to search for semantically similar memory entries"}
                               "limit" {:type "integer"
                                        :description "Maximum number of results to return (default: 10)"}
                               "type" {:type "string"
                                       :enum ["note" "snippet" "convention" "decision"]
                                       :description "Optional filter by memory type"}}
                  :required ["query"]}
    :handler handle-mcp-memory-search-semantic}

   {:name "mcp_memory_set_duration"
    :description "Set the duration/TTL category for a memory entry. Use to manually adjust how long a memory persists."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "ID of the memory entry"}
                               "duration" {:type "string"
                                           :enum ["ephemeral" "short" "medium" "long" "permanent"]
                                           :description "New duration category"}}
                  :required ["id" "duration"]}
    :handler handle-mcp-memory-set-duration}

   {:name "mcp_memory_promote"
    :description "Promote a memory entry to a longer duration category. Moves ephemeral->short->medium->long->permanent."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "ID of the memory entry to promote"}}
                  :required ["id"]}
    :handler handle-mcp-memory-promote}

   {:name "mcp_memory_demote"
    :description "Demote a memory entry to a shorter duration category. Moves permanent->long->medium->short->ephemeral."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "ID of the memory entry to demote"}}
                  :required ["id"]}
    :handler handle-mcp-memory-demote}

   {:name "mcp_memory_cleanup_expired"
    :description "Remove all expired memory entries. Call periodically to clean up old ephemeral and short-lived memories."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-memory-cleanup-expired}

   {:name "mcp_memory_expiring_soon"
    :description "List memory entries expiring within the specified number of days. Useful for reviewing and promoting important memories before they expire."
    :inputSchema {:type "object"
                  :properties {"days" {:type "integer"
                                       :description "Number of days to look ahead (default: 7)"}}
                  :required []}
    :handler handle-mcp-memory-expiring-soon}

   {:name "mcp_memory_log_access"
    :description "Log access to a memory entry. Increments access-count and updates last-accessed timestamp. Use when retrieving a memory to track usage patterns."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "ID of the memory entry accessed"}}
                  :required ["id"]}
    :handler handle-mcp-memory-log-access}

   {:name "mcp_memory_feedback"
    :description "Submit helpfulness feedback for a memory entry. Tracks whether memories are useful for future retrieval decisions."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "ID of the memory entry"}
                               "feedback" {:type "string"
                                           :enum ["helpful" "unhelpful"]
                                           :description "Whether the memory was helpful"}}
                  :required ["id" "feedback"]}
    :handler handle-mcp-memory-feedback}

   {:name "mcp_memory_helpfulness_ratio"
    :description "Get the helpfulness ratio for a memory entry. Returns helpful/(helpful+unhelpful) or null if no feedback has been submitted."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "ID of the memory entry"}}
                  :required ["id"]}
    :handler handle-mcp-memory-helpfulness-ratio}

   {:name "mcp_memory_migrate_project"
    :description "Migrate memory from one project-id to another. Use after renaming a project directory or moving to stable .hive-project.edn based IDs. Optionally updates scope tags in entries."
    :inputSchema {:type "object"
                  :properties {"old-project-id" {:type "string"
                                                 :description "Source project ID (e.g., SHA hash or old name)"}
                               "new-project-id" {:type "string"
                                                 :description "Target project ID (e.g., stable ID from .hive-project.edn)"}
                               "update-scopes" {:type "boolean"
                                                :description "If true, update scope:project:* tags in migrated entries"}}
                  :required ["old-project-id" "new-project-id"]}
    :handler handle-mcp-memory-migrate-project}

   {:name "mcp_memory_import_json"
    :description "Import memory entries from elisp JSON storage to Chroma. Use for one-time migration from the old JSON-based storage. Set dry-run to preview without importing."
    :inputSchema {:type "object"
                  :properties {"project-id" {:type "string"
                                             :description "Project ID to import (defaults to current project)"}
                               "dry-run" {:type "boolean"
                                          :description "If true, only show what would be imported without actually importing"}}
                  :required []}
    :handler handle-mcp-memory-import-json}])

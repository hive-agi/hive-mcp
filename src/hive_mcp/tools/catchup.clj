(ns hive-mcp.tools.catchup
  "Native Catchup workflow implementation.

   Gathers session context from Chroma memory with project scoping.
   Designed for the /catchup skill to restore context at session start.
   
   Priority Loading:
   - Swarm conventions tagged 'catchup-priority' are surfaced FIRST
   - These help coordinator trust swarm patterns immediately"
  (:require [hive-mcp.emacsclient :as ec]
            [hive-mcp.chroma :as chroma]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.set :as set]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Utility Helpers
;; =============================================================================

(defn- distinct-by
  "Return distinct elements from coll by the value of (f item).
   Keeps the first occurrence of each unique (f item) value."
  [f coll]
  (let [seen (volatile! #{})]
    (filterv (fn [item]
               (let [key (f item)]
                 (if (contains? @seen key)
                   false
                   (do (vswap! seen conj key) true))))
             coll)))

;; =============================================================================
;; Project Context Helpers
;; =============================================================================

(defn- get-current-project-id
  "Get current project ID from Emacs, or 'global' if not in a project.
   When directory is provided, uses that path to determine project context."
  ([] (get-current-project-id nil))
  ([directory]
   (try
     (let [elisp (if directory
                   (format "(hive-mcp-memory--project-id %s)" (pr-str directory))
                   "(hive-mcp-memory--project-id)")
           {:keys [success result]} (ec/eval-elisp elisp)]
       (if (and success result (not= result "nil"))
         (str/replace result #"\"" "")
         "global"))
     (catch Exception _
       "global"))))

(defn- get-current-project-name
  "Get current project name from Emacs.
   When directory is provided, uses that path to determine project context."
  ([] (get-current-project-name nil))
  ([directory]
   (try
     (let [elisp (if directory
                   (format "(hive-mcp-memory--get-project-name %s)" (pr-str directory))
                   "(hive-mcp-memory--get-project-name)")
           {:keys [success result]} (ec/eval-elisp elisp)]
       (if (and success result (not= result "nil"))
         (str/replace result #"\"" "")
         nil))
     (catch Exception _
       nil))))

;; =============================================================================
;; Entry Transformation
;; =============================================================================

(defn- entry->catchup-meta
  "Convert a Chroma entry to catchup metadata format.
   Returns map with :id, :type, :preview, :tags."
  [entry preview-len]
  (let [content (:content entry)
        content-str (if (string? content)
                      content
                      (str content))
        preview (subs content-str 0 (min (count content-str) (or preview-len 80)))]
    {:id (:id entry)
     :type (name (or (:type entry) "note"))
     :preview preview
     :tags (vec (or (:tags entry) []))}))

;; =============================================================================
;; Scope Filtering
;; =============================================================================

(defn- matches-project-scope?
  "Check if entry matches project scope (project + global).
   Matches both project-name and project-id for robustness since
   wrap stores with project-name but we may query with project-id."
  [entry project-name project-id]
  (let [tags (set (or (:tags entry) []))
        name-scope (when project-name (str "scope:project:" project-name))
        id-scope (when project-id (str "scope:project:" project-id))]
    (or (and name-scope (contains? tags name-scope))
        (and id-scope (contains? tags id-scope))
        (contains? tags "scope:global"))))

(defn- filter-by-tags
  "Filter entries to only those containing all specified tags."
  [entries tags]
  (if (seq tags)
    (filter (fn [entry]
              (let [entry-tags (set (:tags entry))]
                (every? #(contains? entry-tags %) tags)))
            entries)
    entries))

(defn- query-scoped-entries
  "Query Chroma entries filtered by project scope.
   Uses both project-name and project-id for scope matching."
  [entry-type tags project-name project-id limit]
  (when (chroma/embedding-configured?)
    (let [limit (or limit 20)
          entries (chroma/query-entries :type entry-type :limit (* limit 5))
          scoped (filter #(matches-project-scope? % project-name project-id) entries)]
      (->> scoped
           (filter-by-tags tags)
           (take limit)))))

;; =============================================================================
;; Expiring Entries
;; =============================================================================

(defn- entry-expiring-soon?
  "Check if entry expires within 7 days."
  [entry]
  (when-let [exp (:expires entry)]
    (try
      (let [exp-time (java.time.ZonedDateTime/parse exp)
            now (java.time.ZonedDateTime/now)
            week-later (.plusDays now 7)]
        (.isBefore exp-time week-later))
      (catch Exception _ false))))

(defn- query-expiring-entries
  "Query entries expiring within 7 days, scoped to project."
  [project-name project-id limit]
  (->> (chroma/query-entries :limit 50)
       (filter #(matches-project-scope? % project-name project-id))
       (filter entry-expiring-soon?)
       (take (or limit 5))))

;; =============================================================================
;; Git Context
;; =============================================================================

(defn- gather-git-info
  "Gather git information from Emacs for the given directory."
  [directory]
  (try
    (let [git-elisp (if directory
                      (format "(let ((default-directory %s))
                                 (json-encode
                                  (list :branch (string-trim (shell-command-to-string \"git rev-parse --abbrev-ref HEAD 2>/dev/null || echo 'none'\"))
                                        :uncommitted (not (string-empty-p (shell-command-to-string \"git status --porcelain 2>/dev/null\")))
                                        :last-commit (string-trim (shell-command-to-string \"git log -1 --format='%%h - %%s' 2>/dev/null || echo 'none'\")))))"
                              (pr-str directory))
                      "(json-encode
                         (list :branch (string-trim (shell-command-to-string \"git rev-parse --abbrev-ref HEAD 2>/dev/null || echo 'none'\"))
                               :uncommitted (not (string-empty-p (shell-command-to-string \"git status --porcelain 2>/dev/null\")))
                               :last-commit (string-trim (shell-command-to-string \"git log -1 --format='%h - %s' 2>/dev/null || echo 'none'\"))))")
          {:keys [success result]} (ec/eval-elisp git-elisp)]
      (when success
        (json/read-str result :key-fn keyword)))
    (catch Exception _
      {:branch "unknown" :uncommitted false :last-commit "unknown"})))

;; =============================================================================
;; Metadata Conversion
;; =============================================================================

(defn- entry->axiom-meta
  "Convert entry to axiom metadata with full content."
  [entry]
  {:id (:id entry)
   :type "axiom"
   :tags (vec (or (:tags entry) []))
   :content (:content entry)
   :severity "INVIOLABLE"})

(defn- entry->priority-meta
  "Convert entry to priority convention metadata with full content."
  [entry]
  {:id (:id entry)
   :type "convention"
   :tags (vec (or (:tags entry) []))
   :content (:content entry)})

;; =============================================================================
;; Entry Queries
;; =============================================================================

(defn- query-axioms
  "Query axiom entries (both formal type and legacy tagged conventions)."
  [project-name project-id]
  (let [formal (query-scoped-entries "axiom" nil project-name project-id 10)
        legacy (query-scoped-entries "convention" ["axiom"] project-name project-id 10)]
    (distinct-by :id (concat formal legacy))))

(defn- query-regular-conventions
  "Query conventions excluding axioms and priority ones."
  [project-name project-id axiom-ids priority-ids]
  (let [all-conventions (query-scoped-entries "convention" nil project-name project-id 15)
        excluded-ids (set/union axiom-ids priority-ids)]
    (remove #(contains? excluded-ids (:id %)) all-conventions)))

;; =============================================================================
;; Response Building
;; =============================================================================

(defn- build-scopes
  "Build scope list for display."
  [project-name project-id]
  (cond-> ["scope:global"]
    project-name (conj (str "scope:project:" project-name))
    (and project-id (not= project-id project-name))
    (conj (str "scope:project:" project-id))))

(defn- build-catchup-response
  "Build the final catchup response structure."
  [{:keys [project-name project-id scopes git-info
           axioms-meta priority-meta sessions-meta decisions-meta
           conventions-meta snippets-meta expiring-meta]}]
  {:type "text"
   :text (json/write-str
          {:success true
           :project (or project-name project-id "global")
           :scopes scopes
           :git git-info
           :counts {:axioms (count axioms-meta)
                    :priority-conventions (count priority-meta)
                    :sessions (count sessions-meta)
                    :decisions (count decisions-meta)
                    :conventions (count conventions-meta)
                    :snippets (count snippets-meta)
                    :expiring (count expiring-meta)}
           :axioms axioms-meta
           :priority-conventions priority-meta
           :context {:sessions sessions-meta
                     :decisions decisions-meta
                     :conventions conventions-meta
                     :snippets snippets-meta
                     :expiring expiring-meta}
           :hint "AXIOMS are INVIOLABLE - follow them word-for-word. Priority conventions and axioms loaded with full content. Use mcp_memory_get_full for other entries."})})

(defn- chroma-not-configured-error
  "Return error response when Chroma is not configured."
  []
  {:type "text"
   :text (json/write-str {:success false
                          :error "Chroma not configured"
                          :message "Memory query requires Chroma with embedding provider"})
   :isError true})

(defn- catchup-error
  "Return error response for catchup failures."
  [e]
  (log/error e "native-catchup failed")
  {:type "text"
   :text (json/write-str {:success false :error (.getMessage e)})
   :isError true})

;; =============================================================================
;; Main Catchup Handler
;; =============================================================================

(defn handle-native-catchup
  "Native Clojure catchup implementation that queries Chroma directly.
   Returns structured catchup data with proper project scoping."
  [args]
  (let [directory (:directory args)]
    (log/info "native-catchup: querying Chroma with project scope" {:directory directory})
    ;; Guard: early return if Chroma not configured
    (if-not (chroma/embedding-configured?)
      (chroma-not-configured-error)
      (try
        (let [project-id (get-current-project-id directory)
              project-name (get-current-project-name directory)
              scopes (build-scopes project-name project-id)

              ;; Query entries
              axioms (query-axioms project-name project-id)
              priority-conventions (query-scoped-entries "convention" ["catchup-priority"]
                                                         project-name project-id 5)
              sessions (query-scoped-entries "note" ["session-summary"] project-name project-id 3)
              decisions (query-scoped-entries "decision" nil project-name project-id 10)
              conventions (query-regular-conventions project-name project-id
                                                     (set (map :id axioms))
                                                     (set (map :id priority-conventions)))
              snippets (query-scoped-entries "snippet" nil project-name project-id 5)
              expiring (query-expiring-entries project-name project-id 5)
              git-info (gather-git-info directory)

              ;; Convert to metadata
              axioms-meta (mapv entry->axiom-meta axioms)
              priority-meta (mapv entry->priority-meta priority-conventions)
              sessions-meta (mapv #(entry->catchup-meta % 80) sessions)
              decisions-meta (mapv #(entry->catchup-meta % 80) decisions)
              conventions-meta (mapv #(entry->catchup-meta % 80) conventions)
              snippets-meta (mapv #(entry->catchup-meta % 60) snippets)
              expiring-meta (mapv #(entry->catchup-meta % 80) expiring)]

          (build-catchup-response
           {:project-name project-name :project-id project-id
            :scopes scopes :git-info git-info
            :axioms-meta axioms-meta :priority-meta priority-meta
            :sessions-meta sessions-meta :decisions-meta decisions-meta
            :conventions-meta conventions-meta :snippets-meta snippets-meta
            :expiring-meta expiring-meta}))
        (catch Exception e
          (catchup-error e))))))

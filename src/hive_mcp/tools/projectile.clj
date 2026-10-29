(ns hive-mcp.tools.projectile
  "Projectile integration handlers for MCP.

   Provides project management capabilities through Emacs projectile,
   including project info, file listing, search, and navigation.

   Requires the hive-mcp-projectile addon to be loaded in Emacs."
  (:require [hive-mcp.context.request :as ctx]
            [hive-mcp.tools.core :refer [mcp-error]]
            [taoensso.timbre :as log]
            [hive-spi.editor.services :as svc]
            [clojure.string :as str]
            [malli.core :as m]
            [clojure.data.json :as json]
            [hive-mcp.project.tree :as tree]
            [hive-mcp.project.scope :as project-scope]
            [hive-mcp.extensions.record-hooks :as record-hooks]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Projectile Integration Handlers (requires hive-mcp-projectile addon)
;; =============================================================================

(defn- dispatch-projectile
  "Dispatch a closed projectile op; keep the MCP text/error envelope."
  [op]
  (let [{:keys [success result error]} (svc/invoke :vessel :dispatch op nil)]
    (if success
      {:type "text" :text result}
      (mcp-error (str "Error: " error)))))

(defn handle-projectile-info
  "Get current project info including name, root, type, and file count.

   Honors `directory` param (with ctx fallback): the :project/info op binds
   Emacs `default-directory` so projectile resolves the requested project
   rather than the JVM cwd. A blank candidate counts as absent; with neither,
   the key is omitted (the op's :directory is optional but must be non-blank
   when present)."
  [{:keys [directory]}]
  (let [dir (some #(when-not (str/blank? %) %)
                  [(some-> directory str) (some-> (ctx/current-directory) str)])]
    (log/info "projectile-info" {:directory dir})
    (dispatch-projectile (cond-> {:op :project/info}
                           dir (assoc :directory dir)))))

(defn handle-projectile-files
  "List files in current project, optionally filtered by pattern.
   A blank pattern lists every file, as an absent one does."
  [{:keys [pattern]}]
  (let [pattern (when-not (str/blank? pattern) pattern)]
    (log/info "projectile-files" {:pattern pattern})
    (dispatch-projectile {:op :project/files :pattern pattern})))

(defn handle-projectile-find-file
  "Find files matching a filename in current project."
  [{:keys [filename]}]
  (log/info "projectile-find-file" {:filename filename})
  (dispatch-projectile {:op :project/find-file :filename filename}))

(defn handle-projectile-search
  "Search project for a pattern using ripgrep or grep."
  [{:keys [pattern]}]
  (log/info "projectile-search" {:pattern pattern})
  (dispatch-projectile {:op :project/search :pattern pattern}))

(defn handle-projectile-recent
  "Get recently visited files in current project."
  [_]
  (log/info "projectile-recent")
  (dispatch-projectile {:op :project/recent}))

(defn- root-entity
  "The project-tree entity a projectile ROOT belongs to: the nearest
   .hive-project.edn at or above it. nil for a root in no project."
  [root]
  (let [pid (when (string? root) (project-scope/infer-scope-from-path root))]
    (when (and pid (not= "global" pid))
      {:project/id pid :project/path root})))

(defn- visible-roots
  "The projectile list JSON `text` without the roots whose project the
   :project/visible record hooks hide from the calling session. A root in no
   project is kept. Throws when `text` cannot be read, so nothing unjudged is
   shown."
  [text]
  (let [rows   (json/read-str text)
        judged (mapv (fn [row] [row (root-entity (get row "root"))]) rows)
        shown  (set (map :project/id (tree/visible-projects (keep second judged))))]
    (json/write-str (into [] (keep (fn [[row ent]]
                                     (when (or (nil? ent) (contains? shown (:project/id ent)))
                                       row)))
                          judged))))

(defn handle-projectile-list-projects
  "List all known projectile projects the calling session may see: a root
   whose project the :project/visible record hooks hide (an enclaved project,
   for a session outside its enclave) is left out. With no such hook the
   answer passes through untouched. An answer that cannot be judged is
   refused."
  [_]
  (log/info "projectile-list-projects")
  (let [out (dispatch-projectile {:op :project/list-projects})]
    (if (or (:isError out) (empty? (record-hooks/registered :project/visible)))
      out
      (try
        (assoc out :text (visible-roots (:text out)))
        (catch Exception e
          (log/warn "projectile-list-projects: answer not judged, refused:" (ex-message e))
          (mcp-error "Error: the project list could not be filtered for this session"))))))

(def tools
  "REMOVED: Flat projectile tools no longer exposed. Use consolidated `project` tool."
  [])

;; =============================================================================
;; Contracts
;; =============================================================================

(def ProjectileParams
  "Projectile handler params: an open map; every key optional (nil allowed)."
  [:maybe [:map
           [:directory {:optional true} :any]
           [:pattern {:optional true} [:maybe :string]]
           [:filename {:optional true} [:maybe :string]]]])

(def ProjectileResponse
  "The MCP text envelope on success, or an mcp-error on a failed dispatch."
  [:or
   [:map {:closed true} [:type [:= "text"]] [:text [:maybe :string]]]
   hive-mcp.tools.core/McpErrorResponse])

(def ProjectileHandler
  [:=> [:cat ProjectileParams] ProjectileResponse])

(m/=> handle-projectile-info ProjectileHandler)
(m/=> handle-projectile-files ProjectileHandler)
(m/=> handle-projectile-find-file ProjectileHandler)
(m/=> handle-projectile-search ProjectileHandler)
(m/=> handle-projectile-recent ProjectileHandler)
(m/=> handle-projectile-list-projects ProjectileHandler)

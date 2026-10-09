(ns hive-mcp.tools.projectile
  "Projectile integration handlers for MCP.

   Provides project management capabilities through Emacs projectile,
   including project info, file listing, search, and navigation.

   Requires the hive-mcp-projectile addon to be loaded in Emacs."
  (:require [hive-mcp.context.request :as ctx]
            [hive-mcp.tools.core :refer [mcp-error]]
            [taoensso.timbre :as log]
            [hive-spi.editor.services :as svc]
            [clojure.string :as str]))
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

(defn handle-projectile-list-projects
  "List all known projectile projects."
  [_]
  (log/info "projectile-list-projects")
  (dispatch-projectile {:op :project/list-projects}))

(def tools
  "REMOVED: Flat projectile tools no longer exposed. Use consolidated `project` tool."
  [])

(ns hive-mcp.knowledge-graph.scope
  "Old name of `hive-mcp.project.scope`, kept for the memory and
   knowledge-graph slices and for addons that resolve these symbols by name.

   The scope hierarchy never read a store, so it lives in the kernel. Every
   function here calls through the kernel var on each invocation rather than
   aliasing its value, so a reload or a redef of `hive-mcp.project.scope`
   reaches callers that still spell the old name. New code requires
   `hive-mcp.project.scope` directly."
  (:require [hive-mcp.project.scope :as ps]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn read-direct-project-config [directory] (ps/read-direct-project-config directory))
(defn resolve-project-id [project-id] (ps/resolve-project-id project-id))
(defn register-project-config! [project-id config] (ps/register-project-config! project-id config))
(defn deregister-project-config! [project-id] (ps/deregister-project-config! project-id))
(defn get-project-config [project-id] (ps/get-project-config project-id))
(defn get-alias-index [] (ps/get-alias-index))
(defn clear-config-cache! [] (ps/clear-config-cache!))
(defn get-parent-scope [scope] (ps/get-parent-scope scope))
(defn visible-scopes [scope] (ps/visible-scopes scope))
(defn scope-contains? [parent-scope child-scope] (ps/scope-contains? parent-scope child-scope))
(defn infer-scope-from-path [file-path] (ps/infer-scope-from-path file-path))
(defn scope->tag [scope] (ps/scope->tag scope))
(defn visible-scope-tags [scope] (ps/visible-scope-tags scope))
(defn descendant-scope-tags [scope] (ps/descendant-scope-tags scope))
(defn descendant-scopes [scope] (ps/descendant-scopes scope))
(defn full-hierarchy-scope-tags [scope] (ps/full-hierarchy-scope-tags scope))
(defn derive-hierarchy-scope-filter [scope] (ps/derive-hierarchy-scope-filter scope))

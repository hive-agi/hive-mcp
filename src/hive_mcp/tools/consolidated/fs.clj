(ns hive-mcp.tools.consolidated.fs
  "Consolidated filesystem tool — read, write, glob, grep via subcommands.

   Core handlers delegate to basic-tools-mcp file operations.
   Addons can override/extend via contribute-commands! targeting \"fs\".

   Commands: read, write, glob, grep"
  (:require [hive-mcp.tools.composite :as composite]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Lazy Resolution (basic-tools-mcp handlers)
;; =============================================================================

(defn- resolve-handler [sym]
  (try (requiring-resolve sym) (catch Exception _ nil)))

(defn- handle-read [params]
  (if-let [h (resolve-handler 'basic-tools-mcp.tools/handle-read-file)]
    (h params)
    {:content [{:type "text" :text "read_file handler not available"}] :isError true}))

(defn- handle-write [params]
  (if-let [h (resolve-handler 'basic-tools-mcp.tools/handle-file-write)]
    (h params)
    {:content [{:type "text" :text "file_write handler not available"}] :isError true}))

(defn- handle-glob [params]
  (if-let [h (resolve-handler 'basic-tools-mcp.tools/handle-glob-files)]
    (h params)
    {:content [{:type "text" :text "glob_files handler not available"}] :isError true}))

(defn- handle-grep [params]
  (if-let [h (resolve-handler 'basic-tools-mcp.tools/handle-grep)]
    (h params)
    {:content [{:type "text" :text "grep handler not available"}] :isError true}))

;; =============================================================================
;; Canonical Handlers (core — addons can override via contribute-commands!)
;; =============================================================================

(def canonical-handlers
  {:read  handle-read
   :write handle-write
   :glob  handle-glob
   :grep  handle-grep})

(def handlers canonical-handlers)

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def handle-fs
  "Routes the core `fs` commands plus whatever addons contribute under
   \"fs\". Named, so the tool-def can register it BY VAR: a handler folded
   into the tool map by value never sees a reload of its own namespace
   (20260817195749-0d407e9c)."
  (composite/build-merged-handler "fs" #'canonical-handlers))

(def tool-def
  {:name        "fs"
   :consolidated true
   :description "Filesystem operations: read (file contents), write (create/overwrite file), glob (find files by pattern), grep (search file contents by regex). Use command='help' to list all."
   :inputSchema {:type       "object"
                 :properties {"command"     {:type "string"
                                             :enum ["read" "write" "glob" "grep" "help"]
                                             :description "Filesystem operation to perform"}
                              "path"        {:type "string"
                                             :description "Absolute path to the file"}
                              "file_path"   {:type "string"
                                             :description "Absolute path to write"}
                              "content"     {:type "string"
                                             :description "Content to write"}
                              "pattern"     {:type "string"
                                             :description "Glob or regex pattern"}
                              "offset"      {:type "integer"
                                             :description "Line to start from (default: 0)"}
                              "limit"       {:type "integer"
                                             :description "Max lines to read (default: 2000)"}
                              "include"     {:type "string"
                                             :description "File pattern to include (e.g. *.clj)"}
                              "max_results" {:type "integer"
                                             :description "Max results (default: 100)"}}
                 :required   ["command"]}
   :handler     #'handle-fs})

(def tools [tool-def])

(ns hive-mcp.knowledge-graph.disc.hash
  "DEPRECATED facade. File content hashing is kernel code and lives in
   `hive-mcp.storage.file-hash`; this name call-throughs to it and dies with
   the hive-memory extraction.

   Every fn delegates through the kernel VAR, never `(def f fh/f)`: a
   def-alias captures the fn value at load, so a hot reload or `with-redefs`
   of the kernel var would not reach callers of this name, and the cache the
   kernel namespace owns must stay single-source. See memory
   20260919135346-7838bb58."
  (:require [hive-mcp.storage.file-hash :as fh]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn compute-hash
  "See `hive-mcp.storage.file-hash/compute-hash`."
  [content]
  (fh/compute-hash content))

(defn clear-hash-cache!
  "See `hive-mcp.storage.file-hash/clear-hash-cache!`."
  []
  (fh/clear-hash-cache!))

(defn file-content-hash
  "See `hive-mcp.storage.file-hash/file-content-hash`."
  [path]
  (fh/file-content-hash path))

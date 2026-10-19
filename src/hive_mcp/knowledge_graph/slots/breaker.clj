;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.knowledge-graph.slots.breaker
  "DEPRECATED facade. The breaker is kernel code and lives in
   `hive-mcp.resilience.breaker`; this name call-throughs to it and dies with
   the hive-memory extraction.

   Every fn delegates through the kernel VAR, never `(def f cb/f)`: a
   def-alias captures the fn value at load, so a hot reload or `with-redefs`
   of the kernel var would not reach callers of this name. See memory
   20260919135346-7838bb58."
  (:refer-clojure :exclude [reset!])
  (:require [hive-mcp.resilience.breaker :as cb]))

(def default-policy
  "See `hive-mcp.resilience.breaker/default-policy`."
  cb/default-policy)

(defn fresh
  "See `hive-mcp.resilience.breaker/fresh`."
  []
  (cb/fresh))

(defn on-success
  "See `hive-mcp.resilience.breaker/on-success`."
  [breaker]
  (cb/on-success breaker))

(defn on-failure
  "See `hive-mcp.resilience.breaker/on-failure`."
  [breaker policy ts]
  (cb/on-failure breaker policy ts))

(defn maybe-recover
  "See `hive-mcp.resilience.breaker/maybe-recover`."
  [breaker ts]
  (cb/maybe-recover breaker ts))

(defn decision
  "See `hive-mcp.resilience.breaker/decision`."
  [breaker]
  (cb/decision breaker))

(defn attempt
  "See `hive-mcp.resilience.breaker/attempt`."
  [breaker-atom slot policy]
  (cb/attempt breaker-atom slot policy))

(defn record-success!
  "See `hive-mcp.resilience.breaker/record-success!`."
  [breaker-atom slot]
  (cb/record-success! breaker-atom slot))

(defn record-failure!
  "See `hive-mcp.resilience.breaker/record-failure!`."
  [breaker-atom slot policy]
  (cb/record-failure! breaker-atom slot policy))

(defn snapshot
  "See `hive-mcp.resilience.breaker/snapshot`."
  [breaker-atom]
  (cb/snapshot breaker-atom))

(defn reset!
  "See `hive-mcp.resilience.breaker/reset!`."
  [breaker-atom]
  (cb/reset! breaker-atom))

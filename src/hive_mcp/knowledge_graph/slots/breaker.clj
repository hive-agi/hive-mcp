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
  (:require [hive-mcp.resilience.breaker :as cb]
            [malli.core :as m]))

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

;; Contracts: the kernel's, stated over the kernel's own schemas, so the
;; facade's contract cannot drift from the fn it calls through to.
(m/=> fresh [:=> [:cat] cb/Breaker])
(m/=> on-success [:=> [:cat :any] cb/Breaker])
(m/=> on-failure [:=> [:cat :map cb/Policy :int] cb/Breaker])
(m/=> maybe-recover [:=> [:cat cb/Breaker :int] cb/Breaker])
(m/=> decision [:=> [:cat :map] [:enum :pass :block]])
(m/=> attempt [:=> [:cat cb/BreakerAtom :any cb/Policy] cb/Breaker])
(m/=> record-success! [:=> [:cat cb/BreakerAtom :any] cb/BreakerMap])
(m/=> record-failure! [:=> [:cat cb/BreakerAtom :any cb/Policy] cb/Breaker])
(m/=> snapshot [:=> [:cat cb/BreakerAtom] :map])
(m/=> reset! [:=> [:cat cb/BreakerAtom] :map])

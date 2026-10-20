(ns hive-mcp.vectordb.resilience
  "DEPRECATED facade. Handler-side store resilience is kernel code and lives in
   `hive-mcp.resilience.store`; this name call-throughs to it and dies with the
   hive-memory extraction.

   Every fn delegates through the kernel VAR, never `(def f rs/f)`: a
   def-alias captures the fn value at load, so a hot reload or `with-redefs`
   of the kernel var would not reach callers of this name. The two macros
   expand to the kernel fns for the same reason. See memory
   20260919135346-7838bb58."
  (:require [hive-mcp.resilience.store :as rs]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-budget-ms
  "See `hive-mcp.resilience.store/default-budget-ms`."
  rs/default-budget-ms)

(defn- kernel-schema
  "The contract the kernel fn carries. These names are pure call-throughs, so
   their contract is the kernel's, read from the registry rather than copied
   (a copy would drift)."
  [sym]
  (:schema (get-in (m/function-schemas) ['hive-mcp.resilience.store sym])))

(defn kick-and-wait!
  "See `hive-mcp.resilience.store/kick-and-wait!`."
  ([] (rs/kick-and-wait!))
  ([budget-ms] (rs/kick-and-wait! budget-ms)))

(m/=> kick-and-wait! (kernel-schema 'kick-and-wait!))

(defn transient-failure?
  "See `hive-mcp.resilience.store/transient-failure?`."
  [t]
  (rs/transient-failure? t))

(m/=> transient-failure? (kernel-schema 'transient-failure?))

(defn call-with-resilience-result
  "See `hive-mcp.resilience.store/call-with-resilience-result`."
  ([f] (rs/call-with-resilience-result f))
  ([f budget-ms] (rs/call-with-resilience-result f budget-ms)))

(m/=> call-with-resilience-result (kernel-schema 'call-with-resilience-result))

(defn call-with-resilience
  "See `hive-mcp.resilience.store/call-with-resilience`."
  ([f] (rs/call-with-resilience f))
  ([f budget-ms] (rs/call-with-resilience f budget-ms)))

(m/=> call-with-resilience (kernel-schema 'call-with-resilience))

(defmacro with-resilience
  "See `hive-mcp.resilience.store/with-resilience`."
  [& body]
  `(rs/call-with-resilience (fn [] ~@body)))

(defmacro with-resilience-result
  "See `hive-mcp.resilience.store/with-resilience-result`."
  [& body]
  `(rs/call-with-resilience-result (fn [] ~@body)))

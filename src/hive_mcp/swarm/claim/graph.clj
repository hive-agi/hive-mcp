(ns hive-mcp.swarm.claim.graph
  "The `callers-fn` that `hive-mcp.swarm.claim.span` asks for, backed by carto.

   WHY THIS IS RESOLVED AND NOT REQUIRED
   =====================================
   hive-carto is a proprietary addon, and the host's deps.edn never names one:
   hive-mcp has to build and run for someone who does not have it. So the
   lookup goes through `requiring-resolve` at call time and returns nil when
   carto is absent.

   That is not a workaround, it is the contract. `span/overlap` takes nil as
   'no graph available' and drops ONLY the signature-versus-caller rule. Every
   local rule still fires. The degradation is fewer conflicts reported, never a
   wrong one, so a host without carto coordinates slightly more coarsely
   instead of failing.

   CACHING
   =======
   A claim check asks for the callers of a handful of qns and does it while
   another agent may be waiting, so the answer is memoized per scope for
   `ttl-ms`. The graph moves when somebody writes a form, which is exactly when
   a stale answer is cheap: a missed edge means one conflict goes unreported
   and the write still cannot corrupt anything, because the fingerprint CAS in
   carto is what actually guards the bytes."
  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:dynamic *ttl-ms*
  "How long a callers answer stays good. Short, because it is only ever used to
   decide whether to make an agent wait."
  10000)

(defonce ^:private cache
  ;; {[scope qn] {:at epoch-ms :callers [qn ...]}}
  (atom {}))

(defn reset-cache!
  "Drop every memoized answer. For tests, and for after a rescan."
  []
  (reset! cache {}))

(defn- carto-callers-fn
  "carto's `q/callers`, or nil when carto is not on the classpath."
  []
  (try
    (requiring-resolve 'hive-carto.cartography.q/callers)
    (catch Throwable t
      (log/debug "carto not available for claim graph:" (ex-message t))
      nil)))

(defn available?
  "True when the signature-versus-caller rule can be evaluated at all."
  []
  (some? (carto-callers-fn)))

(defn- fetch
  "Callers of `qn` as a vector of qualified-name strings, [] on any failure.

   A throwing or cold index must not fail a claim: an agent that cannot be told
   about a conflict should still be allowed to work, because the write path
   refuses a stale fingerprint regardless."
  [scope qn]
  (if-let [f (carto-callers-fn)]
    (try
      (->> (if scope (f qn scope) (f qn))
           (keep :qn)
           (mapv str))
      (catch Throwable t
        (log/debug "claim graph: callers lookup failed for" qn (ex-message t))
        []))
    []))

(defn callers
  "Memoized callers of `qn` within `scope`."
  [scope qn]
  (let [k   [scope qn]
        now (System/currentTimeMillis)
        hit (get @cache k)]
    (if (and hit (< (- now (:at hit)) *ttl-ms*))
      (:callers hit)
      (let [v (fetch scope qn)]
        (swap! cache assoc k {:at now :callers v})
        v))))

(defn callers-fn
  "A `(fn [qn] -> [qn ...])` bound to `scope`, or nil when carto is absent.

   Returning nil rather than a function that always answers [] is deliberate:
   `span/overlap` distinguishes 'no graph' from 'no callers', and only the
   first is allowed to silently skip the rule."
  [scope]
  (when (available?)
    (fn [qn] (callers scope qn))))

(ns hive-mcp.telemetry.metrics
  "Late-bound bridge from hive-mcp to the metrics port `hive-ttracking.metrics`.

   Contract:
   - `record!` hands [k elapsed-ns outcome] to the installed recorder and never
     throws. The default recorder is a no-op until `install-default!` finds the
     port; when the port is absent hive-mcp runs unchanged without metrics.
   - `timed` measures a body with System/nanoTime; outcome :error when the body
     throws (rethrown), else :ok. Returns the body's value.
   - `install-recorder!` swaps the recorder (a 3-arg fn); tests install a
     recording stub through it.
   - `install-default!` installs (composite-sink (stats-sink) prom-sink) on the
     port and points the recorder at the port's `record!`.
   - Op keys follow the port: [area op] or [area op sub], closed vocabulary only."
  (:refer-clojure :exclude [reset!])
  (:require [hive-mcp.telemetry.prometheus :as prom]
            [malli.core :as m]
            [taoensso.timbre :as log])
  (:import [java.util.concurrent.atomic LongAdder]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- noop-recorder [_ _ _] nil)

(defonce ^:private recorder (atom noop-recorder))

(defonce ^:private ^LongAdder recorder-errors (LongAdder.))

(defn record!
  "Record one observation through the installed recorder. Never throws; a
   recorder exception is counted in `recorder-error-count`. Returns nil."
  [k elapsed-ns outcome]
  (try
    ((deref recorder) k elapsed-ns outcome)
    (catch Throwable _ (.increment recorder-errors)))
  nil)

(defn recorder-error-count
  "Recorder exceptions swallowed by `record!` since load."
  []
  (.sum recorder-errors))

(m/=> recorder-error-count [:=> [:cat] :int])

(defmacro timed
  "Evaluate body, recording [k elapsed-ns outcome] via `record!`. k is
   evaluated once before body. Rethrows; returns body's value."
  [k & body]
  `(let [k#  ~k
         t0# (System/nanoTime)]
     (try
       (let [r# (do ~@body)]
         (record! k# (- (System/nanoTime) t0#) :ok)
         r#)
       (catch Throwable t#
         (record! k# (- (System/nanoTime) t0#) :error)
         (throw t#)))))

(defn install-recorder!
  "Install recorder f, a fn of [k elapsed-ns outcome]; nil restores the no-op.
   Returns the previous recorder."
  [f]
  (let [[old _] (reset-vals! recorder (or f noop-recorder))]
    old))

(m/=> install-recorder! [:=> [:cat [:maybe ifn?]] ifn?])

(defn- port-var
  "The var `hive-ttracking.metrics/<sym>`, or nil when the port is absent."
  [sym]
  (try (requiring-resolve (symbol "hive-ttracking.metrics" (name sym)))
       (catch Throwable _ nil)))

(defn port-available?
  "True when the metrics port namespace resolves on this classpath."
  []
  (some? (port-var 'record!)))

(m/=> port-available? [:=> [:cat] :boolean])

(defn install-default!
  "Install (composite-sink (stats-sink) prom-sink) on the port and route
   `record!` through the port. Returns the installed sink, or nil (leaving the
   recorder untouched) when the port is absent or installation fails."
  []
  (try
    (if-let [rec (port-var 'record!)]
      (let [stats     @(port-var 'stats-sink)
            composite @(port-var 'composite-sink)
            install   @(port-var 'install!)
            sinks     (cond-> [(stats)]
                        (prom/init!) (conj (prom/prom-sink)))
            sink      (apply composite (remove nil? sinks))]
        (install sink)
        (install-recorder! @rec)
        (log/info "metrics: port sink installed" {:sinks (count (remove nil? sinks))})
        sink)
      (do (log/debug "metrics: hive-ttracking.metrics absent; metrics off")
          nil))
    (catch Throwable e
      (log/warn "metrics: install failed (non-fatal):" (ex-message e))
      nil)))

(defn snapshot
  "The port's current snapshot, {} when the port is absent."
  []
  (if-let [cs (port-var 'current-sink)]
    (@(port-var 'snapshot) (@cs))
    {}))

(m/=> snapshot [:=> [:cat] :map])

(defn hotspots
  "Port `hotspots` over the current snapshot; opts as the port's
   {:by :limit :area}. [] when the port is absent."
  [opts]
  (if-let [h (port-var 'hotspots)]
    (@h (snapshot) (or opts {}))
    []))

(m/=> hotspots [:=> [:cat [:maybe :map]] [:sequential :map]])

(defn reset!
  "Clear the current port sink's aggregates. Returns nil."
  []
  (when-let [r (port-var 'reset-sink!)]
    (@r (@(port-var 'current-sink))))
  nil)

(m/=> reset! [:=> [:cat] :nil])

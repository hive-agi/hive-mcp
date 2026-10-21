(ns hive-mcp.server.routes.metrics-seam
  "Kernel-side seam to the metrics bridge. Resolves
   `hive-mcp.telemetry.metrics/record!` late, so the request boundary records
   operation timings without a require edge out of the kernel.

   Contract:
   - `record!` hands [k elapsed-ns outcome] to the bridge and never throws; a
     no-op when the bridge is absent.
   - `timed` measures a body; outcome :error when it throws (rethrown), else
     :ok. Returns the body's value."
  (:require [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private bridge-record
  (delay (try (requiring-resolve 'hive-mcp.telemetry.metrics/record!)
              (catch Throwable _ nil))))

(defn record!
  "Record one observation through the metrics bridge. Never throws. Returns nil."
  [k elapsed-ns outcome]
  (try
    (when-let [f @bridge-record] (f k elapsed-ns outcome))
    (catch Throwable _ nil))
  nil)

(m/=> record! [:=> [:cat :any :int :any] :nil])

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

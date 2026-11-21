(ns hive-mcp.resilience.swallow
  "Deliberate, visible failure swallowing.

   A best-effort step (a late `requiring-resolve`, a cache probe, a cleanup)
   must not fail its caller, but it must not fail silently either. `attempt`
   answers nil on failure, like `(try ... (catch Throwable _ nil))`, and says
   so through the hive-spi logging port at :debug, naming the form and the
   cause. Replaces the bare silent catch."
  (:require [hive-spi.log.ports :as log-ports]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private max-label-chars 80)

(defn clip
  "S cut to `max-label-chars`, with an ellipsis when cut."
  [s]
  (if (> (count s) max-label-chars)
    (str (subs s 0 max-label-chars) "...")
    s))

(m/=> clip [:=> [:cat :string] :string])

(defn attempt*
  "Call THUNK. Its value on success; nil when it throws, after a :debug line
   through the hive-spi log port naming LABEL and the cause. An interrupt
   re-sets the thread's interrupt flag. Never throws."
  [label thunk]
  (try
    (thunk)
    (catch Throwable t
      (when (instance? InterruptedException t)
        (.interrupt (Thread/currentThread)))
      (log-ports/debug "swallowed (best-effort):" label "->"
                       (.getSimpleName (class t)) (ex-message t))
      nil)))

(m/=> attempt* [:=> [:cat :string ifn?] :any])

(defmacro attempt
  "Run BODY; nil when it throws, with the failure logged at :debug.
   The first body form, printed, is the log label."
  [& body]
  `(attempt* ~(clip (pr-str (first body))) (fn [] ~@body)))

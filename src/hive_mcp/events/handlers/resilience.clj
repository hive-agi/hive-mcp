(ns hive-mcp.events.handlers.resilience
  "Resilience-layer event handlers (ENGINE-L1.4, defense-in-depth).

   Owns the canonical handler for `:resilience/dim-mismatch` — an
   advisory event emitted by `hive-mcp.vectordb.resilience` whenever a
   memory-store call short-circuits with `:err/schema-mismatch`
   (typically embedder dim drift vs collection dim).

   Before L1.4 the event had no handler. Dispatch threw
   'No handler registered', the emitter swallowed it via `rescue nil`,
   and the schema-mismatch signal vanished — leaving no metric, no
   log line, and no audit trail for an actionable misconfiguration."
  (:require [hive-mcp.events.core :as ev]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Handler: :resilience/dim-mismatch
;; =============================================================================

(defn handle-dim-mismatch
  "Advisory observer for vector-dim mismatches. Pure handler — emits a
   `:log` effect carrying structured fields so the schema-mismatch
   surfaces in operator logs and downstream telemetry sinks.

   Event shape:
   [:resilience/dim-mismatch {:message  str
                              :details  any
                              :ex-class str-or-nil}]"
  [_coeffects [_ {:keys [message details ex-class] :as data}]]
  (log/warn "[resilience] dim-mismatch:" message
            "| ex-class:" ex-class
            "| details:" details)
  {:log {:level :warn
         :event :resilience/dim-mismatch
         :message message
         :ex-class ex-class
         :details details
         :data data}})

;; =============================================================================
;; Registration
;; =============================================================================

(defonce ^:private *registered (atom false))

(defn register-handlers!
  "Register resilience-layer event handlers.

   Handlers registered:
   - :resilience/dim-mismatch -- advisory dim-drift observer (L1.4).

   Safe to call multiple times, and it REGISTERS every time: `reg-event` is
   addressed by key and last-writer-wins. The `defonce`'d flag used to skip the
   body, which meant a hot reload could not rewire this handler. Kanban
   20260916134011-1246379c.

   Returns true."
  []
  (let [first? (not @*registered)]
    (ev/reg-event :resilience/dim-mismatch [] handle-dim-mismatch)
    (reset! *registered true)
    (when first?
      (log/info "[hive-events] Resilience handlers registered: :resilience/dim-mismatch"))
    true))

(defn reset-registration!
  "Reset registration state. Tests only."
  []
  (reset! *registered false))

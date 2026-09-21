;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.swarm.adapters.inbox.piggyback
  "The piggyback IInboxSink: the universal fallback delivery sink for the
   platform-agnostic swarm inbox. It carries an instruction on the
   ---HIVEMIND--- piggyback block of the named agent's next digest, so it
   accepts? every non-blank recipient and rides the highest priority
   number (:inbox/priority 1000): any specific platform sink beats it.

   Sibling platform sinks (claude-code, codex, ...) live as sister
   namespaces under hive-mcp.swarm.adapters.inbox.* and register
   themselves the same way. Each sink projects its own transport's limits
   inside itself (for example Claude Code's channel push accepts only
   identifier-shaped meta keys, so THAT sink normalises or drops attrs
   keys): such transport vocabulary never leaks into the
   hive-spi.swarm.ports.messaging port, which only ever speaks the plain
   envelope."
  (:require [clojure.string :as str]
            [hive-spi.swarm.ports.messaging :as msg]
            [hive-mcp.channel.piggyback :as piggyback]
            [taoensso.timbre :as log]))

;;; ============================================================================
;;; The sink
;;; ============================================================================

(defn make-sink
  "The piggyback IInboxSink. deliver! pushes the instruction and returns
   {:delivered? bool :sink :piggyback :receipt ...}; it never throws."
  []
  (with-meta
    (reify msg/IInboxSink
      (sink-id [_] :piggyback)

      (accepts? [_ recipient]
        (and (string? recipient)
             (not (str/blank? recipient))))

      (-deliver! [_ envelope]
        (try
          (let [receipt (piggyback/push-instruction!
                         (:inbox/to envelope)
                         {:content   (str (:inbox/body envelope))
                          :kind      (some-> (:inbox/kind envelope) name)
                          :from      (:inbox/from envelope)
                          :contextId (:inbox/context-id envelope)
                          :meta      (:inbox/attrs envelope)})]
            {:delivered? true :sink :piggyback :receipt receipt})
          (catch Exception e
            (log/warn "[piggyback-sink] delivery failed:"
                      (.getMessage e))
            {:delivered? false :sink :piggyback :receipt nil}))))
    {:inbox/priority 1000}))

;;; ============================================================================
;;; Install
;;; ============================================================================

(defn install!
  "Register the piggyback sink with the swarm inbox. Call once from addon
   init, after hive-mcp.swarm.adapters.messaging/install!. Idempotent per
   sink-id: re-registering replaces the sink of the same id. Returns the
   sink."
  []
  (let [sink (make-sink)]
    (msg/register-sink! sink)
    sink))

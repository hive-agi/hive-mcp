(ns hive-mcp.events.handlers.agora
  "Agora dialogue event handlers.

   Handles events related to Agora multi-ling dialogues:
   - :agora/turn-dispatched  - Relay turn to target ling's terminal
   - :agora/turn-completed   - Unified turn completion (renamed from turn-response)
   - :agora/dispatch-next    - Relay the next turn to a ling participant
   - :agora/consensus        - Crystallize dialogue result to memory
   - :agora/timeout          - Handle dialogue timeout

   Event chain: :agora/turn-completed -> :agora/dispatch-next -> :swarm-send-prompt"

  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]
            [hive-mcp.agora.dialogue :as dialogue]
            [hive-mcp.agora.schema :as schema]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:const max-turns
  "Maximum turns before dialogue times out to prevent infinite loops."
  50)

(def ^:private agora-context-template
  "Template for injecting Agora context into prompts.
   Format args: dialogue-id, from, topic, dialogue-id, from, message"
  "---
AGORA DIALOGUE CONTEXT
Dialogue ID: %s
From: %s
Topic: %s

You are in a Nash Equilibrium dialogue. Respond via agora_dispatch:
- dialogue_id: \"%s\"
- to: \"%s\" (or another participant)  
- signal: [propose|counter|approve|no-change|defer]

Signals: propose/counter=reset equilibrium, approve/no-change=toward consensus
---

%s")

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn- format-agora-prompt
  "Inject Agora context into message for target ling."
  [{:keys [dialogue-id from topic message]}]
  (format agora-context-template
          dialogue-id
          from
          (or topic "Unspecified")
          dialogue-id
          from
          message))

;; =============================================================================
;; Handler: :agora/turn-dispatched
;; =============================================================================

(defn handle-agora-turn-dispatched
  "Handler for :agora/turn-dispatched events.

   Called after a turn is dispatched within an Agora dialogue.
   Relays the message to the target ling's terminal with Agora context.

   Expects event data:
   {:dialogue-id \"dialogue-uuid\"
    :from        \"sender-slave-id\"
    :to          \"target-slave-id\"
    :turn-num    5
    :signal      :propose
    :message     \"The actual message content\"
    :topic       \"Dialogue topic\"}

   Produces effects:
   - :log              - Log relay status
   - :swarm-send-prompt - Send enhanced prompt to target ling
   - :dispatch         - Chain to timeout event if max turns exceeded"
  [_coeffects [_ {:keys [dialogue-id from to turn-num message topic]}]]
  (let [dialogue (dialogue/get-dialogue dialogue-id)]
    (cond
      ;; Dialogue not found
      (nil? dialogue)
      {:log {:level :warn
             :message (str "Agora turn relay skipped - dialogue not found: " dialogue-id)}}

      ;; Dialogue already reached consensus - no need to relay
      (= :consensus (:status dialogue))
      {:log {:level :info
             :message (str "Agora " dialogue-id " already at consensus, not relaying")}}

      ;; Dialogue aborted or timed out
      (#{:aborted :timeout} (:status dialogue))
      {:log {:level :info
             :message (str "Agora " dialogue-id " is " (name (:status dialogue)) ", not relaying")}}

      ;; Max turns exceeded - trigger timeout
      (>= turn-num max-turns)
      {:log {:level :warn
             :message (str "Agora " dialogue-id " exceeded max turns (" max-turns ")")}
       :dispatch [:agora/timeout {:dialogue-id dialogue-id
                                  :reason :max-turns
                                  :turn-count turn-num}]}

      ;; Normal case: relay to target ling
      :else
      {:log {:level :debug
             :message (str "Relaying Agora turn " turn-num " to " to " in dialogue " dialogue-id)}
       :swarm-send-prompt {:slave-id to
                           :prompt (format-agora-prompt
                                    {:dialogue-id dialogue-id
                                     :from from
                                     :topic topic
                                     :message message})}})))

;; =============================================================================
;; Handler: :agora/timeout
;; =============================================================================

(defn handle-agora-timeout
  "Handler for :agora/timeout events.

   Called when a dialogue exceeds max turns or times out.
   Updates dialogue status to :timeout.

   Expects event data:
   {:dialogue-id \"dialogue-uuid\"
    :reason      :max-turns | :inactivity
    :turn-count  50}

   Produces effects:
   - :log - Log timeout message"
  [_coeffects [_ {:keys [dialogue-id reason turn-count]}]]
  {:log {:level :warn
         :message (str "Agora dialogue " dialogue-id " timed out"
                       " (reason: " (name (or reason :unknown)) ")"
                       (when turn-count (str ", turns: " turn-count)))}})

;; =============================================================================
;; Handler: :agora/turn-completed (Unified Turn Completion)
;; =============================================================================

(defn handle-turn-completed
  "Handler for :agora/turn-completed events (unified turn completion).

   Called after any ling turn completes. Routes to :agora/dispatch-next,
   which relays the turn to the next participant.

   Renamed from :agora/turn-response for clarity in the unified event chain:
   :agora/turn-completed -> :agora/dispatch-next -> :swarm-send-prompt

   Expects event data:
   {:dialogue-id      \"dialogue-uuid\"
    :participant-id   \"ling-123\"
    :participant-type  :ling
    :signal           :propose | :counter | :approve | :no-change | :defer
    :message          \"The argument text\"
    :confidence       0.8
    :turn-num         5}

   Produces effects:
   - :log      - Log turn completion
   - :dispatch - Chain to :agora/dispatch-next if the dialogue continues"
  [_coeffects [_ {:keys [dialogue-id participant-id signal turn-num] :as _data}]]
  (let [dialogue (dialogue/get-dialogue dialogue-id)]
    (cond
      ;; Dialogue not found
      (nil? dialogue)
      {:log {:level :warn
             :message (str "Turn completed for unknown dialogue: " dialogue-id)}}

      ;; Consensus already reached - no action needed (consensus handler takes over)
      (= :consensus (:status dialogue))
      {:log {:level :info
             :message (str "Agora " dialogue-id " at consensus after turn " turn-num
                           " (signal: " (name signal) ")")}}

      ;; Dialogue ended (timeout/aborted)
      (#{:aborted :timeout} (:status dialogue))
      {:log {:level :info
             :message (str "Agora " dialogue-id " is " (name (:status dialogue))
                           ", not continuing")}}

      ;; Dialogue still active - dispatch next turn
      (= :active (:status dialogue))
      {:log {:level :debug
             :message (str "Turn " turn-num " completed by " participant-id
                           " (signal: " (name signal) "), dispatching next")}
       :dispatch [:agora/dispatch-next {:dialogue-id dialogue-id}]}

      ;; Unknown status
      :else
      {:log {:level :warn
             :message (str "Agora " dialogue-id " in unexpected status: "
                           (:status dialogue))}})))

;; =============================================================================
;; Handler: :agora/dispatch-next (Next Turn Relay)
;; =============================================================================

(defn handle-dispatch-next
  "Handler for :agora/dispatch-next events.

   Determines the next ling participant (any participant other than the
   sender of the last turn) and relays the last turn to its terminal.

   Expects event data:
   {:dialogue-id \"dialogue-uuid\"}

   Produces effects:
   - :log               - Log dispatch decision
   - :swarm-send-prompt - Relay to the next ling participant"
  [_coeffects [_ {:keys [dialogue-id]}]]
  (try
    (let [dialogue (dialogue/get-dialogue dialogue-id)
          turns (dialogue/get-dialogue-turns dialogue-id)
          last-turn (last turns)
          next-participant (when last-turn
                             (first (disj (:participants dialogue) (:sender last-turn))))]
      (if next-participant
        {:log {:level :info
               :message (str "Relaying to ling participant: " next-participant)}
         :swarm-send-prompt {:slave-id next-participant
                             :prompt (format-agora-prompt
                                      {:dialogue-id dialogue-id
                                       :from (:sender last-turn)
                                       :topic (:topic dialogue)
                                       :message (:message last-turn)})}}
        {:log {:level :warn
               :message (str "No next participant found for dialogue: " dialogue-id)}}))
    (catch Exception e
      {:log {:level :error
             :message (str "dispatch-next failed for " dialogue-id ": " (.getMessage e))}})))

;; =============================================================================
;; Handler: :agora/consensus (P0: Crystallize Result)
;; =============================================================================

(defn- generate-consensus-summary
  "Generate a summary of the dialogue for crystallization.

   Extracts key information:
   - Topic and methodology
   - Final positions from participants
   - Turn count and consensus signal
   - Key arguments from turn log"
  [dialogue turns]
  (let [topic (:topic (:config dialogue))
        methodology (get-in dialogue [:config :methodology] :opinion)
        final-turns (take-last 3 (sort-by :turn-number turns))
        participant-summaries (->> turns
                                   (group-by :sender)
                                   (map (fn [[sender sender-turns]]
                                          (str "- " sender ": "
                                               (:message (last (sort-by :turn-number sender-turns))))))
                                   (str/join "\n"))]
    (str "# Agora Dialogue Consensus: " (or topic "Untitled") "\n\n"
         "**Methodology:** " (name methodology) "\n"
         "**Turns:** " (count turns) "\n"
         "**Status:** Consensus reached\n\n"
         "## Final Positions\n"
         participant-summaries "\n\n"
         "## Key Points\n"
         (->> final-turns
              (map (fn [t] (str "- [" (name (:signal t)) "] " (:message t))))
              (str/join "\n")))))

(defn handle-consensus
  "Handler for :agora/consensus events (P0 Crystallization).

   Called when a dialogue reaches consensus. Crystallizes the result
   to long-term memory for future reference.

   Expects event data:
   {:dialogue-id \"dialogue-uuid\"
    :turns       5}

   Produces effects:
   - :log          - Log consensus achievement
   - :memory-write - Crystallize dialogue summary to memory
   - :channel-publish - Notify Emacs of consensus"
  [_coeffects [_ {:keys [dialogue-id turns]}]]
  (let [dialogue (schema/get-dialogue dialogue-id)
        turn-log (schema/get-turns dialogue-id)
        summary (generate-consensus-summary dialogue turn-log)
        topic (or (get-in dialogue [:config :topic])
                  (:name dialogue)
                  "Untitled dialogue")]
    {:log {:level :info
           :message (str "Dialogue " dialogue-id " reached consensus after "
                         turns " turns, crystallizing result")}
     :memory-write {:type "decision"
                    :content summary
                    :tags ["agora" "debate-result" "consensus"
                           (str "dialogue:" dialogue-id)]
                    :duration "long"}
     :channel-publish {:event :agora-consensus-crystallized
                       :data {:dialogue-id dialogue-id
                              :topic topic
                              :turns turns}}}))

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-handlers!
  "Register agora-related event handlers.

   Handlers registered:
   - :agora/turn-dispatched  - Relay turn to target ling (existing)
   - :agora/timeout          - Handle dialogue timeout (existing)
   - :agora/turn-completed   - Unified turn completion (renamed from turn-response)
   - :agora/dispatch-next    - Relay the next turn to a ling participant
   - :agora/consensus        - Crystallize dialogue result

   Event chain: turn-completed -> dispatch-next -> swarm-send-prompt"
  []
  ;; Existing handlers
  (ev/reg-event :agora/turn-dispatched
                [interceptors/debug]
                handle-agora-turn-dispatched)

  (ev/reg-event :agora/timeout
                [interceptors/debug]
                handle-agora-timeout)

  ;; Unified event chain handlers
  (ev/reg-event :agora/turn-completed
                [interceptors/debug]
                handle-turn-completed)

  ;; Backward compat: :agora/turn-response -> :agora/turn-completed
  (ev/reg-event :agora/turn-response
                [interceptors/debug]
                handle-turn-completed)

  (ev/reg-event :agora/dispatch-next
                [interceptors/debug]
                handle-dispatch-next)

  ;; Consensus crystallization
  (ev/reg-event :agora/consensus
                [interceptors/debug]
                handle-consensus))
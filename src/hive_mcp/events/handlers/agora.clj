(ns hive-mcp.events.handlers.agora
  "Agora dialogue event handlers.

   Handles events related to Agora multi-ling dialogues:
   - :agora/turn-dispatched - Relay turn to target ling's terminal
   - :agora/turn-response   - Auto-continue drone debates (P0)
   - :agora/consensus       - Crystallize debate result to memory (P0)
   - :agora/timeout         - Handle dialogue timeout

   SOLID: SRP - Agora dialogue lifecycle only
   CLARITY: R - Represented intent through agora domain"
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
;; Handler: :agora/turn-response (P0: Auto-Continuation)
;; =============================================================================

(defn handle-turn-response
  "Handler for :agora/turn-response events (P0 Auto-Continuation).

   Called after a debate turn completes. Checks if debate should continue
   and dispatches the next turn execution.

   Key insight: Drones are single-shot (ax: Drone Medium Limitations).
   This event handler orchestrates the sequence between turns.

   Expects event data:
   {:dialogue-id     \"dialogue-uuid\"
    :participant-id  \"drone-123\"
    :participant-type :drone
    :signal          :propose | :counter | :approve | :no-change | :defer
    :message         \"The argument text\"
    :confidence      0.8
    :turn-num        5}

   Produces effects:
   - :log      - Log turn completion
   - :dispatch - Chain to :agora/execute-next-turn if debate continues

   Flow:
   1. Check if consensus was already reached (handled by :agora/consensus)
   2. Check if dialogue still active
   3. If active, dispatch next turn execution"
  [_coeffects [_ {:keys [dialogue-id participant-id signal turn-num] :as data}]]
  (let [dialogue (dialogue/get-dialogue dialogue-id)]
    (cond
      ;; Dialogue not found
      (nil? dialogue)
      {:log {:level :warn
             :message (str "Turn response for unknown dialogue: " dialogue-id)}}

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

      ;; Dialogue still active - continue to next turn
      (= :active (:status dialogue))
      {:log {:level :debug
             :message (str "Turn " turn-num " complete from " participant-id
                           " (signal: " (name signal) "), continuing debate")}
       :dispatch [:agora/execute-next-turn {:dialogue-id dialogue-id}]}

      ;; Unknown status
      :else
      {:log {:level :warn
             :message (str "Agora " dialogue-id " in unexpected status: "
                           (:status dialogue))}})))

;; =============================================================================
;; Handler: :agora/execute-next-turn (P0: Async Turn Execution)
;; =============================================================================

(defn handle-execute-next-turn
  "Handler for :agora/execute-next-turn events.

   Executes the next turn in an async debate. Uses require/resolve to
   avoid circular dependency with debate.clj.

   Key: This runs via :dispatch effect's future, so it won't block
   the event loop while the drone executes (which may take seconds).

   Expects event data:
   {:dialogue-id \"dialogue-uuid\"}

   Produces effects:
   - :log - Log execution attempt/result

   Note: The actual turn execution happens in debate/continue-debate!
   which will emit another :agora/turn-response event on completion,
   creating the event-driven loop."
  [_coeffects [_ {:keys [dialogue-id]}]]
  ;; Log intent - actual execution happens in the effect
  {:log {:level :info
         :message (str "Executing next turn for debate: " dialogue-id)}
   ;; Use a custom effect that wraps the debate call
   ;; This is executed via :dispatch which already uses future
   :agora/continue {:dialogue-id dialogue-id}})

;; =============================================================================
;; Handler: :agora/consensus (P0: Crystallize Result)
;; =============================================================================

(defn- generate-debate-summary
  "Generate a summary of the debate for crystallization.

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
    (str "# Agora Debate Consensus: " (or topic "Untitled") "\n\n"
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

   Called when a debate reaches consensus. Crystallizes the result
   to long-term memory for future reference.

   Expects event data:
   {:dialogue-id \"dialogue-uuid\"
    :turns       5}

   Produces effects:
   - :log          - Log consensus achievement
   - :memory-write - Crystallize debate summary to memory
   - :channel-publish - Notify Emacs of consensus"
  [_coeffects [_ {:keys [dialogue-id turns]}]]
  (let [dialogue (schema/get-dialogue dialogue-id)
        turn-log (schema/get-turns dialogue-id)
        summary (generate-debate-summary dialogue turn-log)
        topic (or (get-in dialogue [:config :topic])
                  (:name dialogue)
                  "Untitled debate")]
    {:log {:level :info
           :message (str "Debate " dialogue-id " reached consensus after "
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
   - :agora/turn-response    - Auto-continue drone debates (P0)
   - :agora/execute-next-turn - Execute next turn async (P0)
   - :agora/consensus        - Crystallize debate result (P0)"
  []
  ;; Existing handlers
  (ev/reg-event :agora/turn-dispatched
                [interceptors/debug]
                handle-agora-turn-dispatched)

  (ev/reg-event :agora/timeout
                [interceptors/debug]
                handle-agora-timeout)

  ;; P0: Auto-continuation handlers
  (ev/reg-event :agora/turn-response
                [interceptors/debug]
                handle-turn-response)

  (ev/reg-event :agora/execute-next-turn
                [interceptors/debug]
                handle-execute-next-turn)

  ;; P0: Consensus crystallization
  (ev/reg-event :agora/consensus
                [interceptors/debug]
                handle-consensus))
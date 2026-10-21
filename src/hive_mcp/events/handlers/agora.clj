(ns hive-mcp.events.handlers.agora
  "Agora dialogue event handlers.

   Handles events related to Agora multi-ling dialogues:
   - :agora/turn-completed   - Unified turn completion (renamed from turn-response)
   - :agora/dispatch-next    - Relay the next turn to a ling participant
   - :agora/consensus        - Crystallize dialogue result to memory

   Event chain: :agora/turn-completed -> :agora/dispatch-next -> :swarm-send-prompt

   Turns themselves go out through `hive-mcp.agora.dialogue/dialogue-dispatch`,
   which delegates to the standard swarm dispatch; no handler here relays them."

  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]
            [hive-mcp.agora.dialogue :as dialogue]
            [hive-mcp.agora.schema :as schema]
            [clojure.string :as str]
            [malli.core :as m]
            [hive-mcp.events.handlers.schema :as hs]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants
;; =============================================================================

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

(def ^:private SendPrompt
  "The :swarm-send-prompt effect: the ling the turn is relayed to and the
   Agora-framed prompt it receives."
  [:map [:slave-id :any] [:prompt :string]])

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

(m/=> handle-turn-completed
      [:=> hs/HandlerArgs
       [:map
        [:log hs/LogEffect]
        [:dispatch {:optional true}
         [:tuple [:= :agora/dispatch-next] [:map [:dialogue-id :any]]]]]])

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

(m/=> handle-dispatch-next
      [:=> hs/HandlerArgs
       [:map
        [:log hs/LogEffect]
        [:swarm-send-prompt {:optional true} SendPrompt]]])

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

(m/=> handle-consensus
      [:=> hs/HandlerArgs
       [:map
        [:log hs/LogEffect]
        [:memory-write [:map
                        [:type [:= "decision"]]
                        [:content :string]
                        [:tags [:vector :string]]
                        [:duration [:= "long"]]]]
        [:channel-publish [:map
                           [:event [:= :agora-consensus-crystallized]]
                           [:data [:map [:dialogue-id :any] [:topic :any] [:turns :any]]]]]]])

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-handlers!
  "Register agora-related event handlers.

   Handlers registered:
   - :agora/turn-completed   - Unified turn completion (renamed from turn-response)
   - :agora/dispatch-next    - Relay the next turn to a ling participant
   - :agora/consensus        - Crystallize dialogue result

   Event chain: turn-completed -> dispatch-next -> swarm-send-prompt"
  []
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

(m/=> register-handlers! [:=> [:cat] hs/HandlerRegistry])

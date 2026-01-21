(ns hive-mcp.agora.dialogue
  "Dialogue-aware dispatch wrapper for ling-to-ling communication.

   Part of the Agora (Multi-Ling Dialogue Infrastructure) epic.

   KEY CONCEPTS
   ============
   - **Dialogue**: A conversation thread between 2+ lings with turn tracking
   - **Turn**: A single message in a dialogue with sender, receiver, signal
   - **Signal**: Nash equilibrium signal indicating participant stance

   SIGNALS (Nash Equilibrium)
   ==========================
   Each turn includes a signal parsed from `[SIGNAL: X]` prefix:

   | Signal      | Meaning                    | Effect                |
   |-------------|----------------------------|-----------------------|
   | :propose    | 'Here's a change'          | Resets equilibrium    |
   | :counter    | 'I disagree, alternative'  | Resets equilibrium    |
   | :no-change  | 'I wouldn't change'        | +1 toward equilibrium |
   | :approve    | 'I accept current state'   | +1 toward equilibrium |
   | :defer      | 'I have no opinion'        | Neutral (won't block) |

   USAGE
   =====
   ```clojure
   ;; Create a new dialogue
   (create-dialogue {:participants [\"writer-123\" \"critic-456\"]
                     :topic \"Code review for auth module\"})

   ;; Dispatch within dialogue context
   (dialogue-dispatch
     {:dialogue-id \"d-uuid\"
      :from \"writer-123\"
      :to \"critic-456\"
      :message \"[SIGNAL: propose] Here's my implementation...\"})
   ```

   SOLID: SRP - Single responsibility for dialogue dispatch
   CLARITY: L - Layer separation from core dispatch"
  (:require [hive-mcp.tools.swarm.dispatch :as dispatch]
            [hive-mcp.events.core :as events]
            [hive-mcp.channel.websocket :as ws]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Signal Definitions (Value Objects)
;; =============================================================================

(def signals
  "Valid dialogue turn signal values.

   :propose   - Proposing a change (resets equilibrium)
   :counter   - Counter-proposal (resets equilibrium)
   :no-change - No changes needed (toward equilibrium)
   :approve   - Accepting current state (toward equilibrium)
   :defer     - No opinion (neutral, doesn't block)"
  #{:propose :counter :no-change :approve :defer})

(def equilibrium-signals
  "Signals that contribute to Nash equilibrium."
  #{:no-change :approve})

(def disruption-signals
  "Signals that reset/disrupt equilibrium."
  #{:propose :counter})

;; =============================================================================
;; State - Dialogues and Turns
;; =============================================================================

;; In-memory dialogue state.
;; Shape: {dialogue-id {:id          string
;;                      :participants #{slave-id ...}
;;                      :topic       string
;;                      :created-at  inst
;;                      :turns       [{turn-map} ...]
;;                      :status      :active|:consensus|:timeout|:ended}}
(defonce ^:private *dialogues (atom {}))

;; Counter for generating unique dialogue IDs.
(defonce ^:private *dialogue-counter (atom 0))

;; =============================================================================
;; Signal Parsing
;; =============================================================================

(defn parse-signal
  "Parse [SIGNAL: X] prefix from message.

   Returns tuple: [signal cleaned-message]

   Examples:
   (parse-signal \"[SIGNAL: propose] Here's my change\")
   => [:propose \"Here's my change\"]

   (parse-signal \"No signal here\")
   => [:propose \"No signal here\"]  ; default to :propose"
  [message]
  (if-let [[_ signal-str rest] (re-matches #"(?i)\[SIGNAL:\s*([\w-]+)\]\s*(.*)" message)]
    (let [signal (keyword (str/lower-case signal-str))]
      (if (contains? signals signal)
        [signal (str/trim rest)]
        (do
          (log/warn "Unknown signal" signal-str "- defaulting to :propose")
          [:propose message])))
    ;; No signal prefix - default to :propose (most common case)
    [:propose message]))

(defn format-signal
  "Format a signal into message prefix.

   (format-signal :approve \"LGTM\")
   => \"[SIGNAL: approve] LGTM\""
  [signal message]
  (str "[SIGNAL: " (name signal) "] " message))

;; =============================================================================
;; Turn Recording
;; =============================================================================

(defn- generate-turn-id
  "Generate unique turn ID."
  []
  (str "turn-" (System/currentTimeMillis) "-" (rand-int 10000)))

(defn- record-turn!
  "Record a turn in the dialogue.

   Turn shape:
   {:id         string
    :turn-num   int (1-indexed)
    :sender     slave-id
    :receiver   slave-id
    :message    string (without signal prefix)
    :signal     keyword
    :timestamp  inst
    :task-id    string (populated after dispatch)}"
  [dialogue-id turn-data]
  (swap! *dialogues
         (fn [dialogues]
           (if-let [dialogue (get dialogues dialogue-id)]
             (let [turn-num (inc (count (:turns dialogue)))
                   turn (assoc turn-data
                               :id (generate-turn-id)
                               :turn-num turn-num
                               :timestamp (java.util.Date.))]
               (assoc-in dialogues [dialogue-id :turns]
                         (conj (or (:turns dialogue) []) turn)))
             (do
               (log/warn "Dialogue not found:" dialogue-id)
               dialogues)))))

(defn- update-turn-task-id!
  "Update a turn with the resulting task-id after dispatch."
  [dialogue-id turn-id task-id]
  (swap! *dialogues
         (fn [dialogues]
           (if-let [dialogue (get dialogues dialogue-id)]
             (let [turns (:turns dialogue)
                   updated-turns (mapv #(if (= (:id %) turn-id)
                                          (assoc % :task-id task-id)
                                          %)
                                       turns)]
               (assoc-in dialogues [dialogue-id :turns] updated-turns))
             dialogues))))

;; =============================================================================
;; Dialogue Lifecycle
;; =============================================================================

(defn create-dialogue
  "Create a new dialogue session.

   Arguments:
   - participants: Vector of slave-ids joining the dialogue
   - topic: Description of what the dialogue is about

   Returns: dialogue-id

   Example:
   (create-dialogue {:participants [\"writer-123\" \"critic-456\"]
                     :topic \"Code review for auth module\"})"
  [{:keys [participants topic] :as opts}]
  {:pre [(vector? participants) (>= (count participants) 2)]}
  (let [dialogue-id (str "dialogue-" (swap! *dialogue-counter inc) "-" (System/currentTimeMillis))
        dialogue {:id dialogue-id
                  :participants (set participants)
                  :topic (or topic "Unspecified topic")
                  :created-at (java.util.Date.)
                  :turns []
                  :status :active}]
    (swap! *dialogues assoc dialogue-id dialogue)
    (log/info "Created dialogue" dialogue-id "with participants:" participants)
    ;; Emit event
    (ws/emit! :agora/created {:dialogue-id dialogue-id
                              :participants participants
                              :topic topic})
    dialogue-id))

(defn join-dialogue
  "Add a participant to an existing dialogue.

   Used for dynamic role assignment when lings recruit new voices.

   Returns: true if joined, false if dialogue not found"
  [dialogue-id slave-id]
  (if-let [dialogue (get @*dialogues dialogue-id)]
    (do
      (swap! *dialogues update-in [dialogue-id :participants] conj slave-id)
      (log/info "Participant" slave-id "joined dialogue" dialogue-id)
      (ws/emit! :agora/participant-joined {:dialogue-id dialogue-id
                                           :slave-id slave-id})
      true)
    (do
      (log/warn "Cannot join - dialogue not found:" dialogue-id)
      false)))

(defn leave-dialogue
  "Remove a participant from a dialogue.

   Typically called during ling /wrap or session end.
   If participant was in active turn, marks as :defer.

   Returns: true if left, false if not found"
  [dialogue-id slave-id]
  (if-let [dialogue (get @*dialogues dialogue-id)]
    (do
      (swap! *dialogues update-in [dialogue-id :participants] disj slave-id)
      (log/info "Participant" slave-id "left dialogue" dialogue-id)
      (ws/emit! :agora/participant-left {:dialogue-id dialogue-id
                                         :slave-id slave-id})
      ;; Check if dialogue should end (< 2 participants)
      (let [remaining (count (get-in @*dialogues [dialogue-id :participants]))]
        (when (< remaining 2)
          (swap! *dialogues assoc-in [dialogue-id :status] :ended)
          (log/info "Dialogue" dialogue-id "ended - insufficient participants")))
      true)
    (do
      (log/warn "Cannot leave - dialogue not found:" dialogue-id)
      false)))

;; =============================================================================
;; Dialogue Queries
;; =============================================================================

(defn get-dialogue
  "Get dialogue by ID."
  [dialogue-id]
  (get @*dialogues dialogue-id))

(defn get-dialogue-turns
  "Get all turns in a dialogue."
  [dialogue-id]
  (get-in @*dialogues [dialogue-id :turns] []))

(defn get-last-turn-for
  "Get the most recent turn from a specific participant."
  [dialogue-id slave-id]
  (->> (get-dialogue-turns dialogue-id)
       (filter #(= (:sender %) slave-id))
       last))

(defn get-participants
  "Get current participants in a dialogue."
  [dialogue-id]
  (get-in @*dialogues [dialogue-id :participants] #{}))

(defn dialogue-active?
  "Check if dialogue is still active."
  [dialogue-id]
  (= :active (get-in @*dialogues [dialogue-id :status])))

;; =============================================================================
;; Nash Equilibrium Detection (delegated to consensus namespace)
;; =============================================================================

(defn nash-equilibrium?
  "Check if dialogue has reached Nash equilibrium.

   Returns true when all participants have signaled :no-change or :approve
   in their most recent turn.

   NOTE: This is a basic implementation. The consensus namespace provides
   more sophisticated detection with thresholds and timeout handling."
  [dialogue-id]
  (let [participants (get-participants dialogue-id)
        last-turns (map #(get-last-turn-for dialogue-id %) participants)
        signals (map :signal (filter some? last-turns))]
    (and (= (count signals) (count participants))
         (every? equilibrium-signals signals))))

;; =============================================================================
;; Dialogue Dispatch
;; =============================================================================

(defn dialogue-dispatch
  "Dispatch within dialogue context.

   Records turn, emits event, checks Nash equilibrium, then delegates
   to underlying swarm-dispatch.

   Arguments:
   - dialogue-id: Conversation thread ID
   - from: Sender slave-id (defaults to env CLAUDE_SWARM_SLAVE_ID)
   - to: Receiver slave-id (required)
   - message: Message content, may include [SIGNAL: X] prefix

   Returns: Result from swarm-dispatch with dialogue metadata

   Example:
   (dialogue-dispatch
     {:dialogue-id \"dialogue-123\"
      :to \"critic-456\"
      :message \"[SIGNAL: propose] Here's iteration 2...\"})"
  [{:keys [dialogue-id from to message timeout_ms files] :as params}]
  {:pre [(some? dialogue-id) (some? to) (some? message)]}
  (let [sender-id (or from (System/getenv "CLAUDE_SWARM_SLAVE_ID") "unknown")
        ;; Parse signal from message
        [signal cleaned-message] (parse-signal message)
        ;; Create turn data
        turn-data {:sender sender-id
                   :receiver to
                   :message cleaned-message
                   :signal signal}]

    ;; Validate dialogue exists and is active
    (when-not (dialogue-active? dialogue-id)
      (throw (ex-info "Dialogue not active" {:dialogue-id dialogue-id})))

    ;; Record turn before dispatch
    (record-turn! dialogue-id turn-data)
    (let [turn (last (get-dialogue-turns dialogue-id))]

      ;; Emit turn event
      (ws/emit! :agora/turn {:dialogue-id dialogue-id
                             :turn-num (:turn-num turn)
                             :from sender-id
                             :to to
                             :signal signal})

      ;; Check for Nash equilibrium after recording turn
      (when (nash-equilibrium? dialogue-id)
        (swap! *dialogues assoc-in [dialogue-id :status] :consensus)
        (log/info "Dialogue" dialogue-id "reached Nash equilibrium (CONSENSUS)")
        (ws/emit! :agora/consensus {:dialogue-id dialogue-id
                                    :turns (count (get-dialogue-turns dialogue-id))}))

      ;; Delegate to standard swarm dispatch
      (let [dispatch-result (dispatch/handle-swarm-dispatch
                             {:slave_id to
                              :prompt message
                              :timeout_ms timeout_ms
                              :files files})]

        ;; Update turn with task-id from result (if available)
        (when-let [task-id (try
                             (some-> dispatch-result :result
                                     (clojure.data.json/read-str :key-fn keyword)
                                     :task-id)
                             (catch Exception _ nil))]
          (update-turn-task-id! dialogue-id (:id turn) task-id))

        ;; Return enriched result
        {:dialogue-id dialogue-id
         :turn (:turn-num turn)
         :signal signal
         :dispatch-result dispatch-result}))))

;; =============================================================================
;; Convenience Functions for Lings
;; =============================================================================

(defn propose
  "Send a proposal in a dialogue.
   Shorthand for dialogue-dispatch with :propose signal."
  [dialogue-id to message & {:keys [timeout_ms files]}]
  (dialogue-dispatch {:dialogue-id dialogue-id
                      :to to
                      :message (format-signal :propose message)
                      :timeout_ms timeout_ms
                      :files files}))

(defn approve
  "Send approval in a dialogue.
   Shorthand for dialogue-dispatch with :approve signal."
  [dialogue-id to message & {:keys [timeout_ms]}]
  (dialogue-dispatch {:dialogue-id dialogue-id
                      :to to
                      :message (format-signal :approve message)
                      :timeout_ms timeout_ms}))

(defn counter
  "Send counter-proposal in a dialogue.
   Shorthand for dialogue-dispatch with :counter signal."
  [dialogue-id to message & {:keys [timeout_ms files]}]
  (dialogue-dispatch {:dialogue-id dialogue-id
                      :to to
                      :message (format-signal :counter message)
                      :timeout_ms timeout_ms
                      :files files}))

(defn no-change
  "Signal no changes needed in a dialogue.
   Shorthand for dialogue-dispatch with :no-change signal."
  [dialogue-id to message & {:keys [timeout_ms]}]
  (dialogue-dispatch {:dialogue-id dialogue-id
                      :to to
                      :message (format-signal :no-change message)
                      :timeout_ms timeout_ms}))

;; =============================================================================
;; Testing/Debug Utilities
;; =============================================================================

(defn list-dialogues
  "List all dialogues with summary info."
  []
  (->> @*dialogues
       vals
       (map #(select-keys % [:id :topic :status :participants
                             :created-at]))
       (sort-by :created-at)
       reverse))

(defn reset-dialogues!
  "Clear all dialogue state. For testing."
  []
  (reset! *dialogues {})
  (reset! *dialogue-counter 0)
  (log/info "Dialogue state reset"))

(defn dialogue-summary
  "Get a human-readable summary of a dialogue."
  [dialogue-id]
  (when-let [d (get-dialogue dialogue-id)]
    {:id dialogue-id
     :topic (:topic d)
     :status (:status d)
     :participants (vec (:participants d))
     :turn-count (count (:turns d))
     :last-signal (some-> (last (:turns d)) :signal)
     :consensus? (= :consensus (:status d))}))

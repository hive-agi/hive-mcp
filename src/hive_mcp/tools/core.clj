(ns hive-mcp.tools.core
  "Core utilities for MCP tool responses.
   
   All tool handler modules should require this namespace for
   consistent response formatting.
   
   Piggyback Instructions:
   Enables hivemind→ling bidirectional communication via tool response.
   When an agent-id is provided to response functions, any pending
   instructions for that agent are included in the response and drained
   from the queue."
  (:require [clojure.data.json :as json]))

;; =============================================================================
;; Instruction Queue for Piggyback Communication
;; =============================================================================

;; Atom storing pending instructions per agent. Map of agent-id -> [instructions...]
;; Used for hivemind→ling communication via tool response piggyback.
;; Instructions are drained when included in a response.
(defonce instruction-queues (atom {}))

(defn push-instruction!
  "Push an instruction to an agent's queue.
   
   The instruction will be piggybacked on the next tool response
   that includes this agent-id.
   
   Example instructions:
   - {:type \"flow\" :action \"pause\"}
   - {:type \"priority\" :level \"urgent\"}
   - {:type \"context\" :file \"/changed.clj\" :message \"Modified\"}
   - {:type \"coordinate\" :wait-for \"ling-2\"}"
  [agent-id instruction]
  (swap! instruction-queues update agent-id (fnil conj []) instruction))

(defn drain-instructions!
  "Drain all pending instructions for an agent.
   
   Returns the vector of instructions and removes them from the queue.
   Returns empty vector if no instructions pending."
  [agent-id]
  (let [instructions (get @instruction-queues agent-id [])]
    (swap! instruction-queues dissoc agent-id)
    instructions))

(defn- attach-instructions
  "Attach pending instructions to a response if agent-id provided and instructions exist."
  [response agent-id]
  (if agent-id
    (let [instructions (drain-instructions! agent-id)]
      (if (seq instructions)
        (assoc response :pending_instructions instructions)
        response))
    response))

;; =============================================================================
;; MCP Response Constructors
;; =============================================================================

(defn mcp-success
  "Create a successful MCP response. Text can be string or will be pr-str'd.
   
   Options:
   - :agent-id - If provided, drains and attaches pending instructions for this agent"
  [text & {:keys [agent-id]}]
  (-> {:type "text" :text (if (string? text) text (pr-str text))}
      (attach-instructions agent-id)))

(defn mcp-error
  "Create an error MCP response."
  [message]
  {:type "text" :text message :isError true})

(defn mcp-json
  "Create a successful MCP response with JSON-encoded data.
   
   Options:
   - :agent-id - If provided, drains and attaches pending instructions for this agent"
  [data & {:keys [agent-id]}]
  (-> {:type "text" :text (json/write-str data)}
      (attach-instructions agent-id)))

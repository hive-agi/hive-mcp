(ns hive-mcp.tools.consolidated.hivemind
  "Consolidated Hivemind coordination CLI tool."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.hivemind.core :as hm]
            [hive-mcp.hivemind.event-registry :as event-registry]
            [hive-mcp.tools.core :refer [mcp-error]]
            [clojure.string :as str]
            [hive-mcp.extensions.soft :as soft]
            [malli.core :as m]))

(def ^:private tools-by-name
  (into {} (map (fn [t] [(keyword (str/replace (:name t) "hivemind_" "")) (:handler t)])
                hm/tools)))

(def ^:const default-nudge-template
  "MIDFLIGHT NUDGE: Shout your current state via mcp__hive__hivemind event='progress' — what phase you're in (survey/implement/verify), what you've done recently, what you're currently working on, any blockers. Then continue your task. Report progress at checkpoints per your instructions; don't batch everything into a final shout.")

(def NudgeResponse
  "What handle-nudge answers: its own mcp-error, or whatever the agent
   domain's handle-dispatch answers, passed through untouched. That is a
   text-content response or a CallToolResult :content envelope."
  [:or hive-mcp.tools.core/McpResponse
   [:map [:content [:sequential [:map [:type :string] [:text {:optional true} :string]]]]]])

(defn handle-nudge
  "Nudge a silent ling into shouting its current state.
   Composes over agent dispatch — sends nudge prompt to running agent."
  [{:keys [agent_id message]}]
  (cond
    (str/blank? agent_id)
    (mcp-error "agent_id is required for nudge")

    :else
    (let [nudge-prompt (or (when-not (str/blank? message) message)
                           default-nudge-template)
          result (if-let [handle-dispatch (soft/resolve-soft 'hive-mcp.tools.agent.dispatch/handle-dispatch)] (handle-dispatch {:agent_id agent_id
                                            :prompt   nudge-prompt}) (mcp-error "nudge needs the agent domain (hive-agent)"))]
      result)))

(m/=> handle-nudge [:=> [:cat [:maybe [:map [:agent_id {:optional true} [:maybe :string]] [:message {:optional true} [:maybe :string]]]]] NudgeResponse])

(def handlers
  {:shout    (:shout tools-by-name)
   :ask      (:ask tools-by-name)
   :status   (:status tools-by-name)
   :respond  (:respond tools-by-name)
   :messages (:messages tools-by-name)
   :nudge    handle-nudge})

(def handle-hivemind
  (make-cli-handler #'handlers))

(def tool-def
  {:name "hivemind"
   :consolidated true
   :description "Hivemind coordination: shout (send a message), ask (request decision), status (coordinator state), respond (answer ask), messages (agent history), nudge (wake silent ling into shouting state). Use command='help' to list all.

Addressing, for shout: `to` names ONE peer and the message reaches that agent and nobody else, which is the cheap path and the right default for anything that concerns one peer. Without `to` it goes to whoever spawned you. `broadcast` reaches everyone and is an exception that requires `broadcast_reason`."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["shout" "ask" "status" "respond" "messages" "nudge" "help"]
                                         :description "Hivemind operation to perform"}
                              "event_type" (event-registry/event-type-schema "Type of event for shout")
                              "task" {:type "string"
                                      :description "Current task description"}
                              "message" {:type "string"
                                         :description "Status message"}
                              "to" {:type "string"
                                    :description "shout: address ONE peer by agent id. The message reaches that agent and nobody else - not the coordinator, not your spawner. Prefer this over broadcasting for anything that concerns a single peer."}
                              "context_id" {:type "string"
                                            :description "The A2A conversation id. On shout, replying to a row you received means passing that row's :ctx (omit it to start an exchange; the reply carries the new id). On messages, read only that conversation."}
                              "broadcast" {:type "boolean"
                                           :description "shout: ask to reach every reader. Requires broadcast_reason, is refused without an admissible one, and is refused on volume once the project's broadcast budget is spent."}
                              "broadcast_reason" {:type "string"
                                                  :enum ["halt" "membership" "shared-discovery" "coordinator-directive"]
                                                  :description "Why every reader needs this. halt: peers must stop. membership: the roster changed. shared-discovery: a finding that invalidates a shared assumption. coordinator-directive: the coordinator addressing its swarm."}
                              "data" {:type "object"
                                      :description "Additional event data"}
                              "directory" {:type "string"
                                           :description "Working directory for project-id derivation"}
                              "project_id" {:type "string"
                                            :description "Explicit project-id override. Wins over directory-derivation across shout/ask/status/messages."}
                              "question" {:type "string"
                                          :description "ask: what you need the human to answer, in one line"}
                              "context" {:type "string"
                                         :description "ask: what the human should read before answering (findings, trade-offs, what you tried)"}
                              "expects" {:type "string"
                                         :enum ["confirm" "choice" "text"]
                                         :description "ask: confirm (yes/no), choice (one of options; typed text still accepted) or text (free-form). Default: choice with options, text without"}
                              "options" {:type "array"
                                         :items {:type "string"}
                                         :description "ask: suggested answers (omit for free-form text)"}
                              "cc" {:type "array"
                                    :items {:type "string"}
                                    :description "ask: other agent ids that are told the answer"}
                              "timeout_ms" {:type "integer"
                                            :description "Timeout in ms (default 300000)"}
                              "ask_id" {:type "string"
                                        :description "respond: ID of the ask to answer"}
                              "answer" {:type "string"
                                        :description "respond: the answer, any text (an option, yes/no, or a typed reply)"}
                              "decision" {:type "string"
                                          :description "respond: alias of answer"}
                              "agent_id" {:type "string"
                                          :description "Agent identifier"}}
                 :required ["command"]}
   :handler #'handle-hivemind})

(def tools [tool-def])

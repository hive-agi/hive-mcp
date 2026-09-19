(ns hive-mcp.hivemind.tools
  "Hivemind MCP tool definitions and inline handlers."

  (:require [hive-mcp.hivemind.state :as state]
            [hive-mcp.hivemind.messaging :as messaging]
            [hive-mcp.hivemind.status :as status]
            [hive-mcp.hivemind.event-registry :as event-registry]
            [hive-mcp.agent.context :as ctx]
            [hive-spi.swarm.protocol :as proto]
            [hive-mcp.swarm.datascript.registry :as registry]
            [hive-mcp.project.scope :as project-scope]
            [clojure.data.json :as json]
            [clojure.set :as set]
            [taoensso.timbre :as log]
            [hive-dsl.bounded-atom :refer [bkeys]]
            [clojure.string :as str]
            [hive-mcp.channel.piggyback :as piggyback]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def tools
  [{:name "hivemind_shout"
    :description "Send a message into the swarm.

ADDRESS IT. `to` names one peer and the message reaches that agent and nobody
else - not the coordinator, not your spawner. That is the cheap path and the
right default for anything that concerns one peer:
- Ask a peer: (hivemind_shout :progress {:to \"ling-b\" :message \"is :order/total a string on purpose?\"})

REPLY IN THE THREAD. A row that arrived with a `:ctx` was addressed to you
privately, and `:a` is who sent it. Answer that peer, in that conversation:
- (hivemind_shout :progress {:to <the row's :a> :context_id <the row's :ctx> :message \"..\"})
Answering without `to` sends your reply to your spawner instead, so the peer
who asked never sees it. Answering without `context_id` starts a second
conversation about the same subject, and neither half can be read whole.

WITHOUT `to` the message goes to whoever spawned you, which is the right address
for your own status:
- Task progress: (hivemind_shout :progress {:task \"..\" :percent 50})
- Completion: (hivemind_shout :completed {:task \"..\" :result \"..\"})
- Errors: (hivemind_shout :error {:task \"..\" :error \"..\"})
- Blocked: (hivemind_shout :blocked {:task \"..\" :reason \"need input\"})

BROADCAST IS AN EXCEPTION, not a delivery mode. It copies your sentence into
every reader's context, so it needs `broadcast_reason`, one of: halt,
membership, shared-discovery, coordinator-directive. Without an admissible
reason the broadcast is REFUSED and the message is delivered by its ordinary
route instead; the reply tells you so. An admissible reason is not a standing
permission either: broadcasts are metered per project, and past the budget you
get broadcast_refused=budget-exhausted and the ordinary route. Address the
peers who need it instead.

IMPORTANT: Lings MUST pass agent_id explicitly (use your $CLAUDE_SWARM_SLAVE_ID).
The env var fallback reads from MCP server process, NOT your ling process!"
    :inputSchema {:type "object"
                  :properties {"agent_id" {:type "string"
                                           :description "REQUIRED for lings: Pass your $CLAUDE_SWARM_SLAVE_ID. Without this, status sync will fail."}
                               "event_type" (event-registry/event-type-schema "Type of event")
                               "task" {:type "string"
                                       :description "Current task description"}
                               "message" {:type "string"
                                          :description "Status message"}
                               "to" {:type "string"
                                     :description "Address ONE peer by agent id. The message reaches that agent and nobody else. Prefer this over broadcasting for anything that concerns a single peer."}
                               "context_id" {:type "string"
                                             :description "The A2A conversation this message belongs to. Replying to a row you received: pass that row's :ctx. Starting an exchange: omit it, and the reply carries the new id."}
                               "broadcast" {:type "boolean"
                                            :description "Ask to reach every reader. Requires broadcast_reason, is refused without an admissible one, and is refused on volume once the project's broadcast budget is spent."}
                               "broadcast_reason" {:type "string"
                                                   :enum ["halt" "membership" "shared-discovery" "coordinator-directive"]
                                                   :description "Why every reader needs this. halt: peers must stop. membership: the roster changed. shared-discovery: a finding that invalidates a shared assumption. coordinator-directive: the coordinator addressing its swarm."}
                               "directory" {:type "string"
                                            :description "Working directory for project-id derivation. Pass your cwd to scope shouts to your project."}
                               "project_id" {:type "string"
                                             :description "Explicit project-id override. Wins over directory-derivation; bypass scope/last-path-segment heuristic when caller already knows the project."}
                               "data" {:type "object"
                                       :description "Additional event data"}}
                  :required ["event_type"]}
    :handler (fn [{:keys [agent_id event_type task message directory project_id data
                          to context_id broadcast broadcast_reason _caller_id] :as _args}]
               (let [ctx-agent (ctx/current-agent-id)
                     env-agent (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                     ;; BUG FIX: _caller_id is injected by bb-mcp from the CHILD
                     ;; process's CLAUDE_SWARM_SLAVE_ID. For headless lings this is
                     ;; the correct ling identity. env-agent reads from the SERVER
                     ;; process env, which is wrong for child lings.
                     effective-id (or agent_id
                                      _caller_id
                                      ctx-agent
                                      env-agent
                                      "unknown-agent")
                     _ (when (and (not agent_id) (not _caller_id) (not ctx-agent))
                         (log/warn "[hivemind_shout] No agent_id in args or context, using fallback:" (or env-agent "unknown-agent")))

                     fallback-used? (and (not agent_id) (not _caller_id) (not ctx-agent))]
                 ;; NOTE: Only pass directory if caller explicitly provided it.
                 ;; Do NOT fall back to ctx/current-directory - that resolves to the
                 ;; MCP server's cwd, not the ling's cwd. shout! has its own slave-cwd
                 ;; lookup from DataScript which correctly uses the ling's registered cwd.
                 ;; :deliberate? - this is the one path where the AGENT chose to
                 ;; speak, as opposed to runtime telemetry shouted on its behalf.
                 ;; The piggyback digest folds :progress bursts to one row per
                 ;; agent and never folds a deliberate one (measured 2026-09-07:
                 ;; a wave member's own shout vanished into its turn telemetry).
                 ;; The VERDICT is returned rather than a bare success: a sender
                 ;; that asked to broadcast has to learn it was refused, and a
                 ;; sender that opened a conversation needs the id to continue
                 ;; under. Reporting only :success would hide both.
                 (let [verdict (messaging/shout-with-verdict!
                                effective-id (keyword event_type)
                                (merge {:task task :message message :deliberate? true}
                                       (when to {:to to})
                                       (when context_id {:context-id context_id})
                                       (when broadcast {:broadcast? true})
                                       (when broadcast_reason {:broadcast-reason broadcast_reason})
                                       (when directory {:directory directory})
                                       (when project_id {:project-id project_id})
                                       data))]
                   {:type "text"
                    :text (json/write-str
                           (cond-> {:success (boolean (:delivered verdict))
                                    :agent_id effective-id
                                    :routing (some-> (:routing verdict) name)}
                             (:context-id verdict) (assoc :context_id (:context-id verdict))
                             (:to verdict) (assoc :to (:to verdict))
                             (:ref verdict) (assoc :payload_ref (:ref verdict))
                             (:to-unresolved verdict)
                             (assoc :to_unresolved (:to-unresolved verdict)
                                    :to_warning (str "No agent named " (:to-unresolved verdict)
                                                     " is registered. A directed message reaches its named"
                                                     " recipient and nobody else, so this one may reach NOBODY."
                                                     " Check the id, or drop `to` to reach your spawner."))
                             (:broadcast-refused verdict)
                             (assoc :broadcast_refused (name (:broadcast-refused verdict))
                                    :note (str "Broadcast refused ("
                                               (name (:broadcast-refused verdict))
                                               "); delivered by its ordinary route instead. "
                                               "Name a peer with `to`, or give an admissible `broadcast_reason`."))
                             fallback-used?
                             (assoc :warning (str "agent_id not provided - using fallback: " effective-id
                                                  ". Lings should always pass agent_id explicitly for status sync."))))})))}

   {:name "hivemind_ask"
    :description "Request a decision from the human coordinator.

USE THIS when you need human approval or guidance:
- Before destructive operations
- When multiple valid approaches exist
- When requirements are ambiguous

BLOCKS until human responds (up to timeout).

Example: hivemind_ask('Should I delete these 50 files?', ['yes', 'no', 'show me first'])

IMPORTANT: Lings MUST pass agent_id explicitly (use your $CLAUDE_SWARM_SLAVE_ID).
The env var fallback reads from MCP server process, NOT your ling process!"
    :inputSchema {:type "object"
                  :properties {"agent_id" {:type "string"
                                           :description "REQUIRED for lings: Pass your $CLAUDE_SWARM_SLAVE_ID. Without this, coordination may fail."}
                               "question" {:type "string"
                                           :description "What decision do you need?"}
                               "options" {:type "array"
                                          :items {:type "string"}
                                          :description "Available options (or omit for free-form)"}
                               "timeout_ms" {:type "integer"
                                             :description "Timeout in ms (default 300000 = 5 min)"}
                               "directory" {:type "string"
                                            :description "Working directory for project-id derivation. Pass your cwd for proper scoping."}
                               "project_id" {:type "string"
                                             :description "Explicit project-id override. Wins over directory-derivation."}}
                  :required ["question"]}
    :handler (fn [{:keys [agent_id _caller_id question options timeout_ms directory project_id]}]
               (let [effective-id (or agent_id
                                      _caller_id
                                      (ctx/current-agent-id)
                                      (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                                      "unknown-agent")
                     effective-dir (or directory
                                       (ctx/current-directory))
                     effective-project-id (or project_id
                                              (when effective-dir (project-scope/get-current-project-id effective-dir)))
                     result (messaging/ask! effective-id question options
                                            :timeout-ms (or timeout_ms 300000))]
                 {:type "text"
                  :text (json/write-str
                         (if (:timeout result)
                           {:timeout true :message "No response within timeout"}
                           {:decision (:decision result)
                            :by (:by result)
                            :directory effective-dir
                            :project-id effective-project-id}))}))}

   {:name "hivemind_status"
    :description "Get current hivemind coordinator status.

Returns:
- Active agents and their status
- Pending questions awaiting human decision
- Channel connection status

When directory is provided, filters to only show agents belonging to that project.
This prevents cross-project pollution in multi-project hivemind sessions."
    :inputSchema {:type "object"
                  :properties {"directory" {:type "string"
                                            :description "Working directory to scope results to a specific project. Pass your cwd to see only agents from your project."}
                               "project_id" {:type "string"
                                             :description "Explicit project-id override. Wins over directory-derivation; use when caller already knows the project (e.g. cross-project audits)."}}
                  :required []}
    :handler (fn [{:keys [directory project_id]}]
               (let [effective-dir (or directory (ctx/current-directory))
                     project-id (or project_id
                                    (when effective-dir (project-scope/get-current-project-id effective-dir)))]
                 {:type "text"
                  :text (json/write-str (status/get-status project-id))}))}

   {:name "hivemind_respond"
    :description "Respond to a pending ask from an agent.

Used by the coordinator to answer agent questions."
    :inputSchema {:type "object"
                  :properties {"ask_id" {:type "string"
                                         :description "ID of the ask to respond to"}
                               "decision" {:type "string"
                                           :description "The decision/response"}}
                  :required ["ask_id" "decision"]}
    :handler (fn [{:keys [ask_id decision]}]
               {:type "text"
                :text (json/write-str
                       (if (messaging/respond-ask! ask_id decision)
                         {:success true}
                         {:error "No pending ask with that ID"}))})}

   {:name "hivemind_messages"
    :description "Get recent messages from a specific agent.

Returns up to 10 recent shout messages with their payloads.
Use this to retrieve message content that agents have broadcast.

When directory is provided, the available-agents list is filtered to that project.
The specific agent lookup is still allowed even if agent is from another project."
    :inputSchema {:type "object"
                  :properties {"agent_id" {:type "string"
                                           :description "Agent identifier to get messages from"}
                               "context_id" {:type "string"
                                             :description "Redeem a conversation id. Returns every message of that A2A conversation, oldest first, without marking anything read. This is how a coordinator reads the peer exchange its peer-traffic summary named. When given, agent_id is not needed."}
                               "limit" {:type "integer"
                                        :description "Max messages to return for a context_id read (default 100)."}
                               "directory" {:type "string"
                                            :description "Working directory to scope available-agents list to a specific project. Pass your cwd to see only agents from your project."}
                               "project_id" {:type "string"
                                             :description "Explicit project-id override. Wins over directory-derivation."}}
                  :required []}
    :handler (fn [args]
               (let [agent_id (or (:agent_id args)
                                  (:agent-id args)
                                  (get args "agent_id")
                                  (get args "agent-id"))
                     context_id (or (:context_id args)
                                    (:context-id args)
                                    (get args "context_id")
                                    (get args "context-id"))
                     limit (or (:limit args) (get args "limit") 100)]
                 (cond
                   ;; Redeeming a conversation id. The peer-traffic summary a
                   ;; coordinator receives names a contextId; that row is only
                   ;; worth its characters if the id can be cashed in, so this
                   ;; branch is what makes the summary actionable rather than
                   ;; decorative. Reading is not receiving: no cursor moves.
                   (not (str/blank? (str context_id)))
                   {:type "text"
                    :text (json/write-str
                           (let [msgs (piggyback/fetch-conversation context_id :limit limit)]
                             (if (seq msgs)
                               {:context_id context_id :messages msgs :count (count msgs)}
                               {:context_id context_id :messages [] :count 0
                                :note "No messages under that conversation id. It may have aged out of the buffer, or the id may be from another project."})))}

                   (str/blank? (str agent_id))
                   {:type "text", :text (json/write-str {:error "hivemind_messages needs either agent_id or context_id"})}

                   :else
                   (let [explicit-project-id (or (:project_id args)
                                                 (:project-id args)
                                                 (get args "project_id")
                                                 (get args "project-id"))
                         effective-dir (or (:directory args)
                                           (get args "directory")
                                           (ctx/current-directory))
                         project-id (or explicit-project-id
                                        (when effective-dir (project-scope/get-current-project-id effective-dir)))
                         ds-agents (if project-id
                                     (clojure.core/set (map :slave/id (proto/get-slaves-by-project registry/default-registry project-id)))
                                     (clojure.core/set (map :slave/id (proto/get-all-slaves registry/default-registry))))
                         ;; Iterate raw bounded-atom entries for project filtering
                         msg-agents (if project-id
                                      (->> @(:atom state/agent-registry)
                                           (filter (fn [[_id entry]]
                                                     (some #(= project-id (:project-id %))
                                                           (:messages (:data entry)))))
                                           (map first)
                                           set)
                                      (clojure.core/set (bkeys state/agent-registry)))
                         available-agents (vec (set/union ds-agents msg-agents))]
                     {:type "text"
                      :text (json/write-str
                             (if-let [messages (status/get-agent-messages agent_id)]
                               {:agent_id agent_id
                                :messages messages
                                :project-filter project-id}
                               {:error (str "Agent not found: " agent_id)
                                :available-agents available-agents
                                :project-filter project-id}))}))))}])
(defn register-tools!
  "Register hivemind tools with the MCP server."
  [register-fn]
  (doseq [tool tools]
    (register-fn tool)))

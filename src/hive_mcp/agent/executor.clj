(ns hive-mcp.agent.executor
  "Tool execution with permission gates."
  (:require [hive-mcp.agent.registry :as registry]
            [hive-mcp.context.request :as ctx]
            [hive-mcp.channel.piggyback-tap :as tap]
            [hive-mcp.hivemind.core :as hivemind]
            [hive-mcp.server.permissions :as permissions]
            [hive-dsl.result :as r]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]
            [hive-mcp.channel.task-signal :as task-signal]
            [hive-mcp.agent.grant :as grant]
            [hive-mcp.channel.activation :as activation]
            [malli.core :as m]
            [hive-addon.registry.commands :as acmds]
            [clojure.string :as str]))

#_{:clj-kondo/ignore [:deprecated-var]}
(def ^:dynamic ^:deprecated *current-agent-id* ctx/*current-agent-id*)

(defn current-agent-id
  "Get the current agent-id from execution context."
  []
  (ctx/current-agent-id))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn requires-approval?
  "Check if a tool call requires human approval."
  [tool-name perms]
  (and (permissions/dangerous-tool? tool-name)
       (not (contains? (set perms) :auto-approve))))

(defn request-approval!
  "Request human approval via hivemind channel."
  [agent-id tool-name arguments]
  (let [question (format "Agent %s wants to call %s with:\n%s\n\nApprove?"
                         agent-id tool-name (json/write-str arguments))
        response (hivemind/ask! agent-id question ["yes" "no"]
                                :timeout-ms 60000)]
    (= "yes" (:decision response))))

(defn- grant-refusal
  "The grant gate on the delegation path (headless lings call raw handlers
   here, outside the route middleware). Keys on the request context's caller
   id, bound by the loop that runs the ling, never on tool arguments: the
   model-written `agent_id` is not read. No bound caller = no gate."
  [tool-name arguments]
  (when-let [caller (some-> (ctx/current-caller-id) str not-empty)]
    (when (grant/recorded-grant grant/registry-get-slave caller)
      (grant/call-refusal (grant/domain) grant/registry-get-slave caller tool-name
                          (or (get arguments :command) (get arguments "command"))))))

(defn- enum-subdomains
  "The first words of TOOL's multi-word `command` enum entries
   (\"docs describe-function\" -> \"docs\"). Pure."
  [tool]
  (into #{}
        (keep (fn [c] (when (and (string? c) (str/includes? c " "))
                        (first (str/split c #" ")))))
        (get-in tool [:inputSchema :properties "command" :enum])))

(defn- registry-subdomain-owners
  "Registered tools that route TOOL-NAME as a subcommand, sorted. Read from
   the tool registry and the addon command registry, never a hardcoded list:
   an addon contribution (`code carto`) or a multi-word command enum entry
   (`emacs docs <verb>`) names the owner. Only tools the delegation registry
   can call are answered."
  [tool-name]
  (let [registered (set (registry/list-tools))
        contributed (filter #(contains? (acmds/get-commands %) tool-name)
                            (acmds/contributed-tool-names))
        enumerated (filter #(contains? (enum-subdomains (registry/get-tool %)) tool-name)
                           registered)]
    (->> (concat contributed enumerated)
         (filter registered)
         (remove #{tool-name})
         distinct
         sort
         vec)))

(def ^:dynamic *subdomain-owners*
  "Port: tool-name -> the registered tools that route it as a subcommand.
   Tests bind a stub rather than mutating the registries."
  registry-subdomain-owners)

(defn unknown-tool-message
  "The error for a call to TOOL-NAME, which no registered tool answers. When
   OWNERS (tools routing TOOL-NAME as a subcommand) is non-empty, the message
   names the route that still works. Pure."
  [tool-name owners]
  (let [base (str "Unknown tool: " tool-name)]
    (if (seq owners)
      (str base ". " tool-name " is a subcommand of the "
           (str/join " or " (map #(str % " tool") owners)) ": call "
           (str/join " or " (map #(str % " with command=\"" tool-name " <verb>\"") owners)))
      base)))

(defn execute-tool
  "Execute a tool by name with arguments. A call the caller's grant does not
   permit is refused before the handler runs. An unknown tool that another
   tool routes as a subcommand is answered with that route."
  [tool-name arguments]
  (if-let [tool (registry/get-tool tool-name)]
    (if-let [no (grant-refusal tool-name arguments)]
      (do (log/warn "grant: refused a delegated tool call" {:tool tool-name})
          {:success false :error no})
      (let [result (r/try-effect* :agent/tool-execution-failed
                                  (let [handler (:handler tool)]
                                    (handler arguments)))]
        (if (r/ok? result)
          {:success true :result (:ok result)}
          (do (log/error "Tool execution failed:" tool-name {:error (:message result)})
              {:success false :error (:message result)}))))
    {:success false
     :error (unknown-tool-message
             tool-name
             (r/guard Exception [] (*subdomain-owners* (str tool-name))))}))

(defn format-tool-result
  "Format tool result as assistant message for conversation history."
  [call-id tool-name result]
  {:role "tool"
   :tool_call_id call-id
   :name tool-name
   :content (if (:success result)
              (let [r (:result result)]
                (if (string? (:text r)) (:text r) (json/write-str r)))
              (str "Error: " (:error result)))})

(defn- append-piggyback-to-results
  "Append piggyback drain text to the last tool result's content.
   Mirrors how wrap-handler-piggybacks appends to the last text item
   in MCP responses — ensures the LLM sees hivemind shouts, memory
   blocks, etc. even on the non-MCP (agentic loop) execution path."
  [results piggyback-text]
  (if (and (seq results) piggyback-text)
    (let [last-idx (dec (count results))
          last-result (nth results last-idx)]
      (assoc results last-idx
             (update last-result :content str piggyback-text)))
    results))

(defn- drain-fn-for
  "Resolve the piggyback drain for a batch: opts' :drain-fn, else tap/drain-all!.
   The result is called as (f agent-id project-id ctx) and returns text or nil."
  [{:keys [drain-fn]}]
  (or drain-fn tap/drain-all!))

(defn execute-tool-calls
  "Execute a batch of tool calls, respecting permissions.
   After execution, drains all piggyback channels (hivemind, memory, async,
   catchup) and appends to the last tool result — ensures headless/OpenRouter
   lings receive hivemind shouts that would otherwise be lost.
   Task cues harvested from this batch steer the MEMORY drain; they are empty
   unless task-signal/enabled?. The cues feed `activation/drain-ctx`, so this
   lane carries the same pins and floor-cap as the MCP tool lane. `:tool-name`
   is the single call's name when the batch holds exactly one, else nil.
   opts may carry `:drain-fn` to supply the piggyback drain; see drain-fn-for.

   Identity: this lane is the server's own agentic loop running the ling, so
   the caller id is bound by the server, not asserted by a client. It is
   recorded as verified with claimant :server-loop."
  ([agent-id tool-calls permissions]
   (execute-tool-calls agent-id tool-calls permissions nil))
  ([agent-id tool-calls permissions {:keys [project-id] :as opts}]
   (ctx/with-request-context {:agent-id   agent-id
                              :caller-id  agent-id
                              :project-id project-id
                              :identity   {:caller-id agent-id
                                           :claimant  :server-loop
                                           :verified? (boolean (not-empty (str agent-id)))}}
     (let [all-results (mapv (fn [{:keys [id name arguments]}]
                               (let [approved? (or (not (requires-approval? name permissions))
                                                   (request-approval! agent-id name arguments))]
                                 (if approved?
                                   (let [result (execute-tool name arguments)]
                                     (format-tool-result id name result))
                                   (format-tool-result id name
                                                       {:success false :error "Rejected by human"}))))
                             tool-calls)
           cues (into #{}
                     (comp (mapcat (fn [{:keys [name arguments]}]
                                     (task-signal/cues name arguments)))
                           (take task-signal/max-tokens))
                     tool-calls)
           drain-ctx (activation/drain-ctx
                      {:tool-name (when (= 1 (count tool-calls)) (:name (first tool-calls)))
                       :cues cues
                       :caller-id agent-id})
           ;; Drain piggyback channels — bridges hivemind shouts to agentic loop
           piggyback-text ((drain-fn-for opts) agent-id project-id drain-ctx)]
       (append-piggyback-to-results all-results piggyback-text)))))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> current-agent-id [:=> [:cat] :any])

;; ---------------------------------------------------------------------------
;; Contract schemas. They live in this namespace because hive-mcp.agent.* is a
;; frozen extraction prefix (hive-mcp.kernel.census): no new namespace may be
;; added under it. Every schema generates.
;; ---------------------------------------------------------------------------

(def ToolResult
  "What `executor/execute-tool` answers."
  [:map
   [:success :boolean]
   [:result {:optional true} :any]
   [:error {:optional true} :any]])

(def ToolMessage
  "A role:tool message for conversation history."
  [:map
   [:role [:= "tool"]]
   [:tool_call_id :any]
   [:name :any]
   [:content :string]])

(def Permissions
  "Permission flags a tool call runs with (a coll, `(set perms)` is taken)."
  [:maybe [:or [:sequential :any] [:set :any]]])

(m/=> unknown-tool-message [:=> [:cat :any [:maybe [:sequential :string]]] :string])

;; `and` answers false/nil/true, so the result is a maybe-boolean.
(m/=> requires-approval? [:=> [:cat :any Permissions] [:maybe :boolean]])

(m/=> request-approval! [:=> [:cat :any :any :any] :boolean])

(m/=> execute-tool [:=> [:cat :any :any] ToolResult])

;; `result` is only read with keyword lookups, so any map (or nil) is fine.
(m/=> format-tool-result [:=> [:cat :any :any [:maybe :map]] ToolMessage])

(m/=> execute-tool-calls
      [:function
       [:=> [:cat :any [:sequential :map] Permissions] [:sequential ToolMessage]]
       [:=> [:cat :any [:sequential :map] Permissions [:maybe :map]]
        [:sequential ToolMessage]]])

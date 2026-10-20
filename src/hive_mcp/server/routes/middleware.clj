(ns hive-mcp.server.routes.middleware
  "Composable handler wrappers (middleware) for MCP tool dispatch.

   Each wrapper has a single responsibility (SRP). They compose via ->
   threading in build-middleware-chain.

   Execution order (request flows inward):
   handler → nats-notify → retry → async → [default-async] → guard
   → normalize → compress → piggybacks → context → response

   DDD: Application Service layer — request processing pipeline."
  (:require [hive-mcp.server.routes.identity :as id]
            [hive-mcp.context.request :as ctx]
            [hive-mcp.spi.session :as crystal]
            [hive-mcp.channel.async-result :as async-buf]
            [hive-mcp.server.routes.async-tasks :as async-tasks]
            [hive-mcp.dsl.response :as compress]
            [hive-mcp.extensions.registry :as ext]
            [hive-dsl.context.identity :as ctx-id]
            [taoensso.timbre :as log]
            [clojure.walk :as walk]
            [clojure.string :as str]
            [hive-mcp.channel.task-signal :as task-signal]
            [hive-mcp.channel.activation :as activation]
            [hive-mcp.channel.blocks :as blocks]
            [hive-spi.guard.ports :as gp]
            [hive-mcp.agent.grant :as grant]
            [hive-mcp.agent.identity :as agent-identity]
            [hive-mcp.server.routes.async-ack :as async-ack]
            [hive-mcp.tools.async-params :as async-params]
            [malli.core :as m])
(:import [java.util.concurrent RejectedExecutionException]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; =============================================================================
;; Hot-Reload Retry
;; =============================================================================

(def ^:const hot-reload-retry-delay-ms 100)
(def ^:const hot-reload-max-retries 3)

(defn- hot-reload-error?
  [^Throwable e]
  (let [msg (str (.getMessage e))]
    (or (re-find #"(?i)var.*not.*found" msg)
        (re-find #"(?i)unbound|undefined" msg)
        (re-find #"(?i)no.*protocol.*method" msg)
        (instance? IllegalStateException e))))

(def Handler
  "A tool handler or a wrapped one: a 1-arity fn of the args map."
  [:=> [:cat :any] :any])

(def ToolName
  "The tool a wrapper serves; nil for a bare handler under test."
  [:maybe :string])

(defn wrap-handler-retry
  "Wrap handler with retry logic for hot-reload resilience."
  [handler]
  (fn [args]
    (loop [attempt 1]
      (let [result (try
                     {:ok (handler args)}
                     (catch Exception e
                       (if (and (hot-reload-error? e)
                                (< attempt hot-reload-max-retries))
                         {:retry e}
                         (throw e))))]
        (if (:retry result)
          (do
            (log/warn "Hot-reload retry" {:attempt attempt
                                          :max hot-reload-max-retries
                                          :error (ex-message (:retry result))})
            (Thread/sleep hot-reload-retry-delay-ms)
            (recur (inc attempt)))
          (:ok result))))))

(m/=> wrap-handler-retry [:=> [:cat Handler] Handler])


;; =============================================================================
;; NATS Notification
;; =============================================================================

(def ^:private mutating-tool-commands
  {"memory"  #{"add" "feedback"}
   "kanban"  #{"move" "create"}
   "session" #{"start" "stop"}
   "agent"   #{"spawn" "kill"}})

(defn- mutating-call? [tool-name args]
  (when-let [commands (get mutating-tool-commands tool-name)]
    (contains? commands (some-> (:command args) name))))

(defn wrap-handler-nats-notify
  "Post-execution hook: publishes NATS notification for mutating operations."
  [handler tool-name]
  (fn [args]
    (let [result (handler args)]
      (when (mutating-call? tool-name args)
        (try
          (when-let [publish! (requiring-resolve 'hive-mcp.nats.bridge/publish-tool-notification!)]
            (publish! {:tool-name  (keyword (str tool-name "-" (name (:command args))))
                       :event-type :tool-executed
                       :timestamp  (System/currentTimeMillis)
                       :data       {:args-summary (select-keys args [:command :name :task-id :id :type])}}))
          (catch Exception e
            (log/debug "[NATS] Tool notification failed for" tool-name (.getMessage e)))))
      result)))

(m/=> wrap-handler-nats-notify [:=> [:cat Handler ToolName] Handler])


;; =============================================================================
;; Context Binding
;; =============================================================================

(def ^:dynamic *identity-get-slave*
  "1-arg port: registry row for a slave id, used to tell a spawned agent's
   caller id from a coordinator's. Rebound by tests."
  grant/registry-get-slave)

(defn- embed-lane-var
  "The memory domain's embedding admission lane var
   (hive-mcp.embeddings.shared-gate/*lane*), resolved at call time: the kernel
   does not require the memory domain. Nil when that namespace is absent."
  []
  (try (requiring-resolve 'hive-mcp.embeddings.shared-gate/*lane*)
       (catch Throwable _ nil)))

(defn in-embed-lane
  "Wrap thunk `f` so it runs with the embedding lane bound to `lane`. Without
   a memory domain, `f` is returned unchanged."
  [lane f]
  (if-let [v (embed-lane-var)]
    (fn [] (with-bindings {v lane} (f)))
    f))

(m/=> in-embed-lane [:=> [:cat :keyword [:=> [:cat] :any]] [:=> [:cat] :any]])

(defn wrap-handler-context
  "Bind request context: keywordize args, verify identity, resolve ids, bind
   ctx vars.

   `:caller-id` is the transport's `:_caller_id`: the MCP session making the
   call, shared by its subagents, distinct per session even when every
   session's agent-id is nil.

   Identity: the spawn credential the transport carries as
   `:_caller_credential` is verified here (hive-mcp.agent.identity) and
   STRIPPED from the args before anything else sees them, so no handler,
   log line, transcript or piggyback ever holds it. The result is bound as
   `:identity` in the request context. Under :enforce a request that claims a
   registered spawned agent's id without a valid credential is refused here;
   under :observe (the default) it is logged and counted and proceeds.

   Every tool call runs in the :interactive embedding lane
   (embeddings.shared-gate/*lane*, via `in-embed-lane`), so a caller waiting
   on an embedding is admitted ahead of queued background (:batch) work such
   as an ingest. Work the call hands to the background rebinds :batch
   (wrap-handler-async, shared-gate/batch-fn)."
  [handler]
  (fn [args]
    (let [raw-args (walk/keywordize-keys args)
          {args :args who :identity decision :decision}
          (agent-identity/verify-request! raw-args "mcp" *identity-get-slave*)]
      (if-not (:allow? decision true)
        [{:type "text" :isError true
          :text (agent-identity/refusal-text decision (:caller-id who))}]
        (binding [ctx/*request-cache* (atom {})]
          (let [agent-id (id/extract-agent-id args nil)
                project-id (id/extract-project-id args)
                directory (id/extract-directory args)]
            (crystal/record-session-start! agent-id)
            (ctx/with-request-context {:agent-id agent-id
                                       :caller-id (:_caller_id args)
                                       :identity who
                                       :project-id project-id
                                       :directory directory}
              ((in-embed-lane :interactive #(handler args))))))))))

(m/=> wrap-handler-context [:=> [:cat Handler] Handler])


;; =============================================================================
;; Content Transform Wrappers
;; =============================================================================

(defn wrap-handler-normalize
  "Normalize handler result to content array."
  [handler]
  (fn [args]
    (id/normalize-content (handler args))))

(m/=> wrap-handler-normalize [:=> [:cat Handler] Handler])

(defn wrap-handler-compress
  "Apply response compression when `compact` param is present."
  [handler]
  (fn [args]
    (let [mode (compress/resolve-compress-mode args)
          content (handler args)]
      (if mode
        (compress/compress-content content mode)
        content))))

(m/=> wrap-handler-compress [:=> [:cat Handler] Handler])

(defn wrap-handler-response
  "Wrap handler result in {:content ...} response map."
  [handler]
  (fn [args]
    {:content (handler args)}))

(m/=> wrap-handler-response [:=> [:cat Handler] Handler])


;; =============================================================================
;; Async Execution
;; =============================================================================

(defn wrap-handler-default-async-for-commands
  "Inject :async true for commands in `commands` set when not explicitly set.
   An explicit :async is normalized to a boolean (`async-params/async-flag`),
   so a string \"false\" opts out instead of reading as truthy."
  [handler commands]
  (fn [{:keys [command] :as args}]
    (let [cmd-kw (when command (keyword command))
          flag   (async-params/async-flag (:async args))]
      (handler (cond
                 (some? flag)                   (assoc args :async flag)
                 (contains? commands cmd-kw)    (assoc args :async true)
                 :else                          args)))))

(m/=> wrap-handler-default-async-for-commands [:=> [:cat Handler [:maybe [:or [:set :any] [:sequential :any]]]] Handler])

(defn wrap-handler-async
  "Intercept async:true calls: return an ack, run the work on the async pool.

   The work goes to `async-tasks/submit!`, NOT to a bare `future`, so the
   handle is retained. A bare future's handle was discarded, which left a
   long-running call impossible to list, bound or stop by any API: the only
   remedy was killing the JVM, and on a shared coordinator that ends every
   other agent's work too.

   `:async-timeout-ms` bounds one call. It is consumed here alongside
   `:async` and never reaches the handler, for the same reason `:async` does
   not: it addresses THIS wrapper, not the tool.

   The failure arm catches Throwable, not Exception. The caller has already
   been acknowledged with `{:queued true}`, so the buffered result is the ONLY
   report this call will ever make. An AssertionError from a malli check, a
   StackOverflowError, an OutOfMemoryError: under `catch Exception` each of
   those enqueued nothing, and the caller waited forever on an ack for work
   that had already died. Errors are rethrown after being reported, so the
   pool still sees them and nothing here pretends to recover from an OOM.

   THE SUBMISSION IS JOURNALLED BEFORE THE ACK GOES OUT, so the promise is
   backed by something that outlives the process making it. The result is
   journalled too, by `enqueue-result!`, but that is not sufficient on its
   own: the case this tool is named for is a task accepted, acknowledged, and
   then killed BEFORE any result exists. Only the submission record turns
   that into an `:interrupted` result the caller is eventually handed,
   instead of silence against an ack it already holds.

   The ack shape is `async-ack/ack`: it names where the result arrives
   (`:result-via`) and says `:durable false` when the journal cannot be
   written.

   A SATURATED POOL IS ANSWERED, NOT ABSORBED. The pool refuses rather than
   handing the work back to the submitting thread, and the submitting thread
   here is the one serving the request. The refusal becomes an error the
   caller can act on, and the journalled submission is closed with the same
   verdict, so a record written a line earlier does not later resurface as an
   `:interrupted` result for work that never started.

   NOTE for addon authors: a top-level `:async` is consumed here and is gone
   before any handler runs, so a tool must not name a parameter `async` and
   expect to receive it. Spell such a flag `background` (see
   `hive-ingestor.addon.registry/dispatcher-shadowed-params`)."
  [handler tool-name]
  (fn [args]
    (if (true? (async-params/async-flag (:async args)))
      (let [task-id    (str "atask-" (random-uuid))
            caller-id  (or (:_caller_id args) "coordinator")
            timeout-ms (:async-timeout-ms args)
            durable?   (async-buf/record-submission! caller-id task-id tool-name)]
        (try
          (async-tasks/submit!
           {:task-id    task-id
            :tool       tool-name
            :caller-id  caller-id
            :timeout-ms timeout-ms
            ;; Background work: back to the :batch embedding lane.
            :f          (in-embed-lane :batch (fn []
                          (try
                            (let [clean-args (dissoc args :async :async-timeout-ms)
                                  result (handler clean-args)]
                              (async-buf/enqueue-result! caller-id
                                                         {:task-id task-id :tool tool-name
                                                          :status :completed :result result}))
                            (catch InterruptedException _
                              ;; Cancelled on purpose. Say so rather than
                              ;; reporting it as a failure of the work.
                              (log/info "async-result: task cancelled" {:task-id task-id})
                              (async-buf/enqueue-result! caller-id
                                                         {:task-id task-id :tool tool-name
                                                          :status :cancelled}))
                            (catch Exception e
                              (log/error e "async-result: background execution failed for task" task-id)
                              (async-buf/enqueue-result! caller-id
                                                         {:task-id task-id :tool tool-name
                                                          :status :error :error (.getMessage e)}))
                            (catch Throwable t
                              ;; Not an Exception, so nothing below would have
                              ;; reported it and the ack would never be answered.
                              (log/error t "async-result: background execution died for task" task-id)
                              (async-buf/enqueue-result! caller-id
                                                         {:task-id task-id :tool tool-name
                                                          :status :error
                                                          :error (str (.getName (class t)) ": " (.getMessage t))})
                              (throw t)))))})
          [{:type "text"
            :text (pr-str (async-ack/ack task-id tool-name timeout-ms durable?))}]
          (catch RejectedExecutionException _
            (log/warn "async call refused: pool saturated"
                      {:tool tool-name :task-id task-id :caller-id caller-id})
            (async-buf/enqueue-result! caller-id
                                       {:task-id task-id :tool tool-name
                                        :status :rejected
                                        :error "async pool saturated"})
            [{:type "text"
              :isError true
              :text (pr-str {:queued false :rejected true :task-id task-id
                             :tool tool-name
                             :error "async pool saturated: nothing was started, retry shortly"})}])))
      (handler args))))

(m/=> wrap-handler-async [:=> [:cat Handler ToolName] Handler])

(defn- guard-refusal
  "Render `decision` as the content array for a refused call."
  [tool-name decision]
  [{:type "text"
    :isError true
    :text (str "REFUSED by the hive guard — " tool-name "\n\n"
               (:guard/reason decision)
               (when-let [cites (seq (:guard/citations decision))]
                 (str "\n\nStated by: " (str/join ", " cites)))
               (when-let [rid (:guard/rule-id decision)]
                 (str "\nRule: " rid)))}])

(defn guard-raw-context
  "The raw context the `:guard/decide` seam is handed for one MCP call.

   Identity is offered from three sources so the guard can attribute the call
   to the strongest one it trusts:
     :verified-caller-id - the caller id whose spawn credential the host
                           verified (hive-mcp.context.request), else absent.
     :caller-id          - the transport-stamped `_caller_id`, as asserted.
     :agent-id           - the model-supplied argument, never verified.
   The two caller keys are ADDITIVE and present only when known: a guard that
   reads only :agent-id and :input sees exactly the map it saw before."
  [tool-name args]
  (let [verified (some-> (ctx/current-verified-caller-id) str not-empty)
        caller   (some-> (:_caller_id args) str not-empty)]
    (cond-> {:tool tool-name
             :input args
             :agent-id (id/extract-agent-id args nil)
             :cwd (id/extract-directory args)}
      caller   (assoc :caller-id caller)
      verified (assoc :verified-caller-id verified))))

(m/=> guard-raw-context [:=> [:cat ToolName [:maybe :map]]
                         [:map [:tool ToolName] [:input :any] [:agent-id :any] [:cwd :any]
                          [:caller-id {:optional true} :string]
                          [:verified-caller-id {:optional true} :string]]])

(defn wrap-handler-guard
  "Gate the call through the `:guard/decide` seam before the handler runs.

   The vendor-independent FLOOR: every client speaks MCP, so a rule enforced
   here holds under clients that have no hook system of their own.

   Soft seam — core does not depend on the guard addon. With no
   `:guard/decide` extension registered the call proceeds untouched.

   The seam is handed `guard-raw-context`: tool, input, cwd and every known
   identity source (verified caller id, transport caller id, agent-id arg).

     :deny — short-circuits; the refusal is returned and the tool never runs.
     :ask  — refused the same way. The guard addon resolves an ask for this
             floor itself (the operator's yes becomes an :allow), so an :ask
             reaching here was granted by nobody.
     :warn — the tool runs and the advisory is appended to its content.
     else  — proceeds.

   A seam that THROWS is logged and the call proceeds: a broken guard must not
   brick every tool on the server."
  [handler tool-name]
  (fn [args]
    (let [decision (when-let [decide (ext/get-extension gp/decide-ext-key)]
                     (try
                       (decide :mcp (guard-raw-context tool-name args))
                       (catch Exception e
                         (log/error e "guard: seam threw; call NOT judged"
                                    {:tool tool-name})
                         nil)))]
      (case (:guard/verdict decision)
        :deny (do (log/warn "guard: refused a tool call"
                            {:tool tool-name :rule (:guard/rule-id decision)})
                  (guard-refusal tool-name decision))
        :ask  (do (log/warn "guard: refused a tool call nobody granted"
                            {:tool tool-name :rule (:guard/rule-id decision)})
                  (guard-refusal tool-name
                                 (update decision :guard/reason str
                                         "\n\nThis call needs the operator's permission and nothing granted it.")))
        :warn (conj (vec (id/normalize-content (handler args)))
                    {:type "text"
                     :text (str "GUARD WARNING — " (:guard/reason decision)
                                (when-let [rid (:guard/rule-id decision)]
                                  (str " [" rid "]")))})
        (handler args)))))

(m/=> wrap-handler-guard [:=> [:cat Handler ToolName] Handler])


;; =============================================================================
;; Piggyback Draining
;; =============================================================================

(defn- resolve-child-project-ids
  "Resolve descendant project-ids using the project hierarchy tree
   (.hive-project.edn), NOT the Datascript slave registry. Tree-based
   resolution survives ling cleanup — shouts from terminated child-project
   lings remain visible to the parent coordinator."
  [project-id]
  (when project-id
    (try
      (when-let [desc-fn (requiring-resolve 'hive-mcp.project.scope/descendant-scopes)]
        (let [child-pids (desc-fn project-id)]
          (when (seq child-pids)
            (log/debug "Piggyback: including descendant project-ids for" project-id ":" child-pids)
            (set child-pids))))
      (catch Exception e
        (log/debug "Piggyback: descendant project-id resolution failed (non-fatal):" (.getMessage e))
        nil))))

(defn- get-piggyback-messages
  ([agent-id project-id] (get-piggyback-messages agent-id project-id nil))
  ([agent-id project-id session-id]
   (require 'hive-mcp.channel.piggyback)
   (let [child-pids (resolve-child-project-ids project-id)]
     ((resolve 'hive-mcp.channel.piggyback/get-messages)
      agent-id
      :project-id project-id
      :additional-project-ids child-pids
      :session-id session-id))))

(defn- drain-memory-piggyback
  ([caller-id] (drain-memory-piggyback caller-id nil))
  ([caller-id ctx]
   (require 'hive-mcp.channel.memory-piggyback)
   ((resolve 'hive-mcp.channel.memory-piggyback/drain!) caller-id ctx)))

(defonce ^{:private true :doc "Provider blocks already drained but not yet fitted to a response, per caller."}
  pending-catchup (atom {}))

(defn- drain-catchup-piggyback
  "Drain the `:cu/piggyback-drain` enrichment channel for this response.

   The provider is an addon, so its arity is not something the host may assume.
   A ctx-aware provider gets `request-ctx` and returns only a budgeted, cue-
   ordered slice, leaving the rest buffered for a later call; a provider that
   still takes the caller alone is called that way and drains one-shot, as
   before. Neither shape is allowed to fail the response.

   Provider-selected blocks are retained locally until their complete delimited
   wire representation fits; a provider that advances on an oversized first
   candidate cannot cause either budget overflow or lost work."
  [caller-id request-ctx]
  (let [budget (:char-budget request-ctx Long/MAX_VALUE)]
    (when (pos? budget)
      (try
        (let [prior (get @pending-catchup caller-id)
              incoming (when (and (empty? prior)
                                  (ext/get-extension :cu/piggyback-drain))
                         (let [drain-fn (ext/get-extension :cu/piggyback-drain)]
                           (try (drain-fn caller-id request-ctx)
                                (catch clojure.lang.ArityException _
                                  (drain-fn caller-id)))))
              all (or prior (seq incoming))
              [deliver defer]
              (loop [pairs (seq all) out {} used 0]
                (if-let [[tag body] (first pairs)]
                  (let [text (if (string? body) body (pr-str body))
                        wire (str "\n\n---" (str/upper-case (name tag)) "---\n"
                                  text "\n---/" (str/upper-case (name tag)) "---")
                        size (count wire)]
                    (if (<= (+ used size) budget)
                      (recur (next pairs) (assoc out tag body) (+ used size))
                      [out (into {} pairs)]))
                  [out nil]))]
          (swap! pending-catchup (fn [m]
                                   (if (seq defer) (assoc m caller-id defer)
                                       (dissoc m caller-id))))
          (when (seq deliver) deliver))
        (catch Exception e
          (log/debug "catchup-piggyback drain failed:" (.getMessage e))
          nil)))))

(def response-piggyback-char-budget
  "Total response allowance for async results, memory, blocks and catchup.
   Hivemind uses its cursor/audience/digest rather than this budget."
  32000)

(defn spend-piggyback-budget
  "Pure accounting for one channel. Returns the permitted allowance and the
   remaining response chars after the actual rendered cost. Channel order is
   the caller's order; unused allowance rolls forward to the next channel."
  [remaining channel-cap rendered-chars]
  (let [allowance (max 0 (min remaining channel-cap))]
    {:allowance allowance
     :remaining (max 0 (- remaining (max 0 rendered-chars)))}))

(m/=> spend-piggyback-budget [:=> [:cat :int :int :int]
                              [:map [:allowance [:int {:min 0}]] [:remaining [:int {:min 0}]]]])

(defn- piggyback-content-chars
  "Total text chars in MCP content, for measuring a channel's actual wire cost."
  [content]
  (reduce + 0 (map (comp count :text) (filter #(= "text" (:type %)) (id/normalize-content content)))))

;; =============================================================================
;; Grant Gate
;; =============================================================================

(def ^:dynamic *grant-get-slave*
  "1-arg port: registry row for a slave id, the lineage the grant gate walks.
   Resolved late (the swarm registry is wired after the routes load); a
   missing registry reads as no row, so nothing is gated. Rebound by tests."
  grant/registry-get-slave)

(def ^:dynamic *grant-domain*
  "0-arg port: the grant domain (hive-mcp.agent.grant/domain). Rebound by tests."
  grant/domain)

(defn wrap-handler-grant
  "Refuse a call the caller's effective grant does not permit.

   The caller is the transport's `:_caller_id` (else the resolved agent id);
   its effective grant is the nearest one recorded on its lineage, walked
   through the swarm registry the same way enclaves walk it. No grant on the
   lineage, which is every session today, means unrestricted and the call
   proceeds untouched. The refusal names the missing capability and the
   `hivemind ask` path. The caller id is self-asserted until spawn
   credentials are verified, so this stops mistakes, not a hostile child."
  [handler tool-name]
  (fn [args]
    (let [caller (some-> (or (ctx/current-verified-caller-id) (:_caller_id args)) str not-empty)
          get-slave *grant-get-slave*
          no (when (and caller (grant/recorded-grant get-slave caller))
               (grant/call-refusal (*grant-domain*) get-slave caller tool-name (:command args)))]
      (if no
        (do (log/warn "grant: refused a tool call" {:tool tool-name :command (:command args) :caller caller})
            [{:type "text" :isError true :text no}])
        (handler args)))))

(m/=> wrap-handler-grant [:=> [:cat Handler ToolName] Handler])

(defn wrap-handler-piggybacks
  "Unified piggyback wrapper: drains all 4 channels in a single pass.
   Task cues harvested from the request args steer the MEMORY drain that rides
   this response; they are empty unless task-signal/enabled?. The cues then feed
   `activation/drain-ctx`, which an activation provider may extend with pinned
   entry ids; absent a provider the ctx is the cues alone.

   The HIVEMIND read is keyed two ways: the reader id (caller plus project)
   owns the project cursor, and the caller alone owns the global cursor, so
   a session that touches several repos reads each global shout once."
  ([handler] (wrap-handler-piggybacks handler nil))
  ([handler tool-name]
   (fn [args]
     (let [task-tokens (task-signal/cues tool-name args)
           content (handler args)
           caller-id (or (:_caller_id args) "coordinator")
           request-ctx {:tool-name tool-name :cues task-tokens :caller-id caller-id}
           async-cap (:allowance (spend-piggyback-budget response-piggyback-char-budget
                                                    async-buf/drain-char-budget 0))
           async-drain (async-buf/drain! caller-id (max 0 (- async-cap 160)))
           async-content (if async-drain
                           (id/wrap-delimited-block content "TOOLRESULT" (pr-str async-drain))
                           content)
           remaining-async (:remaining (spend-piggyback-budget response-piggyback-char-budget
                                                async-buf/drain-char-budget
                                                (- (piggyback-content-chars async-content)
                                                   (piggyback-content-chars content))))
           act-ctx (activation/drain-ctx request-ctx)
           memory-cap (:allowance (spend-piggyback-budget remaining-async 12000 0))
           memory-drain (when (> memory-cap 200)
                          (drain-memory-piggyback caller-id
                            (assoc act-ctx :char-budget (- memory-cap 200))))
           memory-content (if memory-drain
                            (id/wrap-memory-piggyback-content async-content memory-drain)
                            async-content)
           remaining-memory (:remaining (spend-piggyback-budget remaining-async 12000
                                                (- (piggyback-content-chars memory-content)
                                                   (piggyback-content-chars async-content))))
           ;; Blocks are an OPEN set: whatever addons registered under
           ;; :block/*, rendered by tag. The host names none of them, so a new
           ;; block is an addon plus a config entry rather than a commit here.
           blocks-cap (:allowance (spend-piggyback-budget remaining-memory blocks/max-response-chars 0))
           extra-blocks (blocks/render request-ctx blocks-cap)
           blocks-content (reduce (fn [acc [tag body]] (id/wrap-delimited-block acc tag body))
                                  memory-content extra-blocks)
           remaining-blocks (:remaining (spend-piggyback-budget remaining-memory
                                               blocks/max-response-chars
                                               (- (piggyback-content-chars blocks-content)
                                                  (piggyback-content-chars memory-content))))
           catchup-cap (:allowance (spend-piggyback-budget remaining-blocks 4000 0))
           catchup-blocks (when (> catchup-cap 100)
                            (drain-catchup-piggyback caller-id
                              (assoc request-ctx :char-budget (- catchup-cap 100))))
           catchup-content (reduce-kv
                            (fn [acc tag body]
                              (id/wrap-delimited-block acc
                                (str/upper-case (name tag))
                                (if (string? body) body (pr-str body))))
                            blocks-content (or catchup-blocks {}))
           caller (id/extract-caller-identity args)
           scope (id/extract-project-scope args)
           hm-agent-id (ctx-id/make-piggyback-agent-id caller scope)
           hm-project-id (ctx-id/project-scope-string scope)
           hivemind-msgs (get-piggyback-messages hm-agent-id hm-project-id
                                                 (ctx-id/caller-id-string caller))]
       (id/wrap-piggyback catchup-content hivemind-msgs)))))

(m/=> wrap-handler-piggybacks [:function [:=> [:cat Handler] Handler] [:=> [:cat Handler ToolName] Handler]])


;; =============================================================================
;; Middleware Chain Composition
;; =============================================================================

(defn build-middleware-chain
  "Compose the 9-layer middleware chain around a raw tool handler.
   Testable in isolation — no tool definition machinery needed.

   The grant gate sits just outside the guard: a call the caller's grant does
   not permit is refused before any rule is judged or any future is queued.

   The guard sits INSIDE context (so it judges keywordized args with identity
   already resolved) and OUTSIDE async (so a denied call is never spawned as a
   future)."
  [handler tool-name default-async-commands]
  (-> handler
      (wrap-handler-nats-notify tool-name)
      wrap-handler-retry
      (wrap-handler-async tool-name)
      (cond-> (seq default-async-commands)
        (wrap-handler-default-async-for-commands default-async-commands))
      (wrap-handler-guard tool-name)
      (wrap-handler-grant tool-name)
      wrap-handler-normalize
      wrap-handler-compress
      (wrap-handler-piggybacks tool-name)
      wrap-handler-context
      wrap-handler-response))

(m/=> build-middleware-chain [:=> [:cat Handler ToolName [:maybe [:or [:set :any] [:sequential :any]]]] Handler])

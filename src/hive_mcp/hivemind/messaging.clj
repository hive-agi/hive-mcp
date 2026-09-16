(ns hive-mcp.hivemind.messaging
  "Hivemind messaging — shout, ask, respond, and piggyback registration.

   M1 (protocol-first): Shouts publish via IEventBackbone (NATS, Redis, etc.).
   Fallback fanout uses IDeliveryChannel registry — no hardcoded transports.
   Local state (atom, DataScript) still updated synchronously for consistency."

  (:require [clojure.core.async :as async :refer [>!! chan timeout alt!!]]
            [hive-dsl.bounded-atom :refer [bput! bget]]
            [hive-mcp.channel.core :as channel]
            [hive-mcp.channel.piggyback :as piggyback]
            [hive-mcp.hivemind.event-registry :as event-registry]
            [hive-mcp.hivemind.state :as state]
            [hive-mcp.protocols.delivery-channel :as dc]
            [hive-mcp.protocols.event-backbone :as eb]
            [hive-mcp.protocols.vessel :as vessel]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.swarm.datascript.registry :as registry]
            [hive-mcp.swarm.protocol :as proto]
            [hive-mcp.tools.memory.scope :as mem-scope]
            [taoensso.timbre :as log]
            [hive-mcp.channel.a2a :as a2a]
            [hive-mcp.channel.broadcast-policy :as bpolicy]
            [hive-mcp.channel.payload-ref :as pref]
            [hive-mcp.channel.context-store :as ctx-store]
            [hive-mcp.channel.audience :as audience]
            [hive-mcp.channel.broadcast-ledger :as bledger])
  (:import [java.lang Exception]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- all-hivemind-messages
  "Return all hivemind messages for piggyback module.
   Projection mirrors buffer-backbone-event! normalization so dual-path dedup
   keys remain shape-aligned (both paths carry :task and :shout-id when set).
   :parent-id / :broadcast? ride along — hive-mcp.channel.audience routes on
   them, and a projection that dropped them would silently broadcast. :to and
   :context-id ride along for the same reason one level up: :to is the DIRECTED
   address, so a projection that dropped it would turn a message meant for one
   peer back into a message for the whole audience.

   :deliberate? rides along because the digest reads it, and this projection is
   the LOCAL path — the one in use whenever the backbone is disconnected, which
   is the fallback and every cold run. It was missing here while the backbone
   path carried it, so the 2026-09-07 fix (a wave member's own shout folded
   into its turn telemetry) held on one of the two paths and not the other.
   Measured again 2026-09-15: two directed turns from the same peer collapsed
   into one row, so the recipient of a conversation lost a turn of it."
  []
  (mapcat (fn [[_agent-id entry]]
            (let [{:keys [messages]} (:data entry)]
              (for [{:keys [event-type message task timestamp project-id shout-id
                            parent-id broadcast? deliberate? to context-id ref]} messages]
                (cond-> {:agent-id _agent-id
                         :event-type event-type
                         :message (or message task "")
                         :timestamp timestamp
                         :project-id (or project-id "global")}
                  shout-id (assoc :shout-id shout-id)
                  task (assoc :task task)
                  parent-id (assoc :parent-id parent-id)
                  broadcast? (assoc :broadcast? true)
                  to (assoc :to to)
                  context-id (assoc :context-id context-id)
                  ref (assoc :ref ref)
                  deliberate? (assoc :deliberate? true)))))
          @(:atom state/agent-registry)))

(piggyback/register-message-source! all-hivemind-messages)

(defn- event-type->slave-status
  "Map hivemind event type to valid DataScript slave status.
   Derives from event-registry — no hardcoded case statement."
  [event-type]
  (event-registry/slave-status event-type))

(def ^:const default-shout-message-cap
  "Fallback cap when config is unloaded or missing :hivemind/:shout-message-cap.
   One bad shout fans out across (per-agent ring × backbone × subscribers), so
   bound it aggressively. Override via config path [:hivemind :shout-message-cap]."
  2048)

(def ^:const ^:private ellipsis "…")

(defn- resolve-shout-cap
  "Pull :shout-message-cap from loaded config, fall back to default.
   Lazy requiring-resolve avoids circular dep at hivemind bootstrap."
  []
  (or (try
        (when-let [f (requiring-resolve 'hive-mcp.config.core/get-in-config)]
          (f [:hivemind :shout-message-cap]))
        (catch Exception _ nil))
      default-shout-message-cap))

(defn cap-message
  "Cap a shout payload string at `cap` characters. Pure helper.

   Behavior:
   - nil             → nil
   - \"\"              → \"\" (empty passes through)
   - (count s) ≤ cap → s  (under-cap passes through verbatim)
   - else            → (subs s 0 (- cap 3)) + \"…\"  (ellipsis suffix)

   Non-strings are `pr-str`'d first so accidental coll/map payloads are still
   bounded. Invariant: (count (cap-message s cap)) ≤ cap for any input when
   cap ≥ 3."
  ([v] (cap-message v (resolve-shout-cap)))
  ([v cap]
   (cond
     (nil? v) nil
     (and (string? v) (zero? (count v))) v
     :else
     (let [s (if (string? v) v (pr-str v))]
       (if (<= (count s) cap)
         s
         (let [head-n (max 0 (- cap 3))]
           (str (subs s 0 head-n) ellipsis)))))))

(defn- elide-message
  "Bound a shout's message to the configured cap, keeping what does not fit.

   `cap-message` truncates and the tail is gone. This stores the full body in
   the context store and puts its id in the text, so the reader pays for the
   head and can fetch the rest — same cost on the wire, nothing lost. Non-strings
   are pr-str'd first, as cap-message does; nil stays nil.

   -> {:text <what travels> :ref <ctx-id>} | {:text nil}"
  [m]
  (if (nil? m)
    {:text nil}
    (pref/elide! (if (string? m) m (pr-str m))
                 (resolve-shout-cap)
                 (fn [full] (ctx-store/context-put! full :tags #{"shout-payload"})))))

(defn- publish-shout-to-backbone!
  "Publish shout via IEventBackbone. Subscribers handle fanout.
   Uses requiring-resolve for bridge to avoid circular dep."
  [payload]
  (try
    (when-let [publish-fn (requiring-resolve 'hive-mcp.nats.bridge/publish-shout!)]
      (publish-fn payload))
    (catch Exception e
      (log/debug "[Backbone] Shout publish failed (non-fatal):" (.getMessage e)))))

(defn- fanout-shout-direct!
  "Direct fanout via IDeliveryChannel registry when backbone is unavailable.
   Protocol-mediated — no hardcoded transport calls."
  [payload]
  (dc/fanout! payload))

(defn- blank-payload-value?
  "True iff `v` carries no signal: nil, empty string, or empty coll."
  [v]
  (cond
    (nil? v) true
    (string? v) (zero? (count v))
    (coll? v) (empty? v)
    :else false))

(defn empty-shout?
  "Predicate: would this shout carry a zero-information payload?

   A shout is considered empty when *all* of the following hold:
     1. `:task` is missing/nil/blank-string/empty-coll
     2. `:message` is missing/nil/blank-string/empty-coll
     3. The remainder of `data` (after stripping :task :message :directory
        :project-id) has no entries with meaningful values.

   Such shouts get rendered as `[] ()`-shaped no-ops in piggyback HIVEMIND
   blocks during high-throughput batch ops (kanban hygiene, wave dispatch
   side-effects, FSM phase shouts that race past payload assembly).

   `data` may be anything callers pass through — non-map values are treated
   as opaque (so non-nil, non-blank, non-empty data → not empty)."
  [data]
  (cond
    (nil? data) true
    (not (map? data)) (blank-payload-value? data)
    :else
    (let [residual (dissoc data :task :message :directory :project-id)]
      (and (blank-payload-value? (:task data))
           (blank-payload-value? (:message data))
           (every? blank-payload-value? (vals residual))))))

(defn- shout!*
  "Internal shout implementation — assumes payload has been validated
   non-empty by `shout!`. Returns the routing verdict map."
  [agent-id event-type data]
  (let [now (System/currentTimeMillis)
        shout-id (str (random-uuid))
        resolved-slave (queries/get-slave-by-name-or-id agent-id)
        resolved-slave-id (or (:slave/id resolved-slave) agent-id)
        explicit-project-id (:project-id data)
        directory (:directory data)
        ;; IVessel resolution: query all registered vessels for agent context.
        ;; Vessel delegates to DataScript (slave/cwd, slave/project-id) — the
        ;; formal answer to the project-id coupling bug (vessel owns context).
        vessel-ctx (vessel/resolve-agent-context resolved-slave-id)
        ;; Priority: explicit > vessel > directory > global
        project-id (or explicit-project-id
                       (:project-id vessel-ctx)
                       (when directory (mem-scope/get-current-project-id directory))
                       "global")
        ;; Spawner identity — the reader whose context this shout enters when
        ;; nothing more specific addresses it. hive-mcp.channel.audience routes
        ;; on it; absent it, the shout is root-level and reaches coordinator
        ;; readers only. Explicit wins so a caller with no DataScript row can
        ;; still address its reader.
        parent-id (or (:parent-id data) (:slave/parent resolved-slave))
        ;; Routing is DECIDED, not taken from whoever set the flag. A broadcast
        ;; without an admissible argument is downgraded to its ordinary route
        ;; rather than dropped — hive-mcp.channel.broadcast-policy.
        ;; Whether this sender is METERED is decided here, not in the policy:
        ;; identity lives at the call site. A coordinator lane addressing its
        ;; own swarm is above the readers rather than beside them, and its
        ;; directives are how a wave is driven, so metering it would let a busy
        ;; wave silence its own scheduler. Peers are metered, because peers are
        ;; what the budget exists to keep from repeating an admissible reason.
        metered? (not (audience/coordinator-reader? agent-id))
        routed (bpolicy/apply-policy
                (select-keys data [:broadcast? :broadcast-reason :to])
                (when metered?
                  {:recent-broadcasts (bledger/spent-recently project-id now)}))
        to (some-> (:to routed) str)
        broadcast? (boolean (:broadcast? routed))
        broadcast-reason (:broadcast-reason routed)
        broadcast-refused (:broadcast-refused routed)
        ;; A `to` nobody answers to is the ONE way directed addressing can lose
        ;; a message outright: no reader matches it, so it reaches nobody, where
        ;; the old spawner route would at least have reached the coordinator.
        ;;
        ;; Routing is deliberately NOT changed on this evidence. The roster is
        ;; authoritative about slaves and silent about coordinator lanes and
        ;; peers registered elsewhere, so downgrading on it would break valid
        ;; delivery in order to catch a typo. The verdict reports it instead, in
        ;; the same tool result the sender reads, which is feedback it can act
        ;; on in its very next turn.
        to-unresolved? (boolean
                        (when to
                          (and (nil? (queries/get-slave-by-name-or-id to))
                               (not (audience/coordinator-reader? to)))))
        ;; A2A contextId. A directed message with no context gets one, so the
        ;; two peers have a handle to continue the exchange under and a reader
        ;; can fetch the whole exchange by id. Telemetry and broadcasts get
        ;; none: nothing will ever be threaded to them, so the id would be
        ;; tokens for nothing.
        context-id (or (some-> (:context-id data) str)
                       (when to (a2a/new-context-id)))
        ;; A shout the agent CHOSE to make (the hivemind tool path), as opposed
        ;; to runtime telemetry emitted on its behalf. Carried on the message
        ;; so the piggyback digest never collapses it away.
        deliberate? (boolean (:deliberate? data))
        ;; Bound what one shout costs a reader. One bad shout can otherwise
        ;; pollute the per-agent 10-message ring AND every backbone subscriber.
        ;; The message keeps its tail under a context id; the task, being a
        ;; label rather than a body, is still truncated.
        elided (elide-message (:message data))
        capped-message (:text elided)
        payload-ref (:ref elided)
        capped-task (cap-message (:task data))
        payload-data (dissoc data :task :message :directory :project-id
                             :parent-id :broadcast? :broadcast-reason
                             :deliberate? :to :context-id)
        message (cond-> {:event-type event-type
                         :timestamp now
                         :project-id project-id
                         :shout-id shout-id
                         :data payload-data}
                  capped-task (assoc :task capped-task)
                  capped-message (assoc :message capped-message)
                  parent-id (assoc :parent-id parent-id)
                  broadcast? (assoc :broadcast? true)
                  broadcast-reason (assoc :broadcast-reason broadcast-reason)
                  to (assoc :to to)
                  context-id (assoc :context-id context-id)
                  payload-ref (assoc :ref payload-ref)
                  deliberate? (assoc :deliberate? true))
        ;; Backbone payload — flat, self-contained, no internal references
        ;; shout-id enables cross-path dedup (atom + backbone deliver same shout)
        backbone-payload (cond-> {:agent-id agent-id
                                  :event-type event-type
                                  :timestamp now
                                  :project-id project-id
                                  :shout-id shout-id
                                  :message capped-message
                                  :task capped-task
                                  :data payload-data}
                           parent-id (assoc :parent-id parent-id)
                           broadcast? (assoc :broadcast? true)
                           broadcast-reason (assoc :broadcast-reason broadcast-reason)
                           to (assoc :to to)
                           context-id (assoc :context-id context-id)
                           payload-ref (assoc :ref payload-ref)
                           deliberate? (assoc :deliberate? true))]
    ;; 0. Charge the budget — only for a broadcast the policy ADMITTED. A
    ;; refused one costs the readers nothing, so charging for it would let a
    ;; rejected request push a later legitimate one over the edge.
    (when (and metered? broadcast?)
      (bledger/record-broadcast! project-id now))
    ;; 1. Local state — always (bounded-atom for piggyback reads)
    (let [current (or (bget state/agent-registry agent-id) {:messages [] :last-seen nil})
          messages (or (:messages current) [])
          new-messages (vec (take-last 10 (conj messages message)))]
      (bput! state/agent-registry agent-id
             {:messages new-messages
              :last-seen now}))
    ;; 2. DataScript slave status — always
    (when resolved-slave
      (proto/update-slave! registry/default-registry resolved-slave-id
                           {:slave/status (event-type->slave-status event-type)}))
    ;; 3. Backbone publish OR direct fanout (protocol-mediated)
    (let [backbone (eb/get-backbone)]
      (if (eb/connected? backbone)
        (publish-shout-to-backbone! backbone-payload)
        (fanout-shout-direct! backbone-payload)))
    ;; 4. Log
    (log/info "Hivemind shout:" agent-id event-type "project:" project-id
              (cond to (str "-> " to) broadcast? "-> ALL" :else ""))
    (when to-unresolved?
      (log/warn "Hivemind shout addressed to an unknown recipient:" to
                "- no reader may match it"))
    (cond-> {:delivered true
             :shout-id shout-id
             :routing (cond to :direct broadcast? :broadcast :else :spawner)}
      context-id (assoc :context-id context-id)
      to (assoc :to to)
      payload-ref (assoc :ref payload-ref)
      broadcast-refused (assoc :broadcast-refused broadcast-refused)
      to-unresolved? (assoc :to-unresolved to))))

(defn shout-with-verdict!
  "Send a hivemind message and report HOW it was routed.

   `data` may carry, beyond the payload keys:
     :to               address ONE peer. The message reaches that agent and
                       nobody else — not the coordinator, not the sender's
                       spawner. This is the cheap path and the default choice
                       for anything that concerns one peer.
     :context-id       continue an existing A2A conversation. A directed
                       message without one starts a conversation and the
                       verdict carries the new id.
     :broadcast?       ask to reach every reader.
     :broadcast-reason the argument for it, from
                       hive-mcp.channel.broadcast-policy/admissible-reasons.
                       Without an admissible one the broadcast is REFUSED and
                       the message is delivered by its ordinary route instead;
                       :broadcast-refused on the verdict says why.

   A broadcast passes two gates, not one. An admissible reason is not a
   standing permission to repeat it: a peer sender is metered per project over
   a sliding window (hive-mcp.channel.broadcast-ledger), and past the budget
   the verdict comes back :broadcast-refused :budget-exhausted, downgraded to
   its ordinary route like any other refusal. A :halt is exempt from the
   volume gate; a coordinator lane is not metered at all.

   -> {:delivered true :routing :direct|:broadcast|:spawner :shout-id s
       :context-id s? :to s? :ref s? :broadcast-refused kw?}
   -> {:delivered false :reason :empty-payload} when the shout carries no signal."
  [agent-id event-type data]
  (if (empty-shout? data)
    (do
      (log/debug "Hivemind shout suppressed (empty payload):"
                 agent-id event-type)
      {:delivered false :reason :empty-payload})
    (shout!* agent-id event-type data)))

(defn shout!
  "Broadcast a message to the hivemind coordinator.

   M1 Architecture (protocol-first):
   - Local state (atom + DataScript) updated synchronously
   - If backbone connected: single publish → backbone subscribers handle fanout
   - If backbone disconnected: direct fanout via IDeliveryChannel registry
   - Domain events (:ling/completed) are NOT dispatched here — callers
     that need domain side-effects dispatch them explicitly. This avoids
     a feedback loop: shout! → :ling/completed handler → :shout effect → shout!

   Empty-payload guard (kanban-hygiene-2026-04-27):
   - If the shout carries no task/message/data signal (`empty-shout?` true),
     it is suppressed with a debug log and `false` is returned. This prevents
     the `[] ()` no-op shouts observed during bulk-close cascades where
     side-effect chains race past payload assembly. Callers wanting telemetry
     for the no-op transition should pass at minimum a non-blank :message.

   Returns true/false. A caller that needs to know HOW the message was routed
   — whether a broadcast was admitted, what contextId a directed message
   opened — calls `shout-with-verdict!` instead; this stays boolean because
   every existing caller treats it as one."
  [agent-id event-type data]
  (boolean (:delivered (shout-with-verdict! agent-id event-type data))))

(defn ask!
  "Request a decision from the human coordinator, blocking until response or timeout."
  [agent-id question options & {:keys [timeout-ms] :or {timeout-ms 300000}}]
  (let [ask-id (str (random-uuid))
        response-chan (chan 1)
        ask-event {:type :hivemind-ask
                   :ask-id ask-id
                   :agent-id agent-id
                   :question question
                   :options options
                   :timestamp (System/currentTimeMillis)}]
    (swap! state/pending-asks assoc ask-id {:question question
                                            :options options
                                            :agent-id agent-id
                                            :response-chan response-chan})
    (channel/broadcast! ask-event)
    (log/info "Hivemind ask:" agent-id question)
    (let [result (alt!!
                   response-chan ([v] v)
                   (timeout timeout-ms) {:timeout true :ask-id ask-id})]
      (swap! state/pending-asks dissoc ask-id)
      result)))

(defn respond-ask!
  "Respond to a pending ask from an agent."
  [ask-id decision & {:keys [by] :or {by "human"}}]
  (if-let [{:keys [response-chan]} (get @state/pending-asks ask-id)]
    (do
      (>!! response-chan {:decision decision :by by :ask-id ask-id})
      (log/info "Hivemind response:" ask-id decision)
      true)
    (do
      (log/warn "No pending ask for id:" ask-id)
      false)))

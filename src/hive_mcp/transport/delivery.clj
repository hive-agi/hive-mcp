(ns hive-mcp.transport.delivery
  "The :delivery port of the transport chains: put a message in front of an
   agent through [:nats-inbox :tmux :emacs :hivemind-shout] (config
   [:transports :delivery :chain]).

   The target is resolved FIRST, from the id alone:
     :coordinator  `coordinator:<digits>`, a Claude Code session, never in the
                   swarm registry
     :ling         a row in the swarm registry
     nil           unknown: an addressing error, the chain is never tried

   Each adapter answers for the kinds it serves and says :inapplicable for the
   rest (the chain skips it without touching its health):
     :nats-inbox      coordinator with an armed inbox -> inbox send!;
                      headless ling whose inbox is armed AND whose loop's
                      consumer is attached -> the hive-mcp dispatch records
                      the task, then publishes it to hive.v1.inbox.<ling>
                      with the task id as message id. Otherwise
                      :inapplicable, naming the missing fact (bus down,
                      inbox unarmed, consumer detached), so the ling lane
                      falls through to :headless-ling.
     :headless-ling   headless ling -> the in-process loop inbox
     :tmux            coordinator -> hive-agent's coordinator-reply
     :emacs           ling -> the agent dispatch terminal path
     :hivemind-shout  any kind, last resort, a shout tagged to:<id>

   `deliver!` answers the chain Result: {:ok {:value v :via kw :resolved-by
   :primary|:fallback :skipped [..]}} or an error naming :via and :skipped.
   The ports below are dynamic so tests drive the chain with no transport."
  (:require [clojure.string :as str]
            [hive-dsl.result :as r]
            [hive-mcp.extensions.soft :as soft]
            [hive-mcp.nats.client :as client]
            [hive-mcp.nats.inbox :as inbox]
            [hive-mcp.nats.ling-inbox :as ling-inbox]
            [hive-mcp.transport.registry :as reg]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Ports
;; =============================================================================

(def ^:dynamic *connection*
  "0-arg port: the live NATS connection, or nil."
  (fn [] (client/current-connection)))

(defn- registry-row
  "The swarm registry row of `id`, resolved at call time. nil when the swarm
   registry is absent or unreadable, so the id reads as unknown."
  [id]
  (when-let [get-slave (soft/resolve-soft 'hive-mcp.swarm.datascript.queries/get-slave)]
    (try (get-slave id) (catch Exception _ nil))))

(def ^:dynamic *find-ling*
  "1-arg port: the swarm registry row of an agent id, or nil."
  registry-row)

(defn- default-tmux-reply
  [to message]
  (let [reply (soft/resolve-soft 'hive-agent.sixth-sense.api/coordinator-reply)
        port  (soft/resolve-soft 'hive-agent.sixth-sense.api/host-reply-port)]
    (if (and reply port)
      (reply (port) to message)
      {:ok false :error "hive-agent is not loaded, so there is no tmux route"})))

(def ^:dynamic *tmux-reply*
  "2-arg port (coordinator id, message): a map with :ok."
  default-tmux-reply)

(defn- default-ling-dispatch
  [params]
  (if-let [f (soft/resolve-soft 'hive-mcp.tools.agent.dispatch/dispatch-ling!)]
    (f params)
    (throw (ex-info "agent dispatch is not loaded" {}))))

(def ^:dynamic *ling-dispatch*
  "1-arg port (the dispatch params): {:task-id ..}, or throws."
  default-ling-dispatch)

(defn- default-shout
  [to message]
  (if-let [shout! (soft/resolve-soft 'hive-mcp.hivemind.messaging/shout-with-verdict!)]
    (shout! "delivery" :progress {:to to :message message :status-neutral? true})
    {:delivered false :reason :hivemind-absent}))

(def ^:dynamic *shout*
  "2-arg port (target id, message): a verdict map with :delivered."
  default-shout)

(defn- default-headless-backends
  []
  (if-let [f (soft/resolve-soft 'hive-mcp.agent.ling.headless-registry/registered-headless)]
    (set (f))
    #{}))

(def ^:dynamic *headless-backends*
  "0-arg port: the set of registered headless backend ids (keywords)."
  default-headless-backends)

(defn- default-io-model
  [mode]
  (when-let [f (soft/resolve-soft 'hive-mcp.agent.spawn-mode-registry/io-model)]
    (f mode)))

(def ^:dynamic *io-model*
  "1-arg port (spawn mode): the mode's registry :io-model, or nil."
  default-io-model)

(defn- default-ling-route
  [id]
  (binding [ling-inbox/*connection* *connection*]
    (ling-inbox/route id)))

(def ^:dynamic *ling-route*
  "1-arg port (ling id): the ling-lane NATS route view
   {:nats-up? :armed? :consumer-attached? :reachable?}
   (hive-mcp.nats.ling-inbox/route), read on this port's connection."
  default-ling-route)

;; =============================================================================
;; Target resolution
;; =============================================================================

(def ^:private coordinator-prefix "coordinator:")

(defn coordinator-id?
  "True for `coordinator:<digits>`, the id of a Claude Code session."
  [id]
  (boolean
   (and (string? id)
        (str/starts-with? id coordinator-prefix)
        (let [pid (subs id (count coordinator-prefix))]
          (and (seq pid) (every? #(Character/isDigit ^char %) pid))))))

(defn target-kind
  "What `id` addresses: :coordinator, :ling (a swarm registry row) or nil."
  [id]
  (cond
    (coordinator-id? id)  :coordinator
    (str/blank? (str id)) nil
    (*find-ling* id)      :ling
    :else                 nil))

(defn spawn-mode-of
  "The spawn mode keyword of ling `id` from its registry row, or nil."
  [id]
  (let [m (:ling/spawn-mode (*find-ling* id))]
    (cond (keyword? m) m
          (and (string? m) (not (str/blank? m))) (keyword m)
          :else nil)))

(defn headless-mode?
  "True when a ling of spawn mode `mode` runs a headless loop: the abstract
   :headless, a registered headless backend id (e.g. :hive-agent), or a mode
   whose registry io-model is a subprocess or API. A missing mode is the
   dispatch default :claude, a terminal."
  [mode]
  (boolean
   (and (keyword? mode)
        (or (= :headless mode)
            (contains? (*headless-backends*) mode)
            (contains? #{:stdin-stdout :api} (*io-model* mode))))))

(defn- ling-route
  "Which ling adapter serves `op`: :headless or :terminal."
  [{:keys [spawn-mode]}]
  (if (headless-mode? spawn-mode) :headless :terminal))

(m/=> coordinator-id? [:=> [:cat :any] :boolean])
(m/=> spawn-mode-of [:=> [:cat :any] [:maybe :keyword]])
(m/=> headless-mode? [:=> [:cat :any] :boolean])
(m/=> target-kind [:=> [:cat :any] [:maybe [:enum :ling :coordinator]]])

;; =============================================================================
;; Adapters
;; =============================================================================

(defn- inapplicable
  [adapter kind why]
  (r/err :delivery/inapplicable
         {:error/kind :inapplicable
          :message    (str (name adapter) " does not serve " (name kind) " targets: " why)}))

(defn- unavailable
  [category message]
  (r/err category {:error/kind :unavailable :message message}))

(defn- ling-nats-deliver-fn
  "The :deliver-fn a NATS ling dispatch hands Ling.dispatch!: once hive-mcp
   has recorded the task (task-id, dispatch record, :working), the task is
   published to the ling's inbox with the task-id as the message id, so a
   redelivery or a retried publish is the same message to the ling's
   consumer (it dedups by id) and to the stream (duplicate window)."
  [conn inbox-id from]
  (fn [{:keys [task-id task ling-seq]}]
    (let [receipt (inbox/send! conn (cond-> {:id (str task-id) :to inbox-id :body task
                                             :meta (cond-> {:task-id (str task-id)}
                                                     ;; send order across the ling's
                                                     ;; routes (agent.ling.inbox-seq)
                                                     ling-seq (assoc :ling-seq ling-seq))}
                                      from (assoc :from from)))]
      {:dispatched true :task-id task-id :via :nats-inbox
       :seq (:seq receipt) :duplicate? (:duplicate? receipt)})))

(defn- nats-send-ling
  "A ling over its NATS inbox: only when the ling-lane route is reachable
   (bus up, inbox armed, the loop's consumer attached). A ling the bus cannot
   reach is :inapplicable, naming which fact is missing, so the chain falls
   through to :headless-ling without marking the bus down."
  [{:keys [kind to params from] :as op}]
  (let [{:keys [nats-up? armed? consumer-attached?] :as rt} (*ling-route* to)]
    (cond
      (not= :headless (ling-route op))
      (inapplicable :nats-inbox kind (str "spawn mode " (pr-str (:spawn-mode op))
                                          " runs no headless loop"))

      (not nats-up?)
      (inapplicable :nats-inbox kind "NATS is down")

      (not armed?)
      (inapplicable :nats-inbox kind (str "NATS is up, but " to "'s inbox is not armed"))

      (not consumer-attached?)
      (inapplicable :nats-inbox kind (str "NATS is up and the inbox armed, but no consumer of "
                                          to " is attached"))

      :else
      (let [conn (*connection*)
            id   (inbox/safe-encode-inbox-id to)]
        (r/ok (assoc (*ling-dispatch* (assoc params :deliver-fn (ling-nats-deliver-fn conn id from)))
                     :route rt))))))

(defn- nats-send
  [{:keys [kind to message from] :as op}]
  (cond
    (= :ling kind) (nats-send-ling op)
    (not= :coordinator kind) (inapplicable :nats-inbox kind "only coordinators and lings have an inbox")
    :else
    (if-let [conn (*connection*)]
      (if-let [id (inbox/safe-encode-inbox-id to)]
        (if (:armed? (inbox/status conn id))
          (r/ok (select-keys (inbox/send! conn (cond-> {:to id :body message}
                                                 from (assoc :from from)))
                             [:id :to :subject :seq :duplicate?]))
          (inapplicable :nats-inbox kind (str "inbox " id " is not armed")))
        (r/err :delivery/bad-inbox-id
               {:error/kind :addressing :message (str "no inbox id for " (pr-str to))}))
      (unavailable :delivery/nats-down "no NATS connection"))))

(defn- tmux-say
  [{:keys [kind to message]}]
  (if (not= :coordinator kind)
    (inapplicable :tmux kind "only coordinators are reached through a pane")
    (let [res (*tmux-reply* to message)]
      (if (:ok res)
        (r/ok (select-keys res [:routed :delivered? :delivery :to]))
        (unavailable :delivery/tmux-failed (str (or (:error res) "tmux did not take the message")))))))

(defn- emacs-dispatch
  [{:keys [kind params] :as op}]
  (cond
    (not= :ling kind)
    (inapplicable :emacs kind "only lings take a terminal dispatch")

    (not= :terminal (ling-route op))
    (inapplicable :emacs kind (str "spawn mode " (pr-str (:spawn-mode op))
                                   " is not an Emacs terminal"))

    :else (r/ok (*ling-dispatch* params))))

(defn- headless-ling-dispatch
  "A headless ling: the same tools.agent.dispatch/dispatch-ling! path, which
   queues the task into the running loop's in-JVM inbox. No Emacs involved."
  [{:keys [kind params] :as op}]
  (cond
    (not= :ling kind)
    (inapplicable :headless-ling kind "only lings run a headless loop")

    (not= :headless (ling-route op))
    (inapplicable :headless-ling kind (str "spawn mode " (pr-str (:spawn-mode op))
                                           " is not headless"))

    :else (r/ok (*ling-dispatch* params))))

(defn- shout-last-resort
  [{:keys [to message]}]
  (let [v (*shout* to message)]
    (if (:delivered v)
      (r/ok (select-keys v [:shout-id :routing :to]))
      (unavailable :delivery/shout-failed (str "the shout was not delivered: " (pr-str (:reason v)))))))

(defn- probe-nats
  []
  (if (*connection*)
    (r/ok :connected)
    (unavailable :delivery/nats-down "no NATS connection")))

(defn- probe-tmux
  []
  (if (soft/resolve-soft 'hive-agent.sixth-sense.api/coordinator-reply)
    (r/ok :mounted)
    (unavailable :delivery/tmux-absent "hive-agent is not loaded")))

(defn- probe-emacs
  []
  (if-let [running? (soft/resolve-soft 'hive-mcp.emacs.client/emacs-running?)]
    (if (running?) (r/ok :running) (unavailable :delivery/emacs-down "no Emacs daemon answers"))
    (unavailable :delivery/emacs-absent "no Emacs client")))

(defn- probe-headless
  "Healthy while at least one headless backend is registered."
  []
  (if (seq (*headless-backends*))
    (r/ok :registered)
    (unavailable :delivery/headless-absent "no headless backend is registered")))

;; :serves names the lanes (target kinds) an adapter takes part in, so a
;; coordinator delivery and a ling delivery each keep their own sticky
;; :active instead of flipping one shared one (registry lanes).

;; :nats-inbox's probe answers for the BUS only: a lane-wide adapter health
;; can never say whether one ling's consumer is attached. That is a fact of
;; the target, read per call (`ling-route-health`): a ling with no attached
;; consumer makes :nats-inbox :inapplicable, never :degraded, so one ling
;; without a consumer cannot push the whole ling lane off NATS.

(defn ling-route-health
  "The ling-lane NATS health of `ling-id`, as the delivery chain sees it:
   {:nats-up? :armed? :consumer-attached? :reachable?}. :nats-up? is the
   probe's fact; the other two are this ling's."
  [ling-id]
  (*ling-route* ling-id))

(m/=> ling-route-health [:=> [:cat :any] :map])

(defmethod reg/open-adapter [:delivery :nats-inbox] [_ _]
  {:call nats-send :probe probe-nats :serves #{:coordinator :ling}})

(defmethod reg/open-adapter [:delivery :tmux] [_ _]
  {:call tmux-say :probe probe-tmux :serves #{:coordinator}})

(defmethod reg/open-adapter [:delivery :headless-ling] [_ _]
  {:call headless-ling-dispatch :probe probe-headless :serves #{:ling}})

(defmethod reg/open-adapter [:delivery :emacs] [_ _]
  {:call emacs-dispatch :probe probe-emacs :serves #{:ling}})

(defmethod reg/open-adapter [:delivery :hivemind-shout] [_ _]
  {:call shout-last-resort})

;; =============================================================================
;; Entry
;; =============================================================================

(defn deliver!
  "Deliver op {:to id :message text :from id? :params dispatch-params?} through
   the :delivery chain, in the lane of the target's kind (:coordinator, or
   :ling, which keeps its own sticky :active). An id that is neither a
   coordinator nor a registered ling is an addressing error (:error/kind
   :addressing) and no adapter runs."
  [{:keys [to] :as op}]
  (if-let [kind (target-kind to)]
    (reg/call! :delivery (cond-> (assoc op :kind kind :lane kind)
                           (= :ling kind) (assoc :spawn-mode (spawn-mode-of to))))
    (r/err :delivery/unknown-target
           {:error/kind :addressing
            :to         to
            :message    (str "Agent not found: " to)
            :via        nil
            :skipped    []})))

(m/=> deliver! [:=> [:cat :map] :map])

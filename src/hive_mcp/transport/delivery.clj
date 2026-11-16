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
     :nats-inbox      coordinator with an armed inbox -> inbox send!
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

(m/=> coordinator-id? [:=> [:cat :any] :boolean])
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

(defn- nats-send
  [{:keys [kind to message from]}]
  (if (not= :coordinator kind)
    (inapplicable :nats-inbox kind "only coordinators have an inbox")
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
  [{:keys [kind params]}]
  (if (not= :ling kind)
    (inapplicable :emacs kind "only lings take a terminal dispatch")
    (r/ok (*ling-dispatch* params))))

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

(defmethod reg/open-adapter [:delivery :nats-inbox] [_ _]
  {:call nats-send :probe probe-nats})

(defmethod reg/open-adapter [:delivery :tmux] [_ _]
  {:call tmux-say :probe probe-tmux})

(defmethod reg/open-adapter [:delivery :emacs] [_ _]
  {:call emacs-dispatch :probe probe-emacs})

(defmethod reg/open-adapter [:delivery :hivemind-shout] [_ _]
  {:call shout-last-resort})

;; =============================================================================
;; Entry
;; =============================================================================

(defn deliver!
  "Deliver op {:to id :message text :from id? :params dispatch-params?} through
   the :delivery chain. An id that is neither a coordinator nor a registered
   ling is an addressing error (:error/kind :addressing) and no adapter runs."
  [{:keys [to] :as op}]
  (if-let [kind (target-kind to)]
    (reg/call! :delivery (assoc op :kind kind))
    (r/err :delivery/unknown-target
           {:error/kind :addressing
            :to         to
            :message    (str "Agent not found: " to)
            :via        nil
            :skipped    []})))

(m/=> deliver! [:=> [:cat :map] :map])

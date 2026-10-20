(ns hive-mcp.hivemind.conversation
  "Inter-Ling Conversation Protocol — pure protocol layer (Calculations).

   Compat shim: moved to hive-agent.swarm.hivemind.conversation in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Value schemas for the shim contracts. They transcribe the docstrings of
;; hive-agent.swarm.hivemind.conversation, where the behaviour lives. Inputs
;; stay open where the addon checks them itself with :pre.

(def AgentId [:string {:gen/elements ["ling-a" "ling-b" "coordinator"]}])

(def AskId [:string {:gen/elements ["ask-1" "ask-2"]}])

(def EnvelopeOpts
  "The opts map an envelope constructor reads."
  [:map
   [:from AgentId] [:to AgentId]
   [:message {:optional true} :any]
   [:question {:optional true} :any]
   [:answer {:optional true} :any]
   [:ask-id {:optional true} [:maybe :string]]
   [:project-id {:optional true} [:maybe :string]]
   [:data {:optional true} :any]
   [:timestamp {:optional true} :int]])

(def Envelope
  "A :conversation/* event envelope."
  [:map
   [:event-type [:enum :conversation/tell :conversation/ask :conversation/respond]]
   [:from :string] [:to :string] [:timestamp :int]
   [:ask-id {:optional true} :string]
   [:project-id {:optional true} :any]
   [:data {:optional true} :any]])

(def Chan
  "A core.async promise-chan."
  [:fn {:error/message "a core.async channel"
        :gen/schema :nil :gen/fmap (fn [_] (clojure.core.async/promise-chan))}
   #(satisfies? clojure.core.async.impl.protocols/ReadPort %)])

(def PendingAsk
  [:map [:ask-id :string] [:from :string] [:to :string] [:question :any]
   [:timestamp :int] [:response-chan Chan]])


(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.hivemind.conversation" sym))

(defn- impl!
  "impl, after making sure the host adapters fill the swarm port slots.
   The adapters are resolved late: a static require would close a load
   cycle through the events adapter. A failure here (for instance a call
   made while that cycle is still loading) leaves the ports on their
   noops until a later call installs them."
  [sym]
  (try ((requiring-resolve 'hive-mcp.swarm.adapters.boot/ensure!))
       (catch Throwable _ nil))
  (impl sym))

(defn ask-envelope
  {:arglists '([{:keys [from to question ask-id project-id data timestamp], :or {timestamp (now-ms)}}])}
  [& args]
  (apply (impl! 'ask-envelope) args))

(m/=> ask-envelope [:=> [:cat EnvelopeOpts] Envelope])

(defn await-response!
  {:arglists '([ask-id & {:keys [timeout-ms], :or {timeout-ms default-ask-timeout-ms}}])}
  [& args]
  (apply (impl! 'await-response!) args))

;; The delivered answer, or {:timeout true :ask-id id} / {:no-such-ask true :ask-id id}.
(m/=> await-response! [:=> [:cat AskId [:* :any]] :any])

(defn build-ask!
  {:arglists '([{:keys [from to question project-id data timeout-ms], :as opts}])}
  [& args]
  (apply (impl! 'build-ask!) args))

(m/=> build-ask!
      [:=> [:cat EnvelopeOpts]
       [:map [:envelope Envelope] [:ask-id :string] [:response-chan Chan]]])

(defn cancel-ask!
  {:arglists '([ask-id])}
  [& args]
  (apply (impl! 'cancel-ask!) args))

(m/=> cancel-ask! [:=> [:cat :any] :boolean])

(defn clear-pending!
  {:arglists '([])}
  [& args]
  (apply (impl! 'clear-pending!) args))

(m/=> clear-pending! [:=> [:cat] :map])

(def default-ask-timeout-ms @(impl 'default-ask-timeout-ms))

(defn deliver-response!
  {:arglists '([ask-id answer])}
  [& args]
  (apply (impl! 'deliver-response!) args))

(m/=> deliver-response! [:=> [:cat :any :any [:? :map]] :boolean])

(defn pending-ask
  {:arglists '([ask-id])}
  [& args]
  (apply (impl! 'pending-ask) args))

(m/=> pending-ask [:=> [:cat :any] [:maybe PendingAsk]])

(def pending-asks @(impl 'pending-asks))

(defn pending-count
  {:arglists '([])}
  [& args]
  (apply (impl! 'pending-count) args))

(m/=> pending-count [:=> [:cat] nat-int?])

(defn register-ask!
  {:arglists '([{:keys [ask-id from to question]}])}
  [& args]
  (apply (impl! 'register-ask!) args))

(m/=> register-ask! [:=> [:cat [:map [:ask-id AskId] [:from AgentId] [:to AgentId]] [:? :map]] Chan])

(defn respond-envelope
  {:arglists '([{:keys [from to ask-id answer project-id data timestamp], :or {timestamp (now-ms)}}])}
  [& args]
  (apply (impl! 'respond-envelope) args))

(m/=> respond-envelope [:=> [:cat EnvelopeOpts] Envelope])

(defn tell-envelope
  {:arglists '([{:keys [from to message project-id data timestamp], :or {timestamp (now-ms)}}])}
  [& args]
  (apply (impl! 'tell-envelope) args))

(m/=> tell-envelope [:=> [:cat EnvelopeOpts] Envelope])

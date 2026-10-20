(ns hive-mcp.hivemind.state
  "Hivemind state atoms and direct accessors.

   Compat shim: moved to hive-agent.swarm.hivemind.state in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Value schemas for the shim contracts. They transcribe the docstrings of
;; hive-agent.swarm.hivemind.state, where the behaviour lives.

(def AgentId [:string {:gen/elements ["ling-a" "ling-b"]}])

(def BoundedAtom
  "A hive-dsl bounded atom: {:atom :name :opts}."
  [:map {:gen/elements [{:atom (atom {}) :name :agent-registry :opts {}}]}
   [:atom [:fn {:error/message "an atom"} #(instance? clojure.lang.IAtom %)]]])

(def PutResult
  "What a bounded-atom write answers."
  [:map [:result :map]])


(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.hivemind.state" sym))

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

(defn add-swarm-prompt!
  {:arglists '([slave-id prompt-text session-id timestamp])}
  [& args]
  (apply (impl! 'add-swarm-prompt!) args))

(m/=> add-swarm-prompt!
      [:=> [:cat AgentId :string :any :any]
       [:map [:prompt :string] [:timestamp :any] [:session-id :any] [:received-at :int]]])

(def agent-registry
  "LOAD-TIME SNAPSHOT of the swarm's agent registry: nil when hive-agent was
   absent at load, and never refreshed. Kept for existing callers only. Code
   that must work with the swarm absent, or after hive-agent reloads, reads
   `current-agent-registry` instead."
  @(impl 'agent-registry))

(defn current-agent-registry
  "The swarm's agent registry (a bounded atom) resolved through the delegate
   on EVERY call, or nil when hive-agent is not on the classpath.

   Nothing is captured at load: the var is looked up per call (Capture-by-Var),
   so a hive-agent that arrives or reloads later is seen by the next call."
  []
  @(impl 'agent-registry))

(m/=> current-agent-registry [:=> [:cat] [:maybe BoundedAtom]])

(defn clear-agent-messages!
  {:arglists '([agent-id])}
  [& args]
  (apply (impl! 'clear-agent-messages!) args))

(m/=> clear-agent-messages! [:=> [:cat :any] PutResult])

(defn clear-agent-registry!
  {:arglists '([])}
  [& args]
  (apply (impl! 'clear-agent-registry!) args))

;; Guarded in the addon: a no-op (nil) while a coordinator runs.
(m/=> clear-agent-registry! [:=> [:cat] :nil])

(defn clear-ling-results!
  {:arglists '([])}
  [& args]
  (apply (impl! 'clear-ling-results!) args))

(m/=> clear-ling-results! [:=> [:cat] :nil])

(defn get-pending-ling-results
  {:arglists '([])}
  [& args]
  (apply (impl! 'get-pending-ling-results) args))

(m/=> get-pending-ling-results [:=> [:cat] [:map-of :any [:map [:reviewed? :boolean]]]])

(defn get-swarm-prompts
  {:arglists '([])}
  [& args]
  (apply (impl! 'get-swarm-prompts) args))

(m/=> get-swarm-prompts [:=> [:cat] [:map-of :any :any]])

(def ling-results @(impl 'ling-results))

(defn mark-ling-reviewed!
  {:arglists '([agent-id])}
  [& args]
  (apply (impl! 'mark-ling-reviewed!) args))

(m/=> mark-ling-reviewed! [:=> [:cat :any] [:maybe PutResult]])

(def pending-asks @(impl 'pending-asks))

(def pending-swarm-prompts @(impl 'pending-swarm-prompts))

(defn record-ling-result!
  {:arglists '([agent-id result])}
  [& args]
  (apply (impl! 'record-ling-result!) args))

(m/=> record-ling-result! [:=> [:cat :any :any] PutResult])

(defn remove-ling-result!
  {:arglists '([agent-id])}
  [& args]
  (apply (impl! 'remove-ling-result!) args))

(m/=> remove-ling-result! [:=> [:cat :any] PutResult])

(defn remove-swarm-prompt!
  {:arglists '([slave-id])}
  [& args]
  (apply (impl! 'remove-swarm-prompt!) args))

(m/=> remove-swarm-prompt! [:=> [:cat :any] :nil])

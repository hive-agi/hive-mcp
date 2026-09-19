(ns hive-mcp.agora.consensus
  "Agora Consensus: Nash Equilibrium detection for multi-ling dialogues.

   Compat shim: moved to hive-agent.swarm.agora.consensus in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.agora.consensus" sym))

(def equilibrium-signals @(impl 'equilibrium-signals))

(def reset-signals @(impl 'reset-signals))

(def neutral-signals @(impl 'neutral-signals))

(def all-valid-signals @(impl 'all-valid-signals))

(def default-config @(impl 'default-config))

(defn get-participants
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'get-participants) args))

(defn get-dialogue-config
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'get-dialogue-config) args))

(defn extract-short-name
  {:arglists '([slave-id])}
  [& args]
  (apply (impl 'extract-short-name) args))

(defn normalize-participant-match
  {:arglists '([sender participant])}
  [& args]
  (apply (impl 'normalize-participant-match) args))

(defn last-turn-for
  {:arglists '([dialogue-id participant])}
  [& args]
  (apply (impl 'last-turn-for) args))

(defn get-active-proposal
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'get-active-proposal) args))

(defn approvals-aligned?
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'approvals-aligned?) args))

(defn nash-equilibrium?
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'nash-equilibrium?) args))

(defn count-approvals
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'count-approvals) args))

(defn count-participants
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'count-participants) args))

(defn approval-ratio
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'approval-ratio) args))

(defn threshold-consensus?
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'threshold-consensus?) args))

(defn turn-count
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'turn-count) args))

(defn turns-since-last-proposal
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'turns-since-last-proposal) args))

(defn check-consensus
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'check-consensus) args))

(defn stuck?
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'stuck?) args))

(defn calculate-progress-score
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'calculate-progress-score) args))

(defn deadlocked?
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'deadlocked?) args))

(defn should-recruit-mediator?
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'should-recruit-mediator?) args))

(defn consensus-result
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'consensus-result) args))

(defn parse-signal
  {:arglists '([message])}
  [& args]
  (apply (impl 'parse-signal) args))

(defn signal->equilibrium-contribution
  {:arglists '([signal-kw])}
  [& args]
  (apply (impl 'signal->equilibrium-contribution) args))

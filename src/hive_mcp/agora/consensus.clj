(ns hive-mcp.agora.consensus
  "Agora Consensus: Nash Equilibrium detection for multi-ling dialogues.

   Compat shim: moved to hive-agent.swarm.agora.consensus in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]
            [hive-mcp.agora.schema :as schema]
            [hive-mcp.agora.signal :as signal]))
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

;; Value schemas for the shim contracts. They transcribe the docstrings of
;; hive-agent.swarm.agora.consensus, where the behaviour lives.

(def ConsensusStatus
  "What `check-consensus` answers."
  [:enum :consensus :continue :timeout :stuck :insufficient])

(def Count
  "A non-negative count."
  [:int {:min 0}])

(def Ratio
  "An approval ratio."
  [:double {:min 0.0 :max 1.0}])

(def ProgressScore
  "`calculate-progress-score`: the score and the three factors it sums."
  [:map
   [:score :double]
   [:factors [:map
              [:approval-momentum :double]
              [:participation :double]
              [:recent-activity :double]]]])

(def MediatorAdvice
  "`should-recruit-mediator?`: whether, why and how urgently."
  [:map
   [:recruit? :boolean]
   [:reason [:maybe [:enum :deadlock :stuck :low-progress]]]
   [:urgency [:maybe [:enum :high :medium :low]]]])

(def ConsensusResult
  "`consensus-result`: the whole analysis of one dialogue."
  [:map
   [:status ConsensusStatus]
   [:nash-equilibrium? :boolean]
   [:approval-ratio Ratio]
   [:participants Count]
   [:turn-count Count]
   [:progress-score :double]
   [:mediator-needed? :boolean]
   [:mediator-reason [:maybe [:enum :deadlock :stuck :low-progress]]]])

(defn get-participants
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'get-participants) args))

(m/=> get-participants [:=> [:cat schema/DialogueId] [:vector :string]])

(defn get-dialogue-config
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'get-dialogue-config) args))

(m/=> get-dialogue-config [:=> [:cat schema/DialogueId] :map])

(defn extract-short-name
  {:arglists '([slave-id])}
  [& args]
  (apply (impl 'extract-short-name) args))

(m/=> extract-short-name [:=> [:cat [:maybe :string]] [:maybe :string]])

(defn normalize-participant-match
  {:arglists '([sender participant])}
  [& args]
  (apply (impl 'normalize-participant-match) args))

;; nil, not false, when one side has no short name: the fuzzy arm is an `and`.
(m/=> normalize-participant-match
      [:=> [:cat [:maybe :string] [:maybe :string]] [:maybe :boolean]])

(defn last-turn-for
  {:arglists '([dialogue-id participant])}
  [& args]
  (apply (impl 'last-turn-for) args))

(m/=> last-turn-for [:=> [:cat schema/DialogueId [:maybe :string]] [:maybe schema/Turn]])

(defn get-active-proposal
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'get-active-proposal) args))

(m/=> get-active-proposal [:=> [:cat schema/DialogueId] [:maybe schema/Turn]])

(defn approvals-aligned?
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'approvals-aligned?) args))

;; false with no proposal; otherwise an `and` over a `some`, so nil is possible.
(m/=> approvals-aligned? [:=> [:cat schema/DialogueId] [:maybe :boolean]])

(defn nash-equilibrium?
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'nash-equilibrium?) args))

(m/=> nash-equilibrium? [:=> [:cat schema/DialogueId] :boolean])

(defn count-approvals
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'count-approvals) args))

(m/=> count-approvals [:=> [:cat schema/DialogueId] Count])

(defn count-participants
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'count-participants) args))

(m/=> count-participants [:=> [:cat schema/DialogueId] Count])

(defn approval-ratio
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'approval-ratio) args))

(m/=> approval-ratio [:=> [:cat schema/DialogueId] Ratio])

(defn threshold-consensus?
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'threshold-consensus?) args))

(m/=> threshold-consensus? [:=> [:cat schema/DialogueId] :boolean])

(defn turn-count
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'turn-count) args))

(m/=> turn-count [:=> [:cat schema/DialogueId] Count])

(defn turns-since-last-proposal
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'turns-since-last-proposal) args))

(m/=> turns-since-last-proposal [:=> [:cat schema/DialogueId] Count])

(defn check-consensus
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'check-consensus) args))

(m/=> check-consensus [:=> [:cat schema/DialogueId] ConsensusStatus])

(defn stuck?
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'stuck?) args))

(m/=> stuck? [:=> [:cat schema/DialogueId] :boolean])

(defn calculate-progress-score
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'calculate-progress-score) args))

(m/=> calculate-progress-score [:=> [:cat schema/DialogueId] ProgressScore])

(defn deadlocked?
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'deadlocked?) args))

(m/=> deadlocked? [:=> [:cat schema/DialogueId] :boolean])

(defn should-recruit-mediator?
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'should-recruit-mediator?) args))

(m/=> should-recruit-mediator? [:=> [:cat schema/DialogueId] MediatorAdvice])

(defn consensus-result
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'consensus-result) args))

(m/=> consensus-result [:=> [:cat schema/DialogueId] ConsensusResult])

(defn parse-signal
  {:arglists '([message])}
  [& args]
  (apply (impl 'parse-signal) args))

(m/=> parse-signal [:=> [:cat [:maybe :string]] signal/SignalType])

(defn signal->equilibrium-contribution
  {:arglists '([signal-kw])}
  [& args]
  (apply (impl 'signal->equilibrium-contribution) args))

(m/=> signal->equilibrium-contribution [:=> [:cat signal/Classifiable] signal/Contribution])

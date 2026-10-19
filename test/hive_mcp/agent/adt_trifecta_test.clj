(ns hive-mcp.agent.adt-trifecta-test
  "Facets for the two closed ADTs of the agent runtime, session-state and
   spawn-mode, derived from their malli contracts. Each arm of the
   [:maybe ADT] result is driven directly: a variant keyword must produce a
   value that validates against the ADT schema, and anything else nil."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.session-state :as ss]
            [hive-mcp.agent.spawn-mode :as sm]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def StateInput
  "Keywords and strings, valid or not, as from-keyword receives them."
  [:or ss/SessionStateKw
   [:enum "session/idle" "session/running" "bogus" nil 1]
   :keyword])

(deftrifecta-from-schema session-from-keyword-trifecta
  hive-mcp.agent.session-state/from-keyword
  {:in ss/SessionStateKw
   :out ss/SessionState
   :rel (fn [k out] (= k (ss/to-keyword out)))
   :golden-path "test/golden/agent/session-from-keyword.edn"
   :num-tests 100})

(deftrifecta-from-schema session-terminal-trifecta
  hive-mcp.agent.session-state/terminal?
  {:in ss/SessionState
   :out :boolean
   :rel (fn [s out] (and (= out (contains? ss/terminal-states (ss/to-keyword s)))
                         (not= out (ss/active? s))))
   :golden-path "test/golden/agent/session-terminal.edn"})

(deftrifecta-from-schema spawn-mode-from-keyword-trifecta
  hive-mcp.agent.spawn-mode/from-keyword
  {:in sm/SpawnModeKw
   :out sm/SpawnModeValue
   :rel (fn [k out] (= k (sm/to-keyword out)))
   :golden-path "test/golden/agent/spawn-mode-from-keyword.edn"})

(deftrifecta-from-schema spawn-mode-metadata-trifecta
  hive-mcp.agent.spawn-mode/requires-emacs?
  {:in sm/SpawnModeValue
   :out :boolean
   :rel (fn [s out] (= out (contains? sm/emacs-variants (sm/to-keyword s))))
   :golden-path "test/golden/agent/spawn-mode-requires-emacs.edn"})

(deftest from-keyword-both-arms
  (testing "arm 1: a member yields an ADT value and no other arm validates it"
    (doseq [k (:variants ss/AgentSessionState)]
      (let [v (ss/from-keyword k)]
        (is (m/validate ss/SessionState v))
        (is (not (m/validate sm/SpawnModeValue v)))
        (is (= v (ss/from-keyword (subs (str k) 1))) "strings coerce too")))
    (doseq [k (:variants sm/SpawnMode)]
      (is (m/validate sm/SpawnModeValue (sm/from-keyword k)))
      (is (not (m/validate ss/SessionState (sm/from-keyword k))))))
  (testing "arm 2: anything else is nil"
    (doseq [bad [:bogus "bogus" nil 1 {} :hive-agent]]
      (is (nil? (ss/from-keyword bad)))
      (is (nil? (sm/from-keyword bad))))))

(deftest registry-backed-metadata
  (is (= :buffer (sm/io-model (sm/from-keyword :vterm))))
  (is (= :stdin-stdout (sm/io-model (sm/from-keyword :headless))))
  (is (= 6 (sm/slot-limit (sm/from-keyword :claude))))
  (is (nil? (sm/slot-limit (sm/from-keyword :headless))))
  (is (true? (sm/has-capability? (sm/from-keyword :claude) :interrupt)))
  (is (false? (sm/has-capability? (sm/from-keyword :vterm) :interrupt)))
  (is (true? (sm/mcp-visible? (sm/from-keyword :headless))))
  (is (false? (sm/mcp-visible? (sm/from-keyword :agent-sdk))))
  (is (every? #(sm/canonical? (sm/from-keyword %)) (:variants sm/SpawnMode)))
  (is (= (sm/from-keyword :headless) (sm/resolve-alias (sm/from-keyword :headless))))
  (is (contains? (sm/capabilities (sm/from-keyword :vterm)) :emacs-visible)))

(deftest session-predicates
  (is (= #{:session/idle :session/running} ss/active-states))
  (is (true? (ss/valid-state? :session/done)))
  (is (false? (ss/valid-state? :nope)))
  (is (true? (ss/active? (ss/from-keyword :session/running))))
  (is (true? (ss/terminal? (ss/from-keyword :session/aborted)))))

(deftest-contract-coverage session-state-contract-coverage
  {:paths ["src/hive_mcp/agent/session_state.clj" "src/hive_mcp/agent/spawn_mode.clj"]})

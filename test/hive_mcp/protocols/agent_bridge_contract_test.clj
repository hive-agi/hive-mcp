(ns hive-mcp.protocols.agent-bridge-contract-test
  "Contract facets for the agent-bridge slot, its predicates and the noop
   fallbacks. Every test owns the backend slot: it starts empty and the prior
   backend is put back afterwards."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.protocols.agent-bridge :as ab]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden])
  (:import [hive_mcp.protocols.agent_bridge NoopAgentBackend NoopAgentSession]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-the-slot [f]
  (let [prior (when (ab/agent-backend-set?) (ab/get-agent-backend))]
    (ab/clear-agent-backend!)
    (try (f)
         (finally (ab/clear-agent-backend!)
                  (when prior (ab/set-agent-backend! prior))))))

(use-fixtures :each own-the-slot)

(defrecord StubBackend [id]
  ab/IAgentBackend
  (backend-id [_] id)
  (available? [_] true)
  (capabilities [_] #{:tools :streaming})
  (execute! [_ _ _] nil)
  (connect! [_ _] nil)
  (disconnect! [_ _] nil))

(deftrifecta-from-schema noop-session-keeps-its-id
  #'ab/->noop-session
  {:in [:cat :string]
   :out ab/AgentSession
   :rel (fn [[id] out] (and (instance? NoopAgentSession out) (= id (ab/session-id out))))
   :num-tests 50})

(deftrifecta-from-schema agent-backend-pred-recognises-backends
  #'ab/agent-backend?
  {:in [:cat [:or ab/AgentBackend ab/AgentSession :int :string]]
   :out :boolean
   :rel (fn [[x] out] (= out (instance? NoopAgentBackend x)))
   :num-tests 100})

(deftrifecta-from-schema agent-session-pred-recognises-sessions
  #'ab/agent-session?
  {:in [:cat [:or ab/AgentBackend ab/AgentSession :int :keyword]]
   :out :boolean
   :rel (fn [[x] out] (= out (instance? NoopAgentSession x)))
   :num-tests 100})

(deftrifecta-from-schema an-empty-slot-reports-the-noop-backend
  #'ab/backend-capabilities
  {:in [:cat]
   :out ab/BackendCapabilities
   :rel (fn [_ out] (= {:backend-id :noop :enhanced? false :available? false :capabilities #{}} out))
   :num-tests 10})

(golden/deftest-golden slot-lifecycle-golden
  "test/golden/hive-mcp/protocols/agent-bridge-lifecycle.edn"
  (let [before {:set? (ab/agent-backend-set?) :enhanced? (ab/enhanced?)
                :noop? (instance? NoopAgentBackend (ab/get-agent-backend))}
        stub (->StubBackend :m17-stub)
        installed (ab/set-agent-backend! stub)
        during {:set? (ab/agent-backend-set?) :enhanced? (ab/enhanced?)
                :same? (identical? installed (ab/get-agent-backend))
                :caps (ab/backend-capabilities)}
        cleared (ab/clear-agent-backend!)]
    {:before before :during during :cleared cleared
     :after {:set? (ab/agent-backend-set?) :caps (ab/backend-capabilities)}}))

(deftest set-rejects-a-non-backend
  (is (thrown? AssertionError (ab/set-agent-backend! :not-a-backend)))
  (is (false? (ab/agent-backend-set?))))

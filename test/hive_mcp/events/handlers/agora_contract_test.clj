(ns hive-mcp.events.handlers.agora-contract-test
  "Contract facets for the Agora dialogue handlers. They depend on a dialogue
   store hive-agent provides; their contract is checked on what they answer
   in this build."
  (:require [clojure.test :refer [deftest is]]
            [malli.core :as m]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.agora :as agora]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- contract-out [sym]
  (-> (m/function-schemas) (get-in ['hive-mcp.events.handlers.agora sym :schema]) m/children second))

(deftest dispatch-next-answers-a-log-whatever-the-store-says
  (let [out (agora/handle-dispatch-next {} [:agora/dispatch-next {:dialogue-id "m11-absent-dialogue"}])]
    (is (m/validate (contract-out 'handle-dispatch-next) out))
    (is (nil? (:swarm-send-prompt out)) "an unknown dialogue has no next participant")))

(deftest register-handlers-installs-the-agora-events
  (ev/with-clean-registry
    (let [events [:agora/turn-completed :agora/turn-response
                  :agora/dispatch-next :agora/consensus]
          reg (agora/register-handlers!)]
      (is (every? #(contains? reg %) events))
      (is (every? ev/handler-registered? events)))))

(deftest the-dead-relay-and-timeout-events-are-not-registered
  (ev/with-clean-registry
    (let [reg (agora/register-handlers!)]
      (is (not-any? #(contains? reg %) [:agora/turn-dispatched :agora/timeout]))
      (is (not-any? ev/handler-registered? [:agora/turn-dispatched :agora/timeout])))))

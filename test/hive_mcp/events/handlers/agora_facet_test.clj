(ns hive-mcp.events.handlers.agora-facet-test
  "Golden facet for hive-mcp.events.handlers.agora on the unit classpath,
   where the dialogue store (hive-agent.swarm.agora.schema) is absent.

   The handlers read the store through hive-mcp.agora.dialogue and
   hive-mcp.agora.schema, which resolve hive-agent vars through
   hive-mcp.swarm.delegate/resolve-var: a concretion, not an injectable port.
   So only the store-absent behaviour is pinned here (no with-redefs); the
   active-dialogue branches need a store seam the handlers do not have."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.agora :as agora]
            [hive-mcp.events.handlers.schema :as hs]
            [hive-mcp.swarm.delegate :as delegate]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private store-absent?
  "True on the unit classpath, where hive-agent (the dialogue store) is absent.
   With hive-agent present the handlers answer from a live store, which this
   snapshot does not describe."
  (not (delegate/available? "hive-agent.swarm.agora.schema")))

(defn- outcome
  "The handler's answer, or the ex-data it raised (store absent)."
  [f event]
  (try {:answer (f {} event)}
       (catch clojure.lang.ExceptionInfo e
         {:raised (select-keys (ex-data e) [:swarm/addon-missing :addon :ns :var])})))

(defn- store-absent-trace []
  {:dispatch-next
   (into (sorted-map)
         (for [id ["d-1" "dialogue-xyz" ""]]
           [id (outcome agora/handle-dispatch-next [:agora/dispatch-next {:dialogue-id id}])]))
   :turn-completed
   (outcome agora/handle-turn-completed
            [:agora/turn-completed {:dialogue-id "d-1" :participant-id "ling-1"
                                    :signal :approve :turn-num 2}])
   :consensus
   (outcome agora/handle-consensus [:agora/consensus {:dialogue-id "d-1" :turns 3}])
   :registered
   (ev/with-clean-registry (vec (sort (keys (agora/register-handlers!)))))})

(when store-absent?
  (golden/deftest-golden agora-store-absent-golden
    "test/golden/hive-mcp/events/handlers/agora-store-absent.edn"
    (store-absent-trace)))

(deftest dispatch-next-answers-a-contract-valid-log
  (let [out-schema (-> (m/function-schemas)
                       (get-in ['hive-mcp.events.handlers.agora 'handle-dispatch-next :schema])
                       m/children second)]
    (doseq [id ["a" "b"]]
      (let [out (agora/handle-dispatch-next {} [:agora/dispatch-next {:dialogue-id id}])]
        (is (m/validate out-schema out))
        (is (m/validate hs/LogEffect (:log out)))
        (when store-absent?
          (is (= :error (get-in out [:log :level]))))))))

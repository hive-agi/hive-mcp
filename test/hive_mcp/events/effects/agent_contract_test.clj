(ns hive-mcp.events.effects.agent-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.effects.agent :as agent]
            [hive-mcp.events.core :as events]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema agent-effect-registration
  #'agent/register-agent-effects!
  {:in [:cat]
   :out :nil
   :rel (fn [_ result]
          (and (nil? result)
               (ifn? (events/get-fx-handler :dispatch-task))
               (ifn? (events/get-fx-handler :swarm-send-prompt))))
   :num-tests 30})

(deftest agent-effects-are-registered
  (is (nil? (agent/register-agent-effects!)))
  (is (ifn? (events/get-fx-handler :dispatch-task))))

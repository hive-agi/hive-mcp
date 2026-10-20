(ns hive-mcp.events.effects.registration-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.effects.agent :as agent]
            [hive-mcp.events.effects.coeffect :as coeffect]
            [hive-mcp.events.effects.dispatch :as dispatch]
            [hive-mcp.events.effects.infrastructure :as infrastructure]
            [hive-mcp.events.effects.kg :as kg]
            [hive-mcp.events.core :as events]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema dispatch-registration
  #'dispatch/register-dispatch-effects!
  {:in [:cat]
   :out :nil
   :rel (fn [_ result]
          (and (nil? result)
               (ifn? (events/get-fx-handler :dispatch))
               (ifn? (events/get-fx-handler :dispatch-n))))
   :num-tests 100})

(deftest effect-module-registration-contracts
  (is (nil? (agent/register-agent-effects!)))
  (is (nil? (coeffect/register-coeffects!)))
  (is (nil? (infrastructure/register-infrastructure-effects!)))
  (is (nil? (kg/register-kg-effects!)))
  (is (ifn? (events/get-cofx-handler :now)))
  (is (ifn? (events/get-fx-handler :kg-add-edge))))

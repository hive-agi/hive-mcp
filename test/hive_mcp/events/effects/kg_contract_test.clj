(ns hive-mcp.events.effects.kg-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.effects.kg :as kg]
            [hive-mcp.events.core :as events]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema kg-effect-registration
  #'kg/register-kg-effects!
  {:in [:cat]
   :out :nil
   :rel (fn [_ result]
          (and (nil? result)
               (ifn? (events/get-fx-handler :kg-add-edge))
               (ifn? (events/get-fx-handler :kg-remove-edge))))
   :num-tests 30})

(deftest confidence-handler-is-registered
  (is (nil? (kg/register-kg-effects!)))
  (is (ifn? (events/get-fx-handler :kg-update-confidence))))

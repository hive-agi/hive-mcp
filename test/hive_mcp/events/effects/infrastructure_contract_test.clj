(ns hive-mcp.events.effects.infrastructure-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.effects.infrastructure :as infrastructure]
            [hive-mcp.events.core :as events]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema infrastructure-effect-registration
  #'infrastructure/register-infrastructure-effects!
  {:in [:cat]
   :out :nil
   :rel (fn [_ result]
          (and (nil? result)
               (ifn? (events/get-fx-handler :kanban-move-done))
               (ifn? (events/get-fx-handler :report-metrics))))
   :num-tests 30})

(deftest infrastructure-registers-transactions
  (is (nil? (infrastructure/register-infrastructure-effects!)))
  (is (ifn? (events/get-fx-handler :ds-transact))))

(ns hive-mcp.events.effects.coeffect-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.effects.coeffect :as coeffect]
            [hive-mcp.events.core :as events]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema coeffect-registration
  #'coeffect/register-coeffects!
  {:in [:cat]
   :out :nil
   :rel (fn [_ result]
          (and (nil? result)
               (ifn? (events/get-cofx-handler :now))
               (ifn? (events/get-cofx-handler :agent-context))))
   :num-tests 30})

(deftest request-coeffect-is-registered
  (is (nil? (coeffect/register-coeffects!)))
  (is (ifn? (events/get-cofx-handler :request-ctx))))

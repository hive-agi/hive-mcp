(ns hive-mcp.events.effects-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.effects :as effects]
            [hive-mcp.events.core :as events]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema reset-effect-registration
  #'effects/reset-registration!
  {:in [:cat]
   :out :boolean
   :rel (fn [_ result] (boolean? result))
   :num-tests 100})

(deftest facade-installs-contributed-effects
  (is (true? (effects/register-effects!)))
  (is (ifn? (events/get-fx-handler :channel-publish)))
  (is (ifn? (events/get-cofx-handler :now))))

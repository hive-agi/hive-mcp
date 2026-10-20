(ns hive-mcp.events.effects.dispatch-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.effects.dispatch :as dispatch]
            [hive-mcp.events.core :as events]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema dispatch-effect-registration
  #'dispatch/register-dispatch-effects!
  {:in [:cat]
   :out :nil
   :rel (fn [_ result]
          (and (nil? result)
               (ifn? (events/get-fx-handler :dispatch))
               (ifn? (events/get-fx-handler :dispatch-n))))
   :num-tests 30})

(deftest dispatch-registers-both-handlers
  (is (nil? (dispatch/register-dispatch-effects!)))
  (is (ifn? (events/get-fx-handler :dispatch)))
  (is (ifn? (events/get-fx-handler :dispatch-n))))

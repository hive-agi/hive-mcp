(ns hive-mcp.events.core-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.core :as events]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema initialized-event-system
  #'events/init!
  {:in [:cat]
   :out :boolean
   :rel (fn [_ result]
          (and (boolean? result)
               (ifn? (events/get-fx-handler :mcp-response))
               (ifn? (events/get-cofx-handler :now))))
   :num-tests 100})

(deftest absent-handlers-are-nil
  (is (nil? (events/get-fx-handler :m10/missing)))
  (is (nil? (events/get-cofx-handler :m10/missing))))

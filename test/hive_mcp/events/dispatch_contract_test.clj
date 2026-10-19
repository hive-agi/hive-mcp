(ns hive-mcp.events.dispatch-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.dispatch :as dispatch]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema interceptor-shape
  #'dispatch/interceptor?
  {:in [:map [:id {:optional true} [:or :keyword :int]]
        [:before {:optional true} :boolean]
        [:after {:optional true} :boolean]]
   :out :boolean
   :rel (fn [value result]
          (boolean? result))
   :num-tests 500})

(deftest interceptor-execution-keeps-context
  (let [event [:m10/example {:id 1}]
        result (dispatch/execute event [] {:source :test})]
    (is (= event (get-in result [:coeffects :event])))
    (is (= :test (get-in result [:coeffects :source])))
    (is (= {} (:effects result))))
  (is (= {:effects {}} (dispatch/do-fx {:effects {}}))))

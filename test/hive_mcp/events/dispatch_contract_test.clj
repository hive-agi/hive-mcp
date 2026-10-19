(ns hive-mcp.events.dispatch-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.dispatch :as dispatch]
            [malli.instrument :as mi]
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

(deftest execute-accepts-set-of-interceptors-under-instrumentation
  (let [vs #{#'dispatch/execute}]
    (mi/instrument! {:filters [(mi/-filter-var vs)]})
    (try
      (let [result (dispatch/execute [:m10/set-input] #{{:id :set-interceptor}})]
        (is (= [{:id :set-interceptor}] (:stack result)))
        (is (= [:m10/set-input] (get-in result [:coeffects :event]))))
      (finally (mi/unstrument! {:filters [(mi/-filter-var vs)]})))))

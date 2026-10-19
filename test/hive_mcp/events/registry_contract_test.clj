(ns hive-mcp.events.registry-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.registry :as registry]
            [malli.instrument :as mi]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema registry-membership
  #'registry/handler-registered?
  {:in :keyword
   :out :boolean
   :rel (fn [event-id present?]
          (= present? (contains? (registry/registered-events) event-id)))
   :num-tests 500})

(deftest register-append-unregister
  (let [event :m10/registry-contract]
    (try
      (is (map? (registry/reg-event event [] (fn [_] {}))))
      (is (registry/handler-registered? event))
      (is (true? (registry/append-interceptor! event {:id :m10/trace})))
      (is (= [:m10/trace] (mapv :id (registry/get-interceptors event))))
      (finally
        (registry/unreg-event event)))
    (is (false? (registry/handler-registered? event)))))

(deftest reg-event-accepts-numeric-and-nil-interceptor-ids-under-instrumentation
  (let [vs #{#'registry/reg-event}
        event :m10/non-keyword-interceptor]
    (mi/instrument! {:filters [(mi/-filter-var vs)]})
    (try
      (let [entry (registry/reg-event event [{:id 42} {:id nil}] (fn [_] {}))]
        (is (map? entry))
        (is (= [42 nil] (mapv :id (registry/get-interceptors event)))))
      (finally
        (registry/unreg-event event)
        (mi/unstrument! {:filters [(mi/-filter-var vs)]})))))

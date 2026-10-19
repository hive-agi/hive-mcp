(ns hive-mcp.events.registry-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.registry :as registry]
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

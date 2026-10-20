(ns hive-mcp.events.schemas-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.schemas :as schemas]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema event-validity
  #'schemas/valid-event?
  {:in [:vector :any]
   :out :boolean
   :rel (fn [event valid?] (= valid? (and (keyword? (first event)) (boolean (seq event)))))
   :num-tests 500})

(deftrifecta-from-schema interceptor-validity
  #'schemas/valid-interceptor?
  {:in [:map [:id {:optional true} [:or :keyword :int]]]
   :out :boolean
   :rel (fn [interceptor valid?] (= valid? (keyword? (:id interceptor))))
   :num-tests 500})

(deftest validation-is-identity-on-valid-values
  (is (= [:noop] (schemas/validate-event! [:noop])))
  (is (= {:id :trace} (schemas/validate-interceptor! {:id :trace})))
  (is (thrown? clojure.lang.ExceptionInfo (schemas/validate-event! [4]))))

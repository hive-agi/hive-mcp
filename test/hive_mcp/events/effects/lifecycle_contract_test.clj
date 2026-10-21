(ns hive-mcp.events.effects.lifecycle-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.effects.lifecycle :as lifecycle]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema sweep-state-reset
  #'lifecycle/reset-state!
  {:in [:cat]
   :out :nil
   :rel (fn [_ result] (and (nil? result) (nil? (lifecycle/last-sweep-result))))
   :num-tests 100})

(deftest last-sweep-is-nil-after-reset
  (is (nil? (lifecycle/reset-state!)))
  (is (nil? (lifecycle/last-sweep-result))))

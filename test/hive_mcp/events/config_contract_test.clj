(ns hive-mcp.events.config-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.config :as config]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema agent-configuration-overrides
  #'config/agent-config
  {:in [:cat [:map
              [:agent-id {:optional true} [:string {:min 1}]]
              [:parent-id {:optional true} [:string {:min 1}]]
              [:depth {:optional true} [:int {:min 0}]]]]
   :out :map
   :rel (fn [[overrides] result]
          (every? (fn [[k value]] (= value (get result k))) overrides))
   :num-tests 500})

(deftest runtime-metrics-overrides-merge
  (try
    (is (= 17 (:max-timings (config/set-metrics-override! {:max-timings 17}))))
    (is (= 17 (:max-timings (config/metrics-config))))
    (finally (config/clear-metrics-overrides!))))

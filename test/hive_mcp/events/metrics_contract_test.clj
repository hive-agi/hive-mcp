(ns hive-mcp.events.metrics-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.metrics :as metrics]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema cleared-metrics
  #'metrics/reset-metrics!
  {:in [:cat]
   :out [:map [:events-dispatched [:int {:min 0}]]
         [:effects-executed [:int {:min 0}]]
         [:errors [:int {:min 0}]]
         [:timings [:vector :any]]]
   :rel (fn [_ result]
          (and (zero? (:events-dispatched result))
               (zero? (:effects-executed result))
               (zero? (:errors result))
               (empty? (:timings result))))
   :num-tests 100})

(deftest effect-counters-increment
  (try
    (metrics/reset-metrics!)
    (is (= 1 (:effects-executed (metrics/record-effect-executed!))))
    (is (= 1 (:errors (metrics/record-effect-error!))))
    (finally (metrics/reset-metrics!))))

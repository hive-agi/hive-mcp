(ns hive-mcp.events.bridge-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.bridge :as bridge]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema bridge-dispatch-data
  #'bridge/bridge-dispatch!
  {:in [:cat [:enum :task-start :task-complete]
        [:map [:task-id :string]]
        [:fn {:gen/elements [identity]} ifn?]]
   :out [:vector :any]
   :rel (fn [[hook-type payload _] result]
          (and (= (if (= hook-type :task-start) :task/start :task/complete) (first result))
               (= payload (second result))))
   :num-tests 500})

(deftest unknown-hook-does-not-dispatch
  (let [calls (atom [])]
    (is (nil? (bridge/bridge-dispatch! :not-registered {} #(swap! calls conj %))))
    (is (empty? @calls))))

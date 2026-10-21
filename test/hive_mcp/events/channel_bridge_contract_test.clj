(ns hive-mcp.events.channel-bridge-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.channel-bridge :as channel-bridge]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema channel-event-fallback
  #'channel-bridge/hook->event
  {:in [:map [:type [:enum :sample-event :activity-update]] [:data {:optional true} :map]]
   :out [:tuple :keyword :map]
   :rel (fn [event result]
          (= (second result) (dissoc event :type)))
   :num-tests 500})

(deftest string-and-keyword-channel-events
  (is (= [:task/complete {"task-id" "123"}]
         (channel-bridge/hook->event {"type" "task-complete" "task-id" "123"})))
  (is (= [:channel/misc {:data {:id 1}}]
         (channel-bridge/hook->event {:type :misc :data {:id 1}})))
  (is (nil? (channel-bridge/hook->event {}))))

(ns hive-mcp.events.write-events-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.write-events :as writes]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema write-event
  #'writes/->event
  {:in [:cat [:enum :added :updated :deleted]
        [:map [:id {:optional true} :string]
         [:project-id {:optional true} :string]]]
   :out [:map [:type :keyword]]
   :rel (fn [[op payload] result]
          (= result (assoc payload :type (writes/op->channel-type op))))
   :num-tests 500})

(deftest unknown-write-op-is-not-an-event
  (is (nil? (writes/event->write {:type :unknown})))
  (is (= {:op :added :id "m"}
         (writes/event->write (writes/->event :added {:id "m"}))))
  (is (nil? (writes/notify! :unknown {}))))

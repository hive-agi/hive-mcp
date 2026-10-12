(ns hive-mcp.server.routes.async-ack-test
  "An async ack must tell the caller where its result will arrive."
  (:require [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.server.routes.async-ack :as async-ack]))

(defn ack
  "Adapt ack to trifecta's unary port: [task-id tool timeout-ms durable?].
   Returns {:in input :out ack}."
  [[task-id tool timeout-ms durable? :as input]]
  {:in input :out (async-ack/ack task-id tool timeout-ms durable?)})

(deftrifecta async-ack-names-result-channel
  hive-mcp.server.routes.async-ack-test/ack
  {:golden-path "test/golden/hive-mcp/server/async-ack.edn"
   :cases {:plain       ["atask-1" "memory" nil true]
           :timeout     ["atask-2" "bash" 5000 true]
           :not-durable ["atask-3" "git" nil false]}
   :gen (gen/tuple (gen/fmap #(str "atask-" %) gen/nat)
                   (gen/elements ["memory" "bash" "kg"])
                   (gen/one-of [(gen/return nil) gen/nat])
                   gen/boolean)
   :pred (fn [{[task-id tool timeout-ms durable?] :in a :out}]
           (and (true? (:queued a))
                (= task-id (:task-id a))
                (= tool (:tool a))
                (= async-ack/result-via (:result-via a))
                (= timeout-ms (:timeout-ms a))
                (= (not durable?) (false? (:durable a)))))
   :num-tests 100
   :mutations [["no-result-via"
                (fn [[task-id tool timeout-ms durable? :as input]]
                  {:in  input
                   :out (cond-> {:queued true :task-id task-id :tool tool}
                          timeout-ms     (assoc :timeout-ms timeout-ms)
                          (not durable?) (assoc :durable false))})]
               ["drops-durable"
                (fn [[task-id tool timeout-ms _ :as input]]
                  {:in  input
                   :out (cond-> {:queued true :task-id task-id :tool tool
                                 :result-via async-ack/result-via}
                          timeout-ms (assoc :timeout-ms timeout-ms))})]]})

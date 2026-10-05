(ns hive-mcp.swarm.restore-liveness-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.swarm.sync :as sync]
            [hive-mcp.swarm.lifecycle.restore-liveness :as restore]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-spi.swarm.bootstrap :as bootstrap]))

(deftrifecta restore-classification
  restore/classify-row
  {:cases {:dead [{:slave-id "dead" :status :working :created-at 100} {:elisp-ids #{} :live-pids #{}}]
           :alive [{:slave-id "alive" :status :idle :created-at 200} {:elisp-ids #{"alive"} :live-pids #{}}]
           :pid [{:slave-id "pid" :status :working :process-pid 42 :created-at 300} {:elisp-ids #{} :live-pids #{42}}]}
   :gen (gen/tuple (gen/hash-map :slave-id gen/string-alphanumeric
                                  :status (gen/elements [:working :idle])
                                  :created-at gen/pos-int)
                   (gen/return {:elisp-ids #{} :live-pids #{}}))
   :apply? true
   :pred (fn [row] (and (= :zombie (:status row)) (false? (:alive? row))))
   :num-tests 30})

(deftest restore-never-publishes-dead-as-working
  (let [original (java.util.Date. 1234567890)
        rows [{:slave-id "dead" :status :working :created-at original}
              {:slave-id "alive" :status :idle :created-at original}]
        source (reify bootstrap/ISwarmBootstrap
                 (-load-slaves [_] rows)
                 (-snapshot-slave! [this _ _] this)
                 (-forget-slave! [this _] this)
                 (-close! [_] nil))]
    (sync/full-sync-from-bootstrap! source (fn [_] {:elisp-ids #{"alive"} :live-pids #{}}))
    (let [dead (queries/get-slave "dead")
          alive (queries/get-slave "alive")]
      (is (not= :working (:slave/status dead)))
      (is (false? (:slave/alive? dead)))
      (is (= :idle (:slave/status alive)))
      (is (= original (:slave/created-at alive))))))

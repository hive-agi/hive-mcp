(ns hive-mcp.knowledge-graph.connection-flush-failure-test
  "Failure after enqueue must be observable even after the queue drains."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.knowledge-graph.connection.writer :as writer]
            [hive-test.trifecta :refer [deftrifecta]]))

(deftrifecta flush-health-verdict
  hive-mcp.knowledge-graph.connection.writer/flush-status
  {:golden-path "test/golden/hive-mcp/connection-flush-health.edn"
   :cases {:healthy 0
           :failed 1
           :repeated-failures 3
           :after-report 0}
   :gen (gen/choose 0 100)
   :pred #{:ok :weave/write-failed}
   :num-tests 100
   :mutations [["always-healthy" (fn [_] :ok)]
               ["always-failed" (fn [_] :weave/write-failed)]]})

(deftest batch-failure-after-enqueue-is-surfaced
  (let [metrics (var-get #'writer/writer-metrics)
        before @metrics
        in-flight-before @writer/in-flight
        item {:kg-edge/id "failed-after-enqueue"}
        calls (atom [])]
    (try
      (swap! metrics assoc :unreported-failures 0)
      (swap! writer/in-flight inc)
      (#'writer/flush-batch! [item] 1
       (fn [tx]
         (swap! calls conj tx)
         (throw (ex-info "backend died after enqueue" {:backend :unavailable}))))
      (is (= [[item] [item]] @calls) "batch and individual fallback both attempted")
      (is (= in-flight-before @writer/in-flight))
      (is (= (inc (:async-write-failures before))
             (:async-write-failures (writer/writer-stats))))
      (is (= "backend died after enqueue"
             (get-in (writer/writer-stats) [:last-failure :error])))
      (is (= :weave/write-failed (writer/flush-pending!)))
      (is (= :ok (writer/flush-pending!)) "no new drop must not fail the next flush")
      (is (= 0 (:unreported-failures (writer/writer-stats))))
      (is (= (inc (:async-write-failures before))
             (:async-write-failures (writer/writer-stats)))
          "the cumulative failure metric survives acknowledgement")
      (finally
        ;; Writer metrics are process-wide; leave other tests' health unchanged.
        (reset! metrics before)
        (reset! writer/in-flight in-flight-before)))))

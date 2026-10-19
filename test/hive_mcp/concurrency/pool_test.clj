(ns hive-mcp.concurrency.pool-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.concurrency.pool :as pool]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.generator :as mg]))

(def PoolStats
  [:map [:io :map] [:compute :map] [:event :map]
   [:memory :map] [:catchup :map]])

(deftrifecta-from-schema pool-statistics hive-mcp.concurrency.pool/pool-stats
  {:in [:cat] :out PoolStats
   :rel (fn [_ out] (= #{:io :compute :event :memory :catchup} (set (keys out))))
   :num-tests 20})

(deftest named-pools-and-futures
  (golden/assert-golden
   (golden/anchor 'hive-mcp.concurrency.pool-test "test/golden/concurrency/pool-names.edn")
   (mapv #(.getName (class (%)))
         [pool/io-pool pool/compute-pool pool/event-pool
          pool/memory-pool pool/catchup-pool]))
  (doseq [submit [pool/submit-io! pool/submit-compute! pool/submit-event!
                  pool/submit-memory! pool/submit-catchup!]]
    (is (= :accepted @(submit (fn [] :accepted)))))
  (is (instance? java.util.concurrent.ThreadPoolExecutor
                 (mg/generate [:fn {:gen/gen (clojure.test.check.generators/return (pool/io-pool))}
                               #(instance? java.util.concurrent.ThreadPoolExecutor %)]))))

(deftest-contract-coverage concurrency-contracts
  {:paths ["src/hive_mcp/concurrency"]})

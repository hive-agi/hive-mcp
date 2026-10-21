(ns hive-mcp.diag.contract-coverage-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.diag.alloc :as alloc]
            [hive-mcp.diag.cache :as cache]
            [hive-mcp.diag.cache-datahike :as dh]
            [hive-mcp.diag.probe :as probe]
            [hive-mcp.diag.profiler :as profiler]
            [hive-mcp.diag.sizer :as sizer]
            [hive-spi.diag.ports :as p]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]))

(deftest constructors-satisfy-the-injected-ports
  (is (satisfies? p/IAllocationSampler (alloc/make-sampler)))
  (is (satisfies? p/ICacheProbe (cache/make-datalevin-query-cache)))
  (is (satisfies? p/ICacheProbe (dh/make-datahike-query-cache)))
  (is (satisfies? p/IHeapProbe (probe/make-heap-probe)))
  (is (satisfies? p/IProfiler (profiler/make-profiler)))
  (is (satisfies? p/IRetainedSizer (sizer/make-sizer))))

(deftest-contract-coverage diag-contracts
  {:paths ["src/hive_mcp/diag"]})

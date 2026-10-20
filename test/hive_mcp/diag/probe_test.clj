(ns hive-mcp.diag.probe-test
  "The JDK-only IHeapProbe is always available: its snapshot is a HeapSnapshot
   whose readings are mutually consistent (used <= committed, committed <= max
   when max is defined)."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.diag.probe :as probe]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.diag.ports :as p]
            [hive-spi.diag.schema :as ds]
            [malli.core :as m]))

;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(defn- consistent?
  [{:diag/keys [used-bytes committed-bytes max-bytes]}]
  (and (<= used-bytes committed-bytes)
       (or (nil? max-bytes) (<= committed-bytes max-bytes))))

(deftrifecta-from-schema make-heap-probe-trifecta probe/make-heap-probe
  {:in  [:cat]
   :out [:and [:fn #(satisfies? p/IHeapProbe %)] [:map]]
   :rel (fn [_ hp]
          (let [s (p/heap-snapshot hp)]
            (and (m/validate ds/HeapSnapshot s) (consistent? s))))
   :num-tests 5})

(deftest snapshot-reports-heap-and-pools
  (let [s (p/heap-snapshot (probe/make-heap-probe))]
    (is (m/validate ds/HeapSnapshot s))
    (is (pos? (:diag/used-bytes s)))
    (is (seq (:diag/pools s)) "the JVM always has at least one memory pool")))

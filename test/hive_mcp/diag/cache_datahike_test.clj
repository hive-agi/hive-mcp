(ns hive-mcp.diag.cache-datahike-test
  "The datahike query-result ICacheProbe names its cache and answers its
   occupancy as a CacheOccupancy Result (datahike is on the test classpath,
   so the ok arm is the one this JVM takes)."
  (:require [clojure.test :refer [deftest is]]
            [hive-dsl.result :as r]
            [hive-mcp.diag.cache-datahike :as dh]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.diag.ports :as p]
            [hive-spi.diag.schema :as ds]
            [malli.core :as m]))

;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def ^:private OccupancyResult
  [:or [:map [:ok ds/CacheOccupancy]]
   [:map [:error [:= :diag/cache-unavailable]]]])

(deftrifecta-from-schema make-datahike-query-cache-trifecta dh/make-datahike-query-cache
  {:in  [:cat]
   :out [:and [:fn #(satisfies? p/ICacheProbe %)] [:map]]
   :rel (fn [_ probe]
          (and (= :datahike/query-result (p/cache-id probe))
               (m/validate OccupancyResult (p/cache-occupancy probe))))
   :num-tests 5})

(deftest occupancy-names-the-cache
  (let [res (p/cache-occupancy (dh/make-datahike-query-cache))]
    (is (m/validate OccupancyResult res))
    (when (r/ok? res)
      (is (= :datahike/query-result (:diag/cache-id (:ok res))))
      (is (every? #(nat-int? (:diag/entries %)) (:diag/stores (:ok res)))))))

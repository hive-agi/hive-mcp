(ns hive-mcp.diag.cache-test
  "The datalevin query-result ICacheProbe names its cache, and every method
   is a Result: ok when datalevin's private cache registry resolves, or
   `err :diag/cache-unavailable` naming the cache when it does not. Never a
   throw, whichever arm this JVM takes."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.diag.cache :as cache]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.diag.ports :as p]
            [hive-spi.diag.schema :as ds]
            [malli.core :as m]))

;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def ^:private Unavailable
  [:map [:error [:= :diag/cache-unavailable]]
   [:cache-id [:= :datalevin/query-result]]
   [:reason :string]])

(def ^:private OccupancyResult
  [:or [:map [:ok ds/CacheOccupancy]] Unavailable])

(deftrifecta-from-schema make-datalevin-query-cache-trifecta cache/make-datalevin-query-cache
  {:in  [:cat]
   :out [:and [:fn #(satisfies? p/ICacheProbe %)] [:map]]
   :rel (fn [_ probe]
          (and (= :datalevin/query-result (p/cache-id probe))
               (m/validate OccupancyResult (p/cache-occupancy probe))))
   :num-tests 5})

(deftest occupancy-arms-are-distinct
  (let [res (p/cache-occupancy (cache/make-datalevin-query-cache))]
    (is (m/validate OccupancyResult res))
    (if (contains? res :ok)
      (is (not (m/validate Unavailable res)))
      (is (not (m/validate [:map [:ok ds/CacheOccupancy]] res))))))

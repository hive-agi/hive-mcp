(ns hive-mcp.system.sweepers.stale-cache-contract-test
  "Contract facets for hive-mcp.system.sweepers.stale-cache. The evictor set is
   a process-global defonce with no unregister, so the only evictors added
   here are side-effect free and answer a fixed count."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.protocols.lifecycle :as lifecycle]
            [hive-mcp.system.sweepers.stale-cache :as stale]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- evict-three [] 3)
(defn- evict-flag [] :done)

(golden/deftest-golden evictor-registration-golden
  "test/golden/hive-mcp/system/stale-cache-evictors.edn"
  (let [before (stale/registered-evictors)
        after-1 (stale/register-cache-evictor! evict-three)
        after-2 (stale/register-cache-evictor! evict-three)
        after-3 (stale/register-cache-evictor! evict-flag)]
    {:before-is-set     (set? before)
     :first-adds        (contains? after-1 evict-three)
     :repeat-idempotent (= after-1 after-2)
     :grew-by           [(- (count after-1) (count (disj before evict-three)))
                         (- (count after-3) (count after-2))]
     :snapshot-agrees   (= after-3 (stale/registered-evictors))
     :sweep-name        (lifecycle/sweep-name (stale/->StaleCacheSweep))
     :interval-s        (lifecycle/sweep-interval-s (stale/->StaleCacheSweep))}))

(deftest the-sweep-counts-registered-evictors
  (stale/register-cache-evictor! evict-three)
  (stale/register-cache-evictor! evict-flag)
  (let [r (lifecycle/sweep! (stale/->StaleCacheSweep) {})]
    (is (m/validate [:map [:swept [:int {:min 4}]] [:errors vector?]] r)
        "3 from the counting evictor plus 1 for the non-numeric answer")))

(deftest the-pre-condition-refuses-a-non-fn
  ;; the :pre throws AssertionError; under mi/instrument! the input contract
  ;; refuses first with an ExceptionInfo. Either is the refusal.
  (is (thrown? Throwable (stale/register-cache-evictor! :not-a-fn)))
  (is (not (m/validate stale/EvictFn :kw)))
  (is (m/validate stale/EvictFn evict-three)))

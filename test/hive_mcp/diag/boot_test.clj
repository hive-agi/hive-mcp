(ns hive-mcp.diag.boot-test
  "The boot facade owns no capability: it composes the five real adapters
   into one memoized clinic and routes its REPL surface to the matching port.
   These tests read through that surface on the test JVM, so they drive only
   the side-effect-free passthroughs: snapshot, caches, clinic identity and
   the profiler's OFF toggle with no session running. `diagnose`, `relieve!`,
   `histogram` and `on!` (sampling windows, forced GCs, agent attach) are
   covered at the clinic seam against fakes in clinic-test."
  (:require [clojure.test :refer [deftest is]]
            [hive-dsl.result :as r]
            [hive-mcp.diag.boot :as boot]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.diag.ports :as p]
            [hive-spi.diag.schema :as ds]
            [malli.core :as m]))

;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(deftrifecta-from-schema snapshot-trifecta boot/snapshot
  {:in  [:cat]
   :out ds/HeapSnapshot
   :rel (fn [_ s] (<= (:diag/used-bytes s) (:diag/committed-bytes s)))
   :num-tests 10})

(def ^:private CacheResult
  [:or [:map [:ok ds/CacheOccupancy]]
   [:map [:error [:= :diag/cache-unavailable]] [:cache-id :keyword]]])

(deftrifecta-from-schema caches-trifecta boot/caches
  {:in  [:cat]
   :out [:vector CacheResult]
   ;; one Result per registered probe, in registration order
   :rel (fn [_ rs]
          (= [:datalevin/query-result :datahike/query-result]
             (mapv #(or (:cache-id %) (:diag/cache-id (:ok %))) rs)))
   :num-tests 10})

(deftest the-clinic-is-a-memoized-singleton
  (let [c (boot/clinic)]
    (is (satisfies? p/IMemoryClinic c))
    (is (identical? c (boot/clinic)))
    (is (not (identical? c (boot/build-clinic)))
        "build-clinic constructs afresh; only clinic memoizes")))

(deftest off-without-a-session-is-an-err
  (let [res (boot/off!)]
    (is (r/err? res))
    (is (= :diag/profiler-not-running (:error res)))
    (is (not (m/validate [:map [:ok :map]] res)))))

(ns hive-mcp.diag.alloc-test
  "The jvm-alloc-rate-meter IAllocationSampler is a loadable leaf: building it
   never needs the meter library, and a sample taken without the library is an
   `err :diag/sampler-unavailable` Result carrying the requested window, never
   a throw. The test JVM does not carry the dev-only meter, so the degraded arm
   is the one driven here."
  (:require [clojure.test :refer [deftest is]]
            [hive-dsl.result :as r]
            [hive-mcp.diag.alloc :as alloc]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.diag.ports :as p]
            [hive-spi.diag.schema :as ds]
            [malli.core :as m]))

;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def ^:private SamplerResult
  "What `sample-allocation` answers: an ok AllocationProfile, or the
   sampler-unavailable err naming the requested window."
  [:or
   [:map [:ok ds/AllocationProfile]]
   [:map [:error [:= :diag/sampler-unavailable]] [:reason :string] [:duration-ms :int]]])

(deftrifecta-from-schema make-sampler-trifecta alloc/make-sampler
  {:in  [:cat]
   :out [:and [:fn #(satisfies? p/IAllocationSampler %)] [:map]]
   ;; a fresh sampler answers a short window with a Result, never a throw
   :rel (fn [_ sampler]
          (m/validate SamplerResult (p/sample-allocation sampler 1)))
   :num-tests 5})

(deftest sampling-without-the-meter-degrades-to-an-err
  (let [res (p/sample-allocation (alloc/make-sampler) 2)]
    (is (r/err? res))
    (is (= :diag/sampler-unavailable (:error res)))
    (is (= 2 (:duration-ms res)) "the requested window is echoed back")
    (is (m/validate SamplerResult res))
    (is (not (m/validate [:map [:ok ds/AllocationProfile]] res))
        "and it is not the ok arm")))

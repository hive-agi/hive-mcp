(ns hive-mcp.tools.multi-async-contract-test
  "Contract facets for the async batch registry's read side. Only ids that
   were never dispatched are asked about, so no future is started and the
   process-global batch registry is left as it was found."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.multi-async :as async]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [hive-mcp.test.stub.isolated-registry :as iso]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(clojure.test/use-fixtures :each iso/with-fresh-registries)

(def ^:private UnknownId
  [:or [:enum "batch-m24-none" "" "batch-"] :nil :int :keyword])

(deftrifecta-from-schema collect-unknown-is-not-found
  #'async/collect-async-result
  {:in [:cat UnknownId]
   :out async/Collected
   :rel (fn [[id] out] (= {:status "not-found" :batch-id id} out))
   :num-tests 100})

(deftrifecta-from-schema cancel-unknown-is-refused
  #'async/cancel-async-batch
  {:in [:cat UnknownId]
   :out async/Cancelled
   :rel (fn [[id] out] (= {:cancelled false :batch-id id :reason "not-found"} out))
   :num-tests 100})

(golden/deftest-golden unknown-batch-golden
  "test/golden/hive-mcp/tools/multi-async-unknown.edn"
  {:collect (async/collect-async-result "batch-m24-none")
   :await   (async/await-async-batch "batch-m24-none" :timeout-ms 1)
   :cancel  (async/cancel-async-batch "batch-m24-none")})

(deftest collected-arms-are-disjoint
  (is (m/validate async/Collected {:status "cancelled" :batch-id "b"}))
  (is (not (m/validate async/Collected {:status "expired" :batch-id "b"})))
  (is (m/validate async/Dispatched {:error :multi-async/engine-unavailable :message "m"}))
  (is (not (m/validate async/Dispatched {:batch-id "b" :status "completed" :ops 1 :created-at 1}))))

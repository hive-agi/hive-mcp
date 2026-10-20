(ns hive-mcp.agent.executor-trifecta-test
  "Facets for the tool executor, derived from its contracts:
   `format-tool-result` builds a role:tool message that carries the call id
   and tool name through, and approval is required only when asked for."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [hive-mcp.agent.executor :as ex]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema format-tool-result-trifecta
  hive-mcp.agent.executor/format-tool-result
  {:in [:cat [:string {:min 1}] [:string {:min 1}]
        [:or
         [:map [:success [:= true]] [:result [:map-of :keyword :int]]]
         [:map [:success [:= false]] [:error :string]]]]
   :out ex/ToolMessage
   :rel (fn [[id tool-name _] out]
          (and (= "tool" (:role out)) (= id (:tool_call_id out)) (= tool-name (:name out))))
   :num-tests 200})

(golden/deftest-golden-fn executor-golden
  "test/golden/agent/executor-format.edn"
  (fn []
    {:ok (ex/format-tool-result "id1" "memory" {:success true :result {:a 1}})
     :err (ex/format-tool-result "id2" "memory" {:success false :error "no"})
     :approval [(ex/requires-approval? "memory" [:x]) (ex/requires-approval? "file_write" nil)]}))

(deftest-contract-coverage executor-contract-coverage
  {:paths ["src/hive_mcp/agent/executor.clj"]})

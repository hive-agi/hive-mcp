(ns hive-mcp.tools.agent.dispatch-contract-test
  "Contract facets for agent dispatch: a trifecta over handle-dispatch's
   validation table (agent_id first, then prompt) and a golden over the
   envelopes, including the lookup failure when no swarm registry backs the
   unit JVM."
  (:require [clojure.string :as str]
            [hive-mcp.tools.agent.dispatch :as dsp]
            [hive-mcp.tools.result-bridge :as rb]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema dispatch-validates-agent-then-prompt
  #'dsp/handle-dispatch
  {:in [:cat [:map
              [:agent_id {:optional true} [:enum ""]]
              [:prompt {:optional true} [:enum "" "do it"]]
              [:priority {:optional true} [:enum "high" "low"]]]]
   :out rb/McpResponse
   :rel (fn [[_] out]
          (and (true? (:isError out)) (= "agent_id is required" (:text out))))
   :golden-path "test/golden/hive-mcp/tools/agent/dispatch-validation.edn"
   :num-tests 40})

(deftrifecta-from-schema dispatch-requires-a-prompt
  #'dsp/handle-dispatch
  {:in [:cat [:map [:agent_id [:enum "ling-1" "ling-2"]] [:prompt {:optional true} [:enum ""]]]]
   :out rb/McpResponse
   :rel (fn [_ out] (and (true? (:isError out)) (= "prompt is required" (:text out))))
   :golden-path "test/golden/hive-mcp/tools/agent/dispatch-prompt.edn"
   :num-tests 40})

(golden/deftest-golden dispatch-envelopes-golden
  "test/golden/hive-mcp/tools/agent/dispatch-envelopes.edn"
  {:no-agent  (dsp/handle-dispatch {})
   :no-prompt (dsp/handle-dispatch {:agent_id "a"})
   :lookup    (let [r (dsp/handle-dispatch {:agent_id "a" :prompt "p"})]
                [(:isError r) (str/starts-with? (:text r) "Failed to dispatch")])})

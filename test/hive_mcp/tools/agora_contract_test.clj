(ns hive-mcp.tools.agora-contract-test
  "Contract facets for the Agora handlers' validation boundary. Without a
   dialogue_id the handlers refuse before reaching the swarm, so no dialogue
   state is read or written."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.agora :as agora]
            [hive-mcp.tools.core :as core]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private NoDialogue
  [:map [:limit {:optional true} [:maybe :int]] [:topic {:optional true} :string]])

(deftrifecta-from-schema history-without-dialogue-refuses
  #'agora/handle-agora-get-history
  {:in [:cat NoDialogue]
   :out core/McpResponse
   :rel (fn [_ out] (= {:type "text" :text "Missing required parameter: dialogue_id" :isError true} out))
   :num-tests 60})

(deftrifecta-from-schema consensus-without-dialogue-refuses
  #'agora/handle-agora-check-consensus
  {:in [:cat NoDialogue]
   :out core/McpResponse
   :rel (fn [_ out] (= {:type "text" :text "Missing required parameter: dialogue_id" :isError true} out))
   :num-tests 60})

(golden/deftest-golden agora-refusals-golden
  "test/golden/hive-mcp/tools/agora-refusals.edn"
  {:history   (agora/handle-agora-get-history {})
   :consensus (agora/handle-agora-check-consensus {:limit 3})})

(deftest refusals-are-error-responses
  (is (m/validate core/McpErrorResponse (agora/handle-agora-get-history {}))))

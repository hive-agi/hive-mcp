(ns hive-mcp.tools.kg-contract-test
  "Contract facets for the KG versioning handlers' refusal boundary. With no
   versioned backend configured in the unit JVM every versioning handler
   refuses before touching a store, and the refusal is an MCP error."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.knowledge-graph.versioning :as versioning]
            [hive-mcp.tools.core :as core]
            [hive-mcp.tools.kg :as kg]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Params
  [:map [:name {:optional true} [:enum "" "exp" nil]]
   [:source {:optional true} [:enum "" "main"]]
   [:limit {:optional true} [:maybe [:int {:min 1 :max 5}]]]])

(deftrifecta-from-schema checkout-refuses-without-versioning
  #'kg/handle-kg-checkout
  {:in [:cat Params]
   :out core/McpResponse
   :rel (fn [_ out]
          (if (versioning/versioning-available?)
            (m/validate core/McpResponse out)
            (true? (:isError out))))
   :num-tests 60})

(deftest every-versioning-handler-answers-an-mcp-response
  (doseq [h [kg/handle-kg-branch kg/handle-kg-branches kg/handle-kg-snapshot-id
             kg/handle-kg-history kg/handle-kg-merge kg/handle-kg-versioning-status]]
    (is (m/validate core/McpResponse (h {})))))

(ns hive-mcp.tools.kg.queries-contract-test
  "Contract facets for hive-mcp.tools.kg.queries: the node-id validator and
   relation-filter parser (pure), and the read handlers driven against a
   fresh in-memory DataScript KG (the store is the injected port)."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.knowledge-graph.schema :as kg-schema]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-mcp.tools.core :as core]
            [hive-mcp.tools.kg.queries :as q]
            [hive-mcp.tools.kg.schema :as ks]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each fixtures/datascript-fixture)

(defn- valid-id? [x]
  (and (string? x) (seq x) (kg-schema/valid-node-id? x)))

(deftrifecta-from-schema validate-node-id-refuses-exactly-the-bad-ids
  #'q/validate-node-id
  ;; Half the cases are id strings, so the :valid arm is reached often.
  {:in [:cat [:or ks/NodeIdText ks/NodeIdArg] [:enum "from" "to" "node_id"]]
   :out [:maybe ks/NodeIdError]
   :rel (fn [[id param] out]
          (if (valid-id? id)
            (nil? out)
            (and (some? out) (.startsWith ^String (:error out) param))))
   :classify (fn [[id] _] (if (valid-id? id) :valid :refused))
   :classify-domain #{:valid :refused}
   :golden-path "test/golden/hive-mcp/tools/kg/validate-node-id.edn"
   :num-tests 200})

(deftrifecta-from-schema parse-relations-filter-normalizes-to-a-set
  #'q/parse-relations-filter
  {:in [:cat ks/Relations]
   :out ks/RelationSet
   :rel (fn [[r] out]
          (cond
            (nil? r)    (nil? out)
            (set? r)    (= r out)
            (coll? r)   (= (set (map keyword r)) out)
            (string? r) (= #{(keyword r)} out)
            :else       (nil? out)))
   :golden-path "test/golden/hive-mcp/tools/kg/parse-relations-filter.edn"
   :num-tests 200})

(defn- refusal-or-success?
  "A handler answers the bare node-id error for a refused id, and a JSON
   success otherwise (the store is empty but healthy)."
  [ids out]
  (if (every? valid-id? ids)
    (and (m/validate core/McpResponse out)
         (true? (:success (json/read-str (:text out) :key-fn keyword))))
    (m/validate ks/NodeIdError out)))

(deftrifecta-from-schema traverse-refuses-or-walks
  #'q/handle-kg-traverse
  {:in [:cat ks/TraverseParams]
   :out ks/KgResponse
   :rel (fn [[p] out] (refusal-or-success? [(:start_node p)] out))
   :golden-path "test/golden/hive-mcp/tools/kg/handle-kg-traverse.edn"
   :num-tests 60})

(deftrifecta-from-schema impact-analysis-refuses-or-answers
  #'q/handle-kg-impact-analysis
  {:in [:cat ks/ImpactParams]
   :out ks/KgResponse
   :rel (fn [[p] out] (refusal-or-success? [(:node_id p)] out))
   :golden-path "test/golden/hive-mcp/tools/kg/handle-kg-impact-analysis.edn"
   :num-tests 60})

(deftrifecta-from-schema find-path-refuses-or-answers
  #'q/handle-kg-find-path
  {:in [:cat ks/FindPathParams]
   :out ks/KgResponse
   :rel (fn [[p] out] (refusal-or-success? [(:from_node p) (:to_node p)] out))
   :golden-path "test/golden/hive-mcp/tools/kg/handle-kg-find-path.edn"
   :num-tests 60})

(deftrifecta-from-schema node-context-refuses-or-answers
  #'q/handle-kg-node-context
  {:in [:cat ks/NodeParams]
   :out ks/KgResponse
   :rel (fn [[p] out] (refusal-or-success? [(:node_id p)] out))
   :golden-path "test/golden/hive-mcp/tools/kg/handle-kg-node-context.edn"
   :num-tests 60})

(deftrifecta-from-schema subgraph-requires-a-scope
  #'q/handle-kg-subgraph
  {:in [:cat ks/ScopeParams]
   :out core/McpResponse
   :rel (fn [[{:keys [scope]}] out]
          (if (or (nil? scope) (= "" scope))
            (true? (:isError out))
            (not (:isError out))))
   :golden-path "test/golden/hive-mcp/tools/kg/handle-kg-subgraph.edn"
   :num-tests 60})

(deftrifecta-from-schema contradictions-default-to-global
  #'q/handle-kg-contradictions
  {:in [:cat ks/ScopeParams]
   :out core/McpResponse
   :rel (fn [[{:keys [scope]}] out]
          (= (or scope "global")
             (:scope (json/read-str (:text out) :key-fn keyword))))
   :golden-path "test/golden/hive-mcp/tools/kg/handle-kg-contradictions.edn"
   :num-tests 40})

(deftrifecta-from-schema stats-report-the-backend
  #'q/handle-kg-stats
  {:in [:cat ks/AnyParams]
   :out core/McpResponse
   :rel (fn [_ out]
          (let [body (json/read-str (:text out) :key-fn keyword)]
            (and (true? (:success body)) (map? (:backend-health body)))))
   :num-tests 20})

(deftest a-node-id-refusal-escapes-as-a-bare-map
  ;; BUG (recorded, not fixed): the validation refusal is not an MCP error.
  (is (= {:error "start_node must be a non-empty string"}
         (q/handle-kg-traverse {:start_node nil})))
  (is (not (m/validate core/McpResponse (q/handle-kg-traverse {:start_node "$x"})))))

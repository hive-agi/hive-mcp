(ns hive-mcp.tools.kg.queries
  "KG read-only query handlers for traversal, impact analysis, and graph stats."
  (:require [hive-mcp.tools.core :as core :refer [mcp-json mcp-error]]
            [hive-mcp.knowledge-graph.connection :as connection]
            [hive-mcp.knowledge-graph.edges :as edges]
            [hive-mcp.knowledge-graph.queries :as queries]
            [hive-mcp.knowledge-graph.scope :as scope]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive-mcp.knowledge-graph.schema :as kg-schema]
            [malli.core :as m]
            [hive-mcp.tools.kg.schema :as ks]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn validate-node-id
  "Validate node ID is a non-empty string that kg-schema/NodeId accepts, so an
   unresolved $ref or $placeholder is refused at the tool boundary."
  [node-id param-name]
  (cond
    (or (nil? node-id) (not (string? node-id)) (empty? node-id))
    {:error (str param-name " must be a non-empty string")}

    (not (kg-schema/valid-node-id? node-id))
    {:error (str param-name " is an unresolved reference or placeholder ("
                 node-id "), not a node id")}))

(m/=> validate-node-id [:=> [:cat ks/NodeIdArg :string] [:maybe ks/NodeIdError]])

(defn refuse-node-id
  "validate-node-id's refusal as an MCP error result (:isError true), or nil
   for an id the handler may use. Handlers return this, never the bare map."
  [node-id param-name]
  (some-> (validate-node-id node-id param-name) :error mcp-error))

(m/=> refuse-node-id [:=> [:cat ks/NodeIdArg :string] [:maybe ks/NodeIdRefusal]])

(defn parse-relations-filter
  "Parse relations filter from string or array to set of keywords."
  [relations]
  (cond
    (nil? relations) nil
    (set? relations) relations
    (coll? relations) (set (map keyword relations))
    (string? relations) #{(keyword relations)}
    :else nil))

(m/=> parse-relations-filter [:=> [:cat ks/Relations] ks/RelationSet])

(defn- traverse-empty-hint
  "Diagnostic hint when traverse returns 0 results.
   Checks whether the node has any edges at all vs filters excluding them."
  [start_node {:keys [direction relations scope]}]
  (let [all-from (edges/get-edges-from start_node)
        all-to   (edges/get-edges-to start_node)
        total    (+ (count all-from) (count all-to))]
    (cond
      (zero? total)
      "Node has no edges in KG. It may not have been enriched yet."

      (and relations (not= direction :both))
      (str "Node has " total " edges total, but none match the direction/relation filter.")

      :else
      (str "Node has " total " edges total, but none match the current filters "
           "(direction=" (name (or direction :outgoing))
           (when relations (str ", relations=" relations))
           (when scope (str ", scope=" scope))
           ")."))))

(defn- refuse-limit
  "An MCP error for a `limit` that is present but not a positive integer, or
   nil. A limit is a row cap; zero or a negative number would silently answer
   nothing, so it is refused with what to send instead."
  [limit]
  (when (and (some? limit) (not (pos-int? limit)))
    (mcp-error (str "limit must be a positive integer (got " (pr-str limit)
                    "); omit it to get every row"))))

(defn- cap-rows
  "Take at most `limit` of `rows`. Returns [kept total truncated?]."
  [limit rows]
  (let [total (count rows)
        kept  (if limit (vec (take limit rows)) (vec rows))]
    [kept total (< (count kept) total)]))

(defn- coerce-number
  "A caller's number as it may arrive on a path without wire coercion: a
   number stays, a numeric string is parsed, anything else is returned as is
   (so the refusal names what was sent). An integral double becomes a long
   when `integral?`."
  [x integral?]
  (let [n (cond
            (number? x) x
            (and (string? x) (re-matches #"\s*-?\d+(\.\d+)?\s*" x))
            (parse-double (str/trim x))
            :else x)]
    (if (and integral? (number? n) (== n (Math/rint (double n)))
             (< (Math/abs (double n)) 1e15))
      (long n)
      n)))

(defn- refuse-min-confidence
  "An MCP error for a `min_confidence` that is present but not a number, or nil."
  [min-confidence]
  (when (and (some? min-confidence) (not (number? min-confidence)))
    (mcp-error (str "min_confidence must be a number between 0.0 and 1.0 (got "
                    (pr-str min-confidence) "); omit it to keep every confidence"))))

(defn- row-filter
  "A predicate over traverse rows for the row filters a caller may send:
   `exclude_relations` (names or keywords) and `min_confidence` (an edge with
   no confidence counts as 1.0). nil when neither is present.

   These run BEFORE `limit`, so the cap counts rows that survive them. The
   hive-knowledge `kg traverse` wrapper filters on the same params after this
   handler answers; filtering here first keeps its result from being cut
   before it filters (a limit 5 over 10 excluded rows used to answer 0)."
  [exclude-relations min-confidence]
  (let [excl (when (seq exclude-relations)
               (set (map keyword (if (coll? exclude-relations)
                                   exclude-relations
                                   [exclude-relations]))))]
    (when (or excl min-confidence)
      (fn [{:keys [edge]}]
        (and (not (and excl (contains? excl (keyword (:kg-edge/relation edge)))))
             (or (nil? min-confidence)
                 (>= (double (or (:kg-edge/confidence edge) 1.0))
                     (double min-confidence))))))))

(defn handle-kg-traverse
  "Walk graph from a starting node via BFS. Rows are filtered by
   `exclude_relations` and `min_confidence` first, then `limit` caps what is
   answered; a capped answer carries :truncated? true and :total-count (the
   filtered row count). `limit` caps the payload only: the BFS still walks
   to max_depth. A numeric string (\"3\", \"0.5\") is read as its number, for
   paths that skip wire coercion. A `limit` that is not a positive integer is
   refused (it used to mean \"no limit\" on the hive-knowledge path); omit it
   instead."
  [{:keys [start_node direction relations max_depth scope exclude_relations]
    raw-limit :limit raw-min-confidence :min_confidence}]
  (let [limit          (some-> raw-limit (coerce-number true))
        min-confidence (some-> raw-min-confidence (coerce-number false))]
    (log/info "kg_traverse" {:start start_node :direction direction :limit limit})
    (try
      (or (refuse-node-id start_node "start_node")
          (refuse-limit limit)
          (refuse-min-confidence min-confidence)
          (let [dir-kw (case direction
                         (:incoming "incoming") :incoming
                         (:both "both")         :both
                         :outgoing)  ;; default
                rel-set (parse-relations-filter relations)
                opts (cond-> {:direction dir-kw}
                       rel-set (assoc :relations rel-set)
                       max_depth (assoc :max-depth max_depth)
                       scope (assoc :scope scope))
                keep? (row-filter exclude_relations min-confidence)
                rows  (cond->> (queries/traverse start_node opts)
                        keep? (filter keep?))
                [results total truncated?] (cap-rows limit rows)
                response (cond-> {:success true
                                  :start-node start_node
                                  :result-count (count results)
                                  :results (mapv (fn [{:keys [node-id edge depth path]}]
                                                   {:node-id node-id
                                                    :relation (:kg-edge/relation edge)
                                                    :confidence (:kg-edge/confidence edge)
                                                    :depth depth
                                                    :path path})
                                                 results)}
                           truncated?
                           (assoc :truncated? true :total-count total)

                           (empty? results)
                           (assoc :hint (traverse-empty-hint start_node opts)))]
            (mcp-json response)))
      (catch Exception e
        (log/error e "kg_traverse failed")
        (mcp-error (str "Traversal failed: " (.getMessage e)))))))

(m/=> handle-kg-traverse [:=> [:cat ks/TraverseParams] ks/KgResponse])

(defn handle-kg-impact-analysis
  "Find all nodes that depend on a given node."
  [{:keys [node_id max_depth scope]}]
  (log/info "kg_impact_analysis" {:node node_id})
  (try
    (or (refuse-node-id node_id "node_id")
        (let [opts (cond-> {}
                     max_depth (assoc :max-depth max_depth)
                     scope (assoc :scope scope))
              result (queries/impact-analysis node_id opts)]
          (mcp-json {:success true
                     :node-id node_id
                     :direct-count (count (:direct result))
                     :transitive-count (count (:transitive result))
                     :total-count (:total-count result)
                     :direct (:direct result)
                     :transitive (:transitive result)
                     :by-relation (:by-relation result)})))
    (catch Exception e
      (log/error e "kg_impact_analysis failed")
      (mcp-error (str "Impact analysis failed: " (.getMessage e))))))

(m/=> handle-kg-impact-analysis [:=> [:cat ks/ImpactParams] ks/KgResponse])

(defn handle-kg-find-path
  "Find shortest path between two nodes."
  [{:keys [from_node to_node direction relations max_depth scope]}]
  (log/info "kg_find_path" {:from from_node :to to_node})
  (try
    (or (refuse-node-id from_node "from_node")
        (refuse-node-id to_node "to_node")
        (let [dir-kw (case direction
                       (:outgoing "outgoing") :outgoing
                       (:incoming "incoming") :incoming
                       :both)
              rel-set (parse-relations-filter relations)
              opts (cond-> {:direction dir-kw}
                     rel-set (assoc :relations rel-set)
                     max_depth (assoc :max-depth max_depth)
                     scope (assoc :scope scope))
              result (queries/find-path from_node to_node opts)]
          (if result
            (mcp-json {:success true
                       :path-exists true
                       :path (:path result)
                       :length (:length result)
                       :edges (mapv #(select-keys % [:kg-edge/id :kg-edge/relation
                                                     :kg-edge/confidence])
                                    (:edges result))})
            (mcp-json {:success true
                       :path-exists false
                       :message "No path found between nodes"}))))
    (catch Exception e
      (log/error e "kg_find_path failed")
      (mcp-error (str "Path finding failed: " (.getMessage e))))))

(m/=> handle-kg-find-path [:=> [:cat ks/FindPathParams] ks/KgResponse])

(defn handle-kg-subgraph
  "Extract subgraph visible from a scope."
  [{:keys [scope]}]
  (log/info "kg_subgraph" {:scope scope})
  (try
    (if (or (nil? scope) (empty? scope))
      (mcp-error "scope is required")
      (let [result (queries/subgraph scope)]
        (mcp-json {:success true
                   :scope scope
                   :visible-scopes (scope/visible-scopes scope)
                   :node-count (:node-count result)
                   :edge-count (:edge-count result)
                   :nodes (:nodes result)
                   :edges (mapv #(select-keys % [:kg-edge/id :kg-edge/from
                                                 :kg-edge/to :kg-edge/relation
                                                 :kg-edge/confidence :kg-edge/scope])
                                (:edges result))})))
    (catch Exception e
      (log/error e "kg_subgraph failed")
      (mcp-error (str "Subgraph extraction failed: " (.getMessage e))))))

(m/=> handle-kg-subgraph [:=> [:cat ks/ScopeParams] core/McpResponse])

(defn handle-kg-contradictions
  "Find edges with :contradicts relation in scope."
  [{:keys [scope]}]
  (log/info "kg_contradictions" {:scope scope})
  (try
    (let [results (queries/find-contradictions scope)]
      (mcp-json {:success true
                 :scope (or scope "global")
                 :count (count results)
                 :contradictions results}))
    (catch Exception e
      (log/error e "kg_contradictions failed")
      (mcp-error (str "Contradiction search failed: " (.getMessage e))))))

(m/=> handle-kg-contradictions [:=> [:cat ks/ScopeParams] core/McpResponse])

(defn handle-kg-node-context
  "Get full context for a node: incoming, outgoing, confidence stats."
  [{:keys [node_id]}]
  (log/info "kg_node_context" {:node node_id})
  (try
    (or (refuse-node-id node_id "node_id")
        (let [result (queries/get-node-context node_id)]
          (mcp-json {:success true
                     :node-id node_id
                     :incoming (-> (:incoming result)
                                   (update :edges #(mapv (fn [e]
                                                           (select-keys e [:kg-edge/id
                                                                           :kg-edge/from
                                                                           :kg-edge/relation
                                                                           :kg-edge/confidence]))
                                                         %)))
                     :outgoing (-> (:outgoing result)
                                   (update :edges #(mapv (fn [e]
                                                           (select-keys e [:kg-edge/id
                                                                           :kg-edge/to
                                                                           :kg-edge/relation
                                                                           :kg-edge/confidence]))
                                                         %)))
                     :confidence (:confidence result)
                     :scopes (:scopes result)})))
    (catch Exception e
      (log/error e "kg_node_context failed")
      (mcp-error (str "Context retrieval failed: " (.getMessage e))))))

(m/=> handle-kg-node-context [:=> [:cat ks/NodeParams] ks/KgResponse])

(defn handle-kg-stats
  "Get statistics about the Knowledge Graph."
  [_]
  (log/info "kg_stats")
  (try
    (let [stats (edges/edge-stats)]
      (mcp-json {:success true
                 :total-edges (:total-edges stats)
                 :by-relation (:by-relation stats)
                 :by-scope (:by-scope stats)
                 :backend-health (connection/backend-health)}))
    (catch Exception e
      (log/error e "kg_stats failed")
      (mcp-error (str "Stats retrieval failed: " (.getMessage e))))))

(m/=> handle-kg-stats [:=> [:cat ks/AnyParams] core/McpResponse])

(def query-tools
  "Tool definitions for KG read-only operations."
  [{:name "kg_traverse"
    :description "Walk the Knowledge Graph from a starting node using BFS. Returns nodes reachable within max_depth, with paths and edge information."
    :inputSchema {:type "object"
                  :properties {"start_node" {:type "string"
                                             :description "Node ID to start traversal from"}
                               "direction" {:type "string"
                                            :enum ["outgoing" "incoming" "both"]
                                            :description "Edge direction to follow (default: outgoing)"}
                               "relations" {:type "array"
                                            :items {:type "string"}
                                            :description "Relation types to follow (default: all)"}
                               "max_depth" {:type "integer"
                                            :description "Maximum traversal depth (default: 3)"}
                               "scope" {:type "string"
                                        :description "Limit to edges visible from this scope"}
                               "limit" {:type "integer"
                                        :description "Max rows to return; a capped answer carries truncated? and total-count"}}
                  :required ["start_node"]}
    :handler handle-kg-traverse}

   {:name "kg_impact_analysis"
    :description "Find all nodes that depend on given node. Use before modifying or deleting a memory entry to understand impact. Returns direct and transitive dependents."
    :inputSchema {:type "object"
                  :properties {"node_id" {:type "string"
                                          :description "Node to analyze"}
                               "max_depth" {:type "integer"
                                            :description "Max depth for transitive deps (default: 5)"}
                               "scope" {:type "string"
                                        :description "Limit to visible scopes"}}
                  :required ["node_id"]}
    :handler handle-kg-impact-analysis}

   {:name "kg_find_path"
    :description "Find shortest path between two nodes in the Knowledge Graph. Returns path and edges if exists."
    :inputSchema {:type "object"
                  :properties {"from_node" {:type "string"
                                            :description "Source node ID"}
                               "to_node" {:type "string"
                                          :description "Target node ID"}
                               "direction" {:type "string"
                                            :enum ["outgoing" "incoming" "both"]
                                            :description "Edge direction (default: both)"}
                               "relations" {:type "array"
                                            :items {:type "string"}
                                            :description "Relation types to follow"}
                               "max_depth" {:type "integer"
                                            :description "Max search depth (default: 10)"}
                               "scope" {:type "string"
                                        :description "Limit to visible scopes"}}
                  :required ["from_node" "to_node"]}
    :handler handle-kg-find-path}

   {:name "kg_subgraph"
    :description "Extract subgraph visible from a scope. Returns all nodes and edges accessible from that scope level."
    :inputSchema {:type "object"
                  :properties {"scope" {:type "string"
                                        :description "Scope to extract (e.g., 'hive-mcp:agora')"}}
                  :required ["scope"]}
    :handler handle-kg-subgraph}

   {:name "kg_contradictions"
    :description "Find knowledge contradictions in scope. Returns pairs of nodes connected by :contradicts edges."
    :inputSchema {:type "object"
                  :properties {"scope" {:type "string"
                                        :description "Scope to search (optional, default: all)"}}
                  :required []}
    :handler handle-kg-contradictions}

   {:name "kg_node_context"
    :description "Get full context for a node: incoming edges, outgoing edges, confidence stats, and scopes. Useful for understanding a node's role in the graph."
    :inputSchema {:type "object"
                  :properties {"node_id" {:type "string"
                                          :description "Node to get context for"}}
                  :required ["node_id"]}
    :handler handle-kg-node-context}

   {:name "kg_stats"
    :description "Get Knowledge Graph statistics: total edges, counts by relation type, counts by scope."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-kg-stats}])

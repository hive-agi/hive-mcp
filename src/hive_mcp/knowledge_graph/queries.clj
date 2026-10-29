(ns hive-mcp.knowledge-graph.queries
  "Graph queries for Knowledge Graph traversal and analysis.

   Provides traversal algorithms and analytical queries over the KG:
   - traverse: Walk graph from starting node (BFS)
   - find-path: Find shortest path between nodes
   - impact-analysis: Find all dependents of a node
   - subgraph: Extract subgraph visible from a scope
   - find-contradictions: Locate conflicting knowledge
   - get-node-context: Full context for node display
   - batch-get-node-contexts: Batch version for N+1 elimination"

  (:require [malli.core :as m]
            [clojure.set :as set]
            [hive-mcp.knowledge-graph.edges :as edges]
            [hive-mcp.knowledge-graph.edges.scoped :as scoped]
            [hive-mcp.knowledge-graph.scope :as scope]
            [taoensso.timbre :as log]
            [hive-mcp.knowledge-graph.protocols :as reader]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Configuration
;;; =============================================================================

(def ^:private default-max-depth
  "Default maximum traversal depth to prevent infinite loops."
  3)

(def ^:private dependency-relations
  "Relations that indicate dependency (for impact analysis).
   Includes :refines - when A refines B, and B becomes stale, A should also become stale."
  #{:depends-on :implements :derived-from :refines})

;;; =============================================================================
;;; Helper Functions
;;; =============================================================================

(def ^:private edge-matches-scope?
  "Edge visible from scope-set (nil = all); see edges.scoped/visible?."
  scoped/visible?)

(defn- edge-matches-relations?
  "Check if edge relation is in the allowed set.
   If no relations filter, all edges match."
  [edge relations]
  (if (nil? relations)
    true
    (contains? relations (:kg-edge/relation edge))))

(defn- get-adjacent-edges
  "Get edges adjacent to a node in the specified direction.

   Direction:
   - :outgoing - edges FROM this node
   - :incoming - edges TO this node
   - :both - all connected edges"
  [node-id direction]
  (case direction
    :outgoing (edges/get-edges-from node-id)
    :incoming (edges/get-edges-to node-id)
    :both (concat (edges/get-edges-from node-id)
                  (edges/get-edges-to node-id))))

(defn- edge->neighbor
  "Extract neighbor node ID from edge relative to current node.
   For outgoing edges: neighbor is :kg-edge/to
   For incoming edges: neighbor is :kg-edge/from"
  [edge current-node-id]
  (if (= (:kg-edge/from edge) current-node-id)
    (:kg-edge/to edge)
    (:kg-edge/from edge)))

;;; =============================================================================
;;; Traversal (BFS)
;;; =============================================================================

(defn traverse
  "Breadth-first traversal with at most 128 frontier nodes per batch.
   Options: :direction (:outgoing, :incoming, :both), :relations (set),
   :max-depth (default 3), :scope (visible scopes), :edge-reader (IEdgeReader).
   Returns {:node-id :edge :depth :path} rows. No cross-request cache."
  [start-node-id opts]
  {:pre [(string? start-node-id)]}
  (let [{:keys [direction relations max-depth scope edge-reader]
         :or {direction :outgoing
              max-depth default-max-depth
              edge-reader edges/default-reader}} opts
        scope-set (when scope (scope/visible-scope-tags scope))]
    (loop [queue [[start-node-id 0 [start-node-id]]]
           visited #{start-node-id}
           results []]
      (if (or (empty? queue) (>= (second (first queue)) max-depth))
        results
        (let [depth (second (first queue))
              ;; Never prefetch the next depth or an unbounded frontier.
              chunk (vec (take 128 (take-while #(= depth (second %)) queue)))
              ids (mapv first chunk)
              outgoing (when (#{:outgoing :both} direction)
                         (reader/batch-get-edges-from edge-reader ids))
              incoming (when (#{:incoming :both} direction)
                         (reader/batch-get-edges-to edge-reader ids))
              [visited results added]
              (reduce
               (fn [[seen rows added] [node depth path]]
                 (let [adjacent (case direction
                                  :outgoing (get outgoing node)
                                  :incoming (get incoming node)
                                  :both (concat (get outgoing node) (get incoming node)))
                       neighbors (->> adjacent
                                      (filter #(edge-matches-scope? % scope-set))
                                      (filter #(edge-matches-relations? % relations))
                                      (map #(vector (edge->neighbor % node) %))
                                      (remove #(contains? seen (first %)))
                                      vec)]
                   [(into seen (map first neighbors))
                    (into rows (map (fn [[id edge]]
                                      {:node-id id :edge edge :depth (inc depth)
                                       :path (conj path id)}) neighbors))
                    (into added (map (fn [[id _]] [id (inc depth) (conj path id)]) neighbors))]))
               [visited results []] chunk)]
          (recur (into (vec (drop (count chunk) queue)) added)
                 visited results))))))

;;; =============================================================================
;;; Shortest Path (BFS)
;;; =============================================================================

(defn find-path
  "Find shortest path between two nodes.

   Arguments:
     from-node - Source node ID
     to-node   - Target node ID
     opts      - Optional map with:
                 :direction  - :outgoing (default), :incoming, or :both
                 :relations  - Set of relations to follow
                 :max-depth  - Maximum search depth (default: 10)
                 :scope      - Limit to visible scopes

   Returns:
     {:path [<node-ids>] :edges [<edge-maps>] :length n}
     or nil if no path exists"
  [from-node to-node opts]
  {:pre [(string? from-node) (string? to-node)]}
  (when (= from-node to-node)
    (log/debug "find-path: from=to, returning trivial path")
    {:path [from-node] :edges [] :length 0})
  (let [{:keys [direction relations max-depth scope]
         :or {direction :both
              max-depth 10}} opts
        scope-set (when scope (scope/visible-scope-tags scope))]
    (log/debug "Finding path from:" from-node "to:" to-node)
    (loop [queue [[from-node [from-node] []]]  ;; [node, path, edges]
           visited #{from-node}]
      (cond
        (empty? queue)
        (do (log/debug "No path found from" from-node "to" to-node)
            nil)

        (> (count (second (first queue))) (inc max-depth))
        (do (log/debug "Max depth reached, no path found")
            nil)

        :else
        (let [[current path path-edges] (first queue)
              remaining-queue (rest queue)]
          (if (= current to-node)
            ;; Found the target
            {:path path :edges path-edges :length (dec (count path))}
            ;; Explore neighbors
            (let [adjacent-edges (get-adjacent-edges current direction)
                  matching-edges (->> adjacent-edges
                                      (filter #(edge-matches-scope? % scope-set))
                                      (filter #(edge-matches-relations? % relations)))
                  new-neighbors (->> matching-edges
                                     (map #(vector (edge->neighbor % current) %))
                                     (filter #(not (contains? visited (first %)))))
                  new-queue-entries (mapv (fn [[neighbor edge]]
                                            [neighbor
                                             (conj path neighbor)
                                             (conj path-edges edge)])
                                          new-neighbors)
                  new-visited (into visited (map first new-neighbors))]
              (recur (into (vec remaining-queue) new-queue-entries)
                     new-visited))))))))

;;; =============================================================================
;;; Impact Analysis
;;; =============================================================================

(defn impact-analysis
  "Find all nodes that depend on given node.
   Useful before modifying/deleting an entry.

   Arguments:
     node-id - Node to analyze
     opts    - Optional map with:
               :max-depth - Limit transitive depth (default: 5)
               :scope     - Limit to visible scopes

   Returns:
     {:direct [<node-ids>]
      :transitive [<node-ids excluding direct>]
      :total-count n
      :by-relation {<relation> [<node-ids>]}}"
  [node-id & [opts]]
  {:pre [(string? node-id)]}
  (let [{:keys [max-depth scope]
         :or {max-depth 5}} opts
        ;; Direct dependents: nodes with outgoing dependency edges TO this node
        ;; (incoming edges from perspective of node-id)
        direct-edges (edges/get-edges-to node-id)
        direct-dependent-edges (filter #(contains? dependency-relations
                                                   (:kg-edge/relation %))
                                       direct-edges)
        direct-nodes (set (map :kg-edge/from direct-dependent-edges))

        ;; Transitive dependents via traversal
        ;; Follow incoming dependency relations (who depends on nodes that depend on me)
        traversal-results (traverse node-id
                                    {:direction :incoming
                                     :relations dependency-relations
                                     :max-depth max-depth
                                     :scope scope})
        all-transitive (set (map :node-id traversal-results))
        transitive-only (set/difference all-transitive direct-nodes)

        ;; Group by relation type
        by-relation (reduce (fn [acc {:keys [node-id edge]}]
                              (update acc (:kg-edge/relation edge)
                                      (fnil conj []) node-id))
                            {}
                            traversal-results)]
    (log/info "Impact analysis for" node-id
              ": direct=" (count direct-nodes)
              "transitive=" (count transitive-only))
    {:direct (vec direct-nodes)
     :transitive (vec transitive-only)
     :total-count (+ (count direct-nodes) (count transitive-only))
     :by-relation by-relation}))

;;; =============================================================================
;;; Subgraph Extraction
;;; =============================================================================

(defn subgraph
  "Extract subgraph visible from a scope.

   Arguments:
     scope - Scope string (e.g., 'hive-mcp:agora')
     opts  - Optional map:
             :node-id   - only the neighborhood of this node: edges reachable
                          from it (either direction) through visible edges
             :max-depth - hop bound for :node-id (default 3)

   Reads the scope index (edges.scoped/visible-edges) or, with :node-id,
   walks per-node index lookups hop by hop (edges.scoped/neighborhood-edges);
   neither pulls an edge that is not returned.

   Returns:
     {:nodes [<unique node IDs>]
      :edges [<edge maps>]
      :node-count n
      :edge-count n}"
  ([scope] (subgraph scope nil))
  ([scope {:keys [node-id max-depth] :or {max-depth default-max-depth}}]
   {:pre [(or (nil? scope) (string? scope))]}
   (let [scope-set (set (scope/visible-scope-tags scope))
         visible-edges (if node-id
                         (scoped/neighborhood-edges node-id {:scope-set scope-set
                                                             :max-depth max-depth})
                         (scoped/visible-edges scope-set))
         nodes-from-edges (set (concat (map :kg-edge/from visible-edges)
                                       (map :kg-edge/to visible-edges)))]
     (log/debug "Subgraph for scope:" scope
                "- edges:" (count visible-edges)
                "- nodes:" (count nodes-from-edges))
     {:nodes (vec nodes-from-edges)
      :edges visible-edges
      :node-count (count nodes-from-edges)
      :edge-count (count visible-edges)})))

;;; =============================================================================
;;; Contradiction Detection
;;; =============================================================================

(defn find-contradictions
  "Find edges with :contradicts relation in scope.

   Arguments:
     scope - Optional scope to limit search

   Returns:
     Sequence of {:node-a <id> :node-b <id> :edge <edge-map>}"
  [& [scope]]
  (let [scope-set (when scope (scope/visible-scope-tags scope))
        contradiction-edges (edges/get-edges-by-relation :contradicts)
        visible-contradictions (filter #(edge-matches-scope? % scope-set)
                                       contradiction-edges)]
    (log/debug "Found" (count visible-contradictions) "contradictions"
               (when scope (str "in scope " scope)))
    (mapv (fn [edge]
            {:node-a (:kg-edge/from edge)
             :node-b (:kg-edge/to edge)
             :edge edge
             :confidence (:kg-edge/confidence edge)
             :scope (:kg-edge/scope edge)})
          visible-contradictions)))

;;; =============================================================================
;;; Node Context
;;; =============================================================================

(defn get-node-context
  "Get full context for a node: incoming, outgoing, confidence stats.
   Useful for displaying node details.

   Arguments:
     node-id - Node to get context for

   Returns:
     {:node-id <id>
      :incoming {:count n :edges [...] :by-relation {...}}
      :outgoing {:count n :edges [...] :by-relation {...}}
      :confidence {:avg n :min n :max n}
      :scopes [<unique scopes>]}"
  [node-id]
  {:pre [(string? node-id)]}
  (let [incoming (edges/get-edges-to node-id)
        outgoing (edges/get-edges-from node-id)
        all-edges (into incoming outgoing)

        ;; Group by relation
        incoming-by-rel (group-by :kg-edge/relation incoming)
        outgoing-by-rel (group-by :kg-edge/relation outgoing)

        ;; Confidence stats
        confidences (keep :kg-edge/confidence all-edges)
        confidence-stats (when (seq confidences)
                           {:avg (/ (reduce + confidences) (count confidences))
                            :min (apply min confidences)
                            :max (apply max confidences)})

        ;; Unique scopes
        scopes (set (keep :kg-edge/scope all-edges))]
    (log/debug "Node context for" node-id
               ": incoming=" (count incoming)
               "outgoing=" (count outgoing))
    {:node-id node-id
     :incoming {:count (count incoming)
                :edges incoming
                :by-relation (into {} (map (fn [[k v]] [k (count v)])
                                           incoming-by-rel))}
     :outgoing {:count (count outgoing)
                :edges outgoing
                :by-relation (into {} (map (fn [[k v]] [k (count v)])
                                           outgoing-by-rel))}
     :confidence confidence-stats
     :scopes (vec scopes)}))

;;; =============================================================================
;;; Batch Node Context (N+1 elimination)
;;; =============================================================================

(defn batch-get-node-contexts
  "Get full context for multiple nodes in 2 queries instead of 2*N.
   Uses batch edge queries with DataScript/Datalevin collection bindings.

   Arguments:
     node-ids - Collection of node IDs to get context for

   Returns:
     Map of {node-id -> context-map} where context-map has the same shape
     as get-node-context output:
       {:node-id <id>
        :incoming {:count n :edges [...] :by-relation {...}}
        :outgoing {:count n :edges [...] :by-relation {...}}
        :confidence {:avg n :min n :max n}
        :scopes [<unique scopes>]}"
  [node-ids]
  (if (empty? node-ids)
    {}
    (let [ids (vec (distinct node-ids))
          ;; 2 queries total instead of 2*N
          edges-from-map (edges/batch-get-edges-from ids)
          edges-to-map   (edges/batch-get-edges-to ids)]
      (into {}
            (map (fn [node-id]
                   (let [incoming (get edges-to-map node-id [])
                         outgoing (get edges-from-map node-id [])
                         all-edges (into incoming outgoing)

                         incoming-by-rel (group-by :kg-edge/relation incoming)
                         outgoing-by-rel (group-by :kg-edge/relation outgoing)

                         confidences (keep :kg-edge/confidence all-edges)
                         confidence-stats (when (seq confidences)
                                           {:avg (/ (reduce + confidences)
                                                    (count confidences))
                                            :min (apply min confidences)
                                            :max (apply max confidences)})

                         scopes (set (keep :kg-edge/scope all-edges))]
                     [node-id
                      {:node-id node-id
                       :incoming {:count (count incoming)
                                  :edges incoming
                                  :by-relation (into {} (map (fn [[k v]] [k (count v)])
                                                             incoming-by-rel))}
                       :outgoing {:count (count outgoing)
                                  :edges outgoing
                                  :by-relation (into {} (map (fn [[k v]] [k (count v)])
                                                             outgoing-by-rel))}
                       :confidence confidence-stats
                       :scopes (vec scopes)}]))
                 ids)))))

;;; =============================================================================
;;; Connected Components (Utility)
;;; =============================================================================

(defn find-connected-nodes
  "Find all nodes connected to a given node (reachable in either direction).

   Arguments:
     node-id - Starting node
     opts    - Optional map with :max-depth

   Returns:
     Set of connected node IDs (including start node)"
  [node-id & [opts]]
  {:pre [(string? node-id)]}
  (let [max-depth (get opts :max-depth 10)
        results (traverse node-id {:direction :both
                                   :max-depth max-depth})]
    (into #{node-id} (map :node-id results))))

;;; =============================================================================
;;; Orphan Detection
;;; =============================================================================

(defn find-orphan-nodes
  "Find nodes in the graph that have no connections.

   Note: This requires knowing all node IDs, which are stored in Chroma.
   This function only finds nodes that appear in edges but have no other connections.
   For full orphan detection, integrate with Chroma memory query.

   Degrees come from index-only aggregates over :kg-edge/from and
   :kg-edge/to (edges.scoped/node-occurrences); no edge is pulled.

   Returns:
     Set of node IDs that appear only once in the graph (potential orphans)"
  []
  (into #{}
        (keep (fn [[node-id n]] (when (= 1 n) node-id)))
        (scoped/node-occurrences)))

(m/=> traverse [:=> [:cat :string :map] [:sequential [:map [:node-id :string] [:edge :map] [:depth [:int {:min 0}]] [:path [:vector :string]]]]])
(m/=> find-path [:=> [:cat :string :string :map] [:maybe [:map [:path [:vector :string]] [:edges [:vector :map]] [:length [:int {:min 0}]]]]])
(m/=> impact-analysis [:function [:=> [:cat :string] [:map [:direct [:sequential :string]] [:transitive [:sequential :string]] [:total-count [:int {:min 0}]] [:by-relation :map]]] [:=> [:cat :string [:maybe :map]] [:map [:direct [:sequential :string]] [:transitive [:sequential :string]] [:total-count [:int {:min 0}]] [:by-relation :map]]]])
(m/=> subgraph [:function
                [:=> [:cat [:maybe :string]] [:map [:nodes [:sequential :string]] [:edges [:sequential :map]] [:node-count [:int {:min 0}]] [:edge-count [:int {:min 0}]]]]
                [:=> [:cat [:maybe :string] [:maybe [:map [:node-id {:optional true} [:maybe :string]] [:max-depth {:optional true} nat-int?]]]]
                 [:map [:nodes [:sequential :string]] [:edges [:sequential :map]] [:node-count [:int {:min 0}]] [:edge-count [:int {:min 0}]]]]])
(m/=> find-contradictions [:function [:=> [:cat] [:sequential [:map [:node-a :string] [:node-b :string] [:edge :map]]]] [:=> [:cat [:maybe :string]] [:sequential [:map [:node-a :string] [:node-b :string] [:edge :map]]]]])
(m/=> get-node-context [:=> [:cat :string] [:map [:node-id :string] [:incoming :map] [:outgoing :map] [:confidence [:maybe :map]] [:scopes [:sequential :string]]]])
(m/=> batch-get-node-contexts [:=> [:cat [:sequential :string]] [:map-of :string :map]])
(m/=> find-connected-nodes [:function [:=> [:cat :string] [:set :string]] [:=> [:cat :string [:maybe :map]] [:set :string]]])
(m/=> find-orphan-nodes [:=> [:cat] [:set :string]])

(ns hive-mcp.knowledge-graph.edges.scoped
  "Indexed reads behind the scope- and node-bounded KG views
   (`queries/subgraph`, `queries/find-orphan-nodes`).

   The former paths pulled EVERY edge (get-all-edges) and filtered in memory.
   These read the value indexes instead:

     visible-edge-eids  - one Datalog lookup over the :kg-edge/scope index
                          (AVET, norm 001) for the visible scope values, plus
                          the scope-less edges; only visible edges are pulled.
     neighborhood-edges - per-node lookups on :kg-edge/from / :kg-edge/to,
                          one batched lookup per hop, bounded by max-depth;
                          only edges within reach are pulled.
     node-occurrences   - per-node degree as index-only aggregates; no pulls.

   Every row is shaped like a `get-all-edges` row (queries/pull-edge-batch),
   and edges come back in ascending entity id order, the order get-all-edges
   enumerates on the attribute-first index."
  (:require [clojure.set :as set]
            [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.edges.queries :as queries]
            [hive-mcp.knowledge-graph.edges.schema :as es]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ScopeSet
  "Scope values an edge may carry to be visible; nil means no filter."
  [:maybe [:set :string]])

(def ^:private scoped-eids-query
  '[:find [?e ...]
    :in $ [?scope ...]
    :where
    [?e :kg-edge/scope ?scope]
    [?e :kg-edge/id]])

(def ^:private unscoped-eids-query
  '[:find [?e ...]
    :where
    [?e :kg-edge/id]
    [(missing? $ ?e :kg-edge/scope)]])

(def ^:private from-eids-query
  '[:find [?e ...]
    :in $ [?n ...]
    :where
    [?e :kg-edge/from ?n]
    [?e :kg-edge/id]])

(def ^:private to-eids-query
  '[:find [?e ...]
    :in $ [?n ...]
    :where
    [?e :kg-edge/to ?n]
    [?e :kg-edge/id]])

(def ^:private from-degree-query
  '[:find ?n (count ?e)
    :where
    [?e :kg-edge/from ?n]
    [?e :kg-edge/id]])

(def ^:private to-degree-query
  '[:find ?n (count ?e)
    :where
    [?e :kg-edge/to ?n]
    [?e :kg-edge/id]])

(defn visible?
  "True when `edge` is visible from `scope-set`: no scope-set (nil = every
   edge), an edge without a scope, or an edge whose scope is in the set."
  [edge scope-set]
  (boolean (or (nil? scope-set)
               (nil? (:kg-edge/scope edge))
               (contains? scope-set (:kg-edge/scope edge)))))

(defn visible-edge-eids
  "Entity ids of the edges visible from `scope-set` (see `visible?`),
   ascending. A set reads the :kg-edge/scope index for its values plus the
   scope-less edges; nil (no filter) walks :kg-edge/id. Pulls nothing."
  [scope-set]
  (if (nil? scope-set)
    (vec (sort (conn/eids-by-attr :kg-edge/id)))
    (let [scoped   (if (seq scope-set)
                     (conn/query scoped-eids-query (vec scope-set))
                     [])
          unscoped (conn/query unscoped-eids-query)]
      (vec (sort (distinct (concat scoped unscoped)))))))

(defn visible-edges
  "Edges visible from `scope-set`, ascending by entity id, shaped like
   get-all-edges rows. Pulls only the visible edges."
  [scope-set]
  (queries/pull-edge-batch (visible-edge-eids scope-set)))

(defn- incident-eids
  "Entity ids of edges with an endpoint in `node-ids` (one lookup per side)."
  [node-ids]
  (if (empty? node-ids)
    #{}
    (let [ids (vec node-ids)]
      (into (set (conn/query from-eids-query ids))
            (conn/query to-eids-query ids)))))

(defn- pull-by-eid
  "[[eid edge] ...] for the eids that still name an edge."
  [eids]
  (keep (fn [eid]
          (when-let [edge (first (queries/pull-edge-batch [eid]))]
            [eid edge]))
        eids))

(defn neighborhood-edges
  "Edges reachable from `node-id` within `max-depth` hops in either direction,
   following only edges visible from `scope-set` (nil = all).

   Hop k reads the edges incident to the nodes first reached at hop k-1: one
   batched :kg-edge/from and one :kg-edge/to lookup per hop, and only edges
   not seen before are pulled. Equivalent to filtering the full visible edge
   set down to every edge incident to a node at distance < max-depth.

   Returns a vector ascending by entity id, shaped like get-all-edges rows."
  [node-id {:keys [scope-set max-depth]}]
  (loop [frontier #{node-id}
         reached  #{node-id}
         seen     #{}
         acc      (sorted-map)
         depth    0]
    (if (or (empty? frontier) (>= depth max-depth))
      (vec (vals acc))
      (let [fresh   (sort (set/difference (incident-eids frontier) seen))
            kept    (filter #(visible? (second %) scope-set) (pull-by-eid fresh))
            touched (into #{} (mapcat (comp (juxt :kg-edge/from :kg-edge/to) second)) kept)
            nxt     (set/difference touched reached)]
        (recur nxt
               (into reached nxt)
               (into seen fresh)
               (into acc kept)
               (inc depth))))))

(defn node-occurrences
  "{node-id -> number of edge endpoints naming it} over every edge (a
   self-loop counts twice). Index-only degree aggregates; pulls nothing."
  []
  (merge-with +
              (into {} (conn/query from-degree-query))
              (into {} (conn/query to-degree-query))))

(m/=> visible? [:=> [:cat :map ScopeSet] :boolean])
(m/=> visible-edge-eids [:=> [:cat ScopeSet] [:vector :int]])
(m/=> visible-edges [:=> [:cat ScopeSet] [:vector es/Edge]])
(m/=> neighborhood-edges [:=> [:cat :string [:map
                                             [:scope-set {:optional true} ScopeSet]
                                             [:max-depth nat-int?]]]
                          [:vector es/Edge]])
(m/=> node-occurrences [:=> [:cat] [:map-of :string pos-int?]])

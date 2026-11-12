(ns hive-mcp.tools.kg.traverse-limit-test
  "kg traverse honours `limit` at every seam an agent calls (friction
   hypothesis 20261107124812-43cef10c), plus the contract for the class: every
   param a `kg` read verb's handler is TOLD it takes, through the consolidated
   tool's coercion table, is a param that handler declares and reads.

   Driven on the in-memory DataScript KG fixture: no live store is reached."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.edges :as edges]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-mcp.tools.kg :as kg]
            [hive-mcp.tools.kg.queries :as q]
            [hive-mcp.tools.kg.schema :as ks]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each fixtures/datascript-fixture)

(defn- hub! [n]
  (conn/with-tx-batch
    (doseq [i (range n)]
      (edges/add-edge! {:from "hub" :to (str "leaf-" i) :relation :implements}))))

(defn- body [out] (json/read-str (:text out) :key-fn keyword))

(deftest the-handler-caps-rows-at-limit-and-says-so
  (hub! 6)
  (let [b (body (q/handle-kg-traverse {:start_node "hub" :limit 3}))]
    (is (= 3 (count (:results b))) "limit 3 answers at most 3 rows")
    (is (= 3 (:result-count b)))
    (is (true? (:truncated? b)) "a cut answer says it was cut")
    (is (= 6 (:total-count b)) "and how many rows there were")))

(deftest no-limit-or-a-limit-above-the-rows-is-not-truncated
  (hub! 4)
  (doseq [args [{:start_node "hub"} {:start_node "hub" :limit 10}]]
    (let [b (body (q/handle-kg-traverse args))]
      (is (= 4 (:result-count b)) (pr-str args))
      (is (not (:truncated? b)) (pr-str args)))))

(deftest a-non-positive-limit-is-refused-with-the-remedy
  (hub! 2)
  (let [out (q/handle-kg-traverse {:start_node "hub" :limit 0})]
    (is (true? (:isError out)))
    (is (re-find #"limit" (:text out)))
    (is (re-find #"positive integer|omit" (:text out)) "names what to send instead")))

(deftest limit-survives-the-consolidated-kg-tool
  (testing "the CLI path coerces the JSON string \"3\" and the handler applies it"
    (hub! 6)
    (let [b (body (kg/handle-kg {:command "traverse" :start_node "hub" :limit "3"}))]
      (is (= 3 (:result-count b)))
      (is (true? (:truncated? b))))))

;; -----------------------------------------------------------------------------
;; Row filters run before the cap (review H1). The hive-knowledge `kg traverse`
;; wrapper hands core every param, then filters exclude_relations and
;; min_confidence and takes limit itself. `addon-post-filter` is that step,
;; copied from hive-knowledge digest_tools.clj handle-traverse-enhanced, so the
;; seam the agent calls is driven here without the addon on the classpath.
;; -----------------------------------------------------------------------------

(defn- noisy-hub!
  "10 co-accessed edges stored before 5 implements edges, the shape of a real
   node: co-accessed dominates and comes first in BFS order."
  []
  (conn/with-tx-batch
    (doseq [i (range 10)]
      (edges/add-edge! {:from "hub" :to (str "co-" i) :relation :co-accessed :confidence 0.3}))
    (doseq [i (range 5)]
      (edges/add-edge! {:from "hub" :to (str "impl-" i) :relation :implements :confidence 0.9}))))

(defn- addon-post-filter [{:keys [exclude_relations min_confidence limit]} parsed]
  (let [excl (when (seq exclude_relations) (set (map keyword exclude_relations)))
        rows (cond->> (:results parsed)
               excl           (remove #(contains? excl (keyword (:relation %))))
               min_confidence (filter #(>= (or (:confidence %) 1.0) min_confidence)))]
    (if (and limit (pos? limit)) (take limit rows) rows)))

(defn- via-addon [params]
  (let [out (kg/handle-kg-traverse params)]
    (is (not (:isError out)) (:text out))
    (addon-post-filter params (body out))))

(deftest limit-counts-rows-after-exclude-relations
  (noisy-hub!)
  (let [params {:start_node "hub" :limit 5 :exclude_relations ["co-accessed"]}
        b      (body (q/handle-kg-traverse params))]
    (is (= 5 (:result-count b)) "the 5 implements rows, not 5 co-accessed rows cut first")
    (is (every? #(= "implements" (:relation %)) (:results b)))
    (is (not (:truncated? b)) "5 rows survive the filter, so nothing was cut")
    (is (= 5 (count (via-addon params))) "and the addon's own post-filter keeps all 5"))
  (testing "a limit below the filtered count still cuts, and counts the filtered rows"
    (let [b (body (q/handle-kg-traverse {:start_node "hub" :limit 2 :exclude_relations ["co-accessed"]}))]
      (is (= 2 (:result-count b)))
      (is (true? (:truncated? b)))
      (is (= 5 (:total-count b))))))

(deftest limit-counts-rows-after-min-confidence
  (noisy-hub!)
  (let [params {:start_node "hub" :limit 5 :min_confidence 0.5}
        b      (body (q/handle-kg-traverse params))]
    (is (= 5 (:result-count b)))
    (is (every? #(>= (:confidence %) 0.5) (:results b)))
    (is (= 5 (count (via-addon params))))))

(deftest numeric-strings-are-read-as-numbers-and-junk-is-refused
  (noisy-hub!)
  (let [b (body (q/handle-kg-traverse {:start_node "hub" :limit "3" :min_confidence "0.5"}))]
    (is (= 3 (:result-count b)) "\"3\" from a path without wire coercion is 3")
    (is (every? #(= "implements" (:relation %)) (:results b))))
  (is (= 3 (:result-count (body (q/handle-kg-traverse {:start_node "hub" :limit 3.0}))))
      "an integral double is an integer")
  (doseq [[params re] [[{:start_node "hub" :limit "x"} #"limit must be a positive integer"]
                       [{:start_node "hub" :limit 2.5} #"limit must be a positive integer"]
                       [{:start_node "hub" :min_confidence "x"} #"min_confidence must be a number"]]]
    (let [out (q/handle-kg-traverse params)]
      (is (true? (:isError out)) (pr-str params))
      (is (re-find re (:text out)) (pr-str params))
      (is (re-find #"omit it" (:text out)) "the refusal names the remedy"))))

;; -----------------------------------------------------------------------------
;; Contract for the class: a param the tool boundary accepts for a verb must be
;; one the verb's handler declares. A param the boundary coerces and the
;; handler never destructures is exactly how `limit` was swallowed.
;; -----------------------------------------------------------------------------

(def ^:private read-verbs
  "Read verbs whose handler params schema is the declared surface. A verb is
   listed with the boundary params the consolidated `kg` tool documents FOR
   it; the contract is that its handler's params schema declares each."
  {#'q/handle-kg-traverse        {:schema ks/TraverseParams
                                  :params #{:start_node :direction :relations :max_depth :scope :limit}}
   #'q/handle-kg-impact-analysis {:schema ks/ImpactParams
                                  :params #{:node_id :max_depth :scope}}
   #'q/handle-kg-find-path       {:schema ks/FindPathParams
                                  :params #{:from_node :to_node :direction :relations :max_depth :scope}}})

(defn- declared [schema]
  (set (map first (m/children (m/schema schema)))))

(deftest every-boundary-param-of-a-read-verb-is-declared-by-its-handler
  (doseq [[v {:keys [schema params]}] read-verbs]
    (is (empty? (remove (declared schema) params))
        (str v " is sent params its handler does not declare: "
             (vec (remove (declared schema) params))))))

(deftest every-param-the-tool-documents-for-traverse-is-declared-by-the-handler
  (testing "derived from kg/tool-def, not a hand-written list: a property whose
            description is scoped to traverse (\"traverse: ...\") must be in
            TraverseParams, and must be coerced at the boundary"
    (let [props      (get-in kg/tool-def [:inputSchema :properties])
          documented (set (keep (fn [[k {:keys [description]}]]
                                  (when (re-find #"(^|; )traverse:" (or description ""))
                                    (keyword k)))
                                props))
          coerced    (set (keys @#'kg/coerce-schema))]
      (is (contains? documented :limit) "the derivation finds the known traverse params")
      (is (empty? (remove (declared ks/TraverseParams) documented))
          (str "documented for traverse but not declared by its handler: "
               (vec (remove (declared ks/TraverseParams) documented))))
      (is (empty? (remove #(or (coerced %) (= "string" (get-in props [(name %) :type]))) documented))
          "a non-string traverse param is coerced from its JSON string form"))))

(deftest the-traverse-verb-documents-limit-to-callers
  (let [props (get-in kg/tool-def [:inputSchema :properties])]
    (is (re-find #"traverse" (get-in props ["limit" :description] ""))
        "the tool schema tells a caller limit applies to traverse")))

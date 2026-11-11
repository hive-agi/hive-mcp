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

(deftest the-traverse-verb-documents-limit-to-callers
  (let [props (get-in kg/tool-def [:inputSchema :properties])]
    (is (re-find #"traverse" (get-in props ["limit" :description] ""))
        "the tool schema tells a caller limit applies to traverse")))

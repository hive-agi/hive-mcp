(ns hive-mcp.knowledge-graph.edges.m14-support
  "Shared helpers for the m14 edges facets: a fresh-store runner and an edge
   projection without the generated id and timestamps."
  (:require [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn in-fresh-store
  "Run F against a fresh thread-local DataScript store with synchronous
   writes; the edge-stats cache is reset around it by the harness."
  [f]
  (fixtures/datascript-fixture f))

(defn shown
  "An edge without its generated id, entity id and timestamps."
  [edge]
  (select-keys edge [:kg-edge/from :kg-edge/to :kg-edge/relation :kg-edge/scope
                     :kg-edge/confidence :kg-edge/source-type :kg-edge/created-by]))

(defn shown-all
  "Edges projected by `shown`, in a stable order."
  [edges]
  (vec (sort-by (juxt :kg-edge/from :kg-edge/to #(str (:kg-edge/relation %)))
                (map shown edges))))

(defn out-schema
  "The output schema registered for ns/fn (the first arity of a :function)."
  [nsym fsym]
  (let [s (get-in (m/function-schemas) [nsym fsym :schema])
        s (if (= :function (m/type s)) (first (m/children s)) s)]
    (second (m/children s))))

(defn seed!
  "Add the edges every edges facet reads."
  [add-edge!]
  (add-edge! {:from "n1" :to "n2" :relation :implements :scope "s1" :confidence 0.9})
  (add-edge! {:from "n1" :to "n3" :relation :depends-on :scope "s2"})
  (add-edge! {:from "n2" :to "n3" :relation :refines :confidence 0.4})
  (add-edge! {:from "n3" :to "n3" :relation :relates :predicate "Part Of"}))

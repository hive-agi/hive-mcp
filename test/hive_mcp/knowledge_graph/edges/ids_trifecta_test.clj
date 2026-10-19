(ns hive-mcp.knowledge-graph.edges.ids-trifecta-test
  "Trifecta for generate-edge-id: every id is a UUID string, and ids differ."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.knowledge-graph.edges.ids :as ids]
            [hive-mcp.knowledge-graph.edges.m14-support :as sup]
            [hive-schemas.test :as hst]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; No :golden-path: the subject is random by design.
(hst/deftrifecta-from-schema generate-edge-id-contract
  hive-mcp.knowledge-graph.edges.ids/generate-edge-id
  {:in [:cat]
   :out (sup/out-schema 'hive-mcp.knowledge-graph.edges.ids 'generate-edge-id)
   :rel (fn [_ out] (= out (str (java.util.UUID/fromString out))))})

(deftest ids-do-not-repeat
  (is (= 500 (count (set (repeatedly 500 ids/generate-edge-id))))))

(ns hive-mcp.tools.kg.commands-refusal-test
  "The KG write handlers refuse a bad node id or relation with an MCP error
   (:isError true), never with the bare `{:error msg}` map."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-mcp.tools.kg.commands :as cmd]
            [hive-mcp.tools.kg.schema :as ks]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each fixtures/datascript-fixture)

(deftest add-edge-refusals-are-mcp-errors
  (doseq [[label params] {"missing from"     {:to "b-1" :relation "implements"}
                          "placeholder to"   {:from "a-1" :to "$x.data.id" :relation "implements"}
                          "bad relation"     {:from "a-1" :to "b-1" :relation "nope"}
                          "missing relation" {:from "a-1" :to "b-1"}}]
    (testing label
      (let [out (cmd/handle-kg-add-edge params)]
        (is (m/validate ks/NodeIdRefusal out) (pr-str out))))))

(deftest reground-refusal-is-an-mcp-error
  (let [out (cmd/handle-kg-reground {:entry_id ""})]
    (is (m/validate ks/NodeIdRefusal out) (pr-str out))
    (is (= "entry_id must be a non-empty string" (:text out)))))

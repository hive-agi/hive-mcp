(ns hive-mcp.server.routes.async-task.lane-test
  "Facets for the async lane classifier (cov-facets f07): known-lane and
   classify are pure; both are driven from their contracts with inputs that
   reach the declared :short verbs as well as the :batch default."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.server.routes.async-task.lane :as lane]
            [hive-mcp.server.routes.async-task.schema :as s]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema known-lane-contract lane/known-lane
  {:in  [:cat [:enum :batch :short :typo "short" nil]]
   :out s/Lane
   :rel (fn [[l] out] (= out (if (#{:batch :short} l) l :batch)))
   :golden-path "test/golden/hive-mcp/f07-lane-known-lane.edn"})

(def ^:private ToolName [:enum "git" "memory" :git nil])
(def ^:private Command
  [:enum "ship land" "ship watch" "ship release" "live sync" "status"
   :ship-land :status nil])

(deftrifecta-from-schema classify-contract lane/classify
  {:in  [:cat ToolName Command]
   :out s/Lane
   :rel (fn [[tool cmd] out]
          (let [c (cond (keyword? cmd) (name cmd) (nil? cmd) nil :else cmd)]
            (= out (if (and (= "git" (some-> tool str))
                            (#{"ship land" "ship watch" "ship release" "live sync"} c))
                     :short :batch))))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/f07-lane-classify.edn"})

(deftest-golden classify-custom-table-golden
  "test/golden/hive-mcp/f07-lane-classify-table.edn"
  (let [table {"memory" {"add" :short}}]
    {:declared (lane/classify table "memory" "add")
     :undeclared (lane/classify table "memory" "search")
     :default-table-short (lane/classify "git" "ship land")
     :default-table-batch (lane/classify "git" "ship")}))

(deftest declared-lanes-are-known
  (is (every? lane/lanes (mapcat vals (vals lane/declared-lanes)))))

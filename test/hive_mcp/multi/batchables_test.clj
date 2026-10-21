(ns hive-mcp.multi.batchables-test
  "Name-matched facets for `hive-mcp.multi.batchables`.

     constructors   golden: each record satisfies Batchable and advertises
                    its batch schema
     dispatch       defspec over op vectors whose commands no record has a
                    handler for (mixed with the empty batch): the result is
                    the documented {:success :waves :summary} shape with one
                    :success false per op, in input order, naming the tool
                    and the command

   The handled commands (memory add/edit/get..., kg edge/traverse, kanban
   update/create/delete) reach the concrete handlers through
   requiring-resolve on fixed symbols, with no injectable port; driving them
   needs with-redefs or a live store. Recorded in the f06 report."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.batch.protocol :as bproto]
            [hive-mcp.multi.batchables :as batchables]
            [hive-test.golden :refer [deftest-golden]]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private records
  {"memory" batchables/memory-batchable
   "kg"     batchables/kg-batchable
   "kanban" batchables/kanban-batchable})

(deftest-golden batchable-constructors-golden
  "test/golden/hive-mcp/multi/batchables-constructors.edn"
  (into (sorted-map)
        (for [[tool ctor] records
              :let [r (ctor)]]
          [tool {:batchable? (satisfies? bproto/Batchable r)
                 :record     (.getSimpleName (class r))
                 :schema     (bproto/batch-schema r)}])))

(def ^:private UnhandledOps
  "Ops whose command no record handles."
  [:vector {:max 5}
   [:map [:id [:enum "a" "b" "c" "d"]] [:command [:enum "stats" "archive" "nonsense"]]]])

(defn- expected
  "Oracle: dispatch groups ops by command (first-seen command first) and
   fails each with the tool and command named."
  [tool ops]
  (let [order   (distinct (map :command ops))
        grouped (mapcat (fn [c] (filter #(= c (:command %)) ops)) order)]
    {:success (empty? ops)
     :waves   {1 {:ops ops
                  :results (mapv (fn [op] {:id (:id op) :success false
                                           :error (str tool ": no batch handler for command: "
                                                       (keyword (:command op)))})
                                 grouped)}}
     :summary {:total (count ops) :success 0 :failed (count ops) :waves 1}}))

(defspec unhandled-commands-fail-per-op-in-group-order 200
  (prop/for-all [tool (mg/generator (into [:enum] (keys records)))
                 ops  (mg/generator UnhandledOps)]
    (= (expected tool ops)
       (bproto/batch-execute ((records tool)) ops {}))))

(deftest empty-batch-succeeds-vacuously
  (doseq [[tool ctor] records]
    (is (= {:success true :waves {1 {:ops [] :results []}}
            :summary {:total 0 :success 0 :failed 0 :waves 1}}
           (bproto/batch-execute (ctor) [] {}))
        tool)))

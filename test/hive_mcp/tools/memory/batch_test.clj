(ns hive-mcp.tools.memory.batch-test
  "Facets for the memory Batchable runner, with its handler tree and fx
   emitter INJECTED through make-memory-runner's opts (no consolidated
   memory tree, no with-redefs): a trifecta over the factory contract and a
   golden over what the built runner answers for flat, nested (space and
   dotted), failing, missing and dependent ops, live and dry."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.batch.protocol :as bp]
            [hive-mcp.tools.memory.batch :as mb]
            [hive-mcp.tools.memory.schema :as ms]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private handlers
  {:add       (fn [p] {:type "text" :text (str "added " (:content p))})
   :get       (fn [p] {:type "text" :text (str "got " (:id p))})
   :kg        {:edge (fn [p] {:type "text" :text (str "kg edge " (:from p))})}
   :migration {:backup (fn [_] {:type "text" :text "backed up"})}
   :boom      (fn [_] (throw (ex-info "kaboom" {})))})

(def ^:private opts {:handlers handlers :emit-fx (fn [_ _] nil)})

(deftrifecta-from-schema make-memory-runner-wraps-the-given-tree
  hive-mcp.tools.memory.batch/make-memory-runner
  {:in        ms/RunnerOpts
   :out       mb/MemoryRunner
   ;; a given tree is wrapped as is; none falls back to the lazily resolved
   ;; consolidated memory tree (a map, {} when that ns cannot load)
   :rel       (fn [in out]
                (and (satisfies? bp/Batchable out)
                     (if (some? (:handlers in))
                       (= (:handlers in) (:handlers out))
                       (map? (:handlers out)))))
   ;; the :out schema is an instance? predicate: it has no shape to mutate
   :mutation  false
   :num-tests 100})

(def ^:private runner (mb/make-memory-runner opts))

(defn- run [ops & [o]] (bp/batch-execute runner ops (or o {})))

(deftest-golden memory-runner-answers
  "test/golden/tools/memory/batch-runner.edn"
  {:flat      (run [{:id "a" :tool "add" :content "hello"}])
   :nested    (run [{:id "a" :tool "kg edge" :from "x"}
                    {:id "b" :tool "migration.backup"}])
   :mixed     (run [{:id "a" :tool "get" :id_ "x"}
                    {:id "b" :tool "boom" :depends_on "a"}
                    {:id "c" :tool "nope"}])
   :dry-run   (run [{:id "a" :tool "add"} {:id "b" :tool "get" :depends_on "a"}] {:dry-run? true})
   :empty     (run [])})

(deftest runner-schema-is-the-default-runners
  (is (map? (bp/batch-schema runner)))
  (is (= {} (:handlers (mb/make-memory-runner {:handlers {}})))))

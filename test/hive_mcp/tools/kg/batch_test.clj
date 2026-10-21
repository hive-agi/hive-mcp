(ns hive-mcp.tools.kg.batch-test
  "Facets for the kg Batchable runner, with its handler tree and fx emitter
   INJECTED through make-kg-runner's opts (no consolidated kg tree, no
   with-redefs): a trifecta over the factory contract and a golden over what
   the built runner answers for resolved, dotted, failing, missing and
   dependent ops, live and dry."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.batch.protocol :as bp]
            [hive-mcp.tools.kg.batch :as kgb]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private handlers
  {:edge     (fn [p] {:type "text" :text (str "edge " (:from p) "->" (:to p))})
   :traverse (fn [p] {:type "text" :text (str "traverse " (:start_node p))})
   :boom     (fn [_] (throw (ex-info "kaboom" {})))})

(def ^:private opts {:handlers handlers :emit-fx (fn [_ _] nil)})

(deftrifecta-from-schema make-kg-runner-wraps-the-given-tree
  hive-mcp.tools.kg.batch/make-kg-runner
  {:in        kgb/RunnerOpts
   :out       kgb/KGRunner
   ;; a given tree is wrapped as is; none falls back to the lazily resolved
   ;; consolidated kg tree (a map, {} when that ns cannot load)
   :rel       (fn [in out]
                (and (satisfies? bp/Batchable out)
                     (if (some? (:handlers in))
                       (= (:handlers in) (:handlers out))
                       (map? (:handlers out)))))
   ;; the :out schema is an instance? predicate: it has no shape to mutate
   :mutation  false
   :num-tests 100})

(def ^:private runner (kgb/make-kg-runner opts))

(defn- run [ops & [o]] (bp/batch-execute runner ops (or o {})))

(deftest-golden kg-runner-answers
  "test/golden/tools/kg/batch-runner.edn"
  {:one-edge  (run [{:id "a" :tool "edge" :from "x" :to "y"}])
   :mixed     (run [{:id "a" :tool "edge" :from "x" :to "y"}
                    {:id "b" :tool "boom" :depends_on "a"}
                    {:id "c" :tool "nope"}])
   :chain     (run [{:id "a" :tool "traverse" :start_node "n1"}
                    {:id "b" :tool "edge" :from "n1" :to "n2" :depends_on ["a"]}])
   :dry-run   (run [{:id "a" :tool "edge"} {:id "b" :tool "traverse" :depends_on "a"}] {:dry-run? true})
   :empty     (run [])})

(deftest runner-schema-is-the-default-runners
  (is (map? (bp/batch-schema runner)))
  (is (= {} (:handlers (kgb/make-kg-runner {:handlers {}})))))

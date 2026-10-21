(ns hive-mcp.tools.memory.contract-coverage-test
  "Contract coverage gate for the top-level tools/memory namespaces (cov-mcp
   m28). tools/memory/crud and tools/memory/migration are m29's, so the gate
   lists the top-level files rather than the directory. crud.clj and
   migration.clj are (def x #'sub/x) facades with no defn of their own; they
   are in :paths so a facade defn added later is counted. Every public defn
   carries an m/=> contract; there are no exemptions.

   Also the facets of the search pure helpers and the memory batch runner."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]

            [hive-mcp.batch :as batch]
            [hive-mcp.tools.memory.batch :as mbatch]
            [hive-mcp.tools.memory.schema :as ms]
            [hive-mcp.tools.memory.search :as search]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private batch-files
  (mapv #(str "src/hive_mcp/tools/memory/" % ".clj")
        ["analytics" "batch" "classify" "core" "crud" "decay" "duration" "format"
         "gaps" "lifecycle" "migration" "promotion" "reconcile" "review" "schema"
         "scope" "search"]))

(def ^:private contracted-nss
  '[hive-mcp.tools.memory.analytics hive-mcp.tools.memory.batch
    hive-mcp.tools.memory.classify hive-mcp.tools.memory.decay
    hive-mcp.tools.memory.duration hive-mcp.tools.memory.format
    hive-mcp.tools.memory.gaps hive-mcp.tools.memory.lifecycle
    hive-mcp.tools.memory.promotion hive-mcp.tools.memory.reconcile
    hive-mcp.tools.memory.review hive-mcp.tools.memory.schema
    hive-mcp.tools.memory.scope hive-mcp.tools.memory.search])

(coverage/deftest-contract-coverage tools-memory-top-level-contracts
  {:paths batch-files})

(deftest tools-memory-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths batch-files})]
    (is (= 62 (count (:universe report))) "every public defn of the top-level files")
    (is (= 62 (count (:covered report))))))

(deftest every-tools-memory-contract-generates-both-sides
  (doseq [ns-sym contracted-nss
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

(deftrifecta-from-schema resolve-exclude-tags-honours-the-caller
  #'search/resolve-exclude-tags
  {:in [:cat [:map {:closed true}
              [:exclude-tags {:optional true} [:maybe [:vector {:max 2} [:enum "a" "carto"]]]]
              [:default-excludes [:vector {:max 2} [:enum "carto" "ingestion-chunk"]]]
              [:required-tags {:optional true} [:vector {:max 2} [:enum "ingestion-chunk" "a"]]]]]
   :out [:vector :string]
   :rel (fn [[{:keys [exclude-tags default-excludes required-tags]}] out]
          (if (some? exclude-tags)
            (= (vec exclude-tags) out)
            (= (vec (remove (set required-tags) default-excludes)) out)))
   :classify (fn [[p] _] (if (some? (:exclude-tags p)) :caller :default))
   :classify-domain #{:caller :default}
   :golden-path "test/golden/hive-mcp/tools/memory/resolve-exclude-tags-contract.edn"
   :num-tests 200})

(deftrifecta-from-schema search-payload-names-its-exclusion-source
  #'search/search-payload
  {:in [:cat [:map {:closed true}
              [:results [:vector {:max 2} [:map [:id [:enum "a" "b"]]]]]
              [:query [:enum "q" "x"]]
              [:exclude-tags [:vector {:max 2} [:enum "carto" "ingestion-chunk" "a"]]]
              [:default-excludes [:= ["carto" "ingestion-chunk"]]]]]
   :out ms/SearchPayload
   :rel (fn [[{:keys [results exclude-tags default-excludes]}] out]
          (and (= (count results) (:count out))
               (= (cond (empty? exclude-tags) :none
                        (= (set exclude-tags) (set default-excludes)) :default
                        :else :caller)
                  (:excludes-source out))))
   :classify (fn [_ out] (:excludes-source out))
   :classify-domain #{:none :default :caller}
   :classify-floor 10
   :golden-path "test/golden/hive-mcp/tools/memory/search-payload-contract.edn"
   :num-tests 450})

(deftrifecta-from-schema filter-required-tags-keeps-supersets
  #'search/filter-required-tags
  {:in [:cat [:map {:closed true}
              [:entries [:vector {:max 3} [:map {:closed true}
                                           [:id [:enum "a" "b"]]
                                           [:metadata [:map {:closed true}
                                                       [:tags [:enum "x" "x,y" "y" ""]]]]]]]
              [:tags [:maybe [:vector {:max 2} [:enum "x" "y"]]]]]]
   :out [:vector ms/SearchHit]
   :rel (fn [[{:keys [entries tags]}] out]
          (= out (filterv (fn [e] (every? (set (clojure.string/split (get-in e [:metadata :tags]) #","))
                                          tags))
                          entries)))
   :num-tests 200})

(def ^:private UnknownOps
  [:vector {:max 3}
   [:map {:closed true} [:id [:enum "a" "b" "c"]] [:tool [:enum "no-such-command"]]]])

(deftrifecta-from-schema memory-run-batch-fails-unknown-commands
  #'mbatch/run-batch
  {:in [:cat UnknownOps]
   :out batch/RunResult
   :rel (fn [[ops] {:keys [success summary]}]
          (if (not= (count ops) (count (distinct (map :id ops))))
            (false? success)
            (and (= (count ops) (:total summary)) (zero? (:success summary)))))
   :num-tests 60})

(deftest make-memory-runner-builds-a-runner
  (is (m/validate mbatch/MemoryRunner (mbatch/make-memory-runner {:handlers {}})))
  (is (m/validate mbatch/MemoryRunner (mbatch/make-memory-runner))))

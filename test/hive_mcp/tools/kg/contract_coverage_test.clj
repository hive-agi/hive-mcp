(ns hive-mcp.tools.kg.contract-coverage-test
  "Contract coverage gate for tools/kg (cov-mcp m28), plus the facets of the
   kg batch runner: every public defn under src/hive_mcp/tools/kg carries an
   m/=> contract, and every contract generates on both sides."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.batch :as batch]
            [hive-mcp.tools.kg.batch :as kgbatch]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private kg-dir "src/hive_mcp/tools/kg")

(def ^:private kg-nss
  '[hive-mcp.tools.kg.batch hive-mcp.tools.kg.commands
    hive-mcp.tools.kg.queries hive-mcp.tools.kg.synthetics])

(coverage/deftest-contract-coverage tools-kg-contracts
  {:paths [kg-dir]})

(deftest tools-kg-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths [kg-dir]})]
    (is (= 28 (count (:universe report))) "every public defn of tools/kg")
    (is (= 28 (count (:covered report))))))

(deftest every-tools-kg-contract-generates-both-sides
  (doseq [ns-sym kg-nss
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

(def ^:private UnknownOps
  "Ops naming no kg command: the runner must fail each one, never throw."
  [:vector {:max 4}
   [:map {:closed true}
    [:id [:enum "a" "b" "c" "d"]]
    [:tool [:enum "no-such-command" "nope"]]]])

(deftrifecta-from-schema run-batch-fails-unknown-commands
  #'kgbatch/run-batch
  {:in [:cat UnknownOps]
   :out batch/RunResult
   :rel (fn [[ops] {:keys [success summary errors]}]
          (let [ids (map :id ops)]
            (if (not= (count ids) (count (distinct ids)))
              (and (false? success) (seq errors))
              (and (= (count ops) (:total summary))
                   (zero? (:success summary))
                   (= (empty? ops) (boolean success))))))
   :num-tests 60})

(deftest make-kg-runner-builds-a-runner-over-the-given-handlers
  (let [r (kgbatch/make-kg-runner {:handlers {}})]
    (is (m/validate kgbatch/KGRunner r))
    (is (= {} (:handlers r))))
  (is (m/validate kgbatch/KGRunner (kgbatch/make-kg-runner))))

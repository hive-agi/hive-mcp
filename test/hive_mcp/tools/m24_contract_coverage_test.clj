(ns hive-mcp.tools.m24-contract-coverage-test
  "Contract coverage gate for the cov-mcp m24 batch: exactly the seventeen
   top-level tools namespaces below, not the tools/ sub-directories and not
   the other top-level tools files (those belong to m25..m31). Every public
   defn in them carries an m/=> contract; there are no exemptions."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.coverage :as coverage]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private batch-files
  (mapv #(str "src/hive_mcp/tools/" % ".clj")
        ["agora" "analysis_bridge" "catchup" "cli" "clojure_discover" "composite"
         "core" "crystal" "diagnostics" "hive_project" "kg" "magit"
         "memory_kanban" "migration" "multi" "multi_async" "olympus"]))

(def ^:private batch-nss
  '[hive-mcp.tools.agora hive-mcp.tools.analysis-bridge hive-mcp.tools.catchup
    hive-mcp.tools.cli hive-mcp.tools.clojure-discover hive-mcp.tools.composite
    hive-mcp.tools.core hive-mcp.tools.crystal hive-mcp.tools.diagnostics
    hive-mcp.tools.hive-project hive-mcp.tools.kg hive-mcp.tools.magit
    hive-mcp.tools.memory-kanban hive-mcp.tools.migration hive-mcp.tools.multi
    hive-mcp.tools.multi-async hive-mcp.tools.olympus])

(coverage/deftest-contract-coverage m24-tools-contracts
  {:paths batch-files})

(deftest m24-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths batch-files})]
    (is (= (set batch-nss) (set (map (comp symbol namespace) (:universe report))))
        "the universe is exactly the seventeen batch namespaces")
    (is (= 127 (count (:universe report))) "every public defn of the batch")
    (is (= 127 (count (:covered report))))))

(deftest every-m24-contract-generates-both-sides
  (doseq [ns-sym batch-nss
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

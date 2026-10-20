(ns hive-mcp.tools.m25-contract-coverage-test
  "Contract coverage gates for the cov-mcp m25 batch: the seven top-level
   tools namespaces below (the other top-level tools files belong to m24
   and m26..m31) and the whole tools/agent directory. Every public defn in
   them carries an m/=> contract; there are no exemptions."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.coverage :as coverage]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private top-level-files
  (mapv #(str "src/hive_mcp/tools/" % ".clj")
        ["op_outcome" "presets" "projectile" "registry" "result_bridge"
         "schema_keys" "session_complete"]))

(def ^:private agent-dir "src/hive_mcp/tools/agent")

(def ^:private top-level-nss
  '[hive-mcp.tools.op-outcome hive-mcp.tools.presets hive-mcp.tools.projectile
    hive-mcp.tools.registry hive-mcp.tools.result-bridge hive-mcp.tools.schema-keys
    hive-mcp.tools.session-complete])

(def ^:private agent-nss
  '[hive-mcp.tools.agent.dag hive-mcp.tools.agent.dispatch hive-mcp.tools.agent.helpers
    hive-mcp.tools.agent.lifecycle hive-mcp.tools.agent.reconcile hive-mcp.tools.agent.spawn
    hive-mcp.tools.agent.status])

(coverage/deftest-contract-coverage m25-top-level-tools-contracts
  {:paths top-level-files})

(coverage/deftest-contract-coverage m25-tools-agent-contracts
  {:paths [agent-dir]})

(deftest m25-gates-are-not-vacuous
  (let [top   (coverage/coverage {:paths top-level-files})
        agent (coverage/coverage {:paths [agent-dir]})]
    (is (= (set top-level-nss) (set (map (comp symbol namespace) (:universe top)))))
    (is (= (set agent-nss) (set (map (comp symbol namespace) (:universe agent)))))
    (is (= 47 (count (:universe top)) (count (:covered top))) "every top-level public defn")
    (is (= 41 (count (:universe agent)) (count (:covered agent))) "every tools/agent public defn")))

(deftest every-m25-contract-generates-both-sides
  (doseq [ns-sym (concat top-level-nss agent-nss)
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

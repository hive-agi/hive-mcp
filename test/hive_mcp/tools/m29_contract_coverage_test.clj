(ns hive-mcp.tools.m29-contract-coverage-test
  "Contract coverage gate for the cov-mcp m29 batch: tools/memory/crud,
   tools/memory/migration, tools/memory_kanban, tools/migrate (with
   tools/migrate/kanban) and the top-level tools/swarm files (tools/swarm/jvm/
   belongs to m30). Every public defn in them carries an m/=> contract; there
   are no exemptions."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.coverage :as coverage]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private batch-dirs
  (mapv #(str "src/hive_mcp/tools/" %)
        ["memory/crud" "memory/migration" "memory_kanban" "migrate"]))

(def ^:private swarm-files
  (mapv #(str "src/hive_mcp/tools/swarm/" % ".clj")
        ["batch" "channel" "claim" "collect" "core" "dispatch" "jvm" "lifecycle"
         "prompt" "registry" "state" "status"]))

(def ^:private batch-paths (into batch-dirs swarm-files))

(coverage/deftest-contract-coverage m29-tools-contracts
  {:paths batch-paths})

(deftest m29-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths batch-paths})
        nss    (set (map (comp symbol namespace) (:universe report)))]
    (is (= 36 (count nss)) "every namespace of the batch with a public defn")
    (is (not-any? #(re-find #"tools\.swarm\.jvm\." (str %)) nss)
        "tools/swarm/jvm/ is m30's")
    (is (= 166 (count (:universe report))) "every public defn of the batch")
    (is (= 166 (count (:covered report))))))

(deftest every-m29-contract-generates-both-sides
  (let [nss (set (map (comp symbol namespace)
                      (:universe (coverage/coverage {:paths batch-paths}))))]
    (doseq [ns-sym nss
            :let [_ (require ns-sym)]
            [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
            arity (if (= :function (m/type schema)) (m/children schema) [schema])
            side (take 2 (m/children arity))]
      (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
          (str ns-sym "/" fn-sym " " (pr-str (m/form side)))))))

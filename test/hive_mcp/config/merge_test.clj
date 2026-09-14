(ns hive-mcp.config.merge-test
  "Unit tests for hive-mcp.config.merge/default-config — pinning defaults
   for the dedicated carto vector store (qdrant-carto) and the
   :carto-store service key."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.config.merge :as merge]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest default-config-has-qdrant-carto
  (testing ":services :qdrant-carto defaults are present and well-formed"
    (let [svc (get-in merge/default-config [:services :qdrant-carto])]
      (is (map? svc) ":qdrant-carto service entry must exist")
      (is (= :local (:mode svc)))
      (is (= "localhost" (:host svc)))
      (is (= 6333 (:port svc)))
      (is (= "carto-snippets" (:collection svc)))
      (is (not (contains? svc :embedding))
          "the carto embedding provider and model come from user config, not defaults"))))

(deftest default-config-ships-no-model-choice
  (testing "no model id, model list or provider choice lives in default-config"
    (is (not (contains? merge/default-config :agent-defaults)))
    (is (not (contains? merge/default-config :models)))
    (is (not (contains? (:services merge/default-config) :drone)))
    (is (not (contains? (get-in merge/default-config [:services :ollama]) :model)))
    (is (not (contains? (:embedder merge/default-config) :default)))
    (is (not (contains? (:embedder merge/default-config) :providers)))
    (is (nil? (get-in merge/default-config [:embeddings :ollama :model])))
    (is (not (contains? (:embeddings merge/default-config) :openrouter))))

  (testing "so a user's own model choices are the only ones after deep-merge"
    (let [user   {:agent-defaults {:ling {:provider :venice :model "user-model"}}}
          merged (merge/deep-merge merge/default-config user)]
      (is (= user (select-keys merged [:agent-defaults]))))))

(deftest default-config-has-carto-store
  (testing ":services :carto-store defaults to :qdrant-carto backend"
    (let [svc (get-in merge/default-config [:services :carto-store])]
      (is (map? svc) ":carto-store service entry must exist")
      (is (= :qdrant-carto (:backend svc))))))

(deftest deep-merge-preserves-user-carto-overrides
  (testing "user config overrides for :qdrant-carto survive deep-merge"
    (let [user {:services {:qdrant-carto {:host "qdrant.internal"
                                          :port 16333}}}
          merged (merge/deep-merge merge/default-config user)
          svc (get-in merged [:services :qdrant-carto])]
      ;; user wins
      (is (= "qdrant.internal" (:host svc)))
      (is (= 16333 (:port svc)))
      ;; defaults still filled in
      (is (= "carto-snippets" (:collection svc)))
      ;; carto-store backend default still present
      (is (= :qdrant-carto (get-in merged [:services :carto-store :backend]))))))

(ns hive-mcp.embeddings.http-seam-test
  "COV-FACETS-FINDINGS-W2 (f04 seam gap): the OpenAI, OpenRouter and Venice
   embedders had no port for their HTTP make-request, so no test could drive
   embed-text/embed-batch without a live endpoint. Each ->provider now takes a
   :request-fn transport seam (fn [api-base api-key body] -> parsed response)."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.embeddings.openai :as openai]
            [hive-mcp.embeddings.openrouter :as openrouter]
            [hive-mcp.embeddings.protocol :as proto]
            [hive-mcp.embeddings.venice :as venice]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- recording-request
  "A request-fn that records every call and answers the embeddings response
   shape out of index order, so the provider's index sort is exercised."
  [calls]
  (fn [api-base api-key body]
    (swap! calls conj {:api-base api-base :api-key api-key :body body})
    {:data (vec (reverse (map-indexed (fn [i _] {:index i :embedding [(double i)]})
                                      (:input body))))}))

(def ^:private providers
  ;; an unreachable api-base: any real HTTP call fails, so a pass proves the seam
  [["openai" openai/->provider "text-embedding-3-small"]
   ["openrouter" openrouter/->provider "qwen/qwen3-embedding-8b"]
   ["venice" venice/->provider "text-embedding-qwen3-8b"]])

(deftest http-providers-route-through-the-request-seam
  (doseq [[label ctor model] providers]
    (testing label
      (let [calls (atom [])
            p     (ctor {:api-key "k" :api-base "http://127.0.0.1:9" :model model
                         :request-fn (recording-request calls)})]
        (is (= [0.0] (proto/embed-text p "x")))
        (is (= [[0.0] [1.0] [2.0]] (proto/embed-batch p ["a" "b" "c"])))
        (is (= [{:api-base "http://127.0.0.1:9" :api-key "k" :body {:model model :input ["x"]}}
                {:api-base "http://127.0.0.1:9" :api-key "k" :body {:model model :input ["a" "b" "c"]}}]
               @calls))))))

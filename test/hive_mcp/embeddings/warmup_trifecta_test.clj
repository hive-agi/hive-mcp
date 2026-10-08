(ns hive-mcp.embeddings.warmup-trifecta-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.embeddings.warmup :as warmup]
            [hive-mcp.embeddings.resilient :as resilient]))

(deftrifecta local-model-routing
  hive-mcp.embeddings.warmup/local-models
  {:golden-path "test/golden/embeddings/boot-warmup.edn"
   :cases {:none {}
           :remote-only {:embedder {:providers {:remote {:impl :openrouter :model "r"}}}}
           :shared-model {:embedder {:providers {:note {:impl :ollama :model "n"}
                                                :other {:impl :ollama :model "n"}
                                                :decision {:impl :ollama :model "q"}}}}
           :legacy {:embeddings {:ollama {:model "legacy"}}}}
   :gen (gen/let [models (gen/vector (gen/elements ["n" "q" "r"]) 0 12)]
          {:embedder {:providers (into {}
                                       (map-indexed (fn [i model]
                                                      [(keyword (str "route-" i))
                                                       {:impl :ollama :model model}])
                                                    models))}})
   :pred (fn [selected]
           (and (vector? selected)
                (<= (count selected) warmup/max-models)
                (= (count selected) (count (set (map (juxt :host :model) selected))))))
   :num-tests 80
   :mutations [["drops-all-routes" (fn [_] [])]
               ["includes-remotes" (fn [_] [{:host "remote" :model "r" :keys #{}}])]]})

(deftest warmup-provider-port-test
  (let [cfg {:embeddings {:warmup {:enabled true}}
             :embedder {:providers {:first {:impl :ollama :model "n"}
                                    :second {:impl :ollama :model "n"}
                                    :bad {:impl :ollama :model "bad"}}}}
        observed (atom [])
        initial @resilient/warm-providers]
    (try
      (let [worker (warmup/start! cfg (fn [{:keys [model]}]
                                        (swap! observed conj model)
                                        (when (= model "bad")
                                          (throw (ex-info "unavailable" {})))
                                        [1.0]))]
        (.join ^Thread worker 3000)
        (is (not (.isAlive ^Thread worker)))
        (is (= #{"bad" "n"} (set @observed)))
        (is (= 2 (count @observed)))
        (is (contains? @resilient/warm-providers [:first "n"]))
        (is (contains? @resilient/warm-providers [:second "n"]))
        (is (not (contains? @resilient/warm-providers [:bad "bad"]))))
      (is (nil? (warmup/start! {:embeddings {:warmup {:enabled false}}
                                :embedder {:providers {:unused {:impl :ollama :model "u"}}}}
                               (fn [_] (throw (ex-info "must not run" {}))))))
      (finally (reset! resilient/warm-providers initial)))))

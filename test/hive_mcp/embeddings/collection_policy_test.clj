(ns hive-mcp.embeddings.collection-policy-test
  (:require [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.embeddings.config :as config]))

(deftrifecta collection-provider-routing
  hive-mcp.embeddings.config/collection-route
  {:apply? true
   :golden-path "test/golden/embeddings/collection-provider-routing.edn"
   :cases {:memory [(first config/collection-provider-specs) false]
           :presets-openrouter [(second config/collection-provider-specs) true]
           :presets-fallback [(second config/collection-provider-specs) false]
           :plans-fallback [(nth config/collection-provider-specs 2) false]
           :ingest-unconfigured [(last config/collection-provider-specs) false]
           :ingest-openrouter [(last config/collection-provider-specs) true]}
   :gen (gen/tuple (gen/elements config/collection-provider-specs) gen/boolean)
   :pred (fn [{:keys [provider level message]}]
           (and (or (nil? provider) (#{:ollama :openrouter} provider))
                (or (nil? level) (#{:info :warn} level))
                (or (nil? message) (string? message))))
   :num-tests 40
   :mutations [["always-ollama" (fn [_ _] {:provider :ollama})]
               ["always-openrouter" (fn [_ _] {:provider :openrouter})]]})

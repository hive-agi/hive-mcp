(ns hive-mcp.embeddings.registry-trifecta-test
  "Public API characterization of the factory registry and provider cache.
   Uses the registry port for test factories; restores the built-in Venice
   registration and cache after each scenario. No external embedding service."
  (:require [clojure.test.check.generators :as gen]
            [hive-mcp.embeddings.config :as config]
            [hive-mcp.embeddings.protocol :as proto]
            [hive-mcp.embeddings.registry :as registry]
            [hive-mcp.embeddings.shared-gate :as gate]
            [hive-test.trifecta :refer [deftrifecta]]))

(defn- stub-provider []
  (reify proto/EmbeddingProvider
    (embed-text [_ _] [1.0])
    (embed-batch [_ texts] (mapv (constantly [1.0]) texts))
    (embedding-dimension [_] 1)))

(defn- counting-factory [calls provider]
  (fn [_]
    (swap! calls inc)
    provider))

(defn characterize-factory-api
  "Exercise only public registry operations, with a valid Venice config and
   unique model so the cache cannot hit a provider from another test."
  [scenario]
  (let [had-venice? (boolean (some #{:venice} (registry/list-factories)))
        cfg (config/->EmbeddingConfig :venice
                                      (str "registry-multislot-" (random-uuid)) 1 {})
        a (stub-provider)
        b (stub-provider)
        a-calls (atom 0)
        b-calls (atom 0)]
    (try
      (registry/clear-cache!)
      (registry/register-factory! :venice (counting-factory a-calls a))
      (case scenario
        :register
        (let [first-provider (registry/get-provider cfg)
              second-provider (registry/get-provider cfg)]
          {:listed (boolean (some #{:venice} (registry/list-factories)))
           :stat-listed (boolean (some #{:venice} (:factories (registry/cache-stats))))
           :cache-count (:cached-count (registry/cache-stats))
           :gated (identical? a (gate/ungated first-provider))
           :cache-hit (identical? first-provider second-provider)
           :factory-calls @a-calls})

        :replace
        (do (registry/register-factory! :venice (counting-factory b-calls b))
            {:uses-replacement (identical? b (gate/ungated (registry/get-provider cfg)))
             :old-calls @a-calls
             :new-calls @b-calls})

        :remove
        (let [removed (registry/unregister-factory! :venice)
              error (try (registry/get-provider cfg)
                         nil
                         (catch clojure.lang.ExceptionInfo e (ex-data e)))]
          {:remove-return-map? (map? removed)
           :removed-key? (contains? removed :venice)
           :listed (boolean (some #{:venice} (registry/list-factories)))
           :stat-listed (boolean (some #{:venice} (:factories (registry/cache-stats))))
           :missing-type (:provider-type error)
           :missing-registered (boolean (some #{:venice} (:registered error)))})

        :cached-after-remove
        (let [first-provider (registry/get-provider cfg)]
          (registry/unregister-factory! :venice)
          {:cache-hit (identical? first-provider (registry/get-provider cfg))
           :factory-calls @a-calls
           :listed (boolean (some #{:venice} (registry/list-factories)))}))
      (finally
        (registry/clear-cache!)
        (if had-venice?
          (registry/register-venice!)
          (registry/unregister-factory! :venice))))))

(deftrifecta factory-registry-public-api
  hive-mcp.embeddings.registry-trifecta-test/characterize-factory-api
  {:golden-path "test/golden/embeddings/registry-factories.edn"
   :cases {:register :register
           :replace :replace
           :remove :remove
           :cached-after-remove :cached-after-remove}
   :gen (gen/elements [:register :replace :remove :cached-after-remove])
   :pred map?
   :num-tests 40
   :mutations [["never-registers" (fn [_] {})]
               ["always-claims-cache-hit" (fn [_] {:cache-hit true})]]})

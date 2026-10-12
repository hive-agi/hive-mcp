(ns hive-mcp.embeddings.warmup
  "Opt-in, bounded boot probes for routed local embedding models."
  (:require [hive-mcp.embeddings.config :as config]
            [hive-mcp.embeddings.protocol :as proto]
            [hive-mcp.embeddings.registry :as registry]
            [hive-mcp.embeddings.resilient :as resilient]
            [hive-weave.safe :as safe]
            [hive-dsl.result :as result]
            [taoensso.timbre :as log]))

(def max-models
  "Boot never schedules more than this many distinct local models."
  4)

(def probe-budget-ms
  "Maximum time the boot worker waits for one model's probe."
  60000)

(defn local-models
  "Select distinct routed Ollama [host model] identities. A legacy collection
   fallback is included only when explicitly configured. Remote providers are
   intentionally not contacted at boot. Each identity retains ALL routing keys
   so successful probes can mark the resilience budget warm for each route."
  [cfg]
  (let [specs (concat (for [[k spec] (get-in cfg [:embedder :providers])]
                        (assoc spec :provider-key k))
                      (when-let [model (get-in cfg [:embeddings :ollama :model])]
                        [{:impl :ollama :model model
                          :host (get-in cfg [:embeddings :ollama :host])}]))]
    (->> specs
         (filter #(and (= :ollama (:impl %)) (some? (:model %))))
         (group-by (juxt #(or (:host %) "http://localhost:11434") :model))
         (sort-by (comp pr-str key))
         (take max-models)
         (mapv (fn [[[host model] grouped]]
                 (let [routed (first (filter :dimension grouped))]
                   (cond-> {:host host :model model
                            :keys (into #{} (keep :provider-key) grouped)}
                     routed (assoc :spec (select-keys routed
                                                      [:dimension :max-tokens :vram-mb])))))))))

(defn probe!
  "Probe one local identity through an injected embedding port. An unsuccessful
   probe never marks a model warm or touches the live circuit breaker."
  [embed! {:keys [host model keys] :as identity}]
  (let [answer (safe/safe-future-call
                {:timeout-ms probe-budget-ms :name (str "boot-embed:" model)}
                #(embed! identity))]
    (if (and (result/ok? answer) (seq (:ok answer)))
      (do (swap! resilient/warm-providers into
                 (map (fn [k] [k model]) keys))
          (log/info "Boot embedding warmed" model "at" host)
          true)
      (do (log/warn "Boot embedding warmup failed (non-fatal):" model (:error answer))
          false))))

(defn embed-local!
  "Production embedding port: registry provider + protocol, not a collection
   write (which would mutate the vector cache). A routed model keeps its
   declared dimension and Ollama context size."
  [{:keys [host model spec]}]
  (proto/embed-text
   (registry/get-provider
    (if spec
      (config/->EmbeddingConfig :ollama model (:dimension spec)
                                {:host host :num-ctx (:max-tokens spec)
                                 :vram-mb (:vram-mb spec)})
      (config/ollama-config {:host host :model model})))
   "warmup"))

(defn start!
  "Return immediately after starting ONE daemon boot worker. Its work is
   bounded by max-models and per-model probe-budget-ms; no warmup can hold
   server startup or JVM shutdown. embed! is an injectable provider port."
  ([cfg] (start! cfg embed-local!))
  ([cfg embed!]
   (when (true? (get-in cfg [:embeddings :warmup :enabled]))
     (let [models (local-models cfg)]
       (when (seq models)
         (doto (Thread. ^Runnable
                        (fn []
                          (doseq [model models]
                            (try (probe! embed! model)
                                 (catch Throwable t
                                   (log/warn t "Boot embedding probe failed (non-fatal)"))))))
           (.setName "boot-embedding-warmup")
           (.setDaemon true)
           (.start)))))))

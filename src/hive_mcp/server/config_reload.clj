(ns hive-mcp.server.config-reload
  "Re-wire the live JVM after `config reload`.

   Swapping the config atom changes what the NEXT read sees; it does not touch
   what boot already built from the old config: the embedding provider, the
   :default memory store and the decorator chains (cloak work layer) of every
   registered store. This namespace diffs the old config against the new one
   and re-runs, in boot order, only the boot steps whose inputs changed.

   Every step is idempotent and reached through a PORT (a 0-arity fn), so the
   orchestration is tested with stubs and the defaults are resolved by symbol
   at call time: this namespace never loads server.init at compile time."
  (:require [hive-dsl.result :as r]
            [malli.core :as m]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def steps
  "The boot steps a reload may re-run, in boot order, each with the config
   paths it reads. :after names steps whose run also forces this one: a store
   wired anew is decorated at registration, but the chains of the stores it
   did not replace still need the current mode."
  [{:step  :embeddings
    :paths [[:embeddings] [:embedder] [:services :chroma] [:services :ollama]
            [:secrets :openrouter-api-key]]}
   {:step  :memory-store
    :paths [[:memory] [:services :memory-store] [:services :milvus]
            [:services :vector-store] [:addons]]}
   {:step  :redecorate
    :paths [[:services :seal]]
    :after #{:memory-store}}])

(def Step [:enum :embeddings :memory-store :redecorate])

(defn changed-paths
  "Pure. The watched config paths whose value differs between OLD and NEW."
  [old new]
  (into []
        (comp (mapcat :paths)
              (distinct)
              (filter #(not= (get-in old %) (get-in new %))))
        steps))

(m/=> changed-paths [:=> [:cat [:maybe :map] [:maybe :map]] [:vector [:vector :keyword]]])

(defn plan
  "Pure. The steps to re-run, in boot order, for the config change OLD -> NEW."
  [old new]
  (let [changed (set (changed-paths old new))]
    (reduce (fn [acc {:keys [step paths after]}]
              (if (or (some changed paths) (some (set acc) after))
                (conj acc step)
                acc))
            []
            steps)))

(m/=> plan [:=> [:cat [:maybe :map] [:maybe :map]] [:vector Step]])

(def default-ports
  "The live boot steps, by symbol. Each is a 0-arity fn."
  {:embeddings   'hive-mcp.server.init/init-embedding-provider!
   :memory-store 'hive-mcp.server.init/rewire-memory-store!
   :redecorate   'hive-spi.memory.registry/redecorate!})

(defn- port-fn [ports step]
  (let [p (get ports step)]
    (if (symbol? p) (requiring-resolve p) p)))

(defn- run-step
  "Run one step through its port; a Result. A port answering false is a
   failure: the boot steps say so instead of throwing."
  [ports step]
  (let [res (r/try-effect* :config-reload/step-failed
              (if-let [f (port-fn ports step)]
                (f)
                (throw (ex-info "no port for step" {:step step}))))]
    (if (and (r/ok? res) (false? (:ok res)))
      (r/err :config-reload/step-failed {:message (str (name step) " reported failure")})
      res)))

(def Report
  [:map
   [:changed [:vector :string]]
   [:ran [:vector Step]]
   [:failed [:map-of Step :string]]])

(defn rewire!
  "Re-run the boot steps the config change OLD -> NEW affects, through PORTS
   (default `default-ports`). A failing step does not stop the later ones.
   Answers which watched paths changed (never their values: they may be
   secrets), the steps run and the ones that failed."
  ([old new] (rewire! old new default-ports))
  ([old new ports]
   (let [todo (plan old new)
         outs (mapv (fn [s] [s (run-step ports s)]) todo)
         report {:changed (mapv #(pr-str %) (changed-paths old new))
                 :ran     (mapv first (filter (comp r/ok? second) outs))
                 :failed  (into {} (keep (fn [[s res]]
                                           (when (r/err? res) [s (str (:message res))])))
                                outs)}]
     (when (seq todo)
       (log/info "config reload re-wired" report))
     report)))

(defn reload!
  "Snapshot the current config (CURRENT, 0-arity), load the new one (LOAD!,
   0-arity, answers the loaded config), then `rewire!` the difference through
   PORTS. Answers {:loaded config :rewire Report}."
  ([] (reload! {}))
  ([{:keys [current load! ports]
     :or   {current 'hive-mcp.config.core/get-global-config
            load!   'hive-mcp.config.core/load-global-config!
            ports   default-ports}}]
   (let [resolve* #(if (symbol? %) (requiring-resolve %) %)
         old      ((resolve* current))
         loaded   ((resolve* load!))]
     {:loaded loaded
      :rewire (rewire! old loaded ports)})))

(m/=> reload! [:function
               [:=> [:cat] [:map [:loaded :map] [:rewire Report]]]
               [:=> [:cat [:map-of :keyword :any]] [:map [:loaded :map] [:rewire Report]]]])

(m/=> rewire! [:function
               [:=> [:cat [:maybe :map] [:maybe :map]] Report]
               [:=> [:cat [:maybe :map] [:maybe :map] [:map-of :keyword :any]] Report]])

(ns hive-mcp.agent.cider
  "CIDER session backend for agent delegation via nREPL."
  (:require [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ollama :as ollama]
            [hive-mcp.agent.openrouter :as openrouter]
            [hive-spi.editor.services :as svc]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]
            [hive-mcp.agent.provider.model :as provider-model])
  (:import [java.util.concurrent ArrayBlockingQueue TimeUnit]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private session-pool
  (atom {:sessions {}
         :available (ArrayBlockingQueue. 10)}))

(defn- spawn-session!
  "Spawn a new CIDER session via Emacs."
  [session-name]
  (let [{:keys [success result error]} (svc/invoke :vessel :dispatch {:op :cider/spawn-session, :name session-name} 5000)]
    (if success
      (let [data (json/read-str result :key-fn keyword)]
        (log/info "Spawned CIDER session:" session-name "port:" (:port data))
        {:name session-name
         :port (:port data)
         :status :idle
         :spawned-at (System/currentTimeMillis)})
      (do
        (log/error "Failed to spawn CIDER session:" error)
        nil))))

(defn- kill-session!
  "Kill a CIDER session via Emacs."
  [session-name]
  (let [{:keys [success]} (svc/invoke :vessel :dispatch {:op :cider/kill-session, :name session-name} 5000)]
    (when success
      (log/info "Killed CIDER session:" session-name))
    success))

(defn init-pool!
  "Initialize session pool with n sessions."
  [n]
  (log/info "Initializing CIDER session pool with" n "sessions")
  (doseq [i (range n)]
    (let [name (str "agent-pool-" i)
          session (spawn-session! name)]
      (when session
        (swap! session-pool update :sessions assoc name session)
        (.offer (:available @session-pool) name))))
  (log/info "Pool initialized:" (count (:sessions @session-pool)) "sessions"))

(defn shutdown-pool!
  "Shutdown all pooled sessions."
  []
  (log/info "Shutting down CIDER session pool")
  (doseq [[name _] (:sessions @session-pool)]
    (kill-session! name))
  (reset! session-pool {:sessions {} :available (ArrayBlockingQueue. 10)}))

(defn acquire-session!
  "Acquire an available session from pool, blocking up to timeout-ms."
  [timeout-ms]
  (when-let [name (.poll (:available @session-pool) timeout-ms TimeUnit/MILLISECONDS)]
    (swap! session-pool assoc-in [:sessions name :status] :busy)
    (get-in @session-pool [:sessions name])))

(defn release-session!
  "Return a session to the pool."
  [session-name]
  (swap! session-pool assoc-in [:sessions session-name :status] :idle)
  (.offer (:available @session-pool) session-name))

(defn pool-status
  "Get current pool status."
  []
  {:total (count (:sessions @session-pool))
   :available (.size (:available @session-pool))
   :sessions (vals (:sessions @session-pool))})

(defn- nrepl-eval
  "Evaluate code on an nREPL port with timeout."
  [port code timeout-ms]
  (try
    (require 'nrepl.core)
    (let [connect (resolve 'nrepl.core/connect)
          client (resolve 'nrepl.core/client)
          message (resolve 'nrepl.core/message)]
      (with-open [conn (connect :port port)]
        (let [c (client conn timeout-ms)
              responses (message c {:op "eval" :code code})]
          (reduce (fn [acc r]
                    (cond
                      (:value r) (assoc acc :value (:value r))
                      (:err r) (update acc :err str (:err r))
                      (:out r) (update acc :out str (:out r))
                      :else acc))
                  {:value nil :err "" :out ""}
                  responses))))
    (catch Exception e
      {:value nil :err (ex-message e) :out ""})))

(defrecord CiderBackend [session-name port timeout-ms]
  proto/LLMBackend

  (chat [_ messages _tools]
    (let [user-msg (last (filter #(= "user" (:role %)) messages))
          code (:content user-msg)
          result (nrepl-eval port code timeout-ms)]
      (if (and (:value result) (empty? (:err result)))
        {:type :text :content (:value result)}
        {:type :text :content (str "Error: " (:err result) "\nOutput: " (:out result))})))

  (model-name [_]
    (str "cider:" session-name)))

(defn cider-backend
  "Create a CiderBackend using a pooled session."
  ([] (cider-backend {}))
  ([{:keys [timeout-ms session] :or {timeout-ms 60000}}]
   (if-let [sess (if session
                   (get-in @session-pool [:sessions session])
                   (acquire-session! 5000))]
     (->CiderBackend (:name sess) (:port sess) timeout-ms)
     (throw (ex-info "No CIDER session available" {:pool (pool-status)})))))

(defn- require-model!
  "Return `model`, or throw naming the option the caller must supply.
   hive-mcp ships no default model."
  [model option]
  (or model
      (throw (ex-info (str "No Ollama model given: pass " option
                           " (read it from your config.edn, e.g. llm-providers.ollama-compat.default-model)")
                      {:error  :model-not-configured
                       :option option}))))

(defn hybrid-backend
  "Create a hybrid backend: Ollama for reasoning, CIDER for execution.
   :ollama-model is required."
  [{:keys [ollama-model cider-session timeout-ms]
    :or {timeout-ms 60000}}]
  {:ollama (ollama/->OllamaBackend "http://localhost:11434"
                                   (require-model! ollama-model :ollama-model))
   :cider (cider-backend {:timeout-ms timeout-ms :session cider-session})})

(def router-keys
  "Dispatch-routed backend (folded in from the former
   hive-mcp.agent.dispatch-backend namespace: the census freezes new
   namespaces under hive-mcp.agent). LLMBackend for the subscription clients
   that are not OpenAI-compat chat-completions endpoints (:anthropic :chatgpt
   :codex :kimi; see `hive-mcp.agent.provider.model/seed-registry`). hive-mcp
   does not depend on hive-agent: the LLMRouter is reached through a PORT, a
   `complete` fn of one canonical params map ({:messages :model :provider
   :tools}) returning the OpenAI-format assistant message.

   Registry provider keyword -> the :provider key the hive-agent LLMRouter
   routes on. :codex rides the ChatGPT subscription client."
  {:anthropic :anthropic
   :chatgpt   :chatgpt
   :codex     :chatgpt
   :kimi      :kimi})

(defn router-key
  "The LLMRouter :provider key for registry provider `provider`, or nil when
   the router has no client for it."
  [provider]
  (get router-keys provider))

(defn request-params
  "The canonical params map the router takes for one chat turn. Pure.
   `tools` are formatted to the OpenAI function-calling shape; an empty or
   nil tool list adds no :tools key."
  [router-provider model messages tools]
  (cond-> {:messages messages
           :model    model
           :provider router-provider}
    (seq tools) (assoc :tools (openrouter/format-tools tools))))

(defn message->result
  "Fold the router's OpenAI-format assistant message into the LLMBackend
   result: {:type :text :content ..} or {:type :tool_calls :calls [..]} or
   {:type :error :error ..}, with :usage {:input :output :total} when the
   message carries one. Pure."
  [message]
  (let [usage  (:usage message)
        result (openrouter/parse-response message)]
    (cond-> result
      usage (assoc :usage {:input  (:prompt_tokens usage)
                           :output (:completion_tokens usage)
                           :total  (:total_tokens usage)}))))

(defrecord DispatchBackend [complete provider router-provider model]
  proto/LLMBackend

  (chat [_ messages tools]
    (let [result (message->result
                  (complete (request-params router-provider model messages tools)))]
      (when (= :error (:type result))
        (log/warn (str (name provider) " empty response detected")
                  {:model model :error (:error result)}))
      result))

  (model-name [_] model))

(defonce ^:private router-complete-cache (atom nil))

(defn- soft-resolve
  "The var for `sym`, or nil when its namespace is not on the classpath."
  [sym]
  (try (requiring-resolve sym)
       (catch Throwable e
         (log/debug "dispatch-backend: not resolvable" sym (ex-message e))
         nil)))

(defn- build-router-complete
  "A `complete` port over a started hive-agent LLMRouter, or nil when
   hive-agent is not on the classpath or the router cannot start."
  []
  (let [start! (soft-resolve 'hive-agent.init/start-llm-router!)
        chat   (soft-resolve 'hive-agent.llm.protocol/chat-completion)]
    (when (and start! chat)
      (when-let [router (start!)]
        (fn [params] (chat router params))))))

(defn router-complete
  "The default `complete` port. The started router is cached once it exists;
   an absent hive-agent is retried on the next call, so mounting the addon
   later makes the providers reachable without a restart. Nil when none."
  []
  (or @router-complete-cache
      (when-let [f (build-router-complete)]
        (reset! router-complete-cache f)
        f)))

(defn dispatch-backend
  "Create a DispatchBackend for the dispatch-routed `provider`.
   Options:
     :provider         - registry keyword (:anthropic :chatgpt :codex :kimi)
     :entry            - its effective registry entry (supplies :default-model)
     :model            - model string; absent, the entry's configured :default-model
     :complete         - the router port (fn [params]) -> OpenAI-format message;
                         absent, the one :resolve-complete finds
     :resolve-complete - (fn []) -> a router port or nil; absent,
                         `router-complete` (the hive-agent LLMRouter)

   Throws ex-info at build time, with an :error key, when the router has no
   client for the provider (:dispatch-provider-unroutable), when no model is
   given or configured (:model-not-configured) or when no router is
   reachable (:dispatch-router-unavailable)."
  [{:keys [provider entry model complete resolve-complete]}]
  (let [prov-name       (some-> provider name)
        router-provider (router-key provider)
        effective-model (or model (:default-model entry))]
    (when-not router-provider
      (throw (ex-info (str prov-name " has no hive-agent router client")
                      {:error     :dispatch-provider-unroutable
                       :provider  provider
                       :dispatch  (:dispatch entry)
                       :available (vec (keys router-keys))})))
    (when-not effective-model
      (throw (ex-info (str "No model for " prov-name ": pass :model or set llm-providers."
                           prov-name ".default-model")
                      {:error      :model-not-configured
                       :provider   provider
                       :config-key (str "llm-providers." prov-name ".default-model")})))
    (let [complete (or complete ((or resolve-complete router-complete)))]
      (when-not complete
        (throw (ex-info (str prov-name " needs the hive-agent LLMRouter, which is not available")
                        {:error    :dispatch-router-unavailable
                         :provider provider
                         :fix      "Mount the hive-agent addon, or pass :complete"})))
      (->DispatchBackend complete provider router-provider effective-model))))

(defn make-backend
  "Factory function for creating LLM backends.

   Named types: :ollama :cider :openrouter :openai-compat :auto.
   Any other keyword that names a provider in the effective provider
   registry (static openrouter/provider-registry merged with config
   :llm-providers — e.g. :venice :axon :groq :together :fireworks :openai
   :ollama-compat) is routed through openai-compat-backend with that
   :provider, so config keys like :models.synthesis-backend can select a
   provider directly without a bespoke case.

   A DISPATCH-routed registry provider (:anthropic :chatgpt :codex :kimi:
   subscription clients, not chat-completions endpoints) gets a
   DispatchBackend (this namespace) over the hive-agent
   LLMRouter instead of the \"not an OpenAI-compat provider\" refusal. It is
   built eagerly: with no router reachable, no model, or no router client for
   the provider, this throws ex-info (:error :dispatch-router-unavailable |
   :model-not-configured | :dispatch-provider-unroutable), so a fallback chain
   skips the provider at build time. opts :complete overrides the router port."
  ([type] (make-backend type {}))
  ([type opts]
   (case type
     :ollama (ollama/->OllamaBackend
              (or (:host opts) "http://localhost:11434")
              (require-model! (:model opts) :model))
     :cider (cider-backend opts)
     :openrouter (openrouter/openrouter-backend opts)
     :openai-compat (openrouter/openai-compat-backend opts)
     :auto (openrouter/auto-backend opts)
     (let [registry (openrouter/effective-provider-registry)
           entry    (get registry type)]
       (cond
         (provider-model/dispatch-routed? entry)
         (dispatch-backend (assoc opts :provider type :entry entry))

         (contains? registry type)
         (openrouter/openai-compat-backend (assoc opts :provider type))

         :else
         (throw (ex-info "Unknown backend type"
                         {:type type
                          :available (into [:ollama :cider :openrouter
                                            :openai-compat :auto]
                                           (keys registry))})))))))

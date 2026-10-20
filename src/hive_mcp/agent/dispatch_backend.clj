(ns hive-mcp.agent.dispatch-backend
  "LLMBackend for DISPATCH-routed providers: the subscription clients that are
   not OpenAI-compat chat-completions endpoints (:anthropic, :chatgpt, :codex,
   :kimi; see `hive-mcp.agent.provider.model/seed-registry`).

   hive-mcp does not depend on hive-agent. The hive-agent LLMRouter is reached
   through a PORT: a `complete` fn of one canonical params map
   ({:messages :model :provider :tools}) returning the OpenAI-format assistant
   message. The default port soft-resolves
   `hive-agent.init/start-llm-router!` and `hive-agent.llm.protocol/chat-completion`;
   a host without hive-agent gets an ex-info at BUILD time (:error
   :dispatch-router-unavailable), so a fallback chain skips the provider
   instead of failing mid-call. Tests and other hosts pass `:complete`."
  (:require [hive-mcp.agent.openrouter :as openrouter]
            [hive-mcp.agent.protocol :as proto]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def router-keys
  "Registry provider keyword -> the :provider key the hive-agent LLMRouter
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

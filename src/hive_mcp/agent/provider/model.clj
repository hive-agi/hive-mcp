(ns hive-mcp.agent.provider.model
  "Provider DOMAIN: the value objects of the LLM-provider concept, and the seed
   data every other stratum reads.

   CPPB stratum: none. This namespace is pure values and predicates over them.
   It may require malli and clojure.string and nothing else — no config, no
   HTTP, no sibling stratum. `hive-mcp.agent.provider.strata-test` gates that.

   Vocabulary:
     provider entry: how to reach one provider (endpoint, secret, dispatch kind);
                     its :default-model and :available-models come from config only
     registry        — provider keyword -> entry
     dispatch-routed — an entry that is NOT an OpenAI-compat endpoint and must
                       be reached through its own client (e.g. Anthropic OAuth)"
  (:require [clojure.string :as str]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ---------------------------------------------------------------------------
;;; Value objects
;;; ---------------------------------------------------------------------------

(def ChatCompletionsUrl
  "Malli schema for an OpenAI-compatible chat-completions endpoint URL."
  [:and
   [:string {:gen/fmap (fn [s]
                         (let [host (str/replace (str s) #"[^a-zA-Z0-9]" "")]
                           (str "https://api." (if (str/blank? host) "example" host)
                                ".test/v1/chat/completions")))}]
   [:fn {:error/message "must be a URL ending in /chat/completions"}
    (fn [s] (and (string? s) (str/ends-with? s "/chat/completions")))]])

(def ProviderEntry
  "Malli schema for one registry entry, dispatched on `:dispatch`.

   :anthropic-oauth, :chatgpt-oauth and :subscription branches: a
     subscription run through hive-agent (see `seed-registry`). None carries
     an :api-url: none is an OpenAI-compat chat-completions endpoint, and
     `openai-compat-backend` refuses them. :chatgpt-oauth and :subscription
     entries hold `:secret-key nil`; an API key cannot be attached to them.
   default branch (no :dispatch): OpenAI-compat provider. :api-url is
     REQUIRED and must end in \"/chat/completions\"; :secret-key may be nil
     (nil = no auth needed, e.g. :ollama-compat).

   All branches are open maps. :default-model and :available-models are
   optional: the seed carries neither, config `:llm-providers` supplies them."
  [:multi {:dispatch :dispatch}
   [:anthropic-oauth
    [:map
     [:dispatch [:= :anthropic-oauth]]
     [:secret-key :keyword]
     [:default-model {:optional true} :string]
     [:available-models {:optional true} [:sequential :string]]
     [:model-aliases {:optional true} [:map-of :string :string]]]]
   [:chatgpt-oauth
    [:map
     [:dispatch [:= :chatgpt-oauth]]
     [:secret-key :nil]
     [:default-model {:optional true} :string]
     [:available-models {:optional true} [:sequential :string]]]]
   [:subscription
    [:map
     [:dispatch [:= :subscription]]
     [:secret-key :nil]
     [:default-model {:optional true} :string]
     [:available-models {:optional true} [:sequential :string]]]]
   [::m/default
    [:map
     [:api-url ChatCompletionsUrl]
     [:secret-key [:maybe :keyword]]
     [:default-model {:optional true} :string]
     [:available-models {:optional true} [:sequential :string]]
     [:deny-models {:optional true} [:sequential :string]]
     [:auth-probe {:optional true} [:or [:= false] [:map [:kind :keyword] [:url :string]]]]]]])

(def ProviderRegistry
  "Malli schema for the provider registry: provider keyword -> ProviderEntry."
  [:map-of :keyword ProviderEntry])

(defn valid-provider-entry?
  "True when `entry` conforms to `ProviderEntry`."
  [entry]
  (m/validate ProviderEntry entry))

(defn dispatch-routed?
  "True when `entry` names its own client rather than an OpenAI-compat endpoint."
  [entry]
  (boolean (and (map? entry) (:dispatch entry))))

(defn openai-compat?
  "True when `entry` is a reachable OpenAI-compat provider entry."
  [entry]
  (and (map? entry) (not (dispatch-routed? entry))))

;;; ---------------------------------------------------------------------------
;;; Seeds
;;; ---------------------------------------------------------------------------

(def subscription-only-models
  "Model families reached only through their vendor's subscription (Claude
   Max OAuth, the ChatGPT subscription, the Kimi subscription), never through
   an API key. Regex strings, matched case-insensitively anywhere in the model
   id. Every keyed OpenAI-compat entry refuses them unless it names its own
   :deny-models.

   Keep in step with `hive-agent.llm.provider.model/subscription-only-models`."
  ["claude" "anthropic" "^(opus|sonnet|haiku|fable)$"
   "(^|/)(openai-)?gpt-(?!oss)" "^openai/(?!gpt-oss)" "(^|/)(openai-)?o[1-9](-|$)"
   "chatgpt" "codex"
   "kimi" "moonshot"])

(def seed-registry
  "Known LLM providers, as a SEED.

   Dispatch-routed entries run on a subscription through hive-agent and are
   never an OpenAI-compat endpoint:
     :anthropic-oauth  `:anthropic`, the Anthropic Messages API client, which
                       takes the Claude subscription OAuth token only. Its
                       optional :model-aliases maps a bare size name
                       (opus, sonnet, haiku, fable) to a model id.
     :chatgpt-oauth    `:chatgpt`, the ChatGPT backend client, which takes the
                       codex login's OAuth token only.
     :subscription     a hive-agent subscription runtime of the same name
                       (`:codex`, `:kimi`), run as its own CLI.
   All others hit OpenAI-compat /v1/chat/completions endpoints.

   This is a SEED, not the definition: config `:llm-providers` extends,
   overrides and REMOVES entries through `hive-mcp.agent.provider/effective-registry`,
   so a new provider is a config entry, never an edit here.

   Entries are endpoint DESCRIPTORS only (api-url, secret-key, dispatch kind).
   No model id lives here: `:default-model` and `:available-models` are set
   per provider in config, e.g.
     hive config set llm-providers.venice.default-model <model-id>
   `:deny-models` is a refusal list, not a choice: regex strings a model id
   must not match on that provider (see `policy/model-refusal`). A keyed
   OpenAI-compat entry that names none refuses `subscription-only-models`;
   config replaces that by naming the field.

   It is also the ONE literal: `hive-mcp.config.merge/default-config`
   carries this var under `:llm-providers` rather than a second copy."
  {:anthropic     {:dispatch      :anthropic-oauth
                   :secret-key    :anthropic-api-key}
   :chatgpt       {:dispatch      :chatgpt-oauth
                   :secret-key    nil}
   :codex         {:dispatch      :subscription
                   :secret-key    nil}
   :kimi          {:dispatch      :subscription
                   :secret-key    nil}
   :openrouter    {:api-url       "https://openrouter.ai/api/v1/chat/completions"
                   :secret-key    :openrouter-api-key
                   ;; OpenRouter forwards a `cache_control` block marker to an
                   ;; Anthropic model unchanged, so breakpoints placed on the
                   ;; way out are honoured end to end. Read by
                   ;; `hive-mcp.agent.cache/cache-control-style`, which also
                   ;; requires the model to be one that honours the marker.
                   :cache-control :anthropic-style
                   :auth-probe    {:kind :openrouter-key-info
                                   :url  "https://openrouter.ai/api/v1/key"}}
   :venice        {:api-url       "https://api.venice.ai/api/v1/chat/completions"
                   :secret-key    :venice-api-key}
   :groq          {:api-url       "https://api.groq.com/openai/v1/chat/completions"
                   :secret-key    :groq-api-key}
   :together      {:api-url       "https://api.together.xyz/v1/chat/completions"
                   :secret-key    :together-api-key}
   :fireworks     {:api-url       "https://api.fireworks.ai/inference/v1/chat/completions"
                   :secret-key    :fireworks-api-key}
   :openai        {:api-url       "https://api.openai.com/v1/chat/completions"
                   :secret-key    :openai-api-key}
   :ollama-compat {:api-url       "http://localhost:11434/v1/chat/completions"
                   :secret-key    nil}})

(def seed-priority
  "SEED preference order for auto-discovery — not the order that runs.

   `hive-mcp.agent.provider/effective-priority` is what discovery reads: this
   vector minus whatever config removed, plus config-only providers, minus
   dispatch-routed entries. Config `:llm-provider-priority` replaces it outright.

   Every keyword here MUST resolve to a `seed-registry` entry: availability
   reads that entry's :secret-key, and a missing entry reads as nil = no auth
   needed, silently promoting a phantom provider."
  [:openrouter :venice :groq :together :fireworks :openai :ollama-compat])

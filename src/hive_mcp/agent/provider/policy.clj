(ns hive-mcp.agent.provider.policy
  "Provider PROMOTE stratum: pure data to data.

   Everything that decides WHICH provider, in WHAT order, and WITH WHAT model
   lives here, taking the world it needs as arguments (registry, configured
   overlay, present secret keys). No config read, no HTTP, no clock.

   May require `hive-mcp.agent.provider.model` and clojure.string, nothing
   else. `hive-mcp.agent.provider.strata-test` gates that."
  (:require [clojure.string :as str]
            [hive-mcp.agent.provider.model :as model]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ---------------------------------------------------------------------------
;;; Registry overlay
;;; ---------------------------------------------------------------------------

(defn overlay
  "Overlay a config `:llm-providers` map onto `seed`, returning the effective
   registry.

   Per config value:
     map            merges into the seeded entry, field by field (config wins)
     false or nil   REMOVES the provider, the disable lever
     anything else  is ignored, leaving the seeded entry untouched

   A non-map `config-providers` (absent config) leaves the seed alone."
  [seed config-providers]
  (if (map? config-providers)
    (reduce-kv (fn [acc k v]
                 (cond
                   (not (keyword? k))       acc
                   (map? v)                 (update acc k merge v)
                   (or (nil? v) (false? v)) (dissoc acc k)
                   :else                    acc))
               seed
               config-providers)
    seed))

;;; ---------------------------------------------------------------------------
;;; Discovery order
;;; ---------------------------------------------------------------------------

(defn discovery-order
  "The order auto-discovery walks, over `registry`.

   `configured` (config :llm-provider-priority) REPLACES the order outright when
   it is a sequence: nothing is appended to an explicit list. Otherwise the
   order is `seed-order` first, then every provider known only to config,
   sorted by name so the answer is deterministic.

   Dispatch-routed entries are excluded either way: auto-discovery may only
   hand out OpenAI-compat providers, and `openai-compat-backend` refuses a
   dispatch-routed one. Unknown keys are dropped."
  [registry seed-order configured]
  (let [compat?   (fn [k] (model/openai-compat? (get registry k)))
        explicit? (sequential? configured)
        order     (if explicit? (mapv keyword configured) (vec seed-order))
        seeded    (filterv compat? order)]
    (if explicit?
      seeded
      (into seeded
            (->> (keys registry)
                 (remove (set order))
                 (filter compat?)
                 (sort-by name))))))

(defn available
  "Providers in `order` that can actually be called: an entry whose :secret-key
   is nil needs no auth, otherwise its key must be in `present-secret-keys`."
  [registry order present-secret-keys]
  (let [present? (set present-secret-keys)]
    (filterv (fn [p]
               (let [{:keys [secret-key]} (get registry p)]
                 (or (nil? secret-key) (contains? present? secret-key))))
             order)))

(defn secret-keys-of
  "The secret keys `order` would consult, in order, ready for a collector to
   resolve. nil entries (no auth needed) are dropped."
  [registry order]
  (into [] (comp (map #(:secret-key (get registry %))) (remove nil?)) order))

;;; ---------------------------------------------------------------------------
;;; Validation
;;; ---------------------------------------------------------------------------

(defn validate-provider
  "Validate a provider keyword against `registry`. nil on success, error map on
   failure."
  [registry provider]
  (when-not (contains? registry provider)
    {:error     :unknown-provider
     :requested provider
     :available (vec (keys registry))
     :fix       "Use one of the available providers, or add via: hive config set llm-providers.<name>.api-url <url>"}))

(defn validate-model
  "Validate a model against a provider's :available-models, when it declares
   any. nil on success, error map on failure."
  [registry provider model]
  (let [available (:available-models (get registry provider))]
    (when (and (seq available) (not (some #{model} available)))
      {:error     :unknown-model-for-provider
       :provider  provider
       :model     model
       :available (vec available)
       :fix       (str "Use one of the available models for " (name provider)
                       ", or add via: hive config set llm-providers." (name provider)
                       ".available-models [...]")})))

(defn model-refusal
  "nil when `provider`'s entry in `registry` permits `model`, else an error map
   naming the first refusing pattern the model matched. Patterns are regex
   source strings, matched case-insensitively anywhere in the model id: the
   entry's own :deny-models when it names that field,
   `model/subscription-only-models` otherwise. Only a keyed OpenAI-compat
   entry refuses; a dispatch-routed or keyless one refuses nothing. Unlike
   `validate-model` this is a REFUSAL, never a warning."
  [registry provider model]
  (let [entry (get registry provider)]
    (when (and (string? model)
               (model/openai-compat? entry)
               (some? (:secret-key entry)))
      (when-let [pattern (some #(when (re-find (re-pattern (str "(?i)" %)) model) %)
                               (get entry :deny-models model/subscription-only-models))]
        {:error    :model-denied-for-provider
         :provider provider
         :model    model
         :pattern  pattern
         :fix      (str "Route " model " through its own subscription (a bare "
                        "claude-* model goes to :anthropic OAuth; ChatGPT and Kimi "
                        "run on their subscription runtimes), or name "
                        "llm-providers." (name provider) ".deny-models [...]")}))))

(defn unresolved-routing
  "nil when `resolved` names both a provider and a model, else an error map
   naming the config keys that would supply the missing value.

   hive-mcp ships no model or provider choice, so an absent value is an
   error to fix in config, never a fallback."
  [agent-type {:keys [provider model]}]
  (when-not (and provider model)
    (let [type-key (if agent-type (name agent-type) "<agent-type>")
          cfg-keys (cond-> [(str "agent-defaults." type-key)]
                     provider (conj (str "llm-providers." (name provider) ".default-model")))]
      {:error       (if provider :model-not-configured :provider-not-configured)
       :agent-type  agent-type
       :provider    provider
       :config-keys cfg-keys
       :fix         (str "Pass provider and model explicitly, or set one of "
                         (str/join ", " cfg-keys)
                         ", e.g. hive config set agent-defaults." type-key
                         " '{:provider :venice :model \"<model-id>\"}'")})))

;;; ---------------------------------------------------------------------------
;;; Model-name policy
;;; ---------------------------------------------------------------------------

(defn parse-model-prefix
  "Parse an optional '<provider>:<model>' prefix, against `registry`.
   Returns [provider-kw-or-nil clean-model]. Only a prefix naming a provider
   the registry knows is recognized."
  [registry model]
  (if-let [idx (and (string? model) (str/index-of model ":"))]
    (let [prefix  (subs model 0 idx)
          tail    (subs model (inc idx))
          prov-kw (keyword prefix)]
      (if (contains? registry prov-kw)
        [prov-kw tail]
        [nil model]))
    [nil model]))

(defn claude-model-name?
  "True if the model string identifies a native Anthropic Claude model.
   Routes directly through Anthropic OAuth/API (bypassing OpenRouter relay).
   Strips `anthropic/` prefix so `anthropic/claude-sonnet-4-6` and
   `claude-sonnet-4-6` both match."
  [model]
  (when (string? model)
    (let [clean (if (str/starts-with? model "anthropic/")
                  (subs model (count "anthropic/"))
                  model)]
      (boolean (re-find #"^claude-" clean)))))

(def subscription-routes
  "Where each subscription-only model family runs, first match wins.

   `:pattern` (case-insensitive regex) claims the family's model ids, which
   keep their name minus any `:strip` prefix. `:aliases` are bare names
   (case-insensitive) that mean the provider's :default-model. `:families`
   are bare size names resolved through the provider's :model-aliases."
  [{:provider :anthropic
    :pattern  "^(anthropic/)?claude-"
    :strip    "^anthropic/"
    :aliases  #{"claude" "anthropic"}
    :families #{"opus" "sonnet" "haiku" "fable"}}
   {:provider :chatgpt
    :pattern  "^(openai/)?(chatgpt|codex-|gpt-(?!oss)|o[1-9](-|$))"
    :strip    "^openai/"
    :aliases  #{"chatgpt" "gpt" "codex" "openai"}}
   {:provider :kimi
    :pattern  "^(moonshotai/)?kimi-"
    :aliases  #{"kimi" "moonshot"}}])

(defn- route-match
  "The {:provider :model} `route` assigns `model`, or nil when the route does
   not claim it. A bare `:aliases` name takes `registry`'s :default-model for
   that provider; a `:families` name takes that provider's :model-aliases entry
   for it, or stays as given; a model id keeps its name, minus the `:strip`
   prefix."
  [registry {:keys [provider pattern strip aliases families]} model]
  (let [m     (str/trim model)
        lower (str/lower-case m)]
    (cond
      (contains? aliases lower)
      {:provider provider
       :model    (:default-model (get registry provider))}

      (contains? families lower)
      {:provider provider
       :model    (get-in registry [provider :model-aliases lower] m)}

      (and pattern (re-find (re-pattern (str "(?i)" pattern)) m))
      {:provider provider
       :model    (cond-> m
                   strip (str/replace (re-pattern (str "(?i)" strip)) ""))})))

(defn subscription-route
  "The {:provider :model} a subscription-only `model` routes to, or nil.
   The first `subscription-routes` entry that claims `model` wins."
  [registry model]
  (when (string? model)
    (some #(route-match registry % model) subscription-routes)))

;;; ---------------------------------------------------------------------------
;;; Spawn refusals: a pair no client can serve, a credential that cannot work
;;; ---------------------------------------------------------------------------

(defn routing-refusal
  "nil when `provider` can carry `model`, else an error map.

   Only a dispatch-routed provider that owns families in `subscription-routes`
   refuses: it serves its own families, their aliases and its :default-model,
   and nothing else. An OpenAI-compat provider refuses nothing here."
  [registry provider model]
  (let [entry  (get registry provider)
        routes (filterv #(= provider (:provider %)) subscription-routes)]
    (when (and (string? model)
               (model/dispatch-routed? entry)
               (seq routes)
               (not= model (:default-model entry))
               (not-any? #(route-match registry % model) routes))
      {:error    :model-unroutable-on-provider
       :provider provider
       :model    model
       :fix      (str (name provider) " serves only its own model family, so "
                      model " would fail at its first turn. Name the provider "
                      "that serves " model ": the provider param, or a "
                      "'<provider>:<model>' prefix")})))

(defn missing-secret
  "nil when `provider` needs no secret or `present-secret-keys` holds its
   :secret-key, else an error map. A dispatch-routed provider authenticates in
   its own client, so it is never refused here."
  [registry provider present-secret-keys]
  (let [{:keys [secret-key] :as entry} (get registry provider)]
    (when (and (model/openai-compat? entry)
               (some? secret-key)
               (not (contains? (set present-secret-keys) secret-key)))
      {:error      :provider-secret-missing
       :provider   provider
       :secret-key secret-key
       :fix        (str "Configure the secret " (name secret-key)
                        ", or name another provider for this spawn")})))

(defn credential-refusal
  "nil when a credential probe of `provider` shows a usable credential, else an
   error map. `probe` is {:status int :remaining number-or-nil}: a 401 or 403
   refuses, a 2xx with a numeric `:remaining` at or below zero refuses, and any
   other answer is inconclusive and passes."
  [provider {:keys [status remaining]}]
  (cond
    (contains? #{401 403} status)
    {:error    :provider-credential-rejected
     :provider provider
     :status   status
     :fix      (str (name provider) " rejected its configured key (HTTP " status
                    "). Replace the key, or name another provider for this spawn")}

    (and (int? status) (<= 200 status 299) (number? remaining) (<= remaining 0))
    {:error     :provider-quota-exhausted
     :provider  provider
     :remaining remaining
     :fix       (str (name provider) " key has no credit left (limit_remaining "
                     remaining "). Raise its limit, or name another provider "
                     "for this spawn")}))

(defn strip-anthropic-prefix
  "Strip the `anthropic/` prefix from a model name so native Anthropic API
   receives `claude-sonnet-4-6` rather than `anthropic/claude-sonnet-4-6`
   (the latter is OpenRouter's naming scheme, not Anthropic's)."
  [model]
  (if (and (string? model) (str/starts-with? model "anthropic/"))
    (subs model (count "anthropic/"))
    model))

;;; ---------------------------------------------------------------------------
;;; Routing
;;; ---------------------------------------------------------------------------

(defn resolve-routing
  "Decide {:provider :model} for a spawn, purely.

   Inputs are the world already collected: `registry`, the caller's request,
   this agent-type's `type-defaults` from config, and `fallback-provider` (the
   best available one, or nil).

   Resolution order (first match wins):
     0. explicit :provider                    caller forced routing
     1. '<provider>:<model>' prefix in :model  e.g. 'venice:qwen3-...'
     2. `subscription-route`                   Claude, ChatGPT, Kimi to OAuth
     3. type-defaults :provider from config
     4. fallback-provider

   A subscription route also shapes the model (vendor prefix stripped, a bare
   alias resolved to its entry's :default-model) when routing was explicit to
   that same provider.

   Explicit routing (steps 0 and 1) to another provider wins over step 2, but
   routing is not permission: a keyed OpenAI-compat provider refuses the
   subscription-only families at `model-refusal`, so `venice:claude-...` or
   `:provider :openrouter` with a Claude model resolves here and is refused
   by the caller.

   Returns {:provider kw :model str}; validation belongs to the caller."
  [registry {:keys [provider model type-defaults fallback-provider]}]
  (let [explicit-prov             (some-> provider keyword)
        [prefix-prov clean-model] (parse-model-prefix registry model)
        candidate-model           (or clean-model (:model type-defaults))
        explicit                  (or explicit-prov prefix-prov)
        routed                    (subscription-route registry candidate-model)
        sub-route                 (when (or (nil? explicit)
                                            (= explicit (:provider routed)))
                                    routed)
        eff-provider              (or explicit
                                      (:provider sub-route)
                                      (some-> type-defaults :provider keyword)
                                      fallback-provider)]
    {:provider eff-provider
     :model    (if sub-route
                 (:model sub-route)
                 (or candidate-model
                     (:default-model (get registry eff-provider))))}))

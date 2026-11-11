(ns hive-mcp.agent.router.pools
  "Model pools and work-class mapping as pure config data.

   Defines the default pool topology (cheap, mid, frontier) and maps work
   classes to pools. Pure fns take the config map and merge user overrides
   over the defaults, so no filesystem or env is read here.

   Config keys consumed:
     [:router :pools]          map of pool-keyword -> [PoolMember ...]
     [:router :work-classes]   map of work-class-keyword -> pool-keyword

   PoolMembers are {:provider <string> :model <string> :weight <pos-int>}."
  (:require [malli.core :as m]))

;; ── Schemas ────────────────────────────────────────────────────────────────

(def PoolMember
  "A single model-pool entry: which provider, which model, and its weight
   (higher = more likely to be selected)."
  [:map
   [:provider :string]
   [:model :string]
   [:weight pos-int?]])

(def Pools
  "The full pool map: keys are pool keywords (:cheap, :mid, :frontier),
   values are vectors of PoolMember."
  [:map-of :keyword [:vector PoolMember]])

(def WorkClasses
  "Maps a work-class keyword to a pool keyword."
  [:map-of :keyword :keyword])

;; ── Defaults ───────────────────────────────────────────────────────────────

(def default-pools
  "Default model pools keyed by tier. Each pool is a vector of members
   with a weight (higher = more likely to be selected)."
  {:cheap    [{:provider "venice"   :model "deepseek-v4-flash"                   :weight 3}
              {:provider "venice"   :model "qwen3-coder-480b-a35b-instruct-turbo" :weight 1}]
   :mid      [{:provider "chatgpt"  :model "gpt"    :weight 1}
              {:provider "kimi"     :model "kimi"   :weight 1}]
   :frontier [{:provider "anthropic" :model "claude" :weight 1}]})

(def default-work-classes
  "Default mapping from work-class keywords to pool keywords.
   An unknown work class resolves to :frontier (fail towards expensive)."
  {:grunt      :cheap
   :sweep      :cheap
   :triage     :cheap
   :read-only  :cheap
   :review     :mid
   :plan       :frontier
   :design     :frontier
   :money      :frontier
   :auth       :frontier
   :migration  :frontier
   :production :frontier
   :secrets    :frontier})

;; ── Public API ─────────────────────────────────────────────────────────────

(defn pools
  "Resolve the effective pools map from `config`.
   Merges config's [:router :pools] over `default-pools`."
  [config]
  (merge default-pools (get-in config [:router :pools])))

(defn work-classes
  "Resolve the effective work-class mapping from `config`.
   Merges config's [:router :work-classes] over `default-work-classes`."
  [config]
  (merge default-work-classes (get-in config [:router :work-classes])))

(defn pool-for
  "Resolve the pool for a `work-class` given `config`.

   Returns {:pool <pool-keyword> :members <vector of PoolMember>}.
   An unknown work class returns the :frontier pool (fail towards the
   expensive tier, never towards the cheap one)."
  [config work-class]
  (let [ps        (pools config)
        wanted    (get (work-classes config) work-class :frontier)
        ;; A work class mapped to a pool nobody declared also goes to
        ;; :frontier: an empty pool would leave the caller without a model.
        pool-key  (if (seq (get ps wanted)) wanted :frontier)]
    {:pool pool-key :members (get ps pool-key [])}))

;; ── Contracts ──────────────────────────────────────────────────────────────

(def Config
  "The config map the lookups read; only [:router ...] is consulted."
  [:maybe [:map-of :any :any]])

(def PoolChoice
  "What `pool-for` answers."
  [:map [:pool :keyword] [:members [:vector PoolMember]]])

(m/=> pools [:=> [:cat Config] Pools])
(m/=> work-classes [:=> [:cat Config] WorkClasses])
(m/=> pool-for [:=> [:cat Config [:maybe :keyword]] PoolChoice])

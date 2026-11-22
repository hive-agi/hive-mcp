(ns hive-mcp.config.write-policy
  "What an MCP `config set` may write. Pure: no IO, no atoms, no config reads;
   `current-mode` reads one env var and nothing else.

   `classify` answers one write as data. Layers, in order:
     FLOOR      `hard-floor` paths no declaration opens, every ancestor of one,
                any segment naming a secret (`sensitive/sensitive-name-re`) or
                a command, binary, path or endpoint (`exec-name-re`), in the
                path or in a map value's keys. A vector key is also checked
                re-split on dots, as the dotted key it prints as.
     ADDONS     `[:addons :*]` is floor by default (`addon-floor`); an addon's
                `:config/writable` declaration opens literal paths strictly
                under its own `[:addons <id>]`, never over the hard floor.
     ALLOWLIST  default-deny: a write is allowed only at or under an entry,
                after that entry's value check. An ancestor of a checked
                entry is refused, because a map there would skip the check.

   `decide` applies the mode. Rules in `enforced-rules` refuse in every mode;
   under `:warn` only `:not-allowlisted` becomes `:would-refuse`. The mode is
   code (`default-mode`) or HIVE_CONFIG_SET_MODE, never config.edn.

   Paths are keyword vectors. In a pattern `:*` matches any one segment and a
   set matches any of its members."
  (:require [clojure.string :as str]
            [hive-mcp.config.sensitive :as sensitive]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Schemas
;; =============================================================================

(def Mode
  "Enforcement mode of the default-deny layer."
  [:enum :warn :enforce])

(def Pattern
  "A path pattern: keywords, `:*` for any one segment, or a set of keywords."
  [:vector [:or :keyword [:set :keyword]]])

(def Rule
  "Why a write was classified the way it was."
  [:enum :floor :ancestor-of-protected :non-canonical :not-allowlisted :bad-value
   :allowlisted :addon-declared])

(def KeyInput
  "A write's key: the dotted string the caller sent, or a keyword path."
  [:or :string [:vector :any]])

(def Classification
  "`classify`'s answer. `:key` is the key as the caller named it, `:path` the
   parsed path (nil when it is not canonical), `:matched` the floor or
   allowlist pattern that decided it (nil when none did)."
  [:map
   [:decision [:enum :allow :refuse]]
   [:rule Rule]
   [:reason :string]
   [:key :string]
   [:path [:maybe [:vector :any]]]
   [:matched [:maybe Pattern]]])

(def Decision
  "`decide`'s answer: a Classification under a mode, which can now also be
   `:would-refuse` (written, but refused once the mode is `:enforce`)."
  [:map
   [:decision [:enum :allow :refuse :would-refuse]]
   [:rule Rule]
   [:reason :string]
   [:key :string]
   [:path [:maybe [:vector :any]]]
   [:matched [:maybe Pattern]]
   [:mode Mode]])

(def Declarations
  "Addon `:config/writable` declarations: addon id -> keyword paths."
  [:maybe [:map-of :any [:sequential :any]]])

(def AllowEntry
  "One effective allowlist entry and where it came from."
  [:map
   [:path Pattern]
   [:source [:or :keyword :string]]
   [:check {:optional true} :keyword]])

(def RefusalError
  "The `:config/set-refused` error a refused write answers with."
  [:map
   [:error [:= :config/set-refused]]
   [:key :string]
   [:rule Rule]
   [:matched [:maybe Pattern]]
   [:message :string]
   [:fix :string]])

;; =============================================================================
;; Data: floor and central allowlist
;; =============================================================================

(def exec-name-re
  "A key segment naming something run, read or reached: a command, binary,
   script, path, directory, file, host, URL or endpoint."
  #"(?i)(^|[-_.])(commands?|cmds?|exec|execs|executables?|argv|args|bin|binary|binaries|scripts?|shell|programs?|launchers?|paths?|dirs?|directory|directories|files?|hosts?|urls?|uris?|endpoints?|sockets?)($|[-_.])")

(def default-mode
  "Mode of the default-deny layer when HIVE_CONFIG_SET_MODE is unset or blank."
  :warn)

(def mode-env-var
  "Env var that overrides `default-mode`."
  "HIVE_CONFIG_SET_MODE")

(def provider-allowlist
  "The `[:llm-providers <p> ..]` leaves carved out of the `[:llm-providers :*]`
   floor."
  [{:path [:llm-providers :* #{:default-model :available-models :model-aliases}]
    :check :existing-provider-segment}])

(def services-allowlist
  "The `[:services ..]` leaves carved out of the `[:services :*]` floor."
  [{:path [:services :scheduler #{:enabled :interval-minutes :memory-limit
                                  :edge-limit :disc-enabled}]}
   {:path [:services :agent #{:reasoning :headless-spawn-timeout-ms
                              :headless-dispatch-timeout-ms}]}
   {:path [:services :agent :chat-points :gc :keep-days]}])

(def hard-floor
  "Paths no MCP write and no addon declaration may touch, most specific first
   so `:matched` names the narrowest protector. `:except` carves allowlist
   leaves out of an entry."
  [{:path [:secrets]}
   {:path [:services :seal]}
   {:path [:addons :hive.guard]}
   {:path [:addons :hive :guard]}
   {:path [:tool-roots]}
   {:path [:transports]}
   {:path [:headless]}
   {:path [:ling]}
   {:path [:forge :spawn-mode]}
   {:path [:services :forge]}
   {:path [:services :agent :masquerade-as-claude-code?]}
   {:path [:claude-code-version]}
   {:path [:observability]}
   {:path [:services :observability]}
   {:path [:formatters]}
   {:path [:runtimes]}
   {:path [:services :presets]}
   {:path [:cartography]}
   {:path [:memory :stores]}
   {:path [:memory :type-registry]}
   {:path [:embedder :providers]}
   {:path [:embeddings :* :host]}
   {:path [:project-overrides]}
   {:path [:parent-rules]}
   {:path [:project-roots]}
   {:path [:llm-providers :*] :except (mapv :path provider-allowlist)}
   {:path [:services :*] :except (mapv :path services-allowlist)}])

(def addon-floor
  "Floor over every addon's section. Unlike `hard-floor`, an addon's own
   declaration opens a literal path under it."
  [{:path [:addons :*]}])

(def central-allowlist
  "Default-deny allowlist owned by hive-mcp. `:check` names a value check:
     :existing-llm-provider       the value names a key of :llm-providers
     :existing-provider-segment   the path's provider segment is one
     :existing-embedder-provider  the value names an [:embedder :providers] id"
  (-> [{:path [:models :*]}
       {:path [:agent-defaults :* :model]}
       {:path [:agent-defaults :* :provider] :check :existing-llm-provider}
       {:path [:embedder :routes :*] :check :existing-embedder-provider}
       {:path [:embedder :default] :check :existing-embedder-provider}
       {:path [:embeddings :* :model]}
       {:path [:embeddings :warmup :enabled]}
       {:path [:hivemind :shout-message-cap]}]
      (into provider-allowlist)
      (into services-allowlist)))

(def enforced-rules
  "Refusal rules enforced in every mode; only the rest follow the mode."
  #{:floor :ancestor-of-protected :non-canonical :bad-value})

;; =============================================================================
;; Path matching and spelling
;; =============================================================================

(defn- seg-matches?
  "Does pattern segment `p` match path segment `s`? `s` may itself be a set
   (a pattern read as a path), matched when every member matches."
  [p s]
  (cond
    (= :* p) true
    (set? s) (every? #(seg-matches? p %) s)
    (set? p) (contains? p s)
    :else    (= p s)))

(defn at-or-under?
  "True when `path` equals `pattern` or lies below it."
  [pattern path]
  (and (<= (count pattern) (count path))
       (every? true? (map seg-matches? pattern path))))

(m/=> at-or-under? [:=> [:cat Pattern [:vector :any]] :boolean])

(defn ancestor-of?
  "True when `path` is a proper prefix of some path `pattern` matches, so a
   write at `path` would replace what `pattern` protects."
  [pattern path]
  (and (< (count path) (count pattern))
       (every? true? (map seg-matches? pattern path))))

(m/=> ancestor-of? [:=> [:cat Pattern [:vector :any]] :boolean])

(defn- seg->str
  "Dotted-key spelling of one path segment; a pattern set is spelled
   {a|b} with its members sorted."
  [k]
  (cond
    (keyword? k) (subs (str k) 1)
    (set? k)     (str "{" (str/join "|" (sort (map seg->str k))) "}")
    :else        (str k)))

(defn key-string
  "The dotted key a path is spelled as."
  [path]
  (str/join "." (map seg->str path)))

(m/=> key-string [:=> [:cat [:sequential :any]] :string])

(def ^:private seg-re
  ;; one canonical segment: lower-case, no dot, no leading colon, no `*`.
  #"[a-z0-9][a-z0-9_?!+/-]*")

(def ^:private addon-id-re
  ;; an addon id segment: canonical segments joined by dots (hive.keg).
  #"[a-z0-9][a-z0-9_?!+/-]*(\.[a-z0-9][a-z0-9_?!+/-]*)*")

(defn- canonical-seg?
  [re k]
  (boolean (and (keyword? k) (nil? (namespace k)) (re-matches re (name k)))))

(defn parse-key
  "Parse a write's key to a canonical keyword path, or nil when it is not
   canonical. A string must be lower-case dotted segments with no empty
   segment. A vector must hold unqualified keywords that are canonical
   segments; only segment 1 under `:addons` (the addon id) may carry a dot."
  [k]
  (cond
    (string? k)
    (when (re-matches addon-id-re k)
      (mapv keyword (str/split k #"\.")))

    (vector? k)
    (when (and (seq k)
               (every? true?
                       (map-indexed (fn [i s]
                                      (canonical-seg?
                                       (if (and (= 1 i) (= :addons (first k))) addon-id-re seg-re)
                                       s))
                                    k)))
      k)

    :else nil))

(m/=> parse-key [:=> [:cat :any] [:maybe [:vector :keyword]]])

(defn resplit
  "`path` as the dotted key it prints as, split again on dots: the path a
   dotted-key writer would actually write."
  [path]
  (mapv keyword (str/split (key-string path) #"\.")))

(m/=> resplit [:=> [:cat [:vector :keyword]] [:vector :keyword]])

;; =============================================================================
;; Protected names and floor lookups
;; =============================================================================

(defn protected-name?
  "True when `k` names a secret (`sensitive/sensitive-name-re`) or something
   run, read or reached (`exec-name-re`)."
  [k]
  (boolean (and (or (keyword? k) (string? k) (symbol? k))
                (let [n (name k)]
                  (or (re-find sensitive/sensitive-name-re n)
                      (re-find exec-name-re n))))))

(m/=> protected-name? [:=> [:cat :any] :boolean])

(defn- value-protected-key
  "The first key inside `v` (at any depth of nested maps and collections)
   that is a `protected-name?`, else nil."
  [v]
  (cond
    (map? v)  (some (fn [[k x]] (if (protected-name? k) k (value-protected-key x))) v)
    (coll? v) (some value-protected-key v)
    :else     nil))

(defn- entry-hit
  "The first entry of `floor-entries` that `path` is at or under, outside the
   entry's `:except` carve-outs, else nil."
  [floor-entries path]
  (some (fn [{p :path ex :except}]
          (when (and (at-or-under? p path)
                     (not-any? #(at-or-under? % path) ex))
            p))
        floor-entries))

(defn- ancestor-hit
  "The first pattern of `entries` that `path` is a proper ancestor of."
  [entries path]
  (some (fn [{p :path}] (when (ancestor-of? p path) p)) entries))

(defn- hard-floor-verdict
  "[rule matched reason] when the hard floor protects `path`, else nil. A
   floor entry is named before a protected segment name, so `matched` is the
   protecting section where there is one."
  [path]
  (if-let [hit (entry-hit hard-floor path)]
    [:floor hit (str "it is under the protected path " (key-string hit))]
    (if-let [seg (first (filter protected-name? path))]
      [:floor [(keyword (seg->str seg))]
       (str "segment " (seg->str seg) " names a secret, a command, a path or an endpoint")]
      (when-let [hit (ancestor-hit hard-floor path)]
        [:ancestor-of-protected hit
         (str "it is an ancestor of the protected path " (key-string hit)
              "; writing it would replace that subtree")]))))

(defn floored?
  "True when the hard floor protects `path`, read literally or re-split on
   dots: at or under a `hard-floor` entry, an ancestor of one, or carrying a
   protected segment name."
  [path]
  (boolean (or (hard-floor-verdict path)
               (and (every? keyword? path) (hard-floor-verdict (resplit path))))))

;; =============================================================================
;; Addon declarations and the effective allowlist
;; =============================================================================

(defn- ->addon-id
  "The keyword an addon id names, or nil unless it is a canonical addon id
   segment (a wildcard, a qualified keyword or a non-name never is)."
  [id]
  (let [k (cond (keyword? id) id
                (string? id)  (keyword id)
                :else         nil)]
    (when (canonical-seg? addon-id-re k) k)))

(defn- declaration-verdict
  "nil when addon `id` may open path `p`, else the reason it may not."
  [id p]
  (let [own [:addons id]]
    (cond
      (not (and (vector? p) (= p (parse-key p))))
      "not a literal canonical keyword path"
      (not (and (> (count p) (count own)) (at-or-under? own p)))
      (str "outside the addon's own subtree " (key-string own))
      (floored? p)
      "covered by the floor"
      :else nil)))

(defn addon-entries
  "Split addon `:config/writable` declarations into the entries accepted and
   the ones dropped. A declaration is accepted only when its addon id is a
   canonical addon id, its path is literal (no wildcard, no set) and lies
   strictly under that addon's own `[:addons <id>]`, and the hard floor does
   not cover it. Returns {:accepted [AllowEntry] :dropped [{:addon :path :reason}]}."
  [declarations]
  (reduce-kv
   (fn [acc raw-id paths]
     (let [id (->addon-id raw-id)]
       (reduce (fn [acc p]
                 (if-let [why (if id (declaration-verdict id p) "not a canonical addon id")]
                   (update acc :dropped conj {:addon raw-id :path p :reason why})
                   (update acc :accepted conj {:path p :source id})))
               acc
               (if (sequential? paths) paths [paths]))))
   {:accepted [] :dropped []}
   (if (map? declarations) declarations {})))

(m/=> addon-entries
      [:=> [:cat Declarations]
       [:map
        [:accepted [:vector AllowEntry]]
        [:dropped [:vector [:map [:addon :any] [:path :any] [:reason :string]]]]]])

(defn effective-allowlist
  "Every allowlist entry in force: the central entries (source :central), then
   the accepted addon declarations (source the addon id). Floor not included."
  ([] (effective-allowlist nil))
  ([declarations]
   (into (mapv #(assoc % :source :central) central-allowlist)
         (:accepted (addon-entries declarations)))))

(m/=> effective-allowlist
      [:function
       [:=> [:cat] [:vector AllowEntry]]
       [:=> [:cat Declarations] [:vector AllowEntry]]])

;; =============================================================================
;; Value checks
;; =============================================================================

(defn- ->kw
  [v]
  (cond (keyword? v) v
        (and (string? v) (not (str/blank? v))) (keyword (str/replace v #"^:" ""))
        :else nil))

(defn- named-in?
  "True when `v` names a key of map `m`."
  [m v]
  (boolean (when-let [k (->kw v)] (and (map? m) (contains? m k)))))

(defn- value-check
  "nil when `value` at `path` passes `check` against `cfg`, else the reason.
   The reason never quotes the value: a refused value may be a secret."
  [check path value cfg]
  (case check
    :existing-llm-provider
    (when-not (named-in? (:llm-providers cfg) value)
      "the value does not name a provider configured under llm-providers")

    :existing-provider-segment
    (when-not (named-in? (:llm-providers cfg) (nth path 1 nil))
      "its provider segment is not configured under llm-providers")

    :existing-embedder-provider
    (when-not (named-in? (get-in cfg [:embedder :providers]) value)
      "the value does not name a provider configured under embedder.providers")

    nil))

;; =============================================================================
;; Classify
;; =============================================================================

(defn- verdict
  [decision rule k path matched reason]
  {:decision decision :rule rule :reason (str "key " k " refused: " reason)
   :key k :path path :matched matched})

(defn- allow-entry-hit
  "The first entry of `entries` that `path` is at or under, else nil."
  [entries path]
  (some #(when (at-or-under? (:path %) path) %) entries))

(defn- declared?
  "True when `path` is at or under an accepted addon declaration."
  [entries path]
  (boolean (some #(and (not= :central (:source %)) (at-or-under? (:path %) path))
                 entries)))

(defn classify
  "Classify one MCP write of `value` at key `k` (a dotted string or a keyword
   path) against the config map `cfg`, with optional addon `declarations`.
   Pure. Answers a Classification: :allow, or :refuse with the :rule that
   refused it (:non-canonical, :floor, :ancestor-of-protected, :bad-value or
   :not-allowlisted) and the pattern that protects the path in :matched.
   No :reason ever quotes the value."
  ([k value cfg] (classify k value cfg nil))
  ([k value cfg declarations]
   (let [path  (parse-key k)
         k-str (if (string? k) k (key-string k))]
     (if-not path
       (verdict :refuse :non-canonical k-str nil nil
                "it is not canonical (lower-case dotted segments, none empty)")
       (let [entries (effective-allowlist declarations)
             entry   (allow-entry-hit entries path)
             hard    (or (hard-floor-verdict path)
                         (hard-floor-verdict (resplit path)))
             v-key   (value-protected-key value)
             addon   (when-not (declared? entries path)
                       (or (when-let [hit (entry-hit addon-floor path)]
                             [:floor hit (str "it is under " (key-string hit)
                                              ", which only the addon's own declaration opens")])
                           (when-let [hit (ancestor-hit addon-floor path)]
                             [:ancestor-of-protected hit
                              (str "it is an ancestor of " (key-string hit)
                                   "; writing it would replace every addon's section")])))
             checked (ancestor-hit (filter :check entries) path)]
         (cond
           hard
           (let [[rule matched reason] hard]
             (verdict :refuse rule k-str path matched reason))

           v-key
           (verdict :refuse :floor k-str path [(keyword (seg->str v-key))]
                    (str "its value carries key " (seg->str v-key)
                         ", which names a secret, a command, a path or an endpoint"))

           addon
           (let [[rule matched reason] addon]
             (verdict :refuse rule k-str path matched reason))

           checked
           (verdict :refuse :ancestor-of-protected k-str path checked
                    (str "it is an ancestor of " (key-string checked)
                         ", whose value check a map written here would skip"))

           (nil? entry)
           (verdict :refuse :not-allowlisted k-str path nil
                    "it is not on the config set allowlist")

           :else
           (if-let [why (value-check (:check entry) path value cfg)]
             (verdict :refuse :bad-value k-str path (:path entry) why)
             {:decision :allow
              :rule     (if (= :central (:source entry)) :allowlisted :addon-declared)
              :reason   (str "key " k-str " is allowlisted by " (key-string (:path entry)))
              :key      k-str
              :path     path
              :matched  (:path entry)})))))))

(m/=> classify
      [:function
       [:=> [:cat KeyInput :any [:maybe :map]] Classification]
       [:=> [:cat KeyInput :any [:maybe :map] Declarations] Classification]])

;; =============================================================================
;; Mode and decision
;; =============================================================================

(defn parse-mode
  "The mode an HIVE_CONFIG_SET_MODE value names. Unset or blank is
   `default-mode`; \"warn\" (case and a leading colon ignored) is :warn; any
   other value is :enforce, so a typo fails closed."
  [env-val]
  (let [s (when (string? env-val)
            (-> env-val str/trim str/lower-case (str/replace #"^:" "")))]
    (cond
      (or (nil? env-val) (and (string? env-val) (str/blank? env-val))) default-mode
      (= "warn" s) :warn
      :else        :enforce)))

(m/=> parse-mode [:=> [:cat :any] Mode])

(defn current-mode
  "The mode in force: HIVE_CONFIG_SET_MODE through `parse-mode`. Never read
   from config.edn, so a config write cannot loosen it."
  []
  (parse-mode (System/getenv mode-env-var)))

(m/=> current-mode [:=> [:cat] Mode])

(defn apply-mode
  "Decide a Classification under `mode`. A refusal whose rule is in
   `enforced-rules` stays :refuse in every mode; any other refusal is
   :would-refuse under :warn and :refuse under :enforce."
  [mode classification]
  (let [{:keys [decision rule]} classification]
    (assoc classification
           :mode mode
           :decision (if (and (= :refuse decision)
                              (= :warn mode)
                              (not (contains? enforced-rules rule)))
                       :would-refuse
                       decision))))

(m/=> apply-mode [:=> [:cat Mode Classification] Decision])

(defn decide
  "Pure decision for one write under `mode`: `classify`, then `apply-mode`."
  ([mode k value cfg] (decide mode k value cfg nil))
  ([mode k value cfg declarations]
   (apply-mode mode (classify k value cfg declarations))))

(m/=> decide
      [:function
       [:=> [:cat Mode KeyInput :any [:maybe :map]] Decision]
       [:=> [:cat Mode KeyInput :any [:maybe :map] Declarations] Decision]])

(defn refusal-error
  "The :config/set-refused error for a refused Decision or Classification:
   it names the key, the rule and the protecting path, and says how a human
   makes the change instead. It never carries the value."
  [{:keys [key rule matched reason]}]
  {:error   :config/set-refused
   :key     key
   :rule    rule
   :matched matched
   :message (str "config set refused (" (name rule) "): " reason)
   :fix     (str "Edit " key " in ~/.config/hive-mcp/config.edn by hand, then run `config reload`.")})

(m/=> refusal-error [:=> [:cat [:map [:key :string] [:rule Rule]
                                [:matched [:maybe Pattern]] [:reason :string]]]
                     RefusalError])

(m/=> floored? [:=> [:cat [:vector :any]] :boolean])

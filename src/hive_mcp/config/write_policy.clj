(ns hive-mcp.config.write-policy
  "What an MCP `config set` may write. Pure: no IO, no atoms, no config reads;
   `current-mode` reads one env var and nothing else.

   `classify` answers one write as data. Layers, in order:
     FLOOR      central, never writable, no declaration opens it. Also any path
                segment matching `sensitive-name-re`, any `:command` segment
                (in the path or in a map value's keys), and every ANCESTOR of a
                floor path. A key that is not canonical is refused here too.
     ALLOWLIST  central, default-deny: a write is allowed only at or under an
                entry, after that entry's value check passes.
     ADDONS     `:config/writable` paths, accepted only under the declaring
                addon's own `[:addons <id>]` and never over the floor.

   `decide` applies the mode. Rules in `enforced-rules` are refused in every
   mode; under `:warn` any other refusal becomes `:would-refuse`. The mode is
   code (`default-mode`) or HIVE_CONFIG_SET_MODE, never config.edn.

   Paths are keyword vectors. In a pattern `:*` matches any one segment and a
   set matches any of its members."
  (:require [clojure.string :as str]
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
  "A write's key: the dotted string the caller sent, or a parsed keyword path."
  [:or :string [:vector :any]])

(def Classification
  "`classify`'s answer. `:key` is the key as the caller named it, `:path` the
   parsed path (nil when it could not be parsed), `:matched` the floor or
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
  [:maybe [:map-of [:or :keyword :string] [:sequential [:vector :any]]]])

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

(def sensitive-name-re
  "A key segment naming a secret: the expression the config tool's response
   redaction uses."
  #"(?i)(token|secret|passw|passphrase|credential|api[-_]?key|private[-_]?key|(^|[-_.])keys?($|[-_.]))")

(def default-mode
  "Mode of the default-deny layer when HIVE_CONFIG_SET_MODE names none."
  :warn)

(def mode-env-var
  "Env var that overrides `default-mode`."
  "HIVE_CONFIG_SET_MODE")

(def services-allowlist
  "The `[:services ..]` leaves carved out of the `[:services :*]` floor."
  [{:path [:services :scheduler #{:enabled :interval-minutes :memory-limit
                                  :edge-limit :disc-enabled}]}
   {:path [:services :agent #{:reasoning :headless-spawn-timeout-ms
                              :headless-dispatch-timeout-ms}]}
   {:path [:services :agent :chat-points :gc :keep-days]}])

(def floor
  "Paths no MCP write may touch, most specific first so `:matched` names the
   narrowest protector. `:except` patterns are carved out of an entry."
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
   {:path [:llm-providers :* #{:api-url :secret-key :dispatch :key-probe :cache-control}]}
   {:path [:services :*] :except (mapv :path services-allowlist)}])

(def central-allowlist
  "Default-deny allowlist owned by hive-mcp. `:check` names a value check:
     :existing-llm-provider       the value names a key of :llm-providers
     :existing-provider-segment   the path's provider segment is one
     :existing-embedder-provider  the value names an [:embedder :providers] id"
  (into [{:path [:models :*]}
         {:path [:agent-defaults :* :model]}
         {:path [:agent-defaults :* :provider] :check :existing-llm-provider}
         {:path [:llm-providers :* #{:default-model :available-models :model-aliases}]
          :check :existing-provider-segment}
         {:path [:embedder :routes :*] :check :existing-embedder-provider}
         {:path [:embedder :default] :check :existing-embedder-provider}
         {:path [:embeddings :* :model]}
         {:path [:embeddings :warmup :enabled]}
         {:path [:hivemind :shout-message-cap]}]
        services-allowlist))

(def enforced-rules
  "Refusal rules enforced in every mode; the rest follow the mode."
  #{:floor :ancestor-of-protected :non-canonical :bad-value})

;; =============================================================================
;; Path matching
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

(def ^:private dotted-key-re
  ;; lower-case segments, no empty segment, no leading colon, no `*`.
  #"[a-z0-9][a-z0-9_?!+/-]*(\.[a-z0-9][a-z0-9_?!+/-]*)*")

(def ^:private path-seg-re
  ;; a key_path segment may carry a dot (addon ids such as hive.keg).
  #"[a-z0-9][a-z0-9_.?!+/-]*")

(defn parse-key
  "Parse a write's key to a canonical keyword path, or nil when it is not
   canonical. A string must be lower-case dotted segments with no empty
   segment; a vector must hold keywords whose names are canonical segments."
  [k]
  (cond
    (string? k)
    (when (re-matches dotted-key-re k)
      (mapv keyword (str/split k #"\.")))

    (vector? k)
    (when (and (seq k)
               (every? #(and (keyword? %) (re-matches path-seg-re (seg->str %))) k))
      k)

    :else nil))

(m/=> parse-key [:=> [:cat :any] [:maybe [:vector :keyword]]])

(defn- named?
  [k]
  (or (keyword? k) (string? k) (symbol? k)))

(defn- sensitive-seg?
  [k]
  (boolean (and (named? k) (re-find sensitive-name-re (seg->str k)))))

(defn- command-seg?
  [k]
  (and (named? k) (= "command" (str/lower-case (name k)))))

(defn- protected-seg?
  [k]
  (or (sensitive-seg? k) (command-seg? k)))

(defn- value-protected-key
  "The first key inside `v` (at any depth of nested maps and collections)
   that names a secret or a command, else nil."
  [v]
  (cond
    (map? v)  (some (fn [[k x]] (if (protected-seg? k) k (value-protected-key x))) v)
    (coll? v) (some value-protected-key v)
    :else     nil))

(defn- floor-hit
  "The floor entry `path` is at or under (outside its carve-outs), else nil."
  [path]
  (some (fn [{p :path ex :except}]
          (when (and (at-or-under? p path)
                     (not-any? #(at-or-under? % path) ex))
            p))
        floor))

(defn- floor-ancestor-hit
  "The floor pattern `path` is a proper ancestor of, else nil."
  [path]
  (some (fn [{p :path}] (when (ancestor-of? p path) p)) floor))

(defn floored?
  "True when `path` is protected by the floor: at or under a floor entry, an
   ancestor of one, or carrying a secret- or command-named segment."
  [path]
  (boolean (or (some protected-seg? path)
               (floor-hit path)
               (floor-ancestor-hit path))))

;; =============================================================================
;; Addon declarations and the effective allowlist
;; =============================================================================

(defn- ->addon-id
  [id]
  (cond (keyword? id) id
        (string? id)  (keyword id)
        :else         nil))

(defn- declaration-verdict
  "nil when addon `id` may open `p`, else the reason it may not."
  [id p]
  (let [own [:addons id]]
    (cond
      (not (and (vector? p) (seq p) (every? #(or (keyword? %) (set? %)) p)))
      "not a keyword path"
      (not (and (> (count p) (count own)) (at-or-under? own p)))
      (str "outside the addon's own subtree " (key-string own))
      (floored? p)
      "covered by the floor"
      :else nil)))

(defn addon-entries
  "Split addon `:config/writable` declarations into the entries accepted and
   the ones dropped. A path is accepted only strictly under `[:addons <id>]`
   of the addon that declared it, and never where the floor covers it.
   Returns {:accepted [AllowEntry] :dropped [{:addon :path :reason}]}."
  [declarations]
  (reduce-kv
   (fn [acc raw-id paths]
     (let [id (->addon-id raw-id)]
       (reduce (fn [acc p]
                 (if-let [why (if id (declaration-verdict id p) "addon id is not a keyword")]
                   (update acc :dropped conj {:addon raw-id :path p :reason why})
                   (update acc :accepted conj {:path p :source id})))
               acc
               paths)))
   {:accepted [] :dropped []}
   (or declarations {})))

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
  "nil when `value` at `path` passes `check` against `cfg`, else the reason."
  [check path value cfg]
  (case check
    :existing-llm-provider
    (when-not (named-in? (:llm-providers cfg) value)
      (str "provider " (pr-str value) " is not configured under llm-providers"))

    :existing-provider-segment
    (when-not (named-in? (:llm-providers cfg) (nth path 1 nil))
      (str "provider " (seg->str (nth path 1 nil)) " is not configured under llm-providers"))

    :existing-embedder-provider
    (when-not (named-in? (get-in cfg [:embedder :providers]) value)
      (str "embedder provider " (pr-str value) " is not configured under embedder.providers"))

    nil))

;; =============================================================================
;; Classify
;; =============================================================================

(defn- verdict
  [decision rule k path matched reason]
  {:decision decision :rule rule :reason reason
   :key k :path path :matched matched})

(defn- allow-entry-hit
  "The first entry of `entries` that `path` is at or under, else nil."
  [entries path]
  (some #(when (at-or-under? (:path %) path) %) entries))

(defn classify
  "Classify one MCP write of `value` at key `k` (a dotted string or a keyword
   path) against the config map `cfg`, with optional addon `declarations`.
   Pure. Answers a Classification: :allow, or :refuse with the :rule that
   refused it (:non-canonical, :floor, :ancestor-of-protected, :bad-value or
   :not-allowlisted) and the pattern that protects the path in :matched."
  ([k value cfg] (classify k value cfg nil))
  ([k value cfg declarations]
   (let [path  (parse-key k)
         k-str (if (string? k) k (key-string k))]
     (if-not path
       (verdict :refuse :non-canonical k-str nil nil
                (str "key " (pr-str k-str) " is not canonical: lower-case dotted segments, none empty"))
       (let [seg     (first (filter protected-seg? path))
             f-hit   (floor-hit path)
             a-hit   (floor-ancestor-hit path)
             v-key   (value-protected-key value)
             entries (effective-allowlist declarations)
             entry   (allow-entry-hit entries path)]
         (cond
           seg
           (verdict :refuse :floor k-str path [seg]
                    (str "key " k-str " has segment " (seg->str seg)
                         ", which names a secret or a command"))

           f-hit
           (verdict :refuse :floor k-str path f-hit
                    (str "key " k-str " is under the protected path " (key-string f-hit)))

           a-hit
           (verdict :refuse :ancestor-of-protected k-str path a-hit
                    (str "key " k-str " is an ancestor of the protected path "
                         (key-string a-hit) "; writing it would replace that subtree"))

           v-key
           (verdict :refuse :floor k-str path [(keyword (seg->str v-key))]
                    (str "value for " k-str " carries key " (seg->str v-key)
                         ", which names a secret or a command"))

           (nil? entry)
           (verdict :refuse :not-allowlisted k-str path nil
                    (str "key " k-str " is not on the config set allowlist"))

           :else
           (if-let [why (value-check (:check entry) path value cfg)]
             (verdict :refuse :bad-value k-str path (:path entry)
                      (str "value for " k-str " refused: " why))
             (verdict :allow
                      (if (= :central (:source entry)) :allowlisted :addon-declared)
                      k-str path (:path entry)
                      (str "key " k-str " is allowlisted by " (key-string (:path entry)))))))))))

(m/=> classify
      [:function
       [:=> [:cat KeyInput :any [:maybe :map]] Classification]
       [:=> [:cat KeyInput :any [:maybe :map] Declarations] Classification]])

;; =============================================================================
;; Mode and decision
;; =============================================================================

(defn parse-mode
  "The mode an HIVE_CONFIG_SET_MODE value names; `default-mode` for anything
   that is not warn or enforce (case and a leading colon ignored)."
  [env-val]
  (let [s (when (string? env-val)
            (-> env-val str/trim str/lower-case (str/replace #"^:" "")))]
    (case s
      "warn"    :warn
      "enforce" :enforce
      default-mode)))

(m/=> parse-mode [:=> [:cat :any] Mode])

(defn current-mode
  "The mode in force: HIVE_CONFIG_SET_MODE, else `default-mode`. Never read
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
   makes the change instead."
  [{:keys [key rule matched reason]}]
  {:error   :config/set-refused
   :key     key
   :rule    rule
   :matched matched
   :message (str "config set refused for key " key " (" (name rule) "): " reason)
   :fix     (str "Edit " key " in ~/.config/hive-mcp/config.edn by hand, then run `config reload`.")})

(m/=> refusal-error [:=> [:cat [:map [:key :string] [:rule Rule]
                                [:matched [:maybe Pattern]] [:reason :string]]]
                     RefusalError])

(m/=> floored? [:=> [:cat [:vector :any]] :boolean])

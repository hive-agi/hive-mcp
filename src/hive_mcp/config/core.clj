(ns hive-mcp.config.core
  "Global configuration orchestrator — Pipeline/Boundary layer.
   Owns config state atom, orchestrates IO and merge, re-exports accessors.
   Public API surface is unchanged — callers need no modifications."
  (:require [hive-mcp.config.merge :as merge]
            [hive-mcp.config.io :as config-io]
            [hive-mcp.config.resolve :as resolve]
            [hive-mcp.config.secrets :as secrets]
            [hive-mcp.config.schema :as schema]
            [hive-dsl.result :as result]
            [taoensso.timbre :as log]
            [hive-mcp.config.source :as src]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Re-exported Constants
;; =============================================================================

(def default-kg-backend merge/default-kg-backend)
(def default-config merge/default-config)

;; =============================================================================
;; State
;; =============================================================================

;; Cached global configuration atom.
;; nil = not loaded yet, map = loaded config.
(defonce ^:private global-config (atom nil))

(def ^:dynamic *config-source*
  "Bind to an IConfigSource to override where config is read from.

   nil (production) — the disk-loaded `global-config` atom.
   Tests bind `source/static-source` so they assert against config they declare
   rather than whatever the developer last wrote to ~/.config/hive-mcp."
  nil)

(defn- active-config-atom
  "The atom writes land in: the bound source's when one is bound, else the
   process-wide config atom. Throws when the bound source is read-only — a
   write that cannot land must not pretend it did."
  []
  (if-let [source *config-source*]
    (if (satisfies? src/IMutableConfigSource source)
      (src/-config-atom source)
      (throw (ex-info "Config source is read-only — bind a mutable source to write"
                      {:error :config/read-only-source
                       :source (class source)})))
    global-config))

;; =============================================================================
;; Loading (Orchestrator)
;; =============================================================================

(defn boot-write-plan
  "What a boot load may write to the config file at `path`, given whether the
   file exists and what reading it returned.

     :generate   - no file: write the defaults (or a migrated legacy config)
     :merge-back - the file read as a map: write it back deep-merged over the
                   defaults, so new default keys appear in the file
     :refuse     - the file exists but did not read as a map (a parse error,
                   an empty or half-written file): write NOTHING

   A file that exists is the operator's config. Defaults never replace it."
  [exists? read-result]
  (cond
    (not exists?)                                            :generate
    (and (result/ok? read-result) (map? (:ok read-result)))  :merge-back
    :else                                                    :refuse))

(m/=> boot-write-plan [:=> [:cat :boolean :any] [:enum :generate :merge-back :refuse]])

(defn- read-existing-config
  "Read the config at `path`. When the file exists but does not read as a map,
   retry a few times: a writer replacing the file can leave it briefly empty or
   half written, and that moment must not count as \"no config\"."
  [path]
  (loop [attempt 1]
    (let [r (config-io/read-config-file path)]
      (if (or (and (result/ok? r) (map? (:ok r)))
              (not (.exists (java.io.File. ^String path)))
              (>= attempt 3))
        r
        (do (Thread/sleep 150)
            (recur (inc attempt)))))))

(defn load-global-config!
  "Load global config from disk, deep-merge with default-config, cache in atom.
   Orchestrates: io/read → merge/deep-merge → atom reset → io/write.
   An existing file that cannot be read is never overwritten (see
   `boot-write-plan`)."
  ([]
   (load-global-config! config-io/config-path))
  ([path]
   (let [exists?     (.exists (java.io.File. ^String path))
         read-result (read-existing-config path)
         user-config (if (and (result/ok? read-result) (map? (:ok read-result)))
                       (:ok read-result)
                       (when exists?
                         (log/error "config.edn exists but did not read as a map;"
                                    "running on built-in defaults IN MEMORY ONLY and leaving the file untouched."
                                    "Fix the file and reload (project config reload)."
                                    {:path path :read read-result})
                         nil))
         ;; Try legacy path fallback, only when there is no config file at all
         user-config (or user-config
                         (when (and (not exists?) (= path config-io/config-path))
                           (let [legacy-result (config-io/read-config-file config-io/legacy-config-path)]
                             (when (and (result/ok? legacy-result) (:ok legacy-result))
                               (log/info "Migrating config: found legacy" config-io/legacy-config-path
                                         "-> please move to" config-io/config-path)
                               (:ok legacy-result)))))
         merged (if user-config
                  (merge/deep-merge merge/default-config user-config)
                  merge/default-config)
         ;; Validate config schema (warn on invalid, don't block startup)
         validation (schema/validate-config merged)
         _ (when-not (:valid? validation)
             (log/warn "Config validation errors (startup continues with merged config):"
                       (:humanized validation)))
         ;; Validate :memory section specifically for actionable errors
         mem-validation (schema/validate-memory-config merged)
         _ (when (and mem-validation (not (:valid? mem-validation)))
             (log/error "Memory routing config is invalid — memory router may fail:"
                        (:humanized mem-validation)
                        "\nExpected shape: {:default-store :keyword"
                        ":routes {type-kw :store-kw-or-{:primary :kw :projection :kw}}"
                        ":stores {store-kw {:addon :kw ...}}}"))
         ;; Resolve secrets: config → env → pass(1) → nil
         {:keys [secrets sources]} (secrets/resolve-all-secrets (:secrets merged))
         merged (assoc merged :secrets secrets)]
     (reset! global-config merged)
     ;; Persist the pre-secret-resolution config so pass: refs and nils stay in
     ;; the file; runtime secrets live only in the atom.
     (when (= path config-io/config-path)
       (case (boot-write-plan exists? read-result)
         :generate   (do (config-io/write-config! (if user-config
                                                    (merge/deep-merge merge/default-config user-config)
                                                    merge/default-config)
                                                  path)
                         (log/info "Auto-generated config.edn at" path))
         :merge-back (config-io/write-config! (merge/deep-merge merge/default-config user-config) path)
         :refuse     nil))
     (log/info "Global config loaded from" path
               (if user-config "(user config found, deep-merged with defaults)" "(using defaults)"))
     (secrets/log-secret-sources! sources)
     merged)))

;; =============================================================================
;; Accessors (all return defaults if not yet loaded)
;; =============================================================================

(defn get-global-config
  "Return the effective global config.

   Reads through *config-source* when one is bound (tests bind a static EDN map,
   so they never depend on the developer's ~/.config). Unbound — production —
   this is the disk-loaded atom, falling back to defaults."
  []
  (if-let [source *config-source*]
    (src/-config source)
    (or @global-config merge/default-config)))

(defn get-project-roots
  "Return the :project-roots vector from global config."
  []
  (:project-roots (get-global-config)))

(defn get-defaults
  "Return the :defaults map from global config."
  []
  (:defaults (get-global-config)))

(defn get-project-overrides
  "Return overrides for a specific project-id."
  [project-id]
  (get-in (get-global-config) [:project-overrides project-id]))

(defn get-project-config
  "Return the effective config for a project-id."
  [project-id]
  (let [defaults (get-defaults)
        overrides (get-project-overrides project-id)]
    (if overrides
      (merge defaults overrides)
      defaults)))

(defn get-parent-rules
  "Return the :parent-rules vector from global config."
  []
  (:parent-rules (get-global-config)))

(defn get-parent-for-path
  "Resolve parent-id for a directory path via :parent-rules."
  [directory-path]
  (resolve/get-parent-for-path (get-parent-rules) directory-path))

;; =============================================================================
;; Memory Config Accessors
;; =============================================================================

(defn get-memory-config
  "Return the :memory map from global config."
  []
  (:memory (get-global-config)))

(defn get-memory-route
  "Return the route for a given memory type keyword.
   Falls back to :default-store if no specific route exists."
  [type-kw]
  (let [mem (get-memory-config)]
    (or (get-in mem [:routes type-kw])
        (:default-store mem))))

(defn get-memory-store
  "Return the store definition for a given store keyword."
  [store-kw]
  (get-in (get-memory-config) [:stores store-kw]))

(defn get-kanban-store-mode
  "Return the kanban-store routing mode (:default | :dual-read | :kanban).
   Drives hive-mcp.vectordb.kanban-facade backend selection. Defaults to
   :default when the key is absent so legacy configs keep routing through
   the :default slot (milvus)."
  []
  (or (:kanban-store (get-memory-config)) :default))

;; =============================================================================
;; Generic Path Accessor
;; =============================================================================

(defn get-in-config
  "Read a value at a keyword path vector from the cached config.
   Example: (config/get-in-config [:memory :routes :decision]) => :chroma"
  [path]
  (get-in (get-global-config) path))

;; =============================================================================
;; Service & Secret Accessors (delegate to resolve layer)
;; =============================================================================

(defn get-service-config
  "Return config map for a specific service."
  [service-key]
  (get-in (get-global-config) [:services service-key]))

(defn get-service-mode
  "Return the :mode for a service."
  [service-key]
  (get-in (get-global-config) [:services service-key :mode] :local))

(defn get-service-value
  "Get a field from service config with mode-aware resolution and env fallback."
  [service-key field-key & opts]
  (apply resolve/get-service-value (get-global-config) service-key field-key opts))

(defn get-secret
  "Return a secret from config or env var fallback."
  [secret-key]
  (resolve/get-secret (get-global-config) secret-key))

;; =============================================================================
;; Dotted Key Path Access
;; =============================================================================

(defn parse-key-path
  "Parse a dotted key string into a keyword path vector."
  [key-str]
  (merge/parse-key-path key-str))

(defn get-config-value
  "Read a value at a dotted key path from the cached config."
  [key-str]
  (let [path (merge/parse-key-path key-str)]
    (get-in (get-global-config) path)))

;; =============================================================================
;; Set Config Value (Orchestrator)
;; =============================================================================

(defn set-config-value!
  "Update a value at a dotted key path and persist to disk.

   The write is read-modify-write against the FILE, not the boot-time atom:
   keys edited on disk while the server runs survive. An unreadable file is
   left untouched (the in-memory value is still updated).

   Under a bound `*config-source*` the write stays in memory: a test declares
   config, it does not rewrite the developer's ~/.config/hive-mcp."
  ([key-str value] (set-config-value! key-str value config-io/config-path))
  ([key-str value path]
   (let [kp (merge/parse-key-path key-str)]
     (when (empty? kp)
       (throw (ex-info "Invalid config key path" {:key key-str})))
     (if *config-source*
       (swap! (active-config-atom) assoc-in kp value)
       (do
         (when-not @global-config
           (load-global-config! path))
         (let [updated (swap! global-config assoc-in kp value)
               on-disk (config-io/read-config-file path)]
           (if (result/ok? on-disk)
             (config-io/write-config! (assoc-in (or (:ok on-disk) {}) kp value) path)
             (log/warn "Config file unreadable, not persisting" key-str on-disk))
           (log/info "Config updated:" key-str "=" value)
           updated))))))

;; =============================================================================
;; Runtime Mutation (in-memory only — does NOT persist to disk)
;; =============================================================================

(defn update-in-config!
  "In-memory `update-in` on the effective config. Does NOT persist to disk —
   use `set-config-value!` for that.

   Writes land in whichever atom `*config-source*` designates, so a test that
   binds a source sees its own writes and leaves the real config alone.

   IMPORTANT (production): this bypasses `~/.config/hive-mcp/config.edn`, so the
   change is per-JVM. On the next boot, `load-global-config!` deep-merges the
   on-disk user config over defaults — meaning any value the user explicitly
   placed at `path` via `hive config set` will resurface. Treat this as a
   guarded-override mechanism, not a substitute for persisted user intent."
  [path f & args]
  (let [a (active-config-atom)]
    (when-not @a
      (reset! a merge/default-config))
    (apply swap! a update-in path f args)))

;; =============================================================================
;; Reset (for testing)
;; =============================================================================

(defn reset-config!
  "Reset the cached config to nil. Useful for testing."
  []
  (reset! global-config nil))

;; =============================================================================
;; Contracts
;; =============================================================================
;; Config values are user data: accessors that return an arbitrary leaf
;; (get-in-config, get-config-value, get-service-value, get-secret,
;; get-parent-for-path) promise no shape beyond what the config file holds.
;; Validation at load is warn-only, so container outputs are only as tight as
;; the shapes the default config guarantees.

(def ^:private ProjectId
  "A project id as callers pass it: a string, a keyword, or nil when unknown."
  [:maybe [:or :string :keyword]])

(m/=> load-global-config! [:function
                           [:=> [:cat] :map]
                           [:=> [:cat :string] :map]])
(m/=> get-global-config [:=> [:cat] :map])
(m/=> get-project-roots [:=> [:cat] [:maybe [:sequential :string]]])
(m/=> get-defaults [:=> [:cat] [:maybe :map]])
(m/=> get-project-overrides [:=> [:cat ProjectId] [:maybe :map]])
(m/=> get-project-config [:=> [:cat ProjectId] [:maybe :map]])
(m/=> get-parent-rules [:=> [:cat] [:maybe [:sequential :map]]])
(m/=> get-parent-for-path [:=> [:cat [:maybe :string]] :any])
(m/=> get-memory-config [:=> [:cat] [:maybe :map]])
(m/=> get-memory-route [:=> [:cat :any] [:maybe schema/RouteTarget]])
(m/=> get-memory-store [:=> [:cat :any] [:maybe :map]])
(m/=> get-kanban-store-mode [:=> [:cat] schema/KanbanStoreMode])
(m/=> get-in-config [:=> [:cat [:sequential :any]] :any])
(m/=> get-service-config [:=> [:cat :keyword] [:maybe :map]])
(m/=> get-service-mode [:=> [:cat :keyword] [:or :keyword :string]])
(m/=> get-service-value [:=> [:cat :keyword :keyword [:* :any]] :any])
(m/=> get-secret [:=> [:cat :keyword] :any])
(m/=> parse-key-path [:=> [:cat [:maybe :string]] [:maybe [:vector :keyword]]])
(m/=> get-config-value [:=> [:cat [:maybe :string]] :any])
(m/=> set-config-value! [:function
                         [:=> [:cat :string :any] :map]
                         [:=> [:cat :string :any :string] :map]])
(m/=> update-in-config! [:=> [:cat [:sequential :any] ifn? [:* :any]] :map])
(m/=> reset-config! [:=> [:cat] :nil])
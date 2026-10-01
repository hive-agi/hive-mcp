(ns hive-mcp.tools.consolidated.hot
  "Consolidated `hot` tool — hot-reload of mounted IAddon instances.

   `hive hot reload <addon-id>` rebuilds one addon from its mount manifest and
   cascades to every addon that was handed its instance at mount time. The work
   itself lives in hive-addon.hot; this namespace is only the MCP edge:
   resolve which addons are actually mounted, drive the bridge, render a report.

   hive-addon.hot is resolved SOFTLY. hive-mcp pins hive-addon by version, and
   the bridge lands in a later release than the pin — a hard `:require` would
   make this namespace fail to compile against the pinned jar. Until the pin is
   bumped every command answers with an actionable :unavailable message instead
   of breaking the tool surface.

   Only addons that are CURRENTLY REGISTERED in the host are reloadable. The
   classpath manifests are the superset; the composer's plug layers may have
   deliberately dropped some of them, and remounting a dropped addon from its raw
   manifest would resurrect something the system chose not to run.

   hive-hot is the namespace-level engine. It MUST be initialized with the addon
   source dirs this system actually derived (`:hot/dirs`) and with the protocol
   interlock (`:no-reload`) — see `ensure-hot-init!` for why letting it
   self-initialize is a bug, not a convenience."
  (:require [hive-mcp.addons.core :as addon-core]
            [hive-mcp.addons.manifest :as manifest]
            [hive-mcp.tools.composite :as composite]
            [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [taoensso.timbre :as log]
            [hive-mcp.hot.core :as core-hot]
            [hive-mcp.extensions.runtime :as runtime]
            [malli.core :as m]
            [malli.error :as me]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Soft resolution of the bridge
;; =============================================================================

(defn- soft
  "Resolve a var, or nil. Re-resolved per call so a pin bump takes effect without
   restarting this namespace."
  [sym]
  (try (requiring-resolve sym) (catch Throwable _ nil)))

(defn- bridge-available? []
  (some? (soft 'hive-addon.hot/reload-addon!)))

(def ^:private unavailable-msg
  (str "hive-addon.hot is not on the classpath. The hot-reload bridge ships in "
       "hive-addon >= 0.3.5; bump io.github.hive-agi/hive-addon in deps.edn "
       "(or point local.deps.edn at ../hive-addon) and restart the server."))

;; =============================================================================
;; What is actually mounted
;; =============================================================================

(defn- mounted-ids
  "Ids of addons currently in the host registry."
  []
  (into #{} (map :name) (addon-core/list-addons)))

(defn effective-specs
  "MountSpecs for the addons that are actually mounted right now.

   Classpath discovery is the superset; intersecting it with the live registry is
   what keeps a reload from resurrecting an addon the composer dropped."
  []
  (if-let [discover (soft 'hive-addon.mount/discover-specs)]
    (let [live (mounted-ids)]
      (into [] (filter #(contains? live (:addon/id %))) (:specs (discover))))
    []))

(defn- host []
  (when-let [ctor (soft 'hive-mcp.extensions.mount-host/addon-registry-host)]
    (ctor)))

;; =============================================================================
;; hive-hot initialization — the dirs matter
;; =============================================================================

(defn- jvm-start-ms
  "When this JVM started. Everything on disk newer than that is newer than the
   image — the baseline a first reload should measure change against, not the
   moment somebody first ran `hot`."
  []
  (try (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))
       (catch Throwable _ nil)))

(defn ensure-hot-init!
  "Initialize hive-hot with THIS system's addon source dirs, or extend it.

   Without this, the first reload calls `hive-hot.core/reload!` on an
   uninitialized clj-reload, which self-initializes with its default
   `{:dirs [\"src\"]}` — resolved against the SERVER's working directory, not the
   addon repos. clj-reload then watches the wrong tree: it can neither see the
   addon sources that actually changed nor protect the protocol namespaces, and
   every subsequent reload reports success while reloading nothing relevant.

   The dirs come from `hive-addon.hot/plan`, which derives them from where each
   addon's constructor namespace physically resolves — so only `:local/root`
   addons contribute, which is exactly the set whose bytes can change.
   `:no-reload` carries the protocol interlock. `:since` is the JVM start time:
   an edit made after boot but before the first `hot` call is a change, not
   the baseline (hive-hot >= 0.1.9 honours it; older versions ignore it).

   With hive-hot's `ensure-init!` (>= 0.1.9) an already-initialized registry is
   EXTENDED with any new dirs instead of reset, so a `hot inject` adds its
   source root without discarding pending changes.

   Returns {:initialized? bool :dirs [...] :already? bool}. Never throws."
  [plan]
  (let [status  (soft 'hive-hot.core/status)
        init!   (soft 'hive-hot.core/init!)
        ensure! (soft 'hive-hot.core/ensure-init!)
        opts    (cond-> {:dirs (vec (:hot/dirs plan))
                         :no-reload (:hot/no-reload plan)}
                  (jvm-start-ms) (assoc :since (jvm-start-ms)))]
    (cond
      (nil? init!) {:initialized? false :dirs [] :reason :hive-hot-absent}

      ensure!
      (try
        (let [r (ensure! opts)]
          (when (:fresh? r)
            (log/info "hive-hot initialized for addon hot-reload"
                      {:dirs (count (:dirs opts))}))
          (when (seq (:added r))
            (log/info "hive-hot extended with addon dirs" {:added (:added r)}))
          {:initialized? true :already? (not (:fresh? r))
           :dirs (vec (:dirs r)) :added (vec (:added r))})
        (catch Throwable t
          (log/warn "hive-hot init failed" {:error (ex-message t)})
          {:initialized? false :dirs [] :reason (ex-message t)}))

      (:initialized? (status)) {:initialized? true :already? true
                                :dirs (vec (:hot/dirs plan))}

      :else
      (try
        (init! opts)
        (log/info "hive-hot initialized for addon hot-reload"
                  {:dirs (count (:hot/dirs plan))})
        {:initialized? true :already? false :dirs (vec (:hot/dirs plan))}
        (catch Throwable t
          (log/warn "hive-hot init failed" {:error (ex-message t)})
          {:initialized? false :dirs [] :reason (ex-message t)})))))

(defn- reload-opts
  "Options handed to the bridge.

   :resolve-config MUST sit under :mount-opts \u2014 that is the key the bridge
   threads into boundary/mount!. Passing it at the top level is silently
   ignored: mount! then falls back to port/resolve-config-default, which returns
   the bare manifest :addon/config with NO config.edn merge and NO
   :runtime/ports. The addon still constructs, still initializes, still reports
   :success? true \u2014 and comes back DEGRADED, having lost the host adapters it
   needs to contribute its own MCP commands. Measured: remounting hive.carto
   that way left it :active with runtime-ports :configured [] and dropped the
   `code carto \u2026` subdomain entirely.

   manifest/prepare-config is what the original mount used; anything less hands
   a remounted addon a thinner config than its first mount received.

   :provision (hive-mcp.extensions.runtime/mount-opts) rides along so a reload
   or an inject refreshes the addon's client runtime too."
  []
  {:mount-opts (merge {:resolve-config manifest/prepare-config}
                      (runtime/mount-opts))})

(defn- prepared
  "Resolve specs + host + plan and make sure hive-hot is initialized correctly.
   Returns {:specs :host :plan :hot-init} or {:error <mcp-error>}."
  []
  (let [specs (effective-specs)
        h     (host)]
    (cond
      (empty? specs)
      {:error (mcp-error "no mounted addons discovered — is the mount-compose loader enabled?")}

      (nil? h)
      {:error (mcp-error "mount-host adapter unavailable")}

      :else
      (let [plan ((soft 'hive-addon.hot/plan) h specs)]
        {:specs specs :host h :plan plan :hot-init (ensure-hot-init! plan)}))))

(defn- override-strategy
  "Apply an explicit :addon/reload-strategy to the targeted spec."
  [specs addon-id strategy]
  (if-not strategy
    specs
    (mapv (fn [s]
            (cond-> s
              (= addon-id (:addon/id s))
              (assoc :addon/reload-strategy (keyword strategy))))
          specs)))

(def remount-report-keys
  "The RemountReport keys a terminal reader gets. A key hive-addon adds to the
   report is shown by adding it here."
  [:hot/strategy :hot/seeds :hot/roots :hot/affected
   :hot/torn-down :hot/cycles :hot/widened
   :hot/ns-reloaded :hot/ns-skipped :hot/ns-dragged
   :hot/ns-unchanged? :hot/multi-file :hot/stale-ctors
   :hot/refused? :hot/preflight :hot/restored :hot/down :hot/restored?
   :ok? :errors :diagnostic :diagnostics :teardown/data-preserved?])

(def inject-report-keys
  "The InjectReport keys `inject` answers with."
  [:hot/path :hot/paths :hot/classpath
   :hot/discovered :hot/already-mounted
   :hot/injected :hot/affected :hot/torn-down
   :hot/missing :hot/dirs-added :hot/registered
   :hot/remembered :hot/adopted
   :hot/deps :discovery-errors :ok? :errors :diagnostic :diagnostics
   :teardown/data-preserved?])

(def eject-report-keys
  "The EjectReport keys `eject` answers with: what was removed and what stays."
  [:hot/target :hot/ejected :hot/unknown :hot/refused? :hot/blocking
   :hot/torn-down :hot/unregistered :hot/unsupported :hot/ungoverned
   :hot/unhot :hot/forgotten :hot/dirs-removed :hot/dirs-retained
   :hot/dirs-reason :hot/classpath-retained :hot/namespaces-retained
   :hot/remounted :teardown/data-preserved? :ok? :errors])

(def mount-result-keys
  "The per-addon MountResult keys shown under :mounted."
  [:addon/id :success? :phase :errors :diagnostic])

(defn- summarize
  "Trim a RemountReport to what is worth reading in a terminal."
  [report]
  (cond-> (select-keys report remount-report-keys)
    (seq (:mounted report))
    (assoc :mounted (mapv #(select-keys % mount-result-keys) (:mounted report)))))

(defn- surface-refreshed!
  "After a remount or an injection the addon's contributions are live in
   dispatch but the advertised surface — composites, tools/list, schema
   extensions — was assembled earlier. Bring it up to date. Never throws."
  []
  (when-let [refresh! (soft 'hive-mcp.extensions.reactive/refresh-surface!)]
    (try (refresh! nil)
         (catch Throwable t
           (log/warn "surface refresh after hot op failed" {:error (ex-message t)})
           nil))))

;; =============================================================================
;; Handlers
;; =============================================================================

(defn handle-reload
  "Reload one addon (or every addon built from one namespace) plus dependents."
  [{:keys [addon namespace strategy]}]
  (cond
    (not (bridge-available?)) (mcp-error unavailable-msg)
    (and (nil? addon) (nil? namespace))
    (mcp-error "addon (or namespace) is required — e.g. {:command \"reload\" :addon \"hive.carto\"}")

    :else
    (let [{:keys [specs host hot-init error]} (prepared)]
      (if error
        error
        (let [specs  (override-strategy specs addon strategy)
              report (if namespace
                       ((soft 'hive-addon.hot/reload-namespace!) host specs namespace (reload-opts))
                       ((soft 'hive-addon.hot/reload-addon!) host specs addon (reload-opts)))
              surface (surface-refreshed!)]
          (log/info "hot reload" {:addon addon :namespace namespace
                                  :ok? (:ok? report)
                                  :affected (:hot/affected report)})
          (mcp-json (assoc (summarize report) :hive-hot hot-init :surface surface)))))))

(defmulti skip-reason
  "Why `reload-all` leaves an addon out, from its AddonSource (a row of
   hive-addon.hot/plan's :hot/skipped). Dispatches on :hot/source-kind; a new
   source kind is one defmethod."
  :hot/source-kind)

(defmethod skip-reason :jar [_]
  "jar-backed: its bytes cannot change without a restart (restart-required)")

(defmethod skip-reason :absent [_]
  "constructor namespace has no source on the classpath (AOT-only or generated)")

(defmethod skip-reason :default [{kind :hot/source-kind}]
  (str "not reloadable (source-kind " (pr-str kind) ")"))

(def ReloadAllSeeds
  "What `reload-all` reloads and what it leaves out, with why."
  [:map
   [:seeds [:set :string]]
   [:skipped [:vector [:map [:addon/id :string] [:hot/source-kind :any] [:reason :string]]]]])

(defn reload-all-seeds
  "Pure. Partition hive-addon.hot/plan's verdict into reload-all SEEDS (the
   addons it marks reloadable) and SKIPPED ones, each with a reason.

   reload-all used to seed every mounted addon, and a single jar-backed addon
   in the set failed the whole pass. A jar addon can still be REMOUNTED as the
   dependent of a seed; it is only never a seed itself."
  [plan]
  {:seeds   (into (sorted-set) (map :addon/id) (:hot/registered plan))
   :skipped (mapv (fn [src]
                    {:addon/id        (:addon/id src)
                     :hot/source-kind (:hot/source-kind src)
                     :reason          (skip-reason src)})
                  (:hot/skipped plan))})

(defn handle-reload-all
  "Reload every mounted addon that hive-addon.hot/plan marks reloadable, in
   dependency order. The rest are reported under :skipped with a reason."
  [_params]
  (if-not (bridge-available?)
    (mcp-error unavailable-msg)
    (let [{:keys [specs host plan hot-init error]} (prepared)]
      (if error
        error
        (let [{:keys [seeds skipped]} (reload-all-seeds plan)]
          (if (empty? seeds)
            (mcp-json {:ok? true :hot/seeds [] :skipped skipped :hive-hot hot-init
                       :note "no mounted addon has reloadable source; nothing to reload"})
            (let [report  ((soft 'hive-addon.hot/reload-seeds!) host specs seeds (reload-opts))
                  surface (surface-refreshed!)]
              (mcp-json (assoc (summarize report)
                               :skipped skipped :hive-hot hot-init :surface surface)))))))))

(defn handle-inject
  "Mount an addon that was NOT on the classpath at boot.

   `path` is a project dir (its deps.edn :paths become classpath entries), a
   plain source dir, or a jar. Its manifests — and only its — are discovered,
   solved against the mounted addons, mounted through the ordinary pipeline
   with the same config resolver the boot used, registered with hive-hot, and
   advertised on the MCP surface. An addon already mounted is left alone.

   The classpath is extended on the highest DynamicClassLoader above this
   thread; a thread without one refuses rather than mounting code the next
   require could not find."
  [{:keys [path resolve_deps]}]
  (cond
    (not (bridge-available?)) (mcp-error unavailable-msg)

    (nil? path)
    (mcp-error "path is required — a project dir (deps.edn :paths), a source dir, or a jar")

    :else
    (if-let [inject! (soft 'hive-addon.hot.inject/inject!)]
      (let [specs (effective-specs)
            h     (host)]
        (if (nil? h)
          (mcp-error "mount-host adapter unavailable")
          (let [hot-init (when (seq specs)
                           (ensure-hot-init! ((soft 'hive-addon.hot/plan) h specs)))
                opts     (cond-> (reload-opts)
                           resolve_deps (assoc :resolve-deps? true))
                report   (inject! h specs path opts)
                surface  (surface-refreshed!)]
            (log/info "hot inject" {:path path
                                    :ok? (:ok? report)
                                    :injected (:hot/injected report)
                                    :already (:hot/already-mounted report)})
            (mcp-json (-> (select-keys report inject-report-keys)
                          (assoc :mounted (mapv #(select-keys % mount-result-keys)
                                                (:mounted report))
                                 :hive-hot hot-init
                                 :surface surface))))))
      (mcp-error (str "hive-addon.hot.inject is not on the classpath — injection ships in "
                      "hive-addon >= 0.3.8; bump the pin (or point local.deps.edn at "
                      "../hive-addon) and restart the server.")))))

(defn handle-list
  "Per-addon hot-reload readiness: strategy, where its source lives, and whether
   it can be reloaded at all. Effect-free apart from hive-hot init."
  [_params]
  (if-not (bridge-available?)
    (mcp-error unavailable-msg)
    (let [{:keys [plan error]} (prepared)]
      (if error
        error
        (mcp-json
         {:reloadable (mapv #(select-keys % [:addon/id :hot/strategy-id
                                             :addon/init-ns :hot/source-kind])
                            (:hot/registered plan))
          :not-reloadable (mapv #(select-keys % [:addon/id :addon/init-ns
                                                 :hot/source-kind])
                                (:hot/skipped plan))
          :watch-dirs (vec (:hot/dirs plan))
          :never-reload (mapv str (:hot/no-reload plan))
          :hive-hot-available? (:hot/available? plan)})))))

(defn handle-watch
  "Register every reloadable addon with hive-hot and START the file watcher, so
   editing an addon's source remounts it automatically.

   This is the standing-subscription form of `reload`: hive-hot's watcher and
   debouncer detect the change, clj-reload brings the new namespaces in, and the
   registered component callback remounts the affected addons."
  [_params]
  (if-not (bridge-available?)
    (mcp-error unavailable-msg)
    (let [{:keys [specs host plan error]} (prepared)]
      (if error
        error
        (let [report ((soft 'hive-addon.hot/hot!) host specs (reload-opts))
              watch! (soft 'hive-hot.core/init-with-watcher!)]
          (if-not watch!
            (mcp-error "hive-hot is not on the classpath — cannot start the watcher.")
            (let [res (try
                        (watch! {:dirs (vec (:hot/dirs plan))
                                 :no-reload (:hot/no-reload plan)})
                        (catch Throwable t {:error (ex-message t)}))]
              (log/info "hot watch started" {:dirs (count (:hot/dirs plan))
                                             :components (count (:hot/registered report))})
              (mcp-json {:watching (if (map? res) res (str res))
                         :dirs (vec (:hot/dirs plan))
                         :registered (mapv :addon/id (:hot/registered report))
                         :skipped (mapv :addon/id (:hot/skipped report))
                         :never-reload (mapv str (:hot/no-reload plan))
                         :ok? (:ok? report)
                         :errors (:errors report)}))))))))

(defn handle-unwatch
  "Stop the hive-hot file watcher and deregister every addon component."
  [_params]
  (if-not (bridge-available?)
    (mcp-error unavailable-msg)
    (let [specs (effective-specs)
          stop! (soft 'hive-hot.core/stop-watcher!)
          un    ((soft 'hive-addon.hot/unhot!) specs)]
      (mcp-json {:watcher (if stop! (str (stop!)) "hive-hot absent")
                 :unregistered (:hot/unregistered un)}))))

(defn no-reload-split
  "Pure. The effective no-reload set, split by who pins it: CORE-PINS are
   hive-mcp's own protocol definers (hive-mcp.hot.core's interlock), ADDON-PINS
   hive-addon.hot's protocol interlock. Everything is rendered as sorted
   strings; :effective is their union."
  [core-pins addon-pins]
  (let [core  (into (sorted-set) (map str) core-pins)
        addon (into (sorted-set) (map str) addon-pins)]
    {:effective (vec (into core addon))
     :core      (vec core)
     :addon     (vec addon)
     :counts    {:core (count core) :addon (count addon)
                 :effective (count (into core addon))}}))

(defn- core-pins
  "hive-mcp core's protocol pins, as the core reload would apply them. [] when
   they cannot be classified; never throws."
  []
  (try (:no-reload (core-hot/interlock (core-hot/classify)))
       (catch Throwable t
         (log/warn "core pin classification failed" {:error (ex-message t)})
         [])))

(defn handle-status
  "hive-hot availability + watcher state, the installed strategy chain, and the
   protocol interlock: the effective no-reload set, split into core and addon
   pins."
  [_params]
  (if-not (bridge-available?)
    (mcp-error unavailable-msg)
    (let [s ((soft 'hive-addon.hot/status))
          watcher (soft 'hive-hot.core/watcher-status)
          split   (no-reload-split (core-pins) (:hot/no-reload s))]
      (mcp-json (-> s
                    (assoc :hot/no-reload (:effective split)
                           :hot/no-reload-split (dissoc split :effective)
                           :mounted-addon-count (count (effective-specs))
                           :watcher (when watcher (watcher))))))))

(defn handle-strategies
  "The installed reload-strategy chain, in selection order."
  [_params]
  (if-not (bridge-available?)
    (mcp-error unavailable-msg)
    (let [chain ((soft 'hive-addon.hot/installed-strategies))
          sid   (soft 'hive-addon.hot.strategy/-strategy-id)]
      (mcp-json {:ids (mapv sid chain)
                 :note (str "Selection order. A spec may name one explicitly via "
                            ":addon/reload-strategy; modules add their own with "
                            "hive-addon.hot/register-strategy!.")}))))

;; =============================================================================
;; Tool definition
;; =============================================================================

(def ^:private lifecycle-off-msg
  "Addon lifecycle is not running. Enable it with {:services {:addons {:lifecycle {:enabled? true}}}} in config.edn (or HIVE_MCP_ADDON_LIFECYCLE=1) and restart, or the hive-addon on the classpath predates hive-addon.lifecycle.")

(defn- lifecycle-manager []
  (when-let [f (soft 'hive-addon.lifecycle/installed-manager)] (f)))

(defn- with-manager
  [f]
  (if-let [mgr (lifecycle-manager)]
    (f mgr)
    (mcp-error lifecycle-off-msg)))

(defn handle-lifecycle
  "Per-addon lifecycle phase, policy, last use and parts."
  [_params]
  (with-manager #(mcp-json ((soft 'hive-addon.lifecycle/status) %))))

;; -----------------------------------------------------------------------------
;; Lifecycle bridge verbs — the registration seam
;;
;; A verb that forwards to a hive-addon.lifecycle function is DATA: which
;; bridge symbol to call, which params it needs, how params become the call's
;; trailing args. `lifecycle-verb` turns that descriptor into a handler, so a
;; new verb is ONE entry in `canonical-handlers`, e.g. the planned unmount:
;;
;;   :unmount (lifecycle-verb {:bridge   'hive-addon.lifecycle/eject!
;;                             :requires [:addon]
;;                             :example  "{:command \"unmount\" :addon \"hive.rss\"}"})
;;
;; The bridge is held as a SYMBOL and resolved on every call (Capture-by-Var,
;; 20260817195749-0d407e9c): a hive-addon that gains the fn later, or is
;; reloaded, is reached without touching this namespace, and one that lacks it
;; answers an actionable error instead of breaking the tool surface.
;; -----------------------------------------------------------------------------

(def LifecycleVerb
  [:map
   [:bridge qualified-symbol?]
   [:requires {:optional true} [:vector :keyword]]
   [:args {:optional true} ifn?]
   [:render {:optional true} ifn?]
   [:example {:optional true} :string]])

(defn- missing-param-error
  [k example]
  (mcp-error (str (name k) " is required" (when example (str ", e.g. " example)))))

(defn run-lifecycle-verb
  "Run a LifecycleVerb descriptor against PARAMS: check the required params,
   require the installed lifecycle manager, resolve the bridge now and call it
   as (bridge mgr & (args params)). `args` defaults to [(:addon params)];
   `render` (default identity) projects the bridge's answer before it is sent."
  [{:keys [bridge requires args render example] :as verb} params]
  {:pre [(m/validate LifecycleVerb verb)]}
  (if-let [k (some #(when (nil? (get params %)) %) requires)]
    (missing-param-error k example)
    (with-manager
      (fn [mgr]
        (if-let [f (soft bridge)]
          (mcp-json ((or render identity) (apply f mgr ((or args (juxt :addon)) params))))
          (mcp-error (str bridge " is not on the classpath; the hive-addon in this "
                          "image predates it. Bump the hive-addon pin and restart.")))))))

(defn lifecycle-verb
  "A `hot` handler for the LifecycleVerb DESCRIPTOR. The returned fn closes over
   data only and calls `run-lifecycle-verb` through its var, so a reload of
   this namespace reaches it."
  [descriptor]
  {:pre [(m/validate LifecycleVerb descriptor)]}
  (fn lifecycle-verb-handler [params]
    (run-lifecycle-verb descriptor params)))

(def handle-activate
  "Mount a dormant addon now, with the dependencies it lacks."
  (lifecycle-verb {:bridge   'hive-addon.lifecycle/activate!
                   :requires [:addon]
                   :example  "{:command \"activate\" :addon \"hive.carto\"}"}))

(defn eviction-outcome
  "Pure. An EvictionReport with a refusal stated, never dressed as success: a
   report that is :refused?, or that did not evict and names a :reason, answers
   :ok? false :refused? true. Anything else passes through."
  [report]
  (if (and (map? report)
           (or (:refused? report)
               (and (false? (:evicted? report)) (some? (:reason report)))))
    (assoc report :ok? false :refused? true)
    report))

(def handle-evict
  "Release an addon to dormant stubs (force for pinned/eager). A refusal
   answers :ok? false :refused? true with its :reason."
  (lifecycle-verb {:bridge   'hive-addon.lifecycle/evict!
                   :requires [:addon]
                   :args     (fn [{:keys [addon force]}] [addon {:force? (true? force)}])
                   :render   #'eviction-outcome
                   :example  "{:command \"evict\" :addon \"hive.carto\"}"}))

;; -----------------------------------------------------------------------------
;; Host bridge verbs — the second registration seam
;;
;; A verb that forwards to a hive-addon.hot fn of shape
;; (bridge host specs target opts) is DATA too: which bridge, how params name
;; the target, which opts it adds, which report keys it answers with. The
;; effects it needs (the mount host, the mounted specs, the bridge options,
;; the surface refresh) arrive as HostPorts, so a test hands it stubs and the
;; tool hands it the vars below.
;; -----------------------------------------------------------------------------

(def HostVerb
  [:map
   [:bridge qualified-symbol?]
   [:target ifn?]
   [:opts {:optional true} ifn?]
   [:report-keys [:vector :keyword]]
   [:example {:optional true} :string]])

(def HostPorts
  [:map
   [:host ifn?]
   [:specs ifn?]
   [:reload-opts ifn?]
   [:refresh! ifn?]])

(defn default-host-ports
  "The HostPorts the tool runs on, each reached through its var."
  []
  {:host        #'host
   :specs       #'effective-specs
   :reload-opts #'reload-opts
   :refresh!    #'surface-refreshed!})

(defn unsupported-report
  "Pure. What a host verb answers when BRIDGE is absent from this image."
  [bridge]
  {:ok?     false
   :reason  :unsupported
   :bridge  (str bridge)
   :message (str bridge " is not on the classpath; the hive-addon in this image "
                 "predates it. Bump the hive-addon pin (or point local.deps.edn at "
                 "../hive-addon) and restart.")})

(defn run-host-verb
  "Run a HostVerb descriptor against PARAMS over PORTS: derive the target,
   resolve the bridge now, call (bridge host specs target opts), refresh the
   surface, answer the report's :report-keys plus :surface. An absent bridge
   answers `unsupported-report`; nothing here throws."
  [{:keys [bridge target opts report-keys example] :as verb} ports params]
  {:pre [(m/validate HostVerb verb) (m/validate HostPorts ports)]}
  (let [t (target params)
        f (soft bridge)
        h (when (and (some? t) f) ((:host ports)))]
    (cond
      (nil? t) (mcp-error (str "a target is required" (when example (str ", e.g. " example))))
      (nil? f) (mcp-json (unsupported-report bridge))
      (nil? h) (mcp-error "mount-host adapter unavailable")
      :else
      (let [report  (try (f h ((:specs ports)) t
                            (merge ((:reload-opts ports)) (when opts (opts params))))
                         (catch Throwable e
                           (log/warn "host verb bridge threw" {:bridge bridge :error (ex-message e)})
                           {:ok? false :reason :bridge-threw :errors [(str bridge ": " (ex-message e))]}))
            surface ((:refresh! ports))]
        (log/info "hot host verb" {:bridge bridge :target t :ok? (:ok? report)})
        (mcp-json (cond-> (assoc (select-keys report report-keys) :surface surface)
                    (:reason report) (assoc :reason (:reason report))))))))

(defn host-verb
  "A `hot` handler for the HostVerb DESCRIPTOR over `default-host-ports`. The
   returned fn closes over data only and calls `run-host-verb` through its var."
  [descriptor]
  {:pre [(m/validate HostVerb descriptor)]}
  (fn host-verb-handler [params]
    (run-host-verb descriptor (default-host-ports) params)))

(defn eject-target
  "Pure. What `eject` plugs out: the addon id, else the path it was injected from."
  [{:keys [addon path]}]
  (or addon path))

(def handle-eject
  "Plug an addon OUT of the running host (the inverse of inject). Refused while
   mounted addons depend on it unless cascade, which remounts them without it."
  (host-verb {:bridge      'hive-addon.hot.inject/eject!
              :target      #'eject-target
              :opts        (fn [{:keys [cascade]}] {:cascade? (true? cascade)})
              :report-keys eject-report-keys
              :example     "{:command \"eject\" :addon \"hive.rss\"}"}))

;; -----------------------------------------------------------------------------
;; pin — a validated policy change
;; -----------------------------------------------------------------------------

(def ^:private fallback-policies
  "Used only when hive-addon.lifecycle.policy is not resolvable."
  #{:eager :lazy :pinned})

(def ^:private fallback-idle-ms
  "hive-addon.lifecycle.policy/default-idle-ms, when that is not resolvable."
  1800000)

(defn policy-schema
  "Malli enum of the lifecycle policies hive-addon knows, read from
   hive-addon.lifecycle.policy/policies at call time so a policy added there
   needs no edit here."
  []
  (into [:enum] (sort (or (some-> (soft 'hive-addon.lifecycle.policy/policies) deref)
                          fallback-policies))))

(defn pin-decl
  "Pure. The LifecycleDecl a `pin` call sets, or {:error msg}.

   POLICY is the raw string (default \"pinned\") and must be a member of
   POLICY-ENUM. IDLE-MS, when absent, is RESET to BASELINE-IDLE-MS: set-policy!
   MERGES its decl, so omitting :idle-ms used to keep whatever an earlier pin
   had set rather than what the addon declares."
  [{:keys [policy idle_ms]} policy-enum baseline-idle-ms]
  (let [p (keyword (or policy "pinned"))]
    (cond
      (not (m/validate policy-enum p))
      {:error (str "policy " (pr-str policy) " rejected: "
                   (str/join "; " (me/humanize (m/explain policy-enum p))))}

      (and (some? idle_ms) (not (pos-int? idle_ms)))
      {:error (str "idle_ms must be a positive integer; got " (pr-str idle_ms))}

      :else
      {:policy p :idle-ms (or idle_ms baseline-idle-ms)})))

(defmulti dormant-pin-note
  "What to tell the caller when POLICY is set on an addon in PHASE. A policy
   change never mounts anything; for a policy that implies mounted-ness, a
   dormant addon needs an explicit activate. Dispatches on the policy; a new
   policy that implies mounting is one defmethod."
  (fn [policy _phase _addon] policy))

(defmethod dormant-pin-note :default [_ _ _] nil)

(defn- mounted-policy-note
  [policy phase addon]
  (when (contains? #{:dormant :failed} phase)
    (str "policy " policy " recorded, but " addon " is " (name phase)
         ": a policy change does not mount it. Run {:command \"activate\" :addon \""
         addon "\"} to mount it now.")))

(defmethod dormant-pin-note :eager [p ph a] (mounted-policy-note p ph a))
(defmethod dormant-pin-note :pinned [p ph a] (mounted-policy-note p ph a))

(defn- baseline-idle-ms
  "The idle-ms ADDON's manifest and the host defaults resolve to, ignoring any
   runtime pin; hive-addon's default when that cannot be resolved."
  [mgr addon]
  (or (try
        (when-let [resolve-all (soft 'hive-addon.lifecycle.policy/resolve-all)]
          (get-in (resolve-all @(:specs mgr) (select-keys (:opts mgr) [:defaults :overrides]))
                  [addon :idle-ms]))
        (catch Throwable t
          (log/warn "declared idle-ms unresolvable; using hive-addon's default"
                    {:addon addon :error (ex-message t)})
          nil))
      (some-> (soft 'hive-addon.lifecycle.policy/default-idle-ms) deref)
      fallback-idle-ms))

(defn accepts-arity?
  "True when fn var V declares an arglist of exactly N params."
  [v n]
  (boolean (some #(= n (count %)) (:arglists (meta v)))))

(defn set-policy-call!
  "Call SET-POLICY! (a var) with the force flag when it takes one (4-arity);
   against an older hive-addon, call the 3-arity and say that force was not
   honoured. Returns {:lifecycle lc} plus :force-unsupported? when it mattered."
  [set-policy! mgr addon decl force?]
  (if (accepts-arity? set-policy! 4)
    {:lifecycle (set-policy! mgr addon decl {:force? force?})}
    (cond-> {:lifecycle (set-policy! mgr addon decl)}
      force? (assoc :force-unsupported? true))))

(defn handle-pin
  "Set an addon's lifecycle policy at runtime (default :pinned). The policy is
   validated against hive-addon's policy enum; idle_ms, when absent, resets to
   the addon's declared value; and a mounting policy set on a dormant addon
   says so, because it does not mount it. A refused change (e.g. :lazy on a
   surface-less addon without force) answers :ok? false with :refused."
  [{:keys [addon force] :as params}]
  (if-not addon
    (missing-param-error :addon "{:command \"pin\" :addon \"hive.carto\" :policy \"lazy\"}")
    (with-manager
      (fn [mgr]
        (let [decl (pin-decl params (policy-schema) (baseline-idle-ms mgr addon))]
          (if-let [msg (:error decl)]
            (mcp-error msg)
            (let [{lc :lifecycle :keys [force-unsupported?]}
                  (set-policy-call! (soft 'hive-addon.lifecycle/set-policy!) mgr addon decl (true? force))
                  refused (:refused lc)
                  phase   (some-> (soft 'hive-addon.lifecycle/phase) (apply [mgr addon]))
                  note    (dormant-pin-note (:policy decl) phase addon)]
              (mcp-json (cond-> {:addon/id addon :lifecycle lc :phase phase :ok? (nil? refused)}
                          refused            (assoc :refused refused)
                          note               (assoc :note note)
                          force-unsupported? (assoc :force-unsupported? true
                                                    :force-note "this hive-addon's set-policy! takes no force flag; force was not applied"))))))))))

(defn handle-sweep
  "Evict every idle lazy addon and close idle parts now."
  [_params]
  (with-manager #(mcp-json (update ((soft 'hive-addon.lifecycle/sweep!) %) :plan dissoc :kept))))

(defn- core-remounter
  "The addon widening a core reload hands hive-mcp.hot.core: remount every
   mounted addon whose constructor namespace the pass loaded, plus its
   dependents, through the same bridge `reload` uses. nil when no addon is
   mounted, so the core reload proceeds without widening."
  []
  (let [{:keys [specs host error]} (prepared)]
    (when-not error
      (fn [loaded]
        (let [loaded (set loaded)
              seeds  (into #{}
                           (comp (filter #(contains? loaded (str (:addon/init-ns %))))
                                 (map :addon/id))
                           specs)]
          (if (seq seeds)
            (summarize ((soft 'hive-addon.hot/reload-seeds!) host specs seeds
                        (assoc (reload-opts) :ns-reloaded? true :trigger :core-reload)))
            {:seeds [] :note "no mounted addon's constructor namespace was loaded"}))))))

(defn handle-core-plan
  "What a reload of hive-mcp's own source would do: the pending namespaces,
   the cascade, what the interlock pins and what state it keeps. Effect-free
   apart from extending hive-hot with core's root."
  [_params]
  (mcp-json (core-hot/plan)))

(defn handle-core-reload
  "Reload the changes under hive-mcp's own source root: protocol definers
   pinned, state holders kept, then the live records re-seated (:reseated),
   the tool table and the surface refreshed and every addon whose constructor
   namespace was loaded remounted."
  [_params]
  (let [report (core-hot/reload! {:ports (assoc (core-hot/default-ports)
                                                :host/remount! (core-remounter))})]
    (log/info "hot core-reload" (select-keys report [:ok? :loaded :failed :error :ms :reseated]))
    (mcp-json report)))

(def canonical-handlers
  "The `hot` verbs, stored as VARS so a reload of this namespace reaches the
   table (20260817195749-0d407e9c). The first of hive-mcp's 55 dispatch maps to
   be converted, and the worked example the rest follow: flat, every value a
   bare symbol naming a local defn, nothing here that inspects a handler rather
   than calling it.

   Reading it through a var is safe because `tools/cli.clj` classifies every
   tree node through `dispatch/current` (commit cdd876f7). It was NOT safe
   before that, which is the precondition the conversion card names.

   Extending it is ONE entry, never an edit elsewhere:
     - a verb that forwards to hive-addon.lifecycle is a `lifecycle-verb`
       descriptor (see the seam above handle-activate);
     - a verb that forwards to a (bridge host specs target opts) fn is a
       `host-verb` descriptor, as `eject` is;
     - the advertised `command` enum is derived from these keys (tool-def);
     - an addon adds a verb WITHOUT touching this map by contributing a
       command under \"hot\" (hive-addon.registry.commands), which
       build-merged-handler merges in per call."
  {:reload      #'handle-reload
   :reload-all  #'handle-reload-all
   :inject      #'handle-inject
   :eject       #'handle-eject
   :watch       #'handle-watch
   :unwatch     #'handle-unwatch
   :list        #'handle-list
   :status      #'handle-status
   :strategies  #'handle-strategies
   :core-plan   #'handle-core-plan
   :core-reload #'handle-core-reload
   :lifecycle   #'handle-lifecycle
   :activate    #'handle-activate
   :evict       #'handle-evict
   :pin         #'handle-pin
   :sweep       #'handle-sweep})


(def handlers canonical-handlers)

(def handle-hot
  "Routes the core `hot` commands plus whatever addons contribute under
   \"hot\". Named, so the tool-def can register it BY VAR: a handler folded
   into the tool map by value never sees a reload of its own namespace
   (20260817195749-0d407e9c). The tool that performs reloads is the one it
   would be most absurd to leave unreloadable."
  (composite/build-merged-handler "hot" #'canonical-handlers))

(defn command-enum
  "The advertised `command` values: every canonical verb, sorted, plus help.
   Derived, so registering a verb in canonical-handlers advertises it."
  []
  (conj (vec (sort (map name (keys canonical-handlers)))) "help"))

(def tool-def
  {:name "hot"
   :consolidated true
   :description
   (str "Hot-reload and inject IAddon instances via hive-hot. reload (rebuild one "
        "addon from its mount manifest and cascade to every addon holding its "
        "instance; the namespace reload is scoped to the addon's own source roots "
        "and reports what it declined under :hot/ns-skipped), reload-all (every "
        "mounted addon with reloadable source, in dependency order; the rest are "
        "reported under :skipped with a reason), inject (mount an addon that was NOT on "
        "the classpath at boot: a project dir, source dir or jar), eject (plug an addon "
        "OUT by id or by the path it was injected from; refused with :hot/blocking while "
        "mounted addons depend on it unless cascade, which remounts them without it; what "
        "cannot be removed, e.g. classpath URLs, is reported as retained), watch/unwatch "
        "(file-watcher: edit an addon's source and it remounts itself), list "
        "(per-addon strategy + source-kind + whether it is reloadable at all), "
        "status (includes the no-reload set split into core and addon pins), "
        "strategies. Only addons wired as :local/root deps have reloadable "
        "source; jar-backed addons report :restart-required. "
        "core-plan / core-reload: hive-mcp's OWN source, the same way. core-plan lists the "
        "pending namespaces, the cascade, what the interlock pins (protocol definers) and "
        "keeps (state holders); core-reload runs it, re-seats live records (:reseated), then "
        "refreshes the tool table and the surface and remounts any addon whose constructor "
        "namespace was loaded. "
        "Addon lifecycle (when :services :addons :lifecycle :enabled?): lifecycle (per-addon "
        "phase, policy, idle time, parts), activate (mount a dormant addon now), evict (release "
        "an addon to dormant stubs; force for pinned/eager; a refusal is :ok? false :refused? "
        "true with :reason), pin (set a validated policy at runtime; it never mounts a dormant "
        "addon; force allows :lazy on a surface-less addon), "
        "sweep (evict every idle lazy addon and close idle parts now). "
        "Use command='help' to list all.")
   :inputSchema
   {:type "object"
    :properties
    {"command" {:type "string"
                :enum (command-enum)
                :description "Hot-reload operation to perform"}
     "addon" {:type "string"
              :description "[reload/eject/lifecycle verbs] Addon id, e.g. \"hive.carto\". Dependents cascade automatically on reload."}
     "namespace" {:type "string"
                  :description "[reload] Constructor namespace to reload instead of an addon id — seeds every addon built from it."}
     "strategy" {:type "string"
                 :description "[reload] Override the reload strategy for this addon (e.g. \"remount\", \"in-place\", \"inert\"). Omit to let the chain select."}
     "path" {:type "string"
             :description "[inject/eject] Absolute path of the addon: a project dir (its deps.edn :paths go on the classpath), a source dir, or a jar. inject mounts its META-INF/hive-addons manifests (addons already mounted are left alone); eject plugs out what was injected from it."}
     "resolve_deps" {:type "boolean"
                     :description "[inject] Also hand the project's deps.edn :deps to clojure.repl.deps/add-libs before mounting (needs a tools.deps basis in the running image). Default false."}
     "cascade" {:type "boolean"
                :description "[eject] Also tear down the mounted addons that depend on the target and remount them without it. Default false: such an eject is refused."}
     "force" {:type "boolean"
              :description "[evict] Also evict a pinned or eager addon. [pin] Allow :lazy on an addon with no surface a stub could advertise. Default false."}
     "policy" {:type "string"
               :enum (mapv name (rest (policy-schema)))
               :description "[pin] Lifecycle policy to set at runtime; pin defaults to \"pinned\"."}
     "idle_ms" {:type "integer"
                :description "[pin] Idle time in ms before a lazy addon may be evicted. Omitted: reset to the addon's declared value."}}
    :required ["command"]}
   :handler #'handle-hot})

(def tools [tool-def])

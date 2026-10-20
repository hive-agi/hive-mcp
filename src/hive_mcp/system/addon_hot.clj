(ns hive-mcp.system.addon-hot
  "Boundary component :hive/addon-hot. At boot it initializes hive-hot ONCE
   with the source dirs of every mounted addon and registers each reloadable
   addon as a hive-hot component.

   Why it exists: before this key, nothing at boot did either. The only boot
   caller of hive-hot was :hive/hot-reload (server.init/init-hot-reload-watcher!),
   which is gated on the project's :hot-reload flag and covers core's own src.
   Addon dirs and components were wired lazily, by the first `hot reload` /
   `hot watch` call. Until then `hot status` reported
   {:initialized? false :components {}} with every addon mounted, and
   hive-mcp.extensions.lifecycle's on-activated! skipped re-registering a
   lazily activated addon because hive-hot was not initialized.

   This key does NOT start a file watcher; the watcher stays governed by
   :hot-reload. It only makes the registry true from boot on.

   Dirs are claimed PER ADDON: the baseline init carries no addon dir, and
   each addon then extends hive-hot under its own :addon/id as :owner, so
   hive-hot's remove-dirs! (an unmount) releases exactly that addon's claim.

   Strata:
     ports    — IAddonCatalog (what is mounted), IHotEngine (hive-hot + bridge)
     pure     — init-opts, boot-report
     boundary — boot!, the live adapters, the Integrant key"
  (:require [integrant.core :as ig]
            [hive-mcp.dns.result :as r]
            [hive-mcp.hot.claims :as claims]
            [taoensso.timbre :as log]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Ports
;; =============================================================================

(defprotocol IAddonCatalog
  "What is mounted right now."
  (mounted-specs [this] "MountSpecs of the addons currently in the host registry.")
  (mount-host [this] "The IMountHost a remount runs against, or nil."))

(defprotocol IHotEngine
  "hive-hot plus hive-addon's bridge onto it."
  (plan [this host specs] "hive-addon.hot/plan: {:hot/dirs :hot/no-reload :hot/registered ...}.")
  (ensure-init! [this opts] "hive-hot ensure-init!: init once, extend after.")
  (extend-dirs! [this req]
    "hive-hot extend-init! for ONE owner: REQ {:dirs :owner :no-reload}.
     Returns its ExtendInitReport {:dirs :added}.")
  (register! [this host specs] "hive-addon.hot/hot!: one component per reloadable addon.")
  (unregister! [this specs] "hive-addon.hot/unhot!."))

;; =============================================================================
;; Pure
;; =============================================================================

;; =============================================================================
;; Schemas
;; =============================================================================

(def ^:private gen-catalog
  (reify IAddonCatalog
    (mounted-specs [_] [])
    (mount-host [_] nil)))

(def ^:private gen-engine
  (reify IHotEngine
    (plan [_ _host _specs] {})
    (ensure-init! [_ _opts] {})
    (extend-dirs! [_ req] {:dirs (:dirs req) :added []})
    (register! [_ _host _specs] {:ok? true :hot/registered []})
    (unregister! [_ _specs] nil)))

(def AddonCatalog
  "An IAddonCatalog port."
  [:fn {:gen/elements [gen-catalog]} #(satisfies? IAddonCatalog %)])

(def HotEngine
  "An IHotEngine port."
  [:fn {:gen/elements [gen-engine]} #(satisfies? IHotEngine %)])

(def MountSpec
  "A MountSpec as this namespace reads it: keyed by :addon/id. Open map."
  [:map [:addon/id {:optional true} :any]])

(def HotPlan
  "hive-addon.hot/plan's answer, the keys init-opts and boot-report read."
  [:map
   [:hot/dirs {:optional true} [:maybe [:or [:set :string] [:sequential :string]]]]
   [:hot/no-reload {:optional true} [:maybe [:or [:set :symbol] [:sequential :symbol]]]]
   [:hot/registered {:optional true} [:maybe [:sequential MountSpec]]]
   [:hot/skipped {:optional true} [:maybe [:sequential MountSpec]]]])

(def HotRegistration
  "register!'s answer (hive-addon.hot/hot!): what was registered."
  [:map
   [:ok? {:optional true} :any]
   [:hot/registered {:optional true} [:maybe [:sequential MountSpec]]]
   [:errors {:optional true} [:maybe [:sequential :any]]]])

(def InitOpts
  "hive-hot ensure-init! options: baseline and interlock, never an addon dir."
  [:map [:dirs [:vector :string]] [:no-reload [:set :symbol]] [:since {:optional true} :int]])

(def BootReport
  "The data `hot status` shows under :boot."
  [:map
   [:mounted :int]
   [:claims [:maybe :map]]
   [:dirs [:vector :string]]
   [:registered [:vector :string]]
   [:skipped [:vector :string]]
   [:fresh? :boolean]
   [:ok? :boolean]
   [:errors [:vector :any]]])

(def BootResult
  "boot! Result: (ok report) - the full BootReport, or {:mounted 0 :skipped-boot
   ...} for an empty catalog - or (err category data)."
  [:or
   [:map [:ok [:map [:mounted :int]]]]
   [:map [:error :keyword]]])

(def BootRegistration
  "boot-registration Result: the report plus the specs it registered."
  [:or
   [:map [:ok [:map [:report [:map [:mounted :int]]] [:registered [:vector MountSpec]]]]]
   [:map [:error :keyword]]])

(def ComponentState
  "start!'s component state, which stop! consumes."
  [:map [:engine HotEngine] [:result BootResult] [:registered [:vector MountSpec]]])

(defn init-opts
  "Pure. hive-hot init options from a PLAN: the baseline and the protocol
   interlock, with NO addon dir. An addon dir handed to init! would become a
   hive-hot CORE dir that remove-dirs! never releases; each addon claims its
   dirs under its own id instead (hive-mcp.hot.claims). SINCE (epoch ms, the
   JVM start) makes an edit made after boot count as a change on the first
   reload."
  [plan since]
  (claims/base-opts (:hot/no-reload plan) since))

(m/=> init-opts [:=> [:cat [:maybe HotPlan] [:maybe :int]] InitOpts])

(defn boot-report
  "Pure. The data `hot status` shows under :boot. CLAIMS is
   hive-mcp.hot.claims/claims-report: which owner claimed which dirs."
  [specs plan init reg claimed]
  {:mounted    (count specs)
   :claims     (:claims claimed {})
   :dirs       (vec (sort (map str (:hot/dirs plan))))
   :registered (vec (sort (map (comp str :addon/id) (:hot/registered reg))))
   :skipped    (vec (sort (map (comp str :addon/id) (:hot/skipped plan))))
   :fresh?     (boolean (:fresh? init))
   :ok?        (boolean (:ok? reg true))
   :errors     (into (vec (:errors reg)) (:errors claimed))})

(m/=> boot-report [:=> [:cat [:maybe [:sequential MountSpec]] [:maybe HotPlan] [:maybe :map]
                            [:maybe HotRegistration] [:maybe [:map [:claims {:optional true} [:maybe :map]]
                                                              [:errors {:optional true} [:maybe [:sequential :any]]]]]]
                   BootReport])

;; =============================================================================
;; Boundary
;; =============================================================================

(defn registered-specs
  "Pure. The SPECS whose ids REG reports registered: exactly what stop! must
   deregister later, whatever the catalog says by then."
  [specs reg]
  (let [ids (set (map (comp str :addon/id) (:hot/registered reg)))]
    (filterv (comp ids str :addon/id) specs)))

(m/=> registered-specs [:=> [:cat [:maybe [:sequential MountSpec]] [:maybe HotRegistration]] [:vector MountSpec]])

(defn boot-registration
  "Initialize hive-hot with every mounted addon's source dirs and register the
   reloadable ones. Railway: returns (ok {:report BootReport :registered [spec]})
   or (err category data). An empty catalog is not an error: hive-hot is left
   untouched and nothing is registered."
  [catalog engine since]
  (r/let-ok [specs (r/try-effect* :addon-hot/catalog-failed (vec (mounted-specs catalog)))
             host  (r/try-effect* :addon-hot/catalog-failed (mount-host catalog))]
    (cond
      (empty? specs) (r/ok {:report {:mounted 0 :skipped-boot :no-mounted-addons}
                            :registered []})
      (nil? host)    (r/err :addon-hot/no-mount-host {:mounted (count specs)})
      :else
      (r/let-ok [p    (r/try-effect* :addon-hot/plan-failed (plan engine host specs))
                 init (r/try-effect* :addon-hot/init-failed
                                     (ensure-init! engine (init-opts p since)))
                 claimed (r/try-effect* :addon-hot/claim-failed
                                        (claims/claim! #(extend-dirs! engine %)
                                                       (claims/owner-claims p)))
                 reg  (r/try-effect* :addon-hot/register-failed (register! engine host specs))]
        (r/ok {:report     (boot-report specs p init reg claimed)
               :registered (registered-specs specs reg)})))))

(m/=> boot-registration [:=> [:cat AddonCatalog HotEngine [:maybe :int]] BootRegistration])

(defn boot!
  "boot-registration, projected to its BootReport: (ok BootReport) or the error."
  [catalog engine since]
  (let [res (boot-registration catalog engine since)]
    (if (r/ok? res) (r/ok (:report (:ok res))) res)))

(m/=> boot! [:=> [:cat AddonCatalog HotEngine [:maybe :int]] BootResult])

(defonce ^{:doc "The last boot! Result, for `hot status`."} last-boot (atom nil))

(defn last-report
  "The last boot Result, or nil before the key initialized."
  []
  @last-boot)

(m/=> last-report [:=> [:cat] [:maybe BootResult]])

;; -----------------------------------------------------------------------------
;; Live adapters — everything resolved softly, through vars, at call time
;; -----------------------------------------------------------------------------

(defn- soft [sym]
  (try (requiring-resolve sym) (catch Throwable _ nil)))

(defn- call [sym & args]
  (if-let [f (soft sym)]
    (apply f args)
    (throw (ex-info (str sym " is not on the classpath") {:var sym}))))

(defn live-catalog
  "IAddonCatalog over the `hot` tool's effective-specs (classpath manifests
   intersected with the live registry) and the addon-registry mount host."
  []
  (reify IAddonCatalog
    (mounted-specs [_] (call 'hive-mcp.tools.consolidated.hot/effective-specs))
    (mount-host [_] (call 'hive-mcp.extensions.mount-host/addon-registry-host))))

(m/=> live-catalog [:=> [:cat] AddonCatalog])

(defn- live-reload-opts
  "The same :mount-opts the `hot` tool remounts with: the host config resolver
   plus the client-runtime provisioner."
  []
  {:mount-opts (merge {:resolve-config @(requiring-resolve 'hive-mcp.addons.manifest/prepare-config)}
                      (call 'hive-mcp.extensions.runtime/mount-opts))})

(defn live-engine
  "IHotEngine over hive-hot.core and hive-addon.hot."
  []
  (reify IHotEngine
    (plan [_ host specs] (call 'hive-addon.hot/plan host specs))
    (ensure-init! [_ opts] (call 'hive-hot.core/ensure-init! opts))
    (extend-dirs! [_ req] (call 'hive-hot.core/extend-init! req))
    (register! [_ host specs] (call 'hive-addon.hot/hot! host specs (live-reload-opts)))
    (unregister! [_ specs] (call 'hive-addon.hot/unhot! specs))))

(m/=> live-engine [:=> [:cat] HotEngine])

(defn- jvm-start-ms []
  (r/rescue nil (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))))

(defn start!
  "Run boot-registration over CATALOG and ENGINE, record and log the Result.
   Never throws. Returns the component state, which keeps the specs boot
   registered so stop! deregisters exactly those."
  [catalog engine since]
  (let [res0 (r/rescue (r/err :addon-hot/threw {}) (boot-registration catalog engine since))
        res  (if (r/ok? res0) (r/ok (:report (:ok res0))) res0)]
    (reset! last-boot res)
    (if (r/ok? res)
      (log/info ":hive/addon-hot hive-hot initialized for mounted addons"
                (select-keys (:ok res) [:mounted :registered :fresh? :ok?]))
      (log/warn ":hive/addon-hot did not initialize hive-hot" res))
    {:engine     engine
     :result     res
     :registered (if (r/ok? res0) (vec (:registered (:ok res0))) [])}))

(m/=> start! [:=> [:cat AddonCatalog HotEngine [:maybe :int]] ComponentState])

(defn stop!
  "Deregister exactly the addon components start! registered, not whatever is
   mounted at halt time (addons activated or evicted since boot differ). The
   hive-hot baseline and dirs are left as they are: nothing is destroyed."
  [{:keys [engine registered]}]
  (when (and engine (seq registered))
    (r/rescue nil (unregister! engine registered))))

;; answers what the engine's unregister! answers, nil when there is nothing
;; to deregister or unregister! threw (rescued)
(m/=> stop! [:=> [:cat [:maybe [:map [:engine {:optional true} [:maybe HotEngine]]
                                [:registered {:optional true} [:maybe [:sequential MountSpec]]]]]]
             :any])

(defmethod ig/init-key :hive/addon-hot
  [_ config]
  (log/info ":hive/addon-hot init" (dissoc config :extensions))
  (start! (live-catalog) (live-engine) (jvm-start-ms)))

(defmethod ig/halt-key! :hive/addon-hot
  [_ state]
  (stop! state))

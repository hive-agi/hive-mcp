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

   Strata:
     ports    — IAddonCatalog (what is mounted), IHotEngine (hive-hot + bridge)
     pure     — init-opts, boot-report
     boundary — boot!, the live adapters, the Integrant key"
  (:require [integrant.core :as ig]
            [hive-mcp.dns.result :as r]
            [taoensso.timbre :as log]))

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
  (register! [this host specs] "hive-addon.hot/hot!: one component per reloadable addon.")
  (unregister! [this specs] "hive-addon.hot/unhot!."))

;; =============================================================================
;; Pure
;; =============================================================================

(defn init-opts
  "Pure. hive-hot init options from a PLAN. SINCE (epoch ms, the JVM start)
   makes an edit made after boot count as a change on the first reload."
  [plan since]
  (cond-> {:dirs      (vec (sort (map str (:hot/dirs plan))))
           :no-reload (set (:hot/no-reload plan))}
    since (assoc :since (long since))))

(defn boot-report
  "Pure. The data `hot status` shows under :boot."
  [specs plan init reg]
  {:mounted    (count specs)
   :dirs       (vec (sort (map str (:hot/dirs plan))))
   :registered (vec (sort (map (comp str :addon/id) (:hot/registered reg))))
   :skipped    (vec (sort (map (comp str :addon/id) (:hot/skipped plan))))
   :fresh?     (boolean (:fresh? init))
   :ok?        (boolean (:ok? reg true))
   :errors     (vec (:errors reg))})

;; =============================================================================
;; Boundary
;; =============================================================================

(defn boot!
  "Initialize hive-hot with every mounted addon's source dirs and register the
   reloadable ones. Railway: returns (ok BootReport) or (err category data).
   An empty catalog is not an error: hive-hot is left untouched."
  [catalog engine since]
  (r/let-ok [specs (r/try-effect* :addon-hot/catalog-failed (vec (mounted-specs catalog)))
             host  (r/try-effect* :addon-hot/catalog-failed (mount-host catalog))]
    (cond
      (empty? specs) (r/ok {:mounted 0 :skipped-boot :no-mounted-addons})
      (nil? host)    (r/err :addon-hot/no-mount-host {:mounted (count specs)})
      :else
      (r/let-ok [p    (r/try-effect* :addon-hot/plan-failed (plan engine host specs))
                 init (r/try-effect* :addon-hot/init-failed
                                     (ensure-init! engine (init-opts p since)))
                 reg  (r/try-effect* :addon-hot/register-failed (register! engine host specs))]
        (r/ok (boot-report specs p init reg))))))

(defonce ^{:doc "The last boot! Result, for `hot status`."} last-boot (atom nil))

(defn last-report
  "The last boot Result, or nil before the key initialized."
  []
  @last-boot)

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
    (register! [_ host specs] (call 'hive-addon.hot/hot! host specs (live-reload-opts)))
    (unregister! [_ specs] (call 'hive-addon.hot/unhot! specs))))

(defn- jvm-start-ms []
  (r/rescue nil (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))))

(defn start!
  "Run boot! over CATALOG and ENGINE, record and log the Result. Never throws.
   Returns the component state."
  [catalog engine since]
  (let [res (r/rescue (r/err :addon-hot/threw {}) (boot! catalog engine since))]
    (reset! last-boot res)
    (if (r/ok? res)
      (log/info ":hive/addon-hot hive-hot initialized for mounted addons"
                (select-keys (:ok res) [:mounted :registered :fresh? :ok?]))
      (log/warn ":hive/addon-hot did not initialize hive-hot" res))
    {:catalog catalog :engine engine :result res}))

(defn stop!
  "Deregister the addon components this key registered. The hive-hot baseline
   and dirs are left as they are: nothing is destroyed."
  [{:keys [catalog engine result]}]
  (when (and engine (r/ok? result) (seq (:registered (:ok result))))
    (r/rescue nil (unregister! engine (mounted-specs catalog)))))

(defmethod ig/init-key :hive/addon-hot
  [_ config]
  (log/info ":hive/addon-hot init" (dissoc config :extensions))
  (start! (live-catalog) (live-engine) (jvm-start-ms)))

(defmethod ig/halt-key! :hive/addon-hot
  [_ state]
  (stop! state))

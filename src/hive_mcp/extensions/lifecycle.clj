(ns hive-mcp.extensions.lifecycle
  "hive-addon.lifecycle hosted by hive-mcp: addons that mount on first use and
   are released after idle time.

   Config, under :addons in config.edn:

     {:lifecycle {:enabled?          true
                  :defaults          {:policy :eager :idle-ms 1800000}
                  :overrides         {\"hive.carto\" {:policy :lazy :idle-ms 900000}}
                  :sweep-interval-ms 60000}}

   A dormant addon is advertised by stubs: its contributed commands under a
   stub owner id, its tools as dynamic tools. The first call to a stub mounts
   the addon, and the call is handed to the handler the mount contributed.
   Every addon handler dispatched through a composite or the addon tool table
   runs under lifecycle/call-with-use, so it counts as a use and blocks
   eviction while it runs."
  (:require [clojure.string :as str]
            [hive-addon.hot :as hot]
            [hive-addon.lifecycle :as lc]
            [hive-addon.lifecycle.port :as lport]
            [hive-addon.lifecycle.store :as store]
            [hive-addon.lifecycle.surface :as surface]
            [hive-addon.mount.boundary :as boundary]
            [hive-addon.mount.compose :as compose]
            [hive-addon.protocol :as proto]
            [hive-mcp.addons.core :as addon-core]
            [hive-mcp.addons.manifest :as manifest]
            [hive-mcp.dns.result :refer [rescue]]
            [hive-mcp.extensions.mount-host :as mount-host]
            [hive-mcp.extensions.reactive :as reactive]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.core :refer [mcp-error]]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def wrap-handler-key
  "Extension key: (fn [addon-id handler] -> handler), applied by the composite
   and addon-tool dispatch to every addon handler."
  :addon/wrap-handler)

(defn stub-owner
  "The contribution owner id stubs for ADDON-ID are recorded under."
  [addon-id]
  (str addon-id "#dormant"))

(def ^:private stub-marker ::stub-of)

;; =============================================================================
;; Surface observation
;; =============================================================================

(defn- all-contributions []
  (into {} (map (fn [t] [t (ext/get-contributed-commands t)])) (ext/contributed-tool-names)))

(defn observe-surface
  "The Surface mounted ADDON-ID shows now, or nil when it is not registered."
  [addon-id]
  (when-let [addon (addon-core/get-addon addon-id)]
    (surface/observed addon-id (vec (rescue [] (proto/tools addon))) (all-contributions))))

;; =============================================================================
;; Stubs
;; =============================================================================

(declare wrap-handler)

(defn- activation-error [addon-id rep]
  (mcp-error (str "Addon " addon-id " is dormant and could not be activated: "
                  (str/join "; " (or (seq (:errors rep)) ["unknown failure"])))))

(defn- real-command-handler
  [tool-name cmd addon-id]
  (let [spec (get (ext/get-contributed-commands tool-name) cmd)]
    (when (= addon-id (:addon spec))
      (:handler spec))))

(defn- command-stub
  [addon-id tool-name cmd activate!]
  (fn [params]
    (let [rep (activate!)]
      (if-not (:ok? rep)
        (activation-error addon-id rep)
        (if-let [h (real-command-handler tool-name cmd addon-id)]
          ((wrap-handler addon-id h) params)
          (mcp-error (str "Addon " addon-id " activated but did not contribute `"
                          tool-name " " cmd "`. Its declared surface is stale.")))))))

(defn- real-tool-handler
  [addon-id tool-name]
  (some #(when (= tool-name (:name %)) (:handler %)) (addon-core/addon-tools-by-name addon-id)))

(defn- tool-stub
  [addon-id tool-decl activate!]
  (assoc tool-decl
         :description (str "[dormant, mounts on first use] " (:description tool-decl ""))
         :inputSchema (or (:inputSchema tool-decl) {:type "object" :properties {}})
         stub-marker addon-id
         :handler (fn [params]
                    (let [rep (activate!)]
                      (if-not (:ok? rep)
                        (activation-error addon-id rep)
                        (if-let [h (real-tool-handler addon-id (:name tool-decl))]
                          ((wrap-handler addon-id h) params)
                          (mcp-error (str "Addon " addon-id " activated but has no tool "
                                          (:name tool-decl)))))))))

(defn install-stubs!
  [addon-id s activate!]
  (doseq [[tool-name cmds] (:commands s)]
    (ext/contribute-commands! tool-name (stub-owner addon-id)
                              (into {} (map (fn [cmd]
                                              [cmd {:handler (command-stub addon-id tool-name cmd activate!)
                                                    :description (str "[dormant " addon-id "] mounts on first use")}]))
                                    cmds)))
  (doseq [t (:tools s)]
    (ext/register-tool! (tool-stub addon-id t activate!)))
  (when (seq (:tools s)) (reactive/refresh-server-tools!))
  nil)

(defn remove-stubs!
  "Withdraw ADDON-ID's stubs. A tool registered under a stub's name by the real
   addon is left alone."
  [addon-id]
  (let [owner (stub-owner addon-id)]
    (doseq [tool-name (ext/contributed-tool-names)
            :when (some #(= owner (:addon %)) (vals (ext/get-contributed-commands tool-name)))]
      (ext/retract-commands! tool-name owner))
    (let [stubs (filter #(= addon-id (get % stub-marker)) (ext/get-registered-tools))]
      (doseq [t stubs] (ext/deregister-tool! (:name t)))
      (when (seq stubs) (reactive/refresh-server-tools!))))
  nil)

;; =============================================================================
;; Host
;; =============================================================================

(defrecord McpLifecycleHost [mount-host resolve-config on-event]
  lport/ILifecycleHost
  (-mount! [_ specs peers]
    (boundary/mount! {:ordered specs} mount-host
                     {:resolve-config resolve-config :on-event on-event :peer-specs peers}))
  (-unmount! [_ addon-id]
    (let [sd (addon-core/shutdown-addon! addon-id)
          ur (addon-core/unregister-addon! addon-id)
          errs (into (vec (:errors sd)) (when-not (:success? ur) (:errors ur)))]
      {:ok? (empty? errs) :errors errs}))
  (-install-stubs! [_ addon-id s activate!] (install-stubs! addon-id s activate!))
  (-remove-stubs! [_ addon-id] (remove-stubs! addon-id))
  (-observe-surface [_ addon-id] (observe-surface addon-id)))

(defn host
  "An ILifecycleHost over hive-mcp's addon registry. RESOLVE-CONFIG defaults to
   manifest/prepare-config, the resolver boot mounts with."
  ([] (host {}))
  ([{:keys [resolve-config on-event]}]
   (->McpLifecycleHost (mount-host/addon-registry-host)
                       (or resolve-config manifest/prepare-config)
                       (or on-event (constantly nil)))))

;; =============================================================================
;; Use tracking at dispatch
;; =============================================================================

(defn wrap-handler
  "HANDLER counted as a use of ADDON-ID under the installed manager, or HANDLER
   itself when there is no manager."
  [addon-id handler]
  (if-let [mgr (lc/installed-manager)]
    (lc/tracked mgr addon-id handler)
    handler))

;; =============================================================================
;; Hot-reload companions
;; =============================================================================

(defn- hot-initialized? []
  (rescue false (:initialized? ((requiring-resolve 'hive-hot.core/status)))))

(defn- on-activated! [mgr ids]
  (let [ids (set ids)]
    (when (hot-initialized?)
      (rescue nil (hot/hot! (mount-host/addon-registry-host)
                            (filterv #(contains? ids (:addon/id %)) @(:specs mgr))
                            {:mount-opts {:resolve-config manifest/prepare-config}}))))
  (rescue nil (reactive/refresh-surface! nil)))

(defn- on-evicted! [mgr id]
  (rescue nil (hot/unhot! (filterv #(= id (:addon/id %)) @(:specs mgr))))
  (rescue nil (reactive/refresh-surface! nil)))

;; =============================================================================
;; Boot
;; =============================================================================

(defn config
  "The :addons :lifecycle config map, or {}."
  [svc-cfg]
  (let [c (:lifecycle svc-cfg)] (if (map? c) c {})))

(defn enabled? [svc-cfg] (true? (:enabled? (config svc-cfg))))

(defn surface-dir []
  (str (System/getProperty "user.home") "/.config/hive-mcp/data/addon-surfaces"))

(defn boot!
  "Discover, compose and boot the classpath addons under the lifecycle.
   Eager addons mount now; lazy ones get stubs. Installs the manager, the
   dispatch wrap and the sweeper. Returns {:manager m :boot BootReport
   :compose {...}} or {:error ...}."
  [svc-cfg {:keys [on-event]}]
  (let [cfg                        (config svc-cfg)
        {:keys [specs errors]}     (boundary/discover-specs)
        {:keys [layers] lerrs :errors} (compose/read-layers (mapv str (:layer-paths svc-cfg [])))
        planned                    (compose/compose-plan specs layers {})]
    (if-not (:ok planned)
      {:error planned}
      (let [{:keys [plan config-by-id dropped]} (:ok planned)
            h   (host {:resolve-config (compose/compose-config-resolver manifest/prepare-config config-by-id)
                       :on-event on-event})
            mgr (lc/manager {:host h
                             :specs (:ordered plan)
                             :defaults (:defaults cfg)
                             :overrides (:overrides cfg)
                             :surface-store (store/edn-dir-store (or (:surface-dir cfg) (surface-dir)))})
            mgr (assoc-in mgr [:opts :on-activated] #(on-activated! mgr %))
            mgr (assoc-in mgr [:opts :on-evicted] #(on-evicted! mgr %))]
        (lc/install! mgr)
        (ext/register! wrap-handler-key wrap-handler)
        (let [boot (lc/boot! mgr)]
          (lc/start-sweeper! mgr {:interval-ms (:sweep-interval-ms cfg)})
          (log/info "Addon lifecycle booted"
                    {:eager (:eager boot) :dormant (:dormant boot) :downgraded (:downgraded boot)})
          {:manager mgr
           :boot boot
           :compose {:report (:mount boot) :dropped dropped
                     :discovery-errors errors :layer-errors lerrs}})))))

(defn shutdown!
  "Stop the sweeper and uninstall the manager and dispatch wrap."
  []
  (ext/deregister! wrap-handler-key)
  (lc/uninstall!))

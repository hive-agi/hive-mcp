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
            [taoensso.timbre :as log]
            [hive-addon.registry.commands :as acmds]
            [hive-mcp.extensions.runtime :as runtime]
            [hive-mcp.hot.reseat :as reseat]))

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
  (into {} (map (fn [t] [t (acmds/get-commands t)])) (acmds/contributed-tool-names)))

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
  (let [spec (get (acmds/get-commands tool-name) cmd)]
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
         :inputSchema (:inputSchema tool-decl)
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
    (doseq [tool-name (acmds/contributed-tool-names)
            :when (some #(= owner (:addon %)) (vals (acmds/get-commands tool-name)))]
      (ext/retract-commands! tool-name owner))
    (let [stubs (filter #(= addon-id (get % stub-marker)) (ext/get-registered-tools))]
      (doseq [t stubs] (ext/deregister-tool! (:name t)))
      (when (seq stubs) (reactive/refresh-server-tools!))))
  nil)

;; =============================================================================
;; Host
;; =============================================================================

(defn unmount-report
  "PROMOTE: the ILifecycleHost -unmount! answer from a TeardownReport TD and an
   unregister report UN ({:unregistered :unsupported :errors}). The teardown's
   :teardown/data-preserved? claim is carried as the teardown made it, and
   left out when the teardown made none. Pure."
  [td un]
  (let [errs (into (vec (:errors td)) (:errors un))]
    (cond-> {:ok?          (empty? errs)
             :errors       errs
             :torn-down    (vec (:torn-down td))
             :unregistered (vec (:unregistered un))}
      (contains? td :teardown/data-preserved?)
      (assoc :teardown/data-preserved? (:teardown/data-preserved? td))
      (seq (:unsupported un))
      (assoc :unsupported (vec (:unsupported un))))))

(defrecord McpLifecycleHost [mount-host resolve-config on-event]
  lport/ILifecycleHost
  (-mount! [_ specs peers]
    (boundary/mount! {:ordered specs} mount-host
                     (merge {:resolve-config resolve-config :on-event on-event :peer-specs peers}
                            (runtime/mount-opts))))
  (-unmount! [_ addon-id]
    ;; "Shut down ... and forget the instance": teardown through the mount
    ;; host, then plug out (IMountUnregister when hive-addon has it).
    (unmount-report (boundary/teardown! mount-host [addon-id])
                    (mount-host/unregister! mount-host [addon-id])))
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
                            {:mount-opts (merge {:resolve-config manifest/prepare-config}
                                                (runtime/mount-opts))}))))
  (rescue nil (reactive/refresh-surface! nil)))

(defn- on-evicted! [mgr id]
  (rescue nil (hot/unhot! (filterv #(= id (:addon/id %)) @(:specs mgr))))
  (rescue nil (reactive/refresh-surface! nil)))

;; =============================================================================
;; Re-seat after a core reload
;;
;; A reload of this namespace redefines McpLifecycleHost. The installed
;; manager (hive-addon, never reloaded) still holds the instance built from the
;; OLD class, and every stub it armed closes over the old manager value. The
;; re-seater rebuilds the host from the CURRENT constructor, carrying its
;; fields, and swaps it into the manager, keeping every state atom.
;; =============================================================================

(defn stale-host?
  "Was HOST built from an earlier definition of McpLifecycleHost? Same class
   name, different class object: exactly what a namespace reload leaves. The
   current class is read off the constructor var, never a class literal
   compiled into this fn."
  [host]
  (let [c   (class host)
        now (class (map->McpLifecycleHost {}))]
    (and (not (identical? c now))
         (= (.getName ^Class c) (.getName ^Class now)))))

(defn rebuild-host
  "HOST's fields in a host built by the current constructor. Its mount host is
   brought current too (hive-mcp.extensions.mount-host/current), so a reload
   of that namespace does not leave a stale registry host underneath."
  [host]
  (map->McpLifecycleHost (update (into {} host) :mount-host mount-host/current)))

(defn reseat-manager
  "MGR on HOST. Every atom (specs, states, surfaces, sweeper), the lock and
   the opts are shared with MGR, so no state is copied and none is lost; the
   :on-activated/:on-evicted hooks only read the shared :specs atom."
  [mgr host]
  (assoc mgr :host host))

(defn- rearm-stubs!
  "Re-advertise every dormant addon's stubs through MGR's host, activating
   through the manager INSTALLED at call time rather than a captured one."
  [mgr]
  (let [dormant (filterv #(lc/dormant? mgr %) (map :addon/id @(:specs mgr)))]
    (doseq [id dormant
            :let [[s] (lc/surface-of mgr id)]
            :when s]
      (lport/-install-stubs! (:host mgr) id s
                             #(lc/activate! (or (lc/installed-manager) mgr) id)))
    dormant))

(defn- narrow-reseat!
  "The re-seat through hive-addon's public seam, for a hive-addon without
   lifecycle/reseat-host!: install a manager on a rebuilt host, move the
   sweeper and re-arm the dormant stubs. NOT under the manager lock (that API
   is private there), so an activation racing it may briefly use the old host."
  [mgr]
  (let [mgr'    (reseat-manager mgr (rebuild-host (:host mgr)))
        sweeper @(:sweeper mgr)]
    (lc/install! mgr')
    (when sweeper
      (lc/stop-sweeper! mgr)
      (lc/start-sweeper! mgr' {:interval-ms (:interval-ms sweeper)}))
    {:reseated? true
     :via       :narrow-seam
     :host      (.getName (class (:host mgr')))
     :sweeper?  (boolean sweeper)
     :rearmed   (rearm-stubs! mgr')}))

(def ^:dynamic *addon-reseater*
  "0-arg fn answering hive-addon.lifecycle/reseat-host! [mgr host-fn] (its
   var, so the call reaches whatever is interned there NOW), or nil when the
   hive-addon on the classpath has none. Read per call; a test binds it."
  (fn [] (rescue nil (requiring-resolve 'hive-addon.lifecycle/reseat-host!))))

(defn delegated-report
  "PROMOTE: what `reseat-installed!` answers for a delegated re-seat. RET is
   hive-addon's ReseatReport (kept as is, so a failed reseat stays
   :reseated? false with its :errors); anything else is read as success. HOST
   names the host now installed. Pure."
  [ret host]
  (merge (if (and (map? ret) (not (record? ret))) ret {:reseated? true})
         {:via :hive-addon :host host}))

(defn reseat-installed!
  "Re-seat MGR on a host rebuilt by the current constructor. Delegates to
   hive-addon.lifecycle/reseat-host! when hive-addon has it (it swaps the host
   under the manager lock, moves the sweeper and re-arms the stubs itself);
   otherwise re-seats through the narrow public seam. Answers what it did:
   {:reseated? :via :host ...} plus the delegate's ReseatReport keys or the
   narrow seam's :sweeper? / :rearmed."
  [mgr]
  (if-let [reseat! (*addon-reseater*)]
    (let [ret (reseat! mgr rebuild-host)]
      (delegated-report ret (.getName (class (:host (or (lc/installed-manager) mgr))))))
    (narrow-reseat! mgr)))

(defn reseat-host!
  "Re-seater for hive-mcp.hot.reseat: when the installed manager's host is
   stale, re-seat it (`reseat-installed!`). Answers what it did."
  [_loaded]
  (let [mgr (lc/installed-manager)]
    (cond
      (nil? mgr)                      {:reseated? false :reason :no-manager}
      (not (stale-host? (:host mgr))) {:reseated? false :reason :current}
      :else                           (reseat-installed! mgr))))

(reseat/register-reseater! 'hive-mcp.extensions.lifecycle
                           (reseat/via-var `reseat-host!))

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
   :compose {...}} or {:error ...}.

   The dispatch wrap and the manager hooks are reached THROUGH their vars
   (hive-mcp.hot.reseat/via-var): a reload of this namespace re-creates them,
   and a value captured here would keep running the old code."
  [svc-cfg {:keys [on-event]}]
  (let [cfg                        (config svc-cfg)
        {:keys [specs errors]}     (boundary/discover-specs)
        {:keys [layers] lerrs :errors} (compose/read-layers (mapv str (:layer-paths svc-cfg [])))
        planned                    (compose/compose-plan specs layers {})]
    (if-not (:ok planned)
      {:error planned}
      (let [{:keys [plan config-by-id dropped]} (:ok planned)
            activated (reseat/via-var `on-activated!)
            evicted   (reseat/via-var `on-evicted!)
            h   (host {:resolve-config (compose/compose-config-resolver manifest/prepare-config config-by-id)
                       :on-event on-event})
            mgr (lc/manager {:host h
                             :specs (:ordered plan)
                             :defaults (:defaults cfg)
                             :overrides (:overrides cfg)
                             :surface-store (store/edn-dir-store (or (:surface-dir cfg) (surface-dir)))})
            mgr (assoc-in mgr [:opts :on-activated] #(activated mgr %))
            mgr (assoc-in mgr [:opts :on-evicted] #(evicted mgr %))]
        (lc/install! mgr)
        (ext/register! wrap-handler-key (reseat/via-var `wrap-handler))
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

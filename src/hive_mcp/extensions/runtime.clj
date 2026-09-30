(ns hive-mcp.extensions.runtime
  "hive-addon's client-runtime provisioner hosted by hive-mcp: every addon
   mount installs (or refreshes) the addon's :addon/runtime into the client's
   own directory, e.g. a Vim plugin under ~/.vim/pack/hive/start/<id>.

   One process-wide provisioner (hive-addon.runtime.boundary/provisioner) is
   built on first use and reused by every mount path, so what it installed is
   remembered per addon id and `deprovision!` removes exactly that.

   Config, under :addons in the :services config (env overrides):

     {:runtime {:enabled? true            ; HIVE_MCP_ADDON_RUNTIME=0|false
                :home     \"/home/me\"     ; default user.home
                :live?    true}}          ; activate in running clients

   Live activation needs an eval-fn (fn [client commands]). hive-mcp owns no
   editor transport, so the eval-fn is looked up at call time under the
   extension key `client-eval-key`; an addon that owns connected clients
   registers one. With none registered, live activation is skipped and a client
   started afterwards autoloads the installed runtime.

   Rationale lives in hive memory (KG-linked), not here."
  (:require [hive-mcp.addons.core :as addon-core]
            [hive-mcp.dns.result :refer [rescue]]
            [hive-mcp.extensions.registry :as ext]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def client-eval-key
  "Extension key: (fn [client commands]) running COMMANDS in every connected
   client of kind CLIENT (:vim, :nvim, ...). Registered by the addon that owns
   the client transport."
  :addon/runtime-client-eval)

;; =============================================================================
;; Config
;; =============================================================================

(defn- service-config []
  (rescue {} ((requiring-resolve 'hive-mcp.config.core/get-service-config) :addons)))

(defn config
  "The :runtime map of SVC-CFG (the :addons service config), or {}."
  [svc-cfg]
  (let [c (:runtime svc-cfg)] (if (map? c) c {})))

(defn enabled?
  "Provisioning is on unless config or HIVE_MCP_ADDON_RUNTIME turns it off."
  ([svc-cfg] (enabled? svc-cfg (System/getenv "HIVE_MCP_ADDON_RUNTIME")))
  ([svc-cfg env-flag]
   (if (some? env-flag)
     (contains? #{"1" "true"} env-flag)
     (not (false? (:enabled? (config svc-cfg)))))))

(defn registered-eval
  "The eval-fn hive-mcp hands the provisioner: delegates to whatever is
   registered under client-eval-key when it runs. With nothing registered no
   client is connected through hive-mcp, so there is nothing to activate and
   it returns nil."
  [client commands]
  (when-let [f (ext/get-extension client-eval-key)]
    (f client commands)))

(defn provisioner-opts
  "Options for hive-addon.runtime.boundary/provisioner from the :runtime
   config."
  [cfg]
  (cond-> {:home    (or (:home cfg) (System/getProperty "user.home"))
           :eval-fn registered-eval
           :live?   (not (false? (:live? cfg)))}
    (:declarations cfg) (assoc :declarations (:declarations cfg))
    (:profiles cfg)     (assoc :profiles (:profiles cfg))))

;; =============================================================================
;; The process-wide provisioner
;; =============================================================================

(defonce ^:private state (atom nil))

(defn- build
  [opts]
  (when-let [ctor (rescue nil (requiring-resolve 'hive-addon.runtime.boundary/provisioner))]
    (ctor opts)))

(defn provisioner
  "The shared provisioner map {:provision :deprovision :installed}, built on
   first use; nil when disabled or hive-addon.runtime is not on the classpath.
   OPTS, when given, replaces the config-derived provisioner options (tests)."
  ([] (provisioner nil))
  ([opts]
   (let [svc-cfg (service-config)]
     (when (enabled? svc-cfg)
       (or @state
           (let [p (build (or opts (provisioner-opts (config svc-cfg))))]
             (when p (compare-and-set! state nil p))
             @state))))))

(defn reset-provisioner!
  "Drop the shared provisioner (tests, or after a config change). What it
   installed stays on disk."
  []
  (reset! state nil))

(defn provision-fn
  "The :provision fn for hive-addon.mount/mount!, or nil when provisioning is
   off. Never throws: a failure is logged and reported as a failed runtime."
  []
  (when-let [{:keys [provision]} (provisioner)]
    (fn [spec instance]
      (let [report (try (provision spec instance)
                        (catch Throwable t
                          {:addon/id (:addon/id spec) :ok? false :runtimes []
                           :error (or (ex-message t) (str t))}))]
        (when report
          (if (:ok? report)
            (log/info "Addon runtime provisioned"
                      {:addon/id (:addon/id spec)
                       :runtimes (mapv #(select-keys % [:runtime/id :runtime/client :dir :live-ok?])
                                       (:runtimes report))})
            (log/warn "Addon runtime provisioning failed"
                      {:addon/id (:addon/id spec)
                       :error (:error report)
                       :runtimes (mapv #(select-keys % [:runtime/id :runtime/client :error])
                                       (:runtimes report))})))
        report))))

(defn mount-opts
  "{:provision f} to merge into mount! options, or {} when provisioning is off."
  []
  (if-let [f (provision-fn)] {:provision f} {}))

(defn deprovision!
  "Remove the runtimes the shared provisioner installed for ADDON-ID. Returns
   its ProvisionReport, or nil when nothing was installed."
  [addon-id]
  (when-let [{:keys [deprovision]} @state]
    (deprovision addon-id)))

(defn installed
  "Addon id -> RuntimeDecls the shared provisioner installed, or {}."
  []
  (if-let [{:keys [installed]} @state] (installed) {}))

;; =============================================================================
;; Mount paths that cannot thread :provision
;; =============================================================================

(defn provision-mounted
  "REPORT (a MountReport) with every successfully mounted addon that carries
   no :runtime yet provisioned through PROVISION and its report attached as
   :runtime. SPECS are the specs the report was mounted from; INSTANCE-OF maps
   an addon id to its live instance. For mount paths whose driver drops
   :provision (hive-addon.mount.compose/compose!)."
  [report specs provision instance-of]
  (if-not provision
    report
    (let [by-id (into {} (map (juxt :addon/id identity)) specs)]
      (update report :mounted
              (fn [results]
                (mapv (fn [{:keys [success? runtime] id :addon/id :as result}]
                        (let [spec (get by-id id)
                              instance (when (and success? (nil? runtime) spec) (instance-of id))
                              rt (when instance (provision spec instance))]
                          (cond-> result rt (assoc :runtime rt))))
                      results))))))

(defn provision-mounted!
  "provision-mounted over hive-mcp's addon registry with the shared
   provisioner."
  [report specs]
  (provision-mounted report specs (provision-fn) addon-core/get-addon))

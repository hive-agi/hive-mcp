(ns hive-mcp.addons.core
  "Addon registry — domain operations on addon instances.

   Addons implement the IAddon protocol from addons.protocol.
   This module provides the registry lifecycle:
   - register-addon! / unregister-addon!
   - init-addon! / shutdown-addon! / init-all! / shutdown-all!
   - list-addons / active-addon-tools / addons-with-capability
   - registry-status / reset-registry!"
  (:require [clojure.set]
            [hive-addon.protocol :as proto]
            [hive-mcp.addons.tool-claims :as claims]
            [hive-mcp.dns.result :as r]
            [hive-mcp.extensions.registry :as ext]
            [taoensso.timbre :as log]
            [hive-mcp.addons.runtime-ports :as runtime-ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private addon-registry (atom {}))

(defonce ^:private registration-seq (atom 0))

(defn- registry-core-tools
  "The host's core tool defs from hive-mcp.tools.registry, resolved at call
   time. [] (with an error logged) when the registry cannot be loaded."
  []
  (if-let [f (r/rescue nil (requiring-resolve 'hive-mcp.tools.registry/core-tools))]
    (vec (f))
    (do (log/error "Core tool registry unavailable; addon tools are resolved against no core names")
        [])))

(def ^:dynamic *core-tools*
  "Port: 0-arity fn answering the host's core tool defs, read on every tool
   resolution. Addon tools are resolved against the names it returns."
  registry-core-tools)

(defn register-addon!
  "Register an addon in the global registry."
  [addon]
  {:pre [(satisfies? proto/IAddon addon)]}
  (let [id (proto/addon-id addon)]
    (if (contains? @addon-registry id)
      (do
        (log/warn "Addon already registered" {:addon id})
        {:success? false
         :addon-name id
         :errors [(str "Addon " id " is already registered")]})
      (do
        (swap! addon-registry assoc id
               {:addon addon
                :state :registered
                :registered-at (java.time.Instant/now)
                :reg-seq (swap! registration-seq inc)
                :init-time nil
                :init-result nil})
        (log/info "Addon registered" {:addon id
                                      :type (proto/addon-type addon)
                                      :capabilities (proto/capabilities addon)})
        {:success? true
         :addon-name id}))))

(defn get-addon
  "Get addon by id from the registry."
  [id]
  (get-in @addon-registry [id :addon]))

(defn get-addon-entry
  "Get full addon registry entry with state metadata."
  [id]
  (get @addon-registry id))

(defn addon-registered?
  "Check if an addon is registered."
  [id]
  (contains? @addon-registry id))

(defn list-addons
  "List all registered addons with their state."
  []
  (->> @addon-registry
       (mapv (fn [[id {:keys [addon state registered-at init-time]}]]
               {:name id
                :type (proto/addon-type addon)
                :state state
                :registered-at registered-at
                :init-time init-time
                :capabilities (proto/capabilities addon)}))))

(defn unregister-addon!
  "Unregister an addon, calling shutdown! first if active."
  [id]
  (if-let [{:keys [addon state]} (get-addon-entry id)]
    (do
      (when (= state :active)
        (let [result (r/try-effect* :addon/shutdown-error (proto/shutdown! addon))]
          (cond
            (r/err? result)
            (log/error "Addon shutdown failed during unregister"
                       {:addon id :error (:message result)})

            (and (r/ok? result) (not (:success? (:ok result))))
            (log/warn "Addon shutdown had errors during unregister"
                      {:addon id :errors (:errors (:ok result))}))))
      (swap! addon-registry dissoc id)
      (log/info "Addon unregistered" {:addon id})
      {:success? true :addon-name id})
    (do
      (log/warn "Addon not found for unregister" {:addon id})
      {:success? false
       :addon-name id
       :errors [(str "Addon " id " is not registered")]})))

(defn addon-declared-config
  "Declarative config for addon `id`, read from the global config's `:addons`
   map. Returns {} when absent, non-map, or unreadable.

   requiring-resolve rather than a static :require: hive-mcp.config.core pulls
   in the whole config stack, and this namespace sits on the addon boot path."
  [id]
  (or (try
        (when-let [get-in-config (requiring-resolve 'hive-mcp.config.core/get-in-config)]
          (let [m (get-in-config [:addons id])]
            (when (map? m) m)))
        (catch Throwable _ nil))
      {}))

(defn- init-config
  "Addon init config, composed at start-time from three layers, weakest first:

     1. the addon's declared config under `:addons <id>` in the global config
     2. `opts` handed in by the caller
     3. `:runtime/ports`, filled in when neither layer carries one

   Later layers win. An addon mounted programmatically therefore receives the
   same declared configuration as one mounted from a manifest, so a setting is
   not silently lost by choosing one mount path over the other."
  [id opts]
  (let [merged (merge (addon-declared-config id) (or opts {}))]
    (if (contains? merged :runtime/ports)
      merged
      (assoc merged :runtime/ports (runtime-ports/runtime-ports)))))

(defn init-addon!
  "Initialize a registered addon.

   On success, also registers any extensions returned by the addon's
   `(proto/hooks addon)` into the extension registry, and tracks the
   set of registered keys per-addon so shutdown can remove only the
   hooks owned by this addon."
  [id & [opts]]
  (if-let [{:keys [addon state]} (get-addon-entry id)]
    (if (= state :active)
      (do
        (log/info "Addon already active, skipping init" {:addon id})
        {:success? true :addon-name id :already-active? true})
      (let [init-result
            (r/try-effect* :addon/init-exception
                           (let [start-time (System/nanoTime)
                                 result (proto/initialize! addon (init-config id opts))
                                 elapsed-ms (/ (- (System/nanoTime) start-time) 1e6)]
                             (if (:success? result)
                               (do
                                 (swap! addon-registry assoc-in [id :state] :active)
                                 (swap! addon-registry assoc-in [id :init-time]
                                        (java.time.Instant/now))
                                 (swap! addon-registry assoc-in [id :init-result] result)
                    ;; Register schema extensions from protocol method
                                 (let [schema-exts (proto/schema-extensions addon)]
                                   (when (seq schema-exts)
                                     (doseq [[tool-name props] schema-exts]
                                       (ext/register-schema! tool-name props))
                                     (log/debug "Addon registered schema extensions"
                                                {:addon id :tools (keys schema-exts)})))
                    ;; Register extensions from init result metadata (opaque fn registry)
                                 (when-let [exts (:extensions (:metadata result))]
                                   (if (map? exts)
                                     (do (ext/register-many! exts)
                                         (log/debug "Addon registered extensions"
                                                    {:addon id :keys (keys exts)}))
                                     (log/warn "Addon init metadata :extensions is not a {keyword fn} map; nothing registered"
                                               {:addon id :type (type exts)})))
                    ;; Register tools
                                 (doseq [t (proto/tools addon)]
                                   (ext/register-tool! t))
                    ;; Register hooks declared via IAddon `hooks` protocol method.
                    ;; Legacy addons that don't implement `hooks` rescue to {}.
                    ;; Per-addon hook-keys are tracked so shutdown only removes
                    ;; the hooks this addon registered (no clobber across addons).
                                 (let [hooks-map (r/rescue {} (proto/hooks addon))]
                                   (when (seq hooks-map)
                                     (doseq [[k v] hooks-map]
                                       (case (namespace k)
                                         "multi" ((requiring-resolve 'hive-mcp.multi.registry/register-by-key!)
                                                  id k v)
                                         "saa" ((requiring-resolve 'hive-mcp.saa.registry/register-by-key!)
                                                id k v)
                                         "plan" ((requiring-resolve 'hive-mcp.plan.field-registry/register-by-key!)
                                                 id k v)
                                         "wf" ((requiring-resolve 'hive-mcp.workflows.strategy-registry/register-by-key!)
                                               id k v)
                                         "op-schema" ((requiring-resolve 'hive-mcp.spi.op-schema-registry/register-by-key!)
                                                      id k v)
                                         (ext/register! k v)))
                                     (swap! addon-registry assoc-in
                                            [id :hook-keys] (set (keys hooks-map)))
                                     (log/debug "Addon registered hooks"
                                                {:addon id :keys (keys hooks-map)})))

                                 (log/info "Addon initialized" {:addon id
                                                                :elapsed-ms elapsed-ms})
                                 (assoc result :addon-name id :elapsed-ms elapsed-ms))
                               (do
                                 (swap! addon-registry assoc-in [id :state] :error)
                                 (swap! addon-registry assoc-in [id :init-result] result)
                                 (log/warn "Addon init failed" {:addon id
                                                                :errors (:errors result)})
                                 (assoc result :addon-name id)))))]
        (if (r/err? init-result)
          (do (swap! addon-registry assoc-in [id :state] :error)
              (log/error "Addon init threw exception"
                         {:addon id :error (:message init-result)})
              {:success? false
               :addon-name id
               :errors [(:message init-result)]})
          (:ok init-result))))
    {:success? false
     :addon-name id
     :errors [(str "Addon " id " is not registered")]}))

(defn shutdown-addon!
  "Shutdown an active addon.

   Deregisters extensions, tools, composite contributions, and hooks
   that were registered for this addon during init. Hook ownership
   is tracked per-addon (entry's `:hook-keys`) so shutdown only
   removes hooks this addon registered."
  [id]
  (if-let [{:keys [addon state init-result hook-keys]} (get-addon-entry id)]
    (if (not= state :active)
      (do
        (log/info "Addon not active, skipping shutdown" {:addon id :state state})
        {:success? true :addon-name id :already-inactive? true})
      (let [shutdown-result
            (r/try-effect* :addon/shutdown-exception
              ;; Deregister extensions stored during init
                           (when-let [exts (:extensions (:metadata init-result))]
                             (when (map? exts)
                               (doseq [k (keys exts)] (ext/deregister! k))
                               (log/debug "Addon deregistered extensions" {:addon id :keys (keys exts)})))
              ;; Deregister hooks (only those owned by this addon)
                           (when (seq hook-keys)
                             (doseq [k hook-keys]
                               (case (namespace k)
                                 "multi" ((requiring-resolve 'hive-mcp.multi.registry/deregister-by-key!)
                                          id k)
                                 "saa" ((requiring-resolve 'hive-mcp.saa.registry/deregister-by-key!)
                                        id k)
                                 "plan" ((requiring-resolve 'hive-mcp.plan.field-registry/deregister-by-owner!)
                                         id)
                                 "wf" ((requiring-resolve 'hive-mcp.workflows.strategy-registry/deregister-by-owner!)
                                       id)
                                 "op-schema" ((requiring-resolve 'hive-mcp.spi.op-schema-registry/deregister-by-owner!)
                                              id)
                                 (ext/deregister! k)))
                             ;; Belt-and-suspenders: clear any leftover :multi/*, :saa/*, :plan/*, :wf/*, :op-schema/* entries by owner
                             ((requiring-resolve 'hive-mcp.multi.registry/deregister-by-owner!) id)
                             ((requiring-resolve 'hive-mcp.saa.registry/deregister-by-owner!) id)
                             ((requiring-resolve 'hive-mcp.plan.field-registry/deregister-by-owner!) id)
                             ((requiring-resolve 'hive-mcp.workflows.strategy-registry/deregister-by-owner!) id)
                             ((requiring-resolve 'hive-mcp.spi.op-schema-registry/deregister-by-owner!) id)
                             (log/debug "Addon deregistered hooks" {:addon id :keys hook-keys}))
              ;; Deregister tools
                           (doseq [t (proto/tools addon)]
                             (ext/deregister-tool! (:name t)))
              ;; Retract composite tool contributions
                           (ext/retract-all-by-addon! id)
                           (let [result (proto/shutdown! addon)]
                             (swap! addon-registry assoc-in [id :state] :registered)
                             (swap! addon-registry assoc-in [id :init-time] nil)
                             (swap! addon-registry update id dissoc :hook-keys)
                             (log/info "Addon shut down" {:addon id})
                             (assoc result :addon-name id)))]
        (if (r/err? shutdown-result)
          (do (swap! addon-registry assoc-in [id :state] :error)
              (log/error "Addon shutdown threw exception"
                         {:addon id :error (:message shutdown-result)})
              {:success? false
               :addon-name id
               :errors [(:message shutdown-result)]})
          (:ok shutdown-result))))
    {:success? false
     :addon-name id
     :errors [(str "Addon " id " is not registered")]}))

(defn init-all!
  "Initialize all registered addons that are not yet active."
  [& [opts]]
  (->> @addon-registry
       (filter (fn [[_id {:keys [state]}]] (not= state :active)))
       (map (fn [[id _]]
              [id (init-addon! id opts)]))
       (into {})))

(defn shutdown-all!
  "Shutdown all active addons."
  []
  (->> @addon-registry
       (filter (fn [[_id {:keys [state]}]] (= state :active)))
       (map (fn [[id _]]
              [id (shutdown-addon! id)]))
       (into {})))

(defn- safe-excluded-tools
  "Get excluded-tools set from addon, returning #{} for legacy addons
   that don't implement the method."
  [addon]
  (r/rescue #{} (proto/excluded-tools addon)))

(defn active-contributions
  "Active tool-capable addons as `tool-claims` Contributions, in REGISTRATION
   order. Order is the tie-breaker for every first-wins rule, so it is taken
   from the monotonic `:reg-seq` stamped at register-addon! time, never from
   the registry map's own (hash) order."
  []
  (->> @addon-registry
       (filter (fn [[_id {:keys [state addon]}]]
                 (and (= state :active)
                      (contains? (proto/capabilities addon) :tools))))
       (sort-by (fn [[_id entry]] (or (:reg-seq entry) 0)))
       (mapv (fn [[id {:keys [addon]}]]
               {:addon-id   id
                :addon-type (r/rescue nil (proto/addon-type addon))
                :tools      (vec (r/rescue [] (proto/tools addon)))
                :excluded   (set (safe-excluded-tools addon))}))))

(defn resolve-addon-tools
  "Full resolution of active addon tools against the host's core tools: a
   `tool-claims` Resolution (`:installed` / `:refused` / `:claims`). The
   diagnostic form of `active-addon-tools`, which returns only `:installed`."
  []
  (claims/resolve-claims ((or *core-tools* registry-core-tools))
                         (active-contributions)))

(defn claimed-core-names
  "Tool names an addon holds OVER a core tool of the same name, from a
   `resolve-addon-tools` Resolution. Re-exported so the server seam talks to
   this namespace only; the rule itself lives in `tool-claims`."
  [resolution]
  (claims/claimed-core-names resolution))

(defn active-addon-tools
  "Get all MCP tools from active addons, each tagged `:addon-source`.

   Names are resolved by `hive-mcp.addons.tool-claims`: an addon that both
   PROVIDES a name and lists it in `excluded-tools` CLAIMS it, over a core
   tool of that name and over every other addon; an exclusion without a
   provider only refuses other addons; otherwise a core tool of that name
   wins and the addon tool is refused as `:shadows-core`. Refusals are
   dropped here: use `resolve-addon-tools` to see them."
  []
  (:installed (resolve-addon-tools)))

(defn addon-tools-by-name
  "Get MCP tools contributed by a specific addon."
  [id]
  (if-let [{:keys [addon state]} (get-addon-entry id)]
    (if (and (= state :active)
             (contains? (proto/capabilities addon) :tools))
      (vec (proto/tools addon))
      [])
    []))

(defn addons-with-capability
  "Find all active addons providing a specific capability."
  [capability]
  (->> @addon-registry
       (filter (fn [[_id {:keys [state addon]}]]
                 (and (= state :active)
                      (contains? (proto/capabilities addon) capability))))
       (mapv first)))

(defn all-capabilities
  "Get capability summary from all active addons."
  []
  (let [active (->> @addon-registry
                    (filter (fn [[_id {:keys [state]}]] (= state :active))))]
    (->> active
         (mapcat (fn [[id {:keys [addon]}]]
                   (map (fn [cap] [cap id]) (proto/capabilities addon))))
         (reduce (fn [acc [cap id]]
                   (update acc cap (fnil conj []) id))
                 {}))))

(defn check-dependencies
  "Check if all dependencies for an addon are satisfied.
   Dependencies come from health details: {:details {:dependencies #{...}}}"
  [id]
  (if-let [addon (get-addon id)]
    (let [h (r/guard Exception {} (proto/health addon))
          deps (or (get-in h [:details :dependencies]) #{})
          available (set (keys @addon-registry))
          missing (clojure.set/difference deps available)]
      {:satisfied? (empty? missing)
       :missing missing
       :available (clojure.set/intersection deps available)})
    {:satisfied? false
     :missing #{id}
     :available #{}}))

(defn reset-registry!
  "Reset the addon registry, shutting down active addons first.
   Also clears all composite tool contributions."
  []
  (let [shutdown-results (shutdown-all!)]
    (reset! addon-registry {})
    (log/info "Addon registry reset")
    shutdown-results))

(defn registry-status
  "Get comprehensive status of the addon registry."
  []
  (let [entries (vals @addon-registry)
        by-state (group-by :state entries)]
    {:total (count entries)
     :active (count (:active by-state))
     :registered (count (:registered by-state))
     :error (count (:error by-state))
     :tool-count (count (active-addon-tools))
     :capabilities (all-capabilities)
     :addons (list-addons)}))

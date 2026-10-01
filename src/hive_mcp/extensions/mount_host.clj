(ns hive-mcp.extensions.mount-host
  "IMountHost adapter over the hive-mcp addon registry (addons.core).

   Lets the generic composer hive-addon.mount.compose drive hive-mcp's own
   addon lifecycle: register! -> addons.core/register-addon!, init! ->
   addons.core/init-addon! (which itself honors schema-extensions, tools and
   the declarative IAddon `hooks` seam), shutdown! -> addons.core/shutdown-addon!
   (no-nuke), registered -> the mounted instance for sibling injection.

   Seams are injectable (DIP) so the adapter is testable against a fake
   registry with no global state."
  (:require [hive-addon.mount.port :as port]
            [hive-mcp.addons.core :as addon-core]
            [hive-addon.protocol :as proto]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

;; =============================================================================
;; Value: a host is its five registry seams
;; =============================================================================

(defn- shutdown-failure
  "The ex-info a failed shutdown answer ({:success? false :errors [..]}) is
   raised as, or nil when RESULT is not a failure. Pure."
  [addon-id result]
  (when (and (map? result) (false? (:success? result)))
    (ex-info (str/join "; " (or (seq (:errors result)) [(str "shutdown of " addon-id " failed")]))
             {:addon-id addon-id :result result})))

(defrecord AddonRegistryHost [reg-fn init-fn shutdown-fn unreg-fn registered-fn]
  port/IMountHost
  (register! [this addon]
    ;; addons.core/register-addon! REFUSES a duplicate id: it warns and returns
    ;; {:success? false}, leaving the incumbent instance in place. This port
    ;; returns the host either way, so that refusal is invisible to the caller
    ;; — and a remount would then keep the STALE object and merely re-initialize
    ;; it, while reporting success. Dropping the old entry first is what makes
    ;; re-registration actually take.
    ;;
    ;; Safe during a remount: teardown! has already run shutdown-addon!, which
    ;; moves the entry to :registered, so unregister-addon! sees a non-:active
    ;; entry and will not shut the addon down a second time.
    (let [id (proto/addon-id addon)]
      (when (some? (registered-fn id))
        (unreg-fn id))
      (reg-fn addon))
    this)
  (init! [_ addon-id config] (init-fn addon-id config))
  (shutdown! [_ addon-id]
    ;; addons.core/shutdown-addon! answers a failure as DATA. The port returns
    ;; nil, so the failure is raised instead of dropped: boundary/teardown!
    ;; records a throw under :errors, which is how a caller gets to see it.
    (when-let [e (shutdown-failure addon-id (shutdown-fn addon-id))]
      (throw e))
    nil)
  (registered [_ addon-id] (registered-fn addon-id)))

;; =============================================================================
;; Optional capability: plug-out (hive-addon.mount.port/IMountUnregister)
;;
;; The released hive-addon jar has no IMountUnregister. The capability is
;; therefore EXTENDED onto the record when the protocol resolves, never named
;; in a reify: this namespace keeps loading against either hive-addon.
;; =============================================================================

(defn- soft
  "The var SYM names, or nil when it cannot be resolved: an absent var is how
   an older hive-addon answers, and the caller degrades on nil."
  [sym]
  (try (requiring-resolve sym) (catch Throwable _absent-in-this-hive-addon nil)))

(defn unregister-entry!
  "Drop ADDON-ID from HOST's registry through its :unreg-fn, when it is there.
   Called only after shutdown!, so unregister-addon! meets a non-:active entry
   and does not shut the addon down twice. Idempotent. Returns HOST."
  [host addon-id]
  (when (some? ((:registered-fn host) addon-id))
    ((:unreg-fn host) addon-id))
  host)

(def ^:private unregister-methods
  ;; Reaches unregister-entry! through its var on every call (Capture-by-Var).
  {:unregister! (fn [host addon-id] (unregister-entry! host addon-id))})

(defn ensure-unregister!
  "Extend AddonRegistryHost with hive-addon.mount.port/IMountUnregister when
   the hive-addon on the classpath has it. Idempotent, and re-extends after a
   reload of either side (a new protocol object or a new record class).
   Returns true when the host now has the capability."
  []
  (if-let [pv (soft 'hive-addon.mount.port/IMountUnregister)]
    (let [p @pv]
      (when-not (extends? p AddonRegistryHost)
        (extend AddonRegistryHost p unregister-methods))
      true)
    false))

(defn addon-registry-host
  "Construct an IMountHost backed by the hive-mcp addon registry. opts may
   override the default addons.core seams (:reg-fn :init-fn :shutdown-fn
   :unreg-fn :registered-fn) for isolated tests.

   register! has REPLACE semantics, which a remount depends on — see its body.
   The host also plugs out (IMountUnregister) when hive-addon has that port."
  ([] (addon-registry-host {}))
  ([{:keys [reg-fn init-fn shutdown-fn unreg-fn registered-fn]
     :or {reg-fn        addon-core/register-addon!
          init-fn       addon-core/init-addon!
          shutdown-fn   addon-core/shutdown-addon!
          unreg-fn      addon-core/unregister-addon!
          registered-fn (fn [id] (:addon (addon-core/get-addon-entry id)))}}]
   (ensure-unregister!)
   (->AddonRegistryHost reg-fn init-fn shutdown-fn unreg-fn registered-fn)))

(defn current
  "HOST rebuilt by the current AddonRegistryHost constructor when it was
   built from an earlier definition of the record (same class name, other
   class object: what a namespace reload leaves), else HOST itself. Its seams
   are carried over, so nothing is lost; the rebuilt host has the plug-out
   capability the stale class never got."
  [host]
  (if (and (record? host)
           (not (instance? AddonRegistryHost host))
           (= (.getName ^Class (class host)) (.getName ^Class AddonRegistryHost)))
    (do (ensure-unregister!)
        (map->AddonRegistryHost (into {} host)))
    host))

;; =============================================================================
;; Boundary: unregister ids after teardown
;; =============================================================================

(defn- unregister-via-seam
  "The fallback when hive-addon has no boundary/unregister!: an
   AddonRegistryHost still plugs out through its own :unreg-fn; any other host
   cannot, and every id is reported :unsupported."
  [host addon-ids]
  (if-not (instance? AddonRegistryHost host)
    {:unregistered [] :unsupported (vec addon-ids) :errors []}
    (reduce (fn [acc id]
              (let [res (try (unregister-entry! host id) nil
                             (catch Throwable t (str id ": " (ex-message t))))]
                (if res (update acc :errors conj res) (update acc :unregistered conj id))))
            {:unregistered [] :unsupported [] :errors []}
            addon-ids)))

(defn unregister!
  "Drop ADDON-IDS from HOST after they were torn down. Goes through
   hive-addon.mount.boundary/unregister! (resolved per call) when hive-addon
   has it, otherwise through the host's own seam. Returns
   {:unregistered [id] :unsupported [id] :errors [string]}. Never throws."
  [host addon-ids]
  (if-let [f (soft 'hive-addon.mount.boundary/unregister!)]
    (f host addon-ids)
    (unregister-via-seam host addon-ids)))

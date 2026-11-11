(ns hive-mcp.tools.consolidated.hot-eject-fallback
  "Host-native plug-out for `hot eject` and `hot unmount`, used when the
   image's hive-addon has no hive-addon.hot.inject/eject! or plug-out!.

   Same shape as the bridge, (fallback host specs target opts), over the
   IMountHost port: the target must be registered; mounted dependents refuse
   it (cascade stays refused here); the addon is shut down through the host
   (IAddon shutdown!), unregistered through mount-host/unregister!, and
   deregistered from hive-hot when hive-addon.hot/unhot! resolves.

   Calls in flight are NOT drained, and classpath entries, loaded namespaces
   and hive-hot dir claims are RETAINED; the report says both under
   :hot/drain and :retained. Never throws.

   opts may override the collaborators (doubles in tests):
     :dependents    (fn [specs ids] -> #{dependent-id})
     :path-ids      (fn [path] -> [id] | nil)
     :unhot!        (fn [specs] -> {:hot/unregistered [id]})
     :source-dirs   (fn [specs] -> [dir])
     :project-paths (fn [path] -> [entry])"
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [hive-addon.mount.port :as port]
            [hive-dsl.result :as r]
            [hive-mcp.extensions.mount-host :as mount-host]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- soft [sym]
  (try (requiring-resolve sym) (catch Throwable _absent nil)))

;; =============================================================================
;; Pure
;; =============================================================================

(defn declared-dependents
  "Pure. The ids of SPECS that depend on any of IDS through
   :addon/dependencies, transitively, without IDS themselves."
  [specs ids]
  (let [seeds (set ids)]
    (loop [seen seeds]
      (let [more (into #{}
                       (comp (filter #(seq (set/intersection seen (set (:addon/dependencies %)))))
                             (map :addon/id)
                             (remove seen))
                       specs)]
        (if (empty? more)
          (set/difference seen seeds)
          (recur (into seen more)))))))

(m/=> declared-dependents [:=> [:cat [:sequential :map] [:or [:set :any] [:sequential :any]]]
                           [:set :any]])

(def no-drain
  "What :hot/drain says in the fallback: nothing was drained."
  {:drained? false
   :gate     :absent
   :message  (str "not drained: this image's hive-addon has no drain gate "
                  "(hive-addon.hot.drain); calls in flight were not waited for")})

(def cascade-unsupported-msg
  (str "cascade is not available in the host-native fallback: remounting the "
       "dependents needs hive-addon.hot.inject/eject! (bump hive-addon and restart)"))

(defn fallback-base
  "Pure. The EjectReport the fallback starts from."
  [target known unknown]
  (cond-> {:hot/target target
           :hot/ejected (vec known)
           :hot/fallback true
           :hot/drain no-drain
           :hot/torn-down []
           :hot/unregistered []
           :hot/unsupported []
           :hot/unhot []
           :hot/classpath-retained []
           :hot/dirs-retained []
           :hot/namespaces-retained []
           :teardown/data-preserved? true
           :ok? true}
    (seq unknown) (assoc :hot/unknown (vec unknown))))

(defn verdict
  "Pure. What the fallback does with KNOWN ids and their mounted BLOCKING
   dependents: :unknown, :refused, or :proceed."
  [known blocking]
  (cond
    (empty? known) :unknown
    (seq blocking) :refused
    :else          :proceed))

(m/=> verdict [:=> [:cat [:sequential :any] [:sequential :any]]
               [:enum :unknown :refused :proceed]])

(defn refusal
  "Pure. BASE refused because BLOCKING depend on KNOWN."
  [base known blocking cascade?]
  (assoc base :ok? false :hot/refused? true :hot/blocking (vec blocking)
         :errors [(str "mounted addons depend on " (pr-str (vec (sort known))) ": "
                       (pr-str (vec blocking))
                       (if cascade?
                         (str " - " cascade-unsupported-msg)
                         " - eject them first (cascade needs the newer hive-addon)"))]))

(defn retained
  "Pure. What a fallback plug-out leaves in the image, and why."
  [classpath dirs namespaces]
  {:classpath         (vec classpath)
   :hive-hot-dirs     (vec dirs)
   :namespaces        (vec namespaces)
   :restart-required? true
   :message (str "the addon is out of the registry and the tool surface, but its "
                 "classpath entries, loaded namespaces and hive-hot source-dir claims "
                 "stay until a restart")})

(defn outcome
  "Pure. A fallback EjectReport as plug-out!'s Result: (ok report), or an err
   :hot/eject-refused | :hot/eject-unknown | :hot/eject-failed carrying the
   report and its :message."
  [report]
  (cond
    (:ok? report) (r/ok report)
    (:hot/refused? report)
    (r/err :hot/eject-refused (assoc report :message (first (:errors report))))
    (empty? (:hot/ejected report))
    (r/err :hot/eject-unknown (assoc report :message (first (:errors report))))
    :else
    (r/err :hot/eject-failed (assoc report :message (str/join "; " (:errors report))))))

(def FallbackReport
  "The EjectReport slice every fallback answer carries."
  [:map
   [:hot/ejected [:vector :any]]
   [:hot/fallback [:= true]]
   [:hot/drain :map]
   [:ok? :boolean]])

(def FallbackResult
  "plug-out!'s Result form of a fallback report."
  [:or
   [:map [:ok :map]]
   [:map
    [:error [:enum :hot/eject-refused :hot/eject-unknown :hot/eject-failed]]
    [:message [:maybe :string]]]])

(m/=> fallback-base [:=> [:cat :any [:sequential :any] [:sequential :any]] FallbackReport])

(m/=> refusal [:=> [:cat :map [:sequential :any] [:sequential :any] :any]
               [:map
                [:ok? [:= false]]
                [:hot/refused? [:= true]]
                [:hot/blocking [:vector :any]]
                [:errors [:vector :string]]]])

(m/=> retained [:=> [:cat [:sequential :any] [:sequential :any] [:sequential :any]]
                [:map
                 [:classpath [:vector :any]]
                 [:hive-hot-dirs [:vector :any]]
                 [:namespaces [:vector :any]]
                 [:restart-required? [:= true]]
                 [:message :string]]])

(m/=> outcome [:=> [:cat :map] FallbackResult])

;; =============================================================================
;; Collaborators, resolved at call time
;; =============================================================================

(defn- default-dependents [specs ids]
  (let [native (declared-dependents specs ids)]
    (if-let [f (soft 'hive-addon.hot.cascade/dependents)]
      (into native (try (set/difference (set (f specs (set ids))) (set ids))
                        (catch Throwable _degrades-to-empty #{})))
      native)))

(defn- default-project-paths [path]
  (when-let [f (soft 'hive-addon.hot.inject/project-paths)]
    (try (vec (f path)) (catch Throwable _degrades-to-empty nil))))

(defn- default-path-ids [path]
  (let [paths (default-project-paths path)
        under (soft 'hive-addon.hot.inject/manifests-under)]
    (when (and (seq paths) under)
      (try (seq (mapv :addon/id (:specs (under paths))))
           (catch Throwable _degrades-to-empty nil)))))

(defn- default-unhot! [specs]
  (if-let [f (soft 'hive-addon.hot/unhot!)]
    (f specs)
    {:hot/unregistered []}))

(defn- default-source-dirs [specs]
  (if-let [f (soft 'hive-addon.hot/watchable-dirs)]
    (try (vec (f specs)) (catch Throwable _degrades-to-empty []))
    []))

(defn- path-like? [target]
  (and (string? target) (str/includes? target "/")))

(defn- target-ids [path-ids target]
  (or (when (path-like? target) (path-ids target))
      [(str target)]))

(defn- registered? [host id]
  (some? (try (port/registered host id) (catch Throwable _degrades-to-empty nil))))

;; =============================================================================
;; Boundary
;; =============================================================================

(defn- shutdown-failures
  "Shut each id down through HOST. {id message} per failure; never throws."
  [host ids]
  (into {}
        (keep (fn [id]
                (try (port/shutdown! host id) nil
                     (catch Throwable t [id (str id ": shutdown! failed: " (ex-message t))]))))
        ids))

(defn- plug-out-slice!
  [host specs target known base opts]
  (let [unhot!      (:unhot! opts default-unhot!)
        source-dirs (:source-dirs opts default-source-dirs)
        proj-paths  (:project-paths opts default-project-paths)
        by-id       (into {} (map (juxt :addon/id identity)) specs)
        ejected     (mapv #(or (by-id %) {:addon/id %}) known)
        failures    (shutdown-failures host known)
        un          (try (mount-host/unregister! host known)
                         (catch Throwable t {:unregistered [] :unsupported []
                                             :errors [(str "unregister!: " (ex-message t))]}))
        unhot       (try (unhot! ejected)
                         (catch Throwable t {:hot/unregistered [] :error (ex-message t)}))
        dirs        (try (vec (source-dirs (vec (keep by-id known)))) (catch Throwable _degrades-to-empty []))
        classpath   (vec (or (when (path-like? target) (seq (proj-paths target))) dirs))
        namespaces  (vec (sort (distinct (keep #(some-> (:addon/init-ns %) str) ejected))))
        errors      (cond-> (into (vec (vals failures)) (:errors un))
                      (:error unhot) (conj (str "hive-hot unhot!: " (:error unhot))))]
    (cond-> (assoc base
                   :hot/torn-down (vec (remove failures known))
                   :hot/unregistered (vec (:unregistered un))
                   :hot/unsupported (vec (:unsupported un))
                   :hot/unhot (vec (:hot/unregistered unhot))
                   :hot/classpath-retained classpath
                   :hot/dirs-retained dirs
                   :hot/dirs-reason "hive-hot claims are not released by the fallback"
                   :hot/namespaces-retained namespaces
                   :retained (retained classpath dirs namespaces)
                   :ok? (empty? errors))
      (seq errors) (assoc :errors errors))))

(defn eject-fallback
  "Plug TARGET (an addon id, or the path it was injected from) out of HOST
   without hive-addon's eject!. Same arguments as eject!; answers an
   EjectReport with :hot/fallback true. Never throws."
  [host specs target & [opts]]
  (try
    (let [opts     (or opts {})
          specs    (vec specs)
          ids      (target-ids (:path-ids opts default-path-ids) target)
          known    (filterv #(registered? host %) ids)
          unknown  (filterv (complement (set known)) ids)
          base     (fallback-base target known unknown)
          blocking (vec (sort (remove (set known)
                                      ((:dependents opts default-dependents) specs known))))]
      (case (verdict known blocking)
        :unknown (assoc base :ok? false :errors [(str "nothing to eject for " (pr-str target))])
        :refused (refusal base known blocking (:cascade? opts))
        :proceed (plug-out-slice! host specs target known base opts)))
    (catch Throwable t
      {:hot/target target :hot/ejected [] :hot/fallback true :hot/drain no-drain
       :ok? false :errors [(str "fallback eject threw: " (ex-message t))]})))

(m/=> eject-fallback [:=> [:cat :any [:maybe [:sequential :map]] :any [:? [:maybe :map]]]
                      [:map [:ok? :boolean] [:hot/fallback [:= true]]]])

(defn undo-mode
  "How a `hot inject` made now can be undone: :bridge when this image's
   hive-addon has eject!, else :fallback (this namespace's plug-out)."
  []
  (if (soft 'hive-addon.hot.inject/eject!) :bridge :fallback))

(def UndoReport
  "What `undo-report` answers: how an inject made now could be undone.
   :retains names what stays in the JVM after the undo, until a restart."
  [:map
   [:mode [:enum :bridge :fallback :none]]
   [:undoable? :boolean]
   [:drains? :boolean]
   [:cascades? :boolean]
   [:retains [:vector [:enum :classpath :namespaces :hive-hot-dirs :addon]]]
   [:restart-required? :boolean]
   [:message :string]])

(defn undo-report
  "Pure. How a `hot inject` made now could be undone, from what the image has.

   No undo is complete: a JVM cannot drop classpath URLs or unload loaded
   namespaces, so every mode :retains at least those until a restart (the
   eject report lists them as retained too), and :restart-required? is true
   for a clean image in every mode.

   BRIDGE? is hive-addon's eject! and plug-out! both resolving: registry,
   tool surface and hive-hot dir claims are released, calls in flight are
   drained and dependents can cascade. FALLBACK? is the host able to drop a
   registry entry by itself (this namespace's plug-out): registry and tool
   surface only, no drain, no cascade, dir claims kept. Neither: only a
   restart removes the addon.

   Friction 20261110132734-7e9213d3: an inject on an image whose hive-addon
   lacked plug-out! could not be undone, and nothing said so before mounting."
  [bridge? fallback?]
  (cond
    bridge?
    {:mode :bridge :undoable? true :drains? true :cascades? true
     :retains [:classpath :namespaces] :restart-required? true
     :message (str "hot eject / hot unmount can take this inject back out through hive-addon "
                   "eject!/plug-out! (drain, cascade, hive-hot dir claims released); its "
                   "classpath entries and loaded namespaces stay until a restart")}

    fallback?
    {:mode :fallback :undoable? true :drains? false :cascades? false
     :retains [:classpath :namespaces :hive-hot-dirs] :restart-required? true
     :message (str "this image's hive-addon has no eject!/plug-out!: hot eject / hot unmount "
                   "will use the host-native fallback, which drops the addon from the registry "
                   "and the tool surface but keeps its classpath entries, loaded namespaces and "
                   "hive-hot dir claims until a restart (no drain, no cascade)")}

    :else
    {:mode :none :undoable? false :drains? false :cascades? false
     :retains [:addon :classpath :namespaces :hive-hot-dirs] :restart-required? true
     :message (str "this image cannot undo an inject: hive-addon has no eject!/plug-out! and "
                   "the mount host cannot unregister an addon; only a restart removes it")}))

(m/=> undo-report [:=> [:cat :any :any] UndoReport])

(defn- host-can-unregister?
  "Can HOST drop a registry entry without hive-addon's eject!? Through
   hive-addon.mount.boundary/unregister! (resolved by RESOLVE), or the
   AddonRegistryHost's own seam."
  [resolve host]
  (or (some? (resolve 'hive-addon.mount.boundary/unregister!))
      (instance? hive_mcp.extensions.mount_host.AddonRegistryHost (mount-host/current host))))

(defn undo-preflight
  "How a `hot inject` made now on HOST could be undone (`undo-report`), read
   at call time. RESOLVE (symbol -> var or nil) defaults to resolving in this
   image; tests pass a map of the symbols present instead. Never throws."
  ([host] (undo-preflight host soft))
  ([host resolve]
   (try
     (undo-report (and (some? (resolve 'hive-addon.hot.inject/eject!))
                       (some? (resolve 'hive-addon.hot.inject/plug-out!)))
                  (and (some? host) (host-can-unregister? resolve host)))
     (catch Throwable t
       (assoc (undo-report false false)
              :message (str "undo preflight failed: " (ex-message t)))))))

(m/=> undo-preflight [:function
                      [:=> [:cat :any] UndoReport]
                      [:=> [:cat :any ifn?] UndoReport]])

(m/=> undo-mode [:=> [:cat] [:enum :bridge :fallback]])

(defn plug-out-fallback
  "eject-fallback as plug-out!'s Result (see `outcome`). Never throws."
  [host specs target & [opts]]
  (outcome (eject-fallback host specs target opts)))

(m/=> plug-out-fallback [:=> [:cat :any [:maybe [:sequential :map]] :any [:? [:maybe :map]]]
                         FallbackResult])

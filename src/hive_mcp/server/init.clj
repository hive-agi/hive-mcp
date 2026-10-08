(ns hive-mcp.server.init
  "Service initialization: embedding, hot-reload, events, coordinator.

   Bounded context: Service bootstrap and dependency wiring.

   Manages:
   - Embedding provider initialization (Chroma, Ollama, OpenRouter)
   - Hot-reload auto-healing (MCP tool refresh after reload)
   - Event system initialization (re-frame inspired)
   - Coordinator registration in DataScript
   - Memory store wiring (IMemoryStore protocol)
   - Channel bridge + swarm sync + registry sync
   - decay scheduler (periodic memory/edge/disc decay)
   - housekeeping scheduler (periodic GC sweep + stale resource cleanup)"
  (:require [hive-mcp.channel.websocket :as ws-channel]
            [hive-mcp.dns.result :as result]
            [hive-mcp.config.core :as global-config]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.effects :as effects]
            [hive-mcp.events.handlers :as ev-handlers]
            [hive-mcp.events.channel-bridge :as channel-bridge]
            [hive-mcp.tools.swarm :as swarm]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.addons.boot-health :as boot-health]
            [hive-mcp.swarm.sync :as sync]
            [hive-mcp.swarm.bootstrap.factory :as bootstrap-factory]
            [hive-mcp.swarm.event-bridge :as swarm-event-bridge]
            [hive-mcp.channel.piggyback :as piggyback]
            [hive-mcp.channel.instruction-store :as instruction-store]
            [hive-mcp.protocols.event-backbone :as eb]
            [hive-mcp.swarm.logic :as logic]
            [hive-hot.core :as hot]
            [hive-hot.events :as hot-events]
            [taoensso.timbre :as log]
            [clojure.string :as str] [hive-dsl.result :refer [rescue]]
            [hive-mcp.hot.self :as hot-self]
            [hive-mcp.protocols.vector :as vec-proto]
            [hive-mcp.spi.contributions :as contrib]
            [hive-mcp.extensions.soft :as soft]
            [hive-mcp.hot.core :as hot-core]
            [hive-mcp.hot.reseat :as reseat]
            [hive-mcp.swarm.lifecycle.boot-reconcile :as boot-reconcile]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Embedding Warm-Up
;; =============================================================================

(defn warmup-embedding!
  "Fire a background future to warm up the Ollama embedding model.

   First embedding call via ollama/nomic-embed-text takes ~3.5s (cold start
   model loading). Subsequent calls are ~50ms. This pre-loads the model so
   the first real catchup or memory search does not pay the cold start penalty.

   Non-blocking: runs in a future so it does not delay server startup. The
   embedding service is memory domain, so it is resolved BY SYMBOL: with no
   memory domain in the build there is nothing to warm and the future is a
   no-op.

   When embeddings.warmup.enabled is true, the memory-domain warmup starts a
   bounded daemon worker for each distinct routed local model. Disabled by
   default: no network request or thread is made unless explicitly enabled."
  []
  (when (true? (get-in (global-config/get-global-config) [:embeddings :warmup :enabled]))
    (try
      (if-let [warm! (soft/resolve-soft 'hive-mcp.embeddings.warmup/start!)]
        (warm! (global-config/get-global-config))
        (log/debug "no embedding warmup in this build; skipping warmup"))
      (catch Exception e
        (log/warn "Embedding warmup failed (non-fatal):" (ex-message e))))))

;; =============================================================================
;; Embedding Provider Initialization
;; =============================================================================

(def boot-manifest-resource
  "Classpath resource naming the boot steps still shipped inside core."
  "hive-mcp/boot-contributions.edn")

(defn load-boot-contributions!
  "Contribute the in-core boot steps. Idempotent: re-running picks up a domain
   that arrived since the last call and leaves the rest alone."
  []
  (contrib/load-manifest! boot-manifest-resource))

(defn init-embedding-provider!
  "Run the :embeddings boot step: the Chroma connection, the EmbeddingService
   and the per-collection provider routing.

   The kernel keeps the ENTRY POINT and nothing else. The step itself is memory
   domain work and lives in `hive-mcp.embeddings.boot` while it ships inside
   core (declared in `boot-manifest-resource`), or in the hive-memory addon,
   which contributes it at `initialize!`.

   Returns true when a step ran, false when none is contributed or it failed.
   A kernel-only build takes the false branch: there is nothing to embed with,
   and saying so beats pretending the wiring happened."
  []
  (load-boot-contributions!)
  (let [{:keys [ran failed]} (contrib/register-all! :boot)]
    (when (seq failed)
      (log/error "boot contributions failed:" (sort (keys failed))))
    (when (empty? ran)
      (log/info "no embedding boot step contributed; semantic search is unconfigured"))
    (boolean (and (seq ran) (empty? failed)))))

;; =============================================================================
;; Hot-Reload Auto-Healing
;; =============================================================================

(defn- emit-mcp-health-event!
  "Emit health event via WebSocket channel after hot-reload.
   Lings can listen for this to confirm MCP is operational."
  [loaded-ns unloaded-ns ms]
  (result/rescue-log "emit-mcp-health-event!" nil
                 (ws-channel/emit! :mcp-health-restored
                                   {:loaded (count loaded-ns)
                                    :unloaded (count unloaded-ns)
                                    :reload-ms ms
                                    :timestamp (System/currentTimeMillis)
                                    :status "healthy"})
                 (log/info "Emitted :mcp-health-restored event after hot-reload")))

(defn- handle-hot-reload-success!
  "Handler for successful hot-reload - refreshes tools and emits health event.

   Parameters:
     server-context-atom - atom containing MCP server context, or nil when
                           no server has been started in this image"
  [server-context-atom {:keys [loaded unloaded ms]}]
  (log/info "Hot-reload completed:" (count loaded) "loaded," (count unloaded) "unloaded in" ms "ms")
  ;; Refresh MCP tool handlers to point to new var values
  (result/rescue-log "handle-hot-reload-success!" nil
                     (when (some-> server-context-atom deref)
                       (routes/refresh-tools! server-context-atom)
                       (log/info "MCP tools refreshed after hot-reload")))
  ;; Emit health event for lings
  (emit-mcp-health-event! loaded unloaded ms))

(def server-context-var
  "The var holding THE server context atom. server.core requires this
   namespace, so it is named, never required, and resolved per call."
  'hive-mcp.server.core/server-context-atom)

(defn server-context-atom
  "The live server context atom, read through its var NOW; nil before
   hive-mcp.server.core is loaded."
  []
  (some-> (resolve server-context-var) deref))

(defn on-hot-reload-event!
  "The :mcp-auto-heal listener body: on :reload-success, re-seat the live
   record instances of the loaded namespaces (hive-mcp.hot.reseat; a watcher
   reload never passes through hive-mcp.hot.core/reload!), then refresh the
   tools of the server context current at the time of the event. Re-seating
   is idempotent, so a core reload that already ran it finds nothing stale."
  [event]
  (when (= :reload-success (:type event))
    (result/rescue-log "on-hot-reload-event! reseat" nil
                       (reseat/reseat! (mapv str (:loaded event))))
    (handle-hot-reload-success! (server-context-atom) event)))

(def auto-heal-listener-id
  "The key the auto-heal listener is registered under in hive-hot."
  :mcp-auto-heal)

(defn register-hot-reload-listener!
  "Register the MCP auto-heal listener with hive-hot. Keyed, so a re-run
   (a reboot, a reload of this namespace) REPLACES the entry: idempotent with
   no guard to go stale. The listener reaches `on-hot-reload-event!` through
   its var on every event (hive-mcp.hot.reseat/via-var), so a reload of this
   namespace is followed, and the event reads the server context current at
   that moment instead of one captured at boot.

   `add-listener!` is the registration port, (fn [id f]); hive-hot's by default."
  ([] (register-hot-reload-listener! hot/add-listener!))
  ([add-listener!]
   (add-listener! auto-heal-listener-id (reseat/via-var `on-hot-reload-event!))
   (log/info "Registered hot-reload listener for MCP auto-healing")
   auto-heal-listener-id))

;; =============================================================================
;; Event System Initialization
;; =============================================================================

(defn init-events!
  "Initialize hive-events system (re-frame inspired event dispatch).
   EVENTS-01: Event system must init after hooks but before channel."
  []
  (result/rescue-log "init-events!" nil
                 (ev/init!)
                 (effects/register-effects!)
                 (ev-handlers/register-handlers!)
                 (log/info "hive-events system initialized")))

;; =============================================================================
;; Coordinator Registration
;; =============================================================================

(defn register-coordinator!
  "Register coordinator in DataScript + hivemind (Phase 4).


   Parameters:
     coordinator-id-atom - atom to store coordinator project-id"
  [coordinator-id-atom]
  (result/rescue-log "register-coordinator!" nil
                 (require 'hive-mcp.swarm.datascript)
                 (require 'hive-mcp.swarm.datascript.lings)
                 (let [register! (resolve 'hive-mcp.swarm.datascript/register-coordinator!)
                       add-slave! (resolve 'hive-mcp.swarm.datascript.lings/add-slave!)
                       project-id (global-config/get-service-value :project :id :env "HIVE_MCP_PROJECT_ID" :default "hive-mcp")
                       cwd (System/getProperty "user.dir")]
                   (register! project-id {:project project-id})
      ;; Also register "coordinator" as a slave (depth 0) for bb-mcp compatibility
      ;; bb-mcp injects agent_id: "coordinator" on all tool calls for piggyback tracking
                   (add-slave! "coordinator" {:name "coordinator"
                                              :status :idle
                                              :depth 0  ;; depth 0 = coordinator (not a ling)
                                              :project-id project-id
                                              :cwd cwd})
                   (reset! coordinator-id-atom project-id)
                   (log/info "Coordinator registered:" project-id "(also as slave for bb-mcp compat)"))))

;; =============================================================================
;; Memory Store Wiring
;; =============================================================================

(defn- resolve-memory-backend
  "Resolve the configured memory backend id.

   Reads, in order:
     1. :services :memory-store :backend   (authoritative — user's config.edn)
     2. :memory :default-store             (legacy key still used by routes)
     3. \"chroma\"                         (default for green-field install)

   Ensures the global config atom is populated — wire-memory-store! is
   invoked in Phase 4, before Phase 5.5's explicit load-global-config!.
   Returns a lowercase string (e.g. \"milvus\", \"chroma\")."
  []
  (result/rescue-log "resolve-memory-backend" nil (global-config/load-global-config!))
  (let [cfg (global-config/get-global-config)
        b   (or (get-in cfg [:services :memory-store :backend])
                (get-in cfg [:memory :default-store])
                "chroma")]
    (-> b name clojure.string/lower-case)))

(defn- vector-store-adapter-sym
  "The adapter the deployment chose for the vector-collection seam, as a
   fully-qualified factory symbol under :services :vector-store :adapter.
   The symbol names a vendor adapter module OUTSIDE this repo; supporting a
   NEW backend means writing an adapter that satisfies
   protocols.vector/IVectorCollectionStore and naming it here in config —
   this composition root never enumerates backends, nor mentions any vendor
   by name (OCP + no-host-vendor-rule)."
  []
  (some-> (get-in (global-config/get-global-config)
                  [:services :vector-store :adapter])
          symbol))

(defn- wire-vector-store!
  "Install the vector-collection backend. THREE sources, first wins:

    1. an IAddon already installed one (the addon path is the primary
       extension mechanism: a vendor addon registers its own store and this
       root never learns the vendor's name);
    2. the deployment's configured adapter symbol
       (:services :vector-store :adapter) — resolved LAZILY, because a
       static require here would be a kernel -> non-kernel edge needing a
       waiver, and kernel.edn says its waiver list may only shrink. A missing
       jar fails at the call with a reason, not at compile;
    3. the legacy chroma fallback, kept until every deployment names an
       adapter (or the hive-chroma IAddon exists and stops it)."
  [backend-id]
  (result/rescue-log "wire-vector-store!" nil
                     (if (vec-proto/store-set?)
                       (log/info "Vector-collection store already wired by an addon; not overriding"
                                 {:backend backend-id})
                       (if-let [adapter-sym (vector-store-adapter-sym)]
                         (if-let [make (requiring-resolve adapter-sym)]
                           (do (vec-proto/set-store! (make))
                               (log/info "Vector-collection store wired"
                                         {:backend backend-id :adapter (str adapter-sym)}))
                           (log/warn "Configured vector-collection adapter not on classpath"
                                     {:backend backend-id :adapter (str adapter-sym)}))
                         (if-let [make (requiring-resolve 'hive-mcp.chroma.vector-store/chroma-vector-store)]
                           (do (vec-proto/set-store! (make))
                               (log/info "Chroma wired as the IVectorCollectionStore (legacy fallback)"
                                         {:backend backend-id}))
                           (log/warn "No vector-collection backend available"
                                     {:backend backend-id}))))))

(defn- create-chroma-store
  "The legacy Chroma memory store, resolved BY SYMBOL.

   This is the composition root, the one place allowed to name a backend, but
   naming is not requiring: `hive-mcp.memory.store.chroma` is a hive-memory
   extraction target and the axiom 20260711221408-2ae3b6ae wants no backend
   compiled into the kernel. The same file already resolves
   `chroma.vector-store/chroma-vector-store` this way.

   Returns nil when Chroma is not in this build, which leaves the store unset
   for an addon to register."
  []
  (if-let [create (soft/resolve-soft 'hive-mcp.memory.store.chroma/create-store)]
    (create)
    (log/warn "no Chroma store in this build; leaving the memory store for an addon to register")))

(defn- deferred-store-addon
  "The addon id a backend defers its memory store to, or nil when the kernel
   wires it itself."
  [backend]
  (case backend
    "milvus" "hive.milvus"
    nil))

(defn verify-memory-store!
  "Hold wire-memory-store!'s deferral to account once extensions have loaded.
   Records the facts with hive-mcp.addons.boot-health, which logs at ERROR
   when the store a backend deferred to never arrived. Returns the facts."
  []
  (result/rescue-log "verify-memory-store!" nil
                     (let [backend (resolve-memory-backend)
                           facts   {:backend     backend
                                    :deferred-to (deferred-store-addon backend)
                                    :store-set?  (boolean (mem-proto/store-set?))}]
                       (boot-health/record-memory! facts)
                       facts)))

(defn wire-memory-store!
  "Select and wire the memory backend.

     - milvus: defer to the hive-milvus addon, which registers its own store
       during Phase 4.5 (load-extensions!).
     - anything else: wire ChromaMemoryStore immediately (legacy behavior),
       when Chroma is present in the build.

   Must run AFTER init-embedding-provider! since Chroma config is set there.
   A post-extensions fallback in `ensure-memory-store!` guarantees a live
   store even when the selected addon fails to register.

   This is the COMPOSITION ROOT, and the one place allowed to name a concrete
   backend. It wires two INDEPENDENT seams: the memory-entry store
   (protocols.memory) and the named-collection store (protocols.vector) that
   plan.plans and presets.core drive."
  []
  (result/rescue-log "wire-memory-store!" nil
                     (let [backend (resolve-memory-backend)]
                       (case backend
                         "milvus"
                         (log/info "wire-memory-store!: deferring to hive-milvus addon"
                                   "(a PROMISE: verified after extensions load, ERROR if it never mounts)"
                                   {:backend backend :addon (deferred-store-addon backend)})

                         (when-let [store (create-chroma-store)]
                           (mem-proto/set-store! store)
                           (wire-vector-store! backend)
                           (log/info "ChromaMemoryStore wired as active IMemoryStore backend"
                                     {:backend backend}))))))

(defn ensure-memory-store!
  "Guarantee an active IMemoryStore after addon loading.

   Called in Phase 4.6 (after load-extensions!). If the configured backend's
   addon failed to register a store, wire ChromaMemoryStore as a safety
   fallback so memory queries don't throw 'No memory store configured'. With
   no Chroma in the build there is no fallback to wire, and the warning stands
   on its own: a store nobody registered is the honest answer.

   The vector-collection store gets the same treatment, and SEPARATELY: an
   addon may satisfy one seam and not the other, so a single `store-set?`
   check over both would leave whichever it did not name unwired."
  []
  (result/rescue-log "ensure-memory-store!" nil
                     (when-not (mem-proto/store-set?)
                       (log/warn "ensure-memory-store!: no store after extensions; wiring Chroma fallback")
                       (when-let [store (create-chroma-store)]
                         (mem-proto/set-store! store)))
                     (when-not (vec-proto/store-set?)
                       (log/warn "ensure-memory-store!: no vector-collection store after extensions;"
                                 "wiring fallback")
                       (wire-vector-store! (resolve-memory-backend)))))

;; =============================================================================
;; Channel Bridge + Sync
;; =============================================================================

(defn init-channel-bridge!
  "Initialize channel bridge - wires channel events to hive-events dispatch.
   EVENTS-01: Must init after both channel server and event system."
  []
  (result/rescue-log "init-channel-bridge!" nil
                 (channel-bridge/init!)
                 (log/info "Channel bridge initialized - channel events will dispatch to hive-events")))

(defn- build-swarm-bootstrap
  "Construct the configured ISwarmBootstrap.
   Honors explicit opts, falling back to `services.swarm-sync.source` in
   config.edn (default :emacs for backwards compatibility)."
  [opts]
  (bootstrap-factory/make-bootstrap
   opts
   (fn [section key & {:as get-opts}]
     (apply global-config/get-service-value section key (mapcat identity get-opts)))))

(defn- resolve-event-backbone
  "Pick the event backbone for swarm-sync: explicit opts > config.edn > :local.
   :local means in-process channel.core only (legacy default).
   :nats means also bridge slave events from the IEventBackbone."
  [opts]
  (or (:event-backbone opts)
      (rescue nil (some-> (global-config/get-service-value :swarm-sync :event-backbone
                                                 :parse keyword)))
      :local))

(defn start-swarm-sync!
  "Start swarm sync — bridges channel events to logic database, rehydrates
   the in-memory registry from the configured ISwarmBootstrap source, and
   (optionally) bridges the IEventBackbone (NATS) into the in-process event
   bus so distributed slave events reach the same handlers.

   Args:
     opts — {:source         :emacs|:datahike|:none
             :event-backbone :local|:nats
             :db-path        string
             :timeout-ms     int}
            (all optional; defaults resolved from config.edn)

   Order matters:
     1. Build + inject bootstrap (durable slave projection)
     2. start-sync! (classifies restored rows by liveness evidence BEFORE
        registering them, then subscribes to channel.core)
     3. Reconcile: retire rows whose absence the boot probe established
        (idempotent; isolated so a failure cannot skip step 4)
     4. Start NATS event bridge if requested (NATS → channel.core)"
  ([] (start-swarm-sync! {}))
  ([opts]
   (result/rescue nil
                  (let [bs (build-swarm-bootstrap opts)]
                    (sync/set-swarm-bootstrap! bs))
                  (sync/start-sync!)
                  ;; Retires rehydrated slaves (:zombie + :alive? false) — memory 20260423152822-70fe5631.
                  (rescue nil (boot-reconcile/reconcile-rehydrated-slaves!))
                  (let [eb (resolve-event-backbone opts)]
                    (when (= :nats eb)
                      (let [started? (swarm-event-bridge/start-nats-bridge!)]
                        (if started?
                          (log/info "Swarm sync: NATS event bridge started")
                          (log/warn "Swarm sync: NATS event bridge requested but not started"
                                    " (backbone may be disconnected — check :hive/nats init)")))
                      ;; Rewire the piggyback instruction queue through NATS so
                      ;; coordinators and lings in separate JVMs share a queue.
                      (let [bb (eb/get-backbone)]
                        (if (eb/connected? bb)
                          (do (instruction-store/rewire! piggyback/instruction-queues bb)
                              (log/info "Swarm sync: piggyback instruction store bound to NATS"))
                          (log/warn "Swarm sync: piggyback instruction store staying local"
                                    " (NATS backbone not connected)")))))
                  (log/info "Swarm sync started - logic database will track swarm state"))))

;; =============================================================================
;; Hot-Reload Watcher
;; =============================================================================

(defn watch-dirs
  "The ABSOLUTE directories the watcher covers, never relative to the JVM's
   working directory.

   `configured` is what config / env / .hive-project.edn named (nil or empty
   when nothing did); `roots` is core's classpath source roots
   (hive-mcp.hot.core/core-roots), [] when core runs from a jar. With nothing
   configured the roots are the answer. A relative configured dir resolves
   against core's project directory (the parent of its first root), so the
   stock \"src\" names core's own source root wherever the JVM was started.
   Only a jar-backed core, with no root to anchor on, falls back to the
   working directory."
  [configured roots]
  (let [base    (some-> (first roots) str java.io.File. .getParentFile)
        resolve (fn [d]
                  (let [f (java.io.File. (str d))]
                    (cond (.isAbsolute f) (.getPath f)
                          base            (.getPath (java.io.File. ^java.io.File base (str d)))
                          :else           (.getAbsolutePath f))))]
    (mapv resolve (or (seq configured) (seq roots) ["src"]))))

(defn init-hot-reload-watcher!
  "Initialize hot-reload watcher with claim-aware coordination.

   ADR: State-based debouncing - claimed files buffer until release.

   The watcher has always covered hive-mcp's OWN src and has always refreshed
   the tool table on a successful reload (see register-hot-reload-listener!).
   What it never passed was `:no-reload`, which `init-with-watcher!` has
   accepted all along. Without it, a reload of a core namespace that DEFINES a
   protocol orphans every reify and defrecord instance built against the old
   protocol object, and `satisfies?` then answers false for a class that
   plainly implements it (axiom 20260822010805-57856ae1). The set is derived
   from the live image by hive-mcp.hot.self, never listed, because a written
   list of the thirty-seven namespaces that define protocols today is correct
   only until somebody adds or moves one.

   The directories are ABSOLUTE (`watch-dirs`): core's source roots by
   default, and a relative configured dir resolves against core's project
   directory, never against the JVM's working directory.

   Parameters:
     project-config      - map from read-project-config (or nil)"
  [project-config]
  (let [hot-reload-enabled? (get project-config :hot-reload true)]
    (if hot-reload-enabled?
      (result/rescue nil
                     (let [src-dirs (watch-dirs (or (global-config/get-service-value :project :src-dirs
                                                                                     :env "HIVE_MCP_SRC_DIRS"
                                                                                     :parse #(str/split % #":"))
                                                    (:watch-dirs project-config))
                                                (hot-core/core-roots))
                           claim-checker (hot-events/make-claim-checker logic/get-all-claims)
                           no-reload (hot-self/protocol-namespaces)]
                       (hot/init-with-watcher! {:dirs src-dirs
                                                :claim-checker claim-checker
                                                :no-reload no-reload
                                                :debounce-ms 100})
                       (log/info "Hot-reload watcher started:"
                                 {:dirs src-dirs
                                  :protocol-namespaces-protected (count no-reload)})
          ;; Register MCP auto-heal listener to refresh tools after reload
                       (register-hot-reload-listener!)))
      (log/info "Hot-reload disabled via .hive-project.edn"))))

;; =============================================================================
;; Registry Sync
;; =============================================================================

(defn start-registry-sync!
  "Start lings registry sync - keeps Clojure registry in sync with elisp.
   ADR-001: Event-driven sync for lings_available to return accurate counts."
  []
  (result/rescue-log "start-registry-sync!" nil
                 (swarm/start-registry-sync!)
                 (log/info "Lings registry sync started - lings_available will track elisp lings")))

;; =============================================================================
;; Decay Scheduler
;; =============================================================================

(defn start-decay-scheduler!
  "Start the periodic decay scheduler.
   Runs memory staleness decay, edge confidence decay, and disc certainty
   decay on a configurable interval (default: 60 minutes).

   Configure via config.edn :services :scheduler:
     {:enabled true :interval-minutes 60 :memory-limit 50 :edge-limit 100}

   Non-fatal: if scheduler fails to start, system continues without it.
   Decay still runs on wrap/catchup hooks as before."
  []
  (result/rescue-log "start-decay-scheduler!" nil
                 (require 'hive-mcp.scheduler.decay)
                 (let [start-fn (resolve 'hive-mcp.scheduler.decay/start!)]
                   (when start-fn
                     (let [result (start-fn)]
                       (if (:started result)
                         (log/info "Decay scheduler started:" result)
                         (log/info "Decay scheduler not started:" (:reason result))))))))

(defn run-recall-canary!
  "Run the golden recall canary once, at boot, before agents ask anything.

   Returns the verdict map (or nil when the canary ns is absent). Non-fatal by
   construction: a faulting canary must not stop the server from starting, or
   an operator loses the very tool that would tell them what is wrong. The
   fault is on the boot log at ERROR and on the scheduler tick thereafter.

   Configure via config.edn :services :recall-canary {:enabled true}."
  []
  (result/rescue
   nil
   (let [enabled? (not (false? (global-config/get-config-value [:services :recall-canary :enabled])))]
     (if-not enabled?
       (log/info "Recall canary disabled by config")
       (when-let [run! (requiring-resolve 'hive-mcp.recall.canary.live/run!)]
         (let [verdict (run!)]
           (if (:ok? verdict)
             (log/info "Recall canary OK at boot:" (select-keys verdict [:ran :passed :skipped]))
             (log/error "RECALL CANARY FAULT AT BOOT — retrieval answers cannot be"
                        "trusted until this clears:" (:faults verdict)))
           verdict))))))

(defn run-recall-canary-async!
  "Run the boot canary on a daemon thread.

   Off-thread because the canary talks to the embedder and the store, and a
   slow provider must not lengthen boot; daemon because a canary must never
   hold the JVM open. Loudness is unaffected — the verdict lands on the log
   either way. Returns the Thread."
  []
  (doto (Thread. ^Runnable (fn [] (run-recall-canary!)) "recall-canary-boot")
    (.setDaemon true)
    (.start)))

(defn stop-decay-scheduler!
  "Stop the periodic decay scheduler. Called during shutdown."
  []
  (result/rescue-log "stop-decay-scheduler!" nil
                 (require 'hive-mcp.scheduler.decay)
                 (when-let [stop-fn (resolve 'hive-mcp.scheduler.decay/stop!)]
                   (stop-fn))))

;; =============================================================================
;; Housekeeping Scheduler (gc-fix-5)
;; =============================================================================

(defn start-housekeeping-scheduler!
  "Start the periodic housekeeping scheduler (gc-fix-5) and the context-store
   TTL reaper.

   Runs bounded atom GC sweep + stale resource cleanup every 5 minutes.
   Starts the context-store reaper unconditionally; both calls are idempotent.

   Configure via config.edn :services :housekeeping:
     {:enabled true :interval-minutes 5}

   Non-fatal: if either fails to start, system continues without it.
   GC sweep still runs on session wrap/complete as before."
  []
  (result/rescue-log "start-housekeeping-scheduler!" nil
                 (require 'hive-mcp.scheduler.housekeeping)
                 (let [start-fn (resolve 'hive-mcp.scheduler.housekeeping/start!)]
                   (when start-fn
                     (let [result (start-fn)]
                       (if (:started result)
                         (log/info "Housekeeping scheduler started:" result)
                         (log/info "Housekeeping scheduler not started:" (:reason result)))))))
  (result/rescue-log "start-housekeeping-scheduler!" nil
                 (require 'hive-mcp.channel.context-store)
                 (when-let [reaper-start! (resolve 'hive-mcp.channel.context-store/start-reaper!)]
                   (reaper-start!))))

(defn stop-housekeeping-scheduler!
  "Stop the periodic housekeeping scheduler and the context-store TTL reaper.
   Called during shutdown."
  []
  (result/rescue-log "stop-housekeeping-scheduler!" nil
                 (require 'hive-mcp.scheduler.housekeeping)
                 (when-let [stop-fn (resolve 'hive-mcp.scheduler.housekeeping/stop!)]
                   (stop-fn)))
  (result/rescue-log "stop-housekeeping-scheduler!" nil
                 (require 'hive-mcp.channel.context-store)
                 (when-let [reaper-stop! (resolve 'hive-mcp.channel.context-store/stop-reaper!)]
                   (reaper-stop!))))

;; =============================================================================
;; NATS Initialization
;; =============================================================================

(defn init-delivery-channels!
  "Register every IDeliveryChannel impl chosen by `register-default-channels!`.
   Run UNCONDITIONALLY at startup — independent of NATS, Emacs, or any
   editor frontend. Without this, headless hosts with NATS disabled would
   silently lose all delivery (the historical E2E-2 symptom). Non-fatal:
   factory errors are logged but don't abort init."
  []
  (try
    (when-let [reg-channels! (requiring-resolve 'hive-mcp.delivery.channels/register-default-channels!)]
      (reg-channels!))
    (catch Exception e
      (log/warn "[init] Failed to register delivery channels (non-fatal):" (ex-message e)))))

(defn init-nats!
  "Initialize NATS client + bridge + backbone. Opt-in via config: services.nats.enabled = true.
   Non-fatal: system degrades to NoopBackbone + polling if NATS unavailable.

   Delivery channels are registered separately via init-delivery-channels!
   so headless hosts still get a working delivery surface even when NATS
   is disabled."
  []
  (result/rescue-log "init-nats!" nil
                 (let [nats-config (global-config/get-service-config :nats)]
                   (when (:enabled nats-config)
                     (let [start! (requiring-resolve 'hive-mcp.nats.client/start!)
                           bridge! (requiring-resolve 'hive-mcp.nats.bridge/start-subscriptions!)
                           create-bb (requiring-resolve 'hive-mcp.nats.backbone/create-backbone)
                           set-bb! (requiring-resolve 'hive-mcp.protocols.event-backbone/set-backbone!)]
                       (start! nats-config)
                       (let [bb (create-bb)]
                         (set-bb! bb)
                         (log/info "[init] NatsBackbone set as active IEventBackbone"))
                       (bridge!)
                       (when-let [cb-start (requiring-resolve 'hive-mcp.swarm.callback/start-listener!)]
                         (cb-start)))))))

;; =============================================================================
;; Forge Belt Defaults
;; =============================================================================

(defn register-forge-belt-defaults!
  "Register default implementations for forge belt :fb/* extension points.
   Must run before load-extensions! so extensions can override."
  []
  (try
    (require 'hive-mcp.workflows.forge-belt-defaults)
    (when-let [register! (resolve 'hive-mcp.workflows.forge-belt-defaults/register-forge-belt-defaults!)]
      (register!))
    (catch Throwable e
      (log/warn "register-forge-belt-defaults! failed -- forge-belt extension points will return noop:"
                (.getMessage e)))))

;; =============================================================================
;; Extension Loading
;; =============================================================================

(defn load-extensions!
  "Load optional extension capabilities discovered on the classpath.
   Uses classpath manifest scanning + addon self-registration.
   Non-fatal: system works without extensions (noop defaults).

   Must run AFTER embedding/memory services (extensions may use Chroma)."
  []
  (result/rescue-log "load-extensions!" nil
                 (require 'hive-mcp.extensions.loader)
                 (let [load-fn (resolve 'hive-mcp.extensions.loader/load-extensions!)]
                   (when load-fn
                     (let [result (load-fn)]
                       (log/info "Extension loading complete:" result)))))
  ;; The memory deferral is a promise; check it was kept.
  (verify-memory-store!)
  ;; Post-init multi-dispatch coherence check (WARN-only)
  (result/rescue-log "load-extensions!" nil
                 (let [get-adv (requiring-resolve 'hive-mcp.tools.registry/get-advertised-tools)
                       check!  (requiring-resolve 'hive-mcp.multi.registry/check-dispatch-coherence!)]
                   (when (and get-adv check!)
                     (check! (filter :consolidated (get-adv)))))))

;; =============================================================================
;; Workflow Engine Initialization
;; =============================================================================

(defn init-workflow-engine!
  "Initialize FSM workflow registry and wire FSMWorkflowEngine as active engine.

   1. Calls registry/init! to scan EDN specs and register all built-in handlers
   2. Creates FSMWorkflowEngine and sets it as the active IWorkflowEngine

   Must run AFTER embedding/memory services (handlers may need them at runtime).
   Non-fatal: if initialization fails, NoopWorkflowEngine remains as fallback."
  []
  (result/rescue-log "init-workflow-engine!" nil
                 (require 'hive-mcp.workflows.registry)
                 (require 'hive-mcp.workflows.fsm-engine)
                 (require 'hive-mcp.protocols.workflow)
                 (let [registry-init! (resolve 'hive-mcp.workflows.registry/init!)
                       create-engine  (resolve 'hive-mcp.workflows.fsm-engine/create-engine)
                       set-engine!    (resolve 'hive-mcp.protocols.workflow/set-workflow-engine!)]
                   (registry-init!)
                   (set-engine! (create-engine))
                   (log/info "FSM workflow engine initialized and wired as active IWorkflowEngine"))))

;; =============================================================================
;; nREPL-mode initialization (for dev/bb-mcp without full -main)
;; =============================================================================

(defn populate-server-context!
  "Populate server-context-atom with the ONE advertised tool table.

   The table is routes' own (`routes/refresh-tools!`: the advertised defs,
   middleware-wrapped by make-tool, so bb-mcp gets ---MEMORY---/---HIVEMIND---
   blocks), never a variant built here. refresh-tools! also registers the
   context as a live surface, so every later refresh reaches it.

   A context already in the atom (the stdio server's, wired by :hive/mcp-stdio)
   KEEPS its identity and gets the table in its own :tools atom, so bb-mcp,
   the stdio server and the auto-heal listener all read one context. Without
   one, an empty context is created for the refresh to fill. Returns the
   refresh report."
  []
  (require 'hive-mcp.server.core)
  (let [ctx-atom (server-context-atom)]
    (when-not (:tools @ctx-atom)
      (reset! ctx-atom {:tools (atom {})}))
    (let [out (routes/refresh-tools! ctx-atom)]
      (log/info "server-context populated:" (:count out) "tools (middleware-wrapped)")
      out)))

(defn nrepl-init!
  "Initialize essential services for nREPL-mode operation (dev REPL, bb-mcp).
   Runs the subset of -main phases needed for tools to work via nREPL:
   - Phase 4: Embedding provider + memory store
   - Phase 4.5: Extension/addon loading
   - Server context: Populate server-context-atom for bb-mcp dynamic tools

   Safe to call multiple times (idempotent via defonce atoms).
   Called automatically by dev/user.clj on startup."
  []
  (log/info "nrepl-init! starting...")
  ;; Phase 4: Embedding + memory
  (init-embedding-provider!)
  (wire-memory-store!)

  ;; Phase 4.5: Extensions
  (load-extensions!)

  (populate-server-context!)
  (log/info "nrepl-init! complete"))

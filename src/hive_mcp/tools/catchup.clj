(ns hive-mcp.tools.catchup
  "Native Catchup workflow — thin facade delegating to sub-namespaces.

   Gathers session context from the registered IMemoryStore (currently
   Milvus or Qdrant) with project scoping. Designed for the /catchup skill
   to restore context at session start.

   Sub-namespace delegation (Sprint 2):
   - catchup.scope     — scope-filtered store queries, project context
   - catchup.format    — entry metadata transforms, response builders
   - catchup.git       — git status via Emacs
   - catchup.spawn     — spawn-time context injection (dual-mode)
   - catchup.permeation — auto-permeation of ling wraps

   Public API:
   - handle-native-catchup  — main catchup handler
   - handle-native-wrap     — wrap/crystallize handler
   - spawn-context          — re-export from catchup.spawn"
  (:require [hive-mcp.agent.context :as ctx]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.project.scope :as project-scope]
            [hive-mcp.tools.catchup.scope :as catchup-scope]
            [hive-mcp.tools.catchup.format :as fmt]
            [hive-mcp.tools.catchup.git :as catchup-git]
            [hive-mcp.tools.catchup.spawn :as catchup-spawn]
            [hive-mcp.tools.catchup.scope-filter :as sf]
            [hive-mcp.channel.memory-piggyback :as memory-piggyback]
            [hive-mcp.channel.piggyback :as piggyback]
            [hive-mcp.channel.context-store :as context-store]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.concurrency.pool :as pool]
            [hive-mcp.project.tree :as project-tree]
            [hive-mcp.dns.result :refer [rescue ok ok? let-ok try-effect* ok->]]
            [hive-dsl.context.identity :as ctx-id]
            [hive-ttracking.core :as tt]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]
            [hive-mcp.tools.catchup.relevance :as relevance]
            [hive-mcp.tools.catchup.outcome :as outcome]
            [hive-mcp.tools.catchup.caller :as catchup-caller]
            [hive-mcp.spi.catchup-registry :as blocks]
            [hive-mcp.swarm.adapters.soft :as soft]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Re-exports (backward compatibility)
;; =============================================================================

(defn spawn-context
  "Generate a compact context payload for ling spawn injection.
   Delegates to catchup.spawn/spawn-context. See that ns for full docs."
  ([directory] (catchup-spawn/spawn-context directory))
  ([directory opts] (catchup-spawn/spawn-context directory opts)))

;; =============================================================================
;; Parallel Execution Helpers
;; =============================================================================

(defn- safe-deref
  [fut timeout-ms label]
  (try
    (let [result (deref fut timeout-ms ::timeout)]
      (if (= result ::timeout)
        (do
          (future-cancel fut)
          (log/warn "catchup: parallel query timed out after" timeout-ms "ms:" label)
          (outcome/failure :timeout label
                           (str "query timed out after " timeout-ms "ms")))
        (if (outcome/outcome? result)
          result
          (outcome/ok result))))
    (catch Exception e
      (log/warn "catchup: parallel deref failed for" label ":" (.getMessage e))
      (outcome/failure :error label (or (ex-message e) (str (class e)))))))

(def ^:private ^:const query-timeout-ms
  "Outer safe-deref timeout for the bundle/git/carto futures. The bundle
   future runs query-all-scoped (fork-join with its own 60s per-branch
   budget) AND hydrate-buckets (single batch-get for ~270 survivors).
   Measured 2026-04-17 on a 45-project hive hierarchy:
     query-all-scoped   : ~42s
     hydrate-buckets    : ~162s  (Milvus batch-get on ~270 ids)
     total bundle       : ~204s
   60s was the original value and caused silent empty bundles whenever the
   hierarchy was non-trivial — the outer safe-deref hit its timeout long
   before hydrate finished, returning the {} default and propagating 0
   counts to every catchup bucket. 300s gives hydrate realistic headroom
   until the Milvus batch-get path is itself optimized."
  300000)

(def ^:private empty-kanban-summary
  "The kanban block's value when no contributor registered one."
  {:counts {} :recent-todos []})

(defn- compose-blocks
  "Every registered catchup block's value for CTX, keyed by block id.
   A contributor that throws is logged and omitted; the others still land."
  [ctx]
  (let [{:keys [blocks failed]} (blocks/compose ctx)]
    (when (seq failed)
      (log/warn "catchup: contributed blocks failed" failed))
    blocks))

;; =============================================================================
;; Main Catchup Handler
;; =============================================================================

(defn handle-native-catchup
  "Native Clojure catchup implementation that queries the registered
   IMemoryStore directly. Returns structured catchup data with proper
   project scoping.

   If an enrichment addon is registered via :cu/a, it runs
   fire-and-forget. Results arrive via piggyback on subsequent calls."
  [args]
  ;; HCR directory resolution: explicit :directory > :_caller_cwd (bb-mcp) >
  ;; request-ctx :directory > server user.dir. Matches handle-native-wrap so
  ;; catchup auto-resolves scope from caller's bash pwd when :directory absent.
  (let [directory (ctx/resolve-caller-directory args)
        dir-source (ctx/caller-directory-source args)]
    (log/info "native-catchup: querying memory store with project scope"
              {:directory directory :source dir-source})
    ;; Guard: early return if no store registered
    (if-not (mem-proto/store-set?)
      (fmt/store-not-configured-error)
      (try
        ;; Project-id resolution priority:
        ;;   1. request-ctx project-id (pre-resolved by wrap-handler-context)
        ;;   2. :project-id from .hive-project.edn in the exact dir
        ;;   3. Walk up the path finding the nearest .hive-project.edn
        ;;      (covers calls from deep subdirs of a hive project — without
        ;;      this, project-scope/get-current-project-id returns the last path
        ;;      segment, producing a bogus project scope like "catchup".)
        ;;   4. Legacy fallback: last-path-segment / "global"
        (let [ctx-pid          (ctx/current-project-id)
              direct-cfg-pid   (when directory
                                 (rescue nil (:project-id (project-scope/read-direct-project-config directory))))
              walked-pid       (when (and directory (not direct-cfg-pid))
                                 (rescue nil (project-scope/infer-scope-from-path directory)))
              project-id       (or ctx-pid
                                   direct-cfg-pid
                                   (when (and walked-pid (not= walked-pid "global")) walked-pid)
                                   (project-scope/get-current-project-id directory))
              project-name (catchup-scope/get-current-project-name directory)
              scopes (fmt/build-scopes project-name project-id)

              ;; ── Tree scan: ensure project hierarchy is populated before queries ──
              ;; Without this, descendant-scopes returns [] and sessions stored
              ;; under child projects (e.g. hive-mcp under hive) are invisible.
              _ (rescue nil (project-tree/maybe-scan-project-tree! (or directory ".")))

              ;; ── Wave 1: ONE memory bundle + git + extension status in parallel ──
              ;; The bundle replaces 7 per-type Milvus RPCs with 2 queries
              ;; (hierarchy + global-pierce), grouped by :type in memory. Avoids
              ;; Milvus type-filter scalar-scan storms that blew the budget.
              ;; The :catchup/status-providers extension lets registered
              ;; addons attach status fields to the response without the
              ;; core knowing about them — DIP.
              bundle-profile (when-let [profile-fn (ext/get-extension :catchup/bundle-profile)]
                               (catchup-caller/resolve-for-caller profile-fn (:_caller_id args) project-id))
              f-bundle (pool/with-io ((tt/timed-query "catchup/bundle-total"
                                                      #(if (seq (:caps bundle-profile))
                                                         (catchup-scope/query-catchup-bundle project-id bundle-profile)
                                                         (catchup-scope/query-catchup-bundle project-id)))))
              f-git    (pool/with-io ((tt/timed-query "catchup/git-total"
                                                      #(catchup-git/gather-git-info directory))))
              status-providers (or (ext/get-extension :catchup/status-providers) {})
              f-status (pool/with-io ((tt/timed-query "catchup/status-providers-total"
                                                      #(reduce-kv (fn [acc k provider-fn]
                                                                    (assoc acc k
                                                                           (rescue nil (provider-fn project-id))))
                                                                  {} status-providers))))
              ;; Contributed blocks (hive-mcp.spi.catchup-registry): core's own
              ;; domains and addons register {:block/id :block/fn :block/order};
              ;; catchup composes them without naming a contributor.
              f-blocks (pool/with-io ((tt/timed-query "catchup/blocks-total"
                                                      #(compose-blocks {:project-id project-id
                                                                        :directory  directory
                                                                        :caller-id  (:_caller_id args)}))))

              bundle        (safe-deref f-bundle query-timeout-ms "bundle")
              git-info      (outcome/value-or (safe-deref f-git query-timeout-ms "git-info") {})
              addon-status  (outcome/value-or (safe-deref f-status query-timeout-ms "addon-status") {})
              carto-status  (:carto-status addon-status)
              contributed   (outcome/value-or (safe-deref f-blocks query-timeout-ms "blocks") {})
              kanban-summary (or (:kanban contributed) empty-kanban-summary)

              axioms               (:axioms (outcome/value-or bundle {}) [])
              axiom-candidates     (:axiom-candidates (outcome/value-or bundle {}) [])
              principles           (:principles (outcome/value-or bundle {}) [])
              priority-principles  (:priority-principles (outcome/value-or bundle {}) [])
              priority-conventions (:priority-conventions (outcome/value-or bundle {}) [])
              sessions             (:sessions (outcome/value-or bundle {}) [])
              recent-wraps-raw     (:recent-wraps (outcome/value-or bundle {}) [])
              decisions            (:decisions (outcome/value-or bundle {}) [])
              snippets             (:snippets (outcome/value-or bundle {}) [])
              expiring             (:expiring (outcome/value-or bundle {}) [])
              conventions          (:conventions (outcome/value-or bundle {}) [])

              ;; Convert to metadata (pure, fast)
              axioms-meta (mapv fmt/entry->axiom-meta axioms)
              ;; Review queue. Deliberately absent from piggyback-raw below —
              ;; a nomination is not law and must never drain as if it were.
              axiom-candidates-meta (mapv fmt/entry->review-meta axiom-candidates)
              principles-meta (mapv #(fmt/entry->catchup-meta % 80) principles)
              priority-principles-meta (mapv #(fmt/entry->catchup-meta % 80) priority-principles)
              priority-meta (mapv fmt/entry->priority-meta priority-conventions)
              sessions-meta (mapv #(fmt/entry->catchup-meta % 80) sessions)
              recent-wraps (mapv fmt/entry->wrap-preview recent-wraps-raw)
              decisions-base (mapv #(fmt/entry->catchup-meta % 80) decisions)
              conventions-base (mapv #(fmt/entry->catchup-meta % 80) conventions)
              snippets-meta (mapv #(fmt/entry->catchup-meta % 60) snippets)
              expiring-meta (mapv #(fmt/entry->catchup-meta % 80) expiring)

              ;; Addon extension: fire-and-forget (async, returns nil immediately).
              _ (when (outcome/available? bundle) (when-let [enrich-fn (ext/get-extension :cu/a)] (enrich-fn {:directory directory
                              :project-id project-id
                              :caller-id (:_caller_id args)
                              :decisions decisions-base
                              :decisions-raw decisions
                              :conventions conventions-base
                              :conventions-raw conventions
                              :sessions sessions-meta
                              :sessions-raw sessions
                              :axioms axioms
                              :principles principles
                              :priority-principles priority-principles
                              :priority-conventions priority-conventions})))

              ;; Memory piggyback: enqueue axioms + priority conventions for
              ;; incremental delivery via ---MEMORY--- blocks on subsequent calls.
              ;; Axioms first (highest priority), then priority conventions.
              ;;
              ;; SESSION-SCOPED: memory piggyback uses raw caller-id (no project
              ;; dimension) for buffer key alignment with routes.clj drain wrappers.
              ;; Hivemind cursor still uses project-scoped piggyback-agent-id.
              raw-caller-id (or (:_caller_id args) "coordinator")
              caller (ctx-id/parse-caller-id raw-caller-id)
              scope (ctx-id/parse-project-scope project-id)
              piggyback-agent-id (ctx-id/make-piggyback-agent-id caller scope)

              ;; Cursor hygiene: adopt previous coordinator's cursor position
              ;; so we don't re-read hivemind messages from timestamp 0 after
              ;; a bb-mcp restart. Also evict stale cursors (> 30 min) and
              ;; adopt orphaned memory-piggyback buffers from dead instances.
              _ (rescue nil
                        (do
                          (piggyback/adopt-cursor! piggyback-agent-id project-id)
                          (piggyback/evict-stale-cursors! 1800000) ;; 30 min
                          (memory-piggyback/adopt-buffer! raw-caller-id)))

              ;; Scope-filter piggyback: keep entries relevant to this agent's
              ;; project hierarchy. Axioms used to ALWAYS pierce scope which
              ;; flooded sessions with off-topic axioms (windows-ntlm,
              ;; bufferbloat, JMM, typography). Now axioms must also pass a
              ;; tag-overlap relevance score against the project's vocabulary
              ;; — `catchup-priority` and `scope:project:<current>` still
              ;; pierce. See `hive-mcp.tools.catchup.relevance`.
              ;; Entries without scope tags pass through (global by convention).
              ;; Optional persona lens (e.g. hive-agent addon, via
              ;; :catchup/persona-lens) — mirror of :catchup/status-providers.
              ;; Resolves an OPAQUE per-caller lens (plain EDN) for the current
              ;; agent, keyed by (caller-id, project-id). rescue-wrapped: a
              ;; throwing or absent provider yields nil => identity compose
              ;; downstream (no persona boost). DIP: core never names the
              ;; persona NOR the lens shape — hive-knowledge.catchup.lens
              ;; coerces the EDN at the :catchup/lens seam below.
              persona-lens-fn (ext/get-extension :catchup/persona-lens)
              persona-lens    (when persona-lens-fn
                                (rescue nil (catchup-caller/resolve-for-caller persona-lens-fn raw-caller-id project-id)))
              relevance-ctx
              (cond-> (relevance/build-context
                       {:project-id project-id
                        :co-loaded-entries (concat priority-conventions
                                                   decisions
                                                   sessions)})
                persona-lens (assoc :persona-lens persona-lens))
              ;; Optional lens provider (hive-knowledge addon, via :catchup/lens).
              ;; Re-ranks/refines the relevance-filtered axioms against active
              ;; work. Absent addon => nil => relevant-axioms is byte-for-byte
              ;; today's filter output (cond-> guard false). DIP: core never
              ;; names the provider — see hive-knowledge.catchup.lens-addon.
              lens-fn (ext/get-extension :catchup/lens)
              relevant-axioms
              (cond-> (relevance/filter-by-relevance (vec axioms) relevance-ctx)
                lens-fn (lens-fn relevance-ctx))
              piggyback-raw (into (into (vec relevant-axioms) priority-principles) priority-conventions)
              piggyback-entries
              (let [in-project? (and project-id (not= project-id "global"))]
                (if-not in-project?
                  piggyback-raw
                  (let [scope-tags (sf/compute-full-scope-tags project-id)]
                    (filterv (fn [entry]
                               (let [tags (set (or (:tags entry) []))
                                     entry-type (str (or (:type entry) ""))]
                                 (or
                                  ;; Axioms already filtered above by relevance —
                                  ;; survivors continue to pierce the scope filter.
                                  (= entry-type "axiom")
                                  ;; catchup-priority entries pierce scope
                                  (contains? tags "catchup-priority")
                                  ;; No scope tag = global, passes through
                                  (not-any? #(.startsWith ^String % "scope:project:") tags)
                                  ;; Scope-matching entries pass through
                                  (some tags scope-tags))))
                             piggyback-raw))))

              ;; Dual-write: Cache entry categories in context-store for pass-by-ref mode.
              ;; Uses context-put-batch! to write all categories in parallel via futures.
              ;; Each category gets its own ctx-id with 'catchup' + category tags.
              ;; TTL: 1 hour.
              ;; Non-fatal: context-store failure doesn't break catchup.
              catchup-ttl 3600000
              scope-tag  (or project-id "global")
              context-refs
              (rescue nil
                      (let [refs (context-store/context-put-batch!
                                  {:axioms                {:data axioms
                                                           :tags #{"catchup" "axioms" scope-tag}
                                                           :ttl-ms catchup-ttl}
                                   :principles            {:data principles
                                                           :tags #{"catchup" "principles" scope-tag}
                                                           :ttl-ms catchup-ttl}
                                   :priority-principles   {:data priority-principles
                                                           :tags #{"catchup" "priority-principles" scope-tag}
                                                           :ttl-ms catchup-ttl}
                                   :priority-conventions  {:data priority-conventions
                                                           :tags #{"catchup" "priority-conventions" scope-tag}
                                                           :ttl-ms catchup-ttl}
                                   :sessions              {:data sessions
                                                           :tags #{"catchup" "sessions" scope-tag}
                                                           :ttl-ms catchup-ttl}
                                   :decisions             {:data decisions
                                                           :tags #{"catchup" "decisions" scope-tag}
                                                           :ttl-ms catchup-ttl}
                                   :conventions           {:data conventions
                                                           :tags #{"catchup" "conventions" scope-tag}
                                                           :ttl-ms catchup-ttl}
                                   :snippets              {:data snippets
                                                           :tags #{"catchup" "snippets" scope-tag}
                                                           :ttl-ms catchup-ttl}
                                   :recent-wraps          {:data recent-wraps-raw
                                                           :tags #{"catchup" "recent-wraps" scope-tag}
                                                           :ttl-ms catchup-ttl}})]
                        (when (seq refs)
                          (log/info "catchup: stored" (count refs) "categories in context-store"
                                    {:refs (keys refs)}))
                        refs))

              _ (when (seq piggyback-entries)
                  (memory-piggyback/enqueue! raw-caller-id
                                             (mapv fmt/cap-piggyback-entry piggyback-entries)
                                             context-refs))]

          (fmt/build-catchup-response
           {:scopes scopes, :axiom-candidates-meta axiom-candidates-meta, :project-name project-name, :principles-meta principles-meta, :priority-principles-meta priority-principles-meta, :recent-wraps recent-wraps, :context-refs context-refs, :axioms-meta axioms-meta, :memory-status (outcome/summary bundle), :priority-meta priority-meta, :carto-status carto-status, :expiring-meta expiring-meta, :git-info git-info, :sessions-meta sessions-meta, :snippets-meta snippets-meta, :decisions-meta decisions-base, :conventions-meta conventions-base, :project-id project-id, :kanban-summary kanban-summary}))
        (catch Exception e
          (fmt/catchup-error e))))))

;; =============================================================================
;; Wrap Handler
;; =============================================================================

(defn handle-native-wrap
  "Native multi-scope wrap: harvest a session, fan it out per scope, persist
   the entries. This is the kernel's ENTRY POINT; the pipeline itself is
   memory domain work in `hive-mcp.crystal.wrap-handler` and is resolved by
   symbol, so the kernel names the wrap without requiring the crystal
   namespaces.

   With the memory domain absent there is nothing to harvest INTO, so the
   answer is an error naming what is missing rather than an empty success.

   Returns MCP text payload with aggregate shape:
     {:session   <session-id>
      :directory <dir>
      :total     <count>
      :persisted <count>
      :failed    <count>
      :wraps     [{:pid :project-id :id :success? :error?} ...]}"
  [args]
  (if-let [h (soft/resolve-soft 'hive-mcp.crystal.wrap-handler/handle-native-wrap)]
    (h args)
    {:type "text"
     :text (json/write-str
            {:error "wrap unavailable: this build has no memory domain (hive-memory)"})
     :isError true}))

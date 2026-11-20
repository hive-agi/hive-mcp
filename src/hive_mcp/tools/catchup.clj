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
  (:require [hive-mcp.context.request :as ctx]
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
            [hive-spi.catchup.registry :as blocks]
            [hive-mcp.extensions.soft :as soft]
            [hive-mcp.tools.catchup.block-cache :as block-cache]
            [hive-mcp.tools.catchup.caps :as catchup-caps]
            [hive-mcp.tools.catchup.private-view :as pv]
            [malli.core :as m]
            [hive-mcp.tools.core :as core]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Re-exports (backward compatibility)
;; =============================================================================

(defn- memory-status-with-writes
  "catchup's memory-status: store reachability from `bundle`, folded with the
   memory add write-path health (hive-mcp.memory.write-latency, resolved
   softly so the kernel takes no static edge into the memory extraction).
   A failure to read write health never fails catchup.
   Kanban 20260928204307-01ca0962."
  [bundle]
  (let [summary (outcome/summary bundle)
        health-fn (soft/resolve-soft 'hive-mcp.memory.write-latency/write-health)]
    (outcome/with-write-health summary
                               (when health-fn
                                 (try (health-fn)
                                      (catch Exception e
                                        (log/debug "catchup: write health unavailable" (ex-message e))
                                        nil))))))

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

(defn- compose-blocks
  "Every registered catchup block's value for CTX as [id value] pairs in
   :block/order. A contributor that throws is logged and omitted; the others
   still land. Ordered by the registry, never by naming a contributor.
   A block that declares :block/cache is served from the shared block cache."
  [ctx]
  (let [{:keys [blocks failed]} (block-cache/compose ctx)
        order (into {} (map-indexed (fn [i b] [(:block/id b) i])) (blocks/registered-blocks))]
    (when (seq failed)
      (log/warn "catchup: contributed blocks failed" failed))
    (->> blocks
         (sort-by (fn [[id _]] [(get order id Long/MAX_VALUE) (str id)]))
         vec)))

;; =============================================================================
;; Main Catchup Handler — CPPB decomposition (QH-2)
;;
;;   Collect   — resolve caller context, fan out parallel queries
;;   Promote   — unwrap the bundle outcome into named entry buckets
;;   Pipeline  — pure transforms: entry -> metadata, piggyback filtering
;;   Boundary  — effects (extensions, cursors, context-store, piggyback) and
;;               the final response builder
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Collect: project-id resolution
;; -----------------------------------------------------------------------------

(defn- resolve-project-id
  "Resolve the project-id for DIRECTORY using the four-step priority:
     1. request-ctx project-id (pre-resolved by wrap-handler-context)
     2. :project-id from .hive-project.edn in the exact dir
     3. Walk up the path finding the nearest .hive-project.edn
     4. Legacy fallback: last-path-segment / \"global\""
  [directory]
  (let [ctx-pid        (ctx/current-project-id)
        direct-cfg-pid (when directory
                         (rescue nil (:project-id (project-scope/read-direct-project-config directory))))
        walked-pid     (when (and directory (not direct-cfg-pid))
                         (rescue nil (project-scope/infer-scope-from-path directory)))]
    (or ctx-pid
        direct-cfg-pid
        (when (and walked-pid (not= walked-pid "global")) walked-pid)
        (project-scope/get-current-project-id directory))))

;; -----------------------------------------------------------------------------
;; Collect: caller context (project, scopes, private-view, tree)
;; -----------------------------------------------------------------------------

(defn- collect-catchup-context
  "Gather the static per-call context that downstream stages read.

   Returns a map with :directory :project-id :project-name :scopes :private?
   :caller-id. Also runs the project-tree scan + private-view pre-query!
   seam (both effects required before any memory read). The caller is
   responsible for the initial directory resolution + info log; this fn is
   invoked only after the store-configured guard."
  [args]
  (let [directory    (ctx/resolve-caller-directory args)
        project-id   (resolve-project-id directory)
        project-name (catchup-scope/get-current-project-name directory)
        scopes       (fmt/build-scopes project-name project-id)
        caller-id    (:_caller_id args)
        _            (rescue nil (project-tree/maybe-scan-project-tree! (or directory ".")))
        _            (pv/run-pre-query! {:caller-id caller-id :directory directory :project-id project-id})
        private?     (pv/private-view? caller-id project-id)]
    {:directory    directory
     :project-id   project-id
     :project-name project-name
     :scopes       scopes
     :caller-id    caller-id
     :private?     private?}))

;; -----------------------------------------------------------------------------
;; Collect: launch parallel queries
;; -----------------------------------------------------------------------------

(defn- launch-bundle-future
  "Fire the bundle future. Reads the :catchup/bundle-profile + :catchup/caps
   extensions to decide whether to pass caps to query-catchup-bundle."
  [{:keys [project-id caller-id private?]}]
  (let [bundle-profile (when-let [profile-fn (ext/get-extension :catchup/bundle-profile)]
                         (catchup-caller/resolve-for-caller profile-fn caller-id project-id))]
    (pool/with-io
      (pv/in-view private?
                  (tt/timed-query "catchup/bundle-total"
                                  #(let [caps-provider (ext/get-extension :catchup/caps)
                                         caps (catchup-caps/resolve-caps caps-provider project-id bundle-profile)]
                                     (if (or caps-provider (seq (:caps bundle-profile)))
                                       (catchup-scope/query-catchup-bundle project-id {:caps caps})
                                       (catchup-scope/query-catchup-bundle project-id))))))))

(defn- launch-git-future
  [{:keys [directory]}]
  (pool/with-io
    ((tt/timed-query "catchup/git-total"
                     #(catchup-git/gather-git-info directory)))))

(defn- launch-status-future
  [{:keys [project-id]}]
  (let [status-providers (or (ext/get-extension :catchup/status-providers) {})]
    (pool/with-io
      ((tt/timed-query "catchup/status-providers-total"
                       #(reduce-kv (fn [acc k provider-fn]
                                     (assoc acc k
                                            (rescue nil (block-cache/run-status-provider k provider-fn project-id))))
                                   {} status-providers))))))

(defn- launch-blocks-future
  [{:keys [project-id directory caller-id]}]
  (pool/with-io
    ((tt/timed-query "catchup/blocks-total"
                     #(compose-blocks {:project-id project-id
                                       :directory  directory
                                       :caller-id  caller-id})))))

(defn- launch-catchup-queries
  "Fire the four parallel queries. Returns a map of futures:
     {:f-bundle :f-git :f-status :f-blocks}"
  [catchup-ctx]
  {:f-bundle (launch-bundle-future catchup-ctx)
   :f-git    (launch-git-future    catchup-ctx)
   :f-status (launch-status-future catchup-ctx)
   :f-blocks (launch-blocks-future catchup-ctx)})

;; -----------------------------------------------------------------------------
;; Collect: await the parallel queries
;; -----------------------------------------------------------------------------

(defn- await-catchup-queries
  "Deref every future under safe-deref with the shared timeout budget.
   Returns {:bundle outcome :git-info map :addon-status map :contributed vec}."
  [{:keys [f-bundle f-git f-status f-blocks]}]
  (let [bundle       (safe-deref f-bundle query-timeout-ms "bundle")
        git-info     (outcome/value-or (safe-deref f-git query-timeout-ms "git-info") {})
        addon-status (outcome/value-or (safe-deref f-status query-timeout-ms "addon-status") {})
        contributed  (outcome/value-or (safe-deref f-blocks query-timeout-ms "blocks") [])]
    {:bundle       bundle
     :git-info     git-info
     :addon-status addon-status
     :contributed  contributed}))

;; -----------------------------------------------------------------------------
;; Promote: bundle outcome -> named entry buckets
;; -----------------------------------------------------------------------------

(def ^:private bundle-buckets
  "The eleven buckets the catchup bundle is decomposed into. Order matches the
   historical let-bindings so the metadata map built downstream is identical."
  [:axioms :axiom-candidates :principles :priority-principles
   :priority-conventions :sessions :recent-wraps :decisions :snippets
   :expiring :conventions])

(defn- promote-bundle
  "Unwrap the bundle OUTCOME into a map of {bucket -> vector-of-entries}.
   Each bucket defaults to [] so downstream mapv calls never see nil."
  [bundle]
  (let [data (outcome/value-or bundle {})]
    (reduce (fn [acc k]
              (assoc acc k (get data k [])))
            {}
            bundle-buckets)))

;; -----------------------------------------------------------------------------
;; Pipeline: entries -> metadata (pure)
;; -----------------------------------------------------------------------------

(defn- entries->metadata
  "Apply the per-bucket format/entry->... transform to each bucket.
   Pure: no IO, no clocks, no extensions. Returns a map of meta keys ready
   for fmt/build-catchup-response."
  [buckets]
  (let [{:keys [axioms axiom-candidates principles priority-principles
                priority-conventions sessions recent-wraps decisions
                snippets expiring conventions]} buckets]
    {:axioms-meta               (mapv fmt/entry->axiom-meta axioms)
     :axiom-candidates-meta     (mapv fmt/entry->review-meta axiom-candidates)
     :principles-meta           (mapv #(fmt/entry->catchup-meta % 80) principles)
     :priority-principles-meta  (mapv #(fmt/entry->catchup-meta % 80) priority-principles)
     :priority-meta             (mapv fmt/entry->priority-meta priority-conventions)
     :sessions-meta             (mapv #(fmt/entry->catchup-meta % 80) sessions)
     :recent-wraps              (mapv fmt/entry->wrap-preview recent-wraps)
     :decisions-base            (mapv #(fmt/entry->catchup-meta % 80) decisions)
     :conventions-base          (mapv #(fmt/entry->catchup-meta % 80) conventions)
     :snippets-meta             (mapv #(fmt/entry->catchup-meta % 60) snippets)
     :expiring-meta             (mapv #(fmt/entry->catchup-meta % 80) expiring)}))

;; -----------------------------------------------------------------------------
;; Pipeline: piggyback relevance filtering (pure, except for ext lookup)
;; -----------------------------------------------------------------------------

(defn- piggyback-scope-pass?
  "Predicate: does ENTRY survive the project-scope piggyback filter?
   Axioms always pierce (already relevance-filtered). catchup-priority tags
   pierce. Untagged entries pass. Tagged entries pass when any tag is in
   SCOPE-TAGS."
  [entry scope-tags]
  (let [tags       (set (or (:tags entry) []))
        entry-type (str (or (:type entry) ""))]
    (or (= entry-type "axiom")
        (contains? tags "catchup-priority")
        (not-any? #(.startsWith ^String % "scope:project:") tags)
        (some tags scope-tags))))

(defn- filter-piggyback-by-scope
  "Filter ENTRIES down to the subset that should be piggybacked for PROJECT-ID.
   Outside a real project scope every entry passes through."
  [entries project-id]
  (let [in-project? (and project-id (not= project-id "global"))]
    (if-not in-project?
      entries
      (let [scope-tags (sf/compute-full-scope-tags project-id)]
        (filterv #(piggyback-scope-pass? % scope-tags) entries)))))

(defn- build-relevance-context
  "Compute the relevance context map, optionally augmented with the
   :persona-lens resolved via the :catchup/persona-lens extension."
  [{:keys [project-id raw-caller-id priority-conventions decisions sessions]}]
  (let [persona-lens-fn (ext/get-extension :catchup/persona-lens)
        persona-lens    (when persona-lens-fn
                          (rescue nil (catchup-caller/resolve-for-caller persona-lens-fn raw-caller-id project-id)))]
    (cond-> (relevance/build-context
             {:project-id        project-id
              :co-loaded-entries (concat priority-conventions decisions sessions)})
      persona-lens (assoc :persona-lens persona-lens))))

(defn- compute-piggyback-entries
  "Relevance-filter axioms, run the optional :catchup/lens provider, concat
   with priority-principles + priority-conventions, then apply the
   project-scope piggyback filter. Pure apart from extension lookups."
  [{:keys [project-id raw-caller-id axioms priority-principles
           priority-conventions decisions sessions]}]
  (let [relevance-ctx  (build-relevance-context
                        {:project-id           project-id
                         :raw-caller-id        raw-caller-id
                         :priority-conventions priority-conventions
                         :decisions            decisions
                         :sessions             sessions})
        lens-fn        (ext/get-extension :catchup/lens)
        relevant-axioms (cond-> (relevance/filter-by-relevance (vec axioms) relevance-ctx)
                          lens-fn (lens-fn relevance-ctx))
        piggyback-raw   (into (into (vec relevant-axioms) priority-principles) priority-conventions)]
    (filter-piggyback-by-scope piggyback-raw project-id)))

;; -----------------------------------------------------------------------------
;; Boundary: enrichment extension
;; -----------------------------------------------------------------------------

(defn- run-enrichment!
  "Fire the optional :cu/a enrichment addon. No-op when the bundle is
   unavailable or the extension is absent."
  [{:keys [bundle directory project-id caller-id buckets metadata]}]
  (when (outcome/available? bundle)
    (when-let [enrich-fn (ext/get-extension :cu/a)]
      (enrich-fn {:directory           directory
                  :project-id          project-id
                  :caller-id           caller-id
                  :decisions           (:decisions-base metadata)
                  :decisions-raw       (:decisions buckets)
                  :conventions         (:conventions-base metadata)
                  :conventions-raw     (:conventions buckets)
                  :sessions            (:sessions-meta metadata)
                  :sessions-raw        (:sessions buckets)
                  :axioms              (:axioms buckets)
                  :principles          (:principles buckets)
                  :priority-principles (:priority-principles buckets)
                  :priority-conventions (:priority-conventions buckets)}))))

;; -----------------------------------------------------------------------------
;; Boundary: cursor hygiene
;; -----------------------------------------------------------------------------

(defn- run-cursor-hygiene!
  "Adopt cursor + evict stale cursors + adopt orphaned piggyback buffers.
   All wrapped in rescue so a failure never fails catchup."
  [{:keys [piggyback-agent-id project-id raw-caller-id]}]
  (rescue nil
          (do
            (piggyback/adopt-cursor! piggyback-agent-id project-id)
            (piggyback/evict-stale-cursors! 1800000) ;; 30 min
            (memory-piggyback/adopt-buffer! raw-caller-id))))

;; -----------------------------------------------------------------------------
;; Boundary: dual-write entry buckets into context-store
;; -----------------------------------------------------------------------------

(defn- context-store-payload
  "Build the {category -> {:data :tags :ttl-ms}} map for context-put-batch!.
   Pure function of the raw buckets + scope-tag."
  [buckets scope-tag catchup-ttl]
  (let [{:keys [axioms principles priority-principles priority-conventions
                sessions decisions conventions snippets recent-wraps]} buckets]
    {:axioms               {:data axioms               :tags #{"catchup" "axioms" scope-tag}               :ttl-ms catchup-ttl}
     :principles           {:data principles           :tags #{"catchup" "principles" scope-tag}           :ttl-ms catchup-ttl}
     :priority-principles  {:data priority-principles  :tags #{"catchup" "priority-principles" scope-tag}  :ttl-ms catchup-ttl}
     :priority-conventions {:data priority-conventions :tags #{"catchup" "priority-conventions" scope-tag} :ttl-ms catchup-ttl}
     :sessions             {:data sessions             :tags #{"catchup" "sessions" scope-tag}             :ttl-ms catchup-ttl}
     :decisions            {:data decisions            :tags #{"catchup" "decisions" scope-tag}            :ttl-ms catchup-ttl}
     :conventions          {:data conventions          :tags #{"catchup" "conventions" scope-tag}          :ttl-ms catchup-ttl}
     :snippets             {:data snippets             :tags #{"catchup" "snippets" scope-tag}             :ttl-ms catchup-ttl}
     :recent-wraps         {:data recent-wraps         :tags #{"catchup" "recent-wraps" scope-tag}         :ttl-ms catchup-ttl}}))

(defn- persist-context-refs!
  "Dual-write the bundle's raw buckets into the context-store under a single
   shared refs key so callers sharing a cached bundle reuse refs. Returns the
   {category -> ctx-id} map or nil on failure (non-fatal)."
  [{:keys [scope-tag private? bundle buckets catchup-ttl]}]
  (rescue nil
          (block-cache/shared-context-refs
           scope-tag (when-not private? (outcome/value-or bundle nil))
           (fn []
             (let [refs (context-store/context-put-batch!
                         (context-store-payload buckets scope-tag catchup-ttl))]
               (when (seq refs)
                 (log/info "catchup: stored" (count refs) "categories in context-store"
                           {:refs (keys refs)}))
               refs)))))

;; -----------------------------------------------------------------------------
;; Boundary: piggyback enqueue
;; -----------------------------------------------------------------------------

(defn- enqueue-piggyback!
  [{:keys [raw-caller-id piggyback-entries context-refs]}]
  (when (seq piggyback-entries)
    (memory-piggyback/enqueue! raw-caller-id
                               (mapv fmt/cap-piggyback-entry piggyback-entries)
                               context-refs)))

;; -----------------------------------------------------------------------------
;; Top-level orchestration: run-catchup-pipeline
;; -----------------------------------------------------------------------------

(defn- run-catchup-pipeline
  "Sequence the catchup pipeline after the store guard. Returns the final
   fmt/build-catchup-response value. Throws propagate to the caller's
   try/catch — same scope as the historical inline implementation."
  [args]
  (let [catchup-ctx (collect-catchup-context args)
        futures     (launch-catchup-queries catchup-ctx)
        {:keys [bundle git-info addon-status contributed]} (await-catchup-queries futures)
        carto-status (:carto-status addon-status)
        buckets     (promote-bundle bundle)
        metadata    (entries->metadata buckets)

        {:keys [directory project-id project-name scopes caller-id private?]} catchup-ctx
        raw-caller-id (or caller-id "coordinator")
        caller        (ctx-id/parse-caller-id raw-caller-id)
        scope         (ctx-id/parse-project-scope project-id)
        piggyback-agent-id (ctx-id/make-piggyback-agent-id caller scope)

        _ (run-enrichment! {:bundle bundle
                            :directory directory
                            :project-id project-id
                            :caller-id caller-id
                            :buckets buckets
                            :metadata metadata})

        _ (run-cursor-hygiene! {:piggyback-agent-id piggyback-agent-id
                                :project-id project-id
                                :raw-caller-id raw-caller-id})

        piggyback-entries (compute-piggyback-entries
                           {:project-id           project-id
                            :raw-caller-id        raw-caller-id
                            :axioms               (:axioms buckets)
                            :priority-principles  (:priority-principles buckets)
                            :priority-conventions (:priority-conventions buckets)
                            :decisions            (:decisions buckets)
                            :sessions             (:sessions buckets)})

        catchup-ttl 3600000
        scope-tag   (or project-id "global")
        context-refs (persist-context-refs!
                      {:scope-tag   scope-tag
                       :private?    private?
                       :bundle      bundle
                       :buckets     buckets
                       :catchup-ttl catchup-ttl})

        _ (enqueue-piggyback! {:raw-caller-id     raw-caller-id
                               :piggyback-entries piggyback-entries
                               :context-refs      context-refs})]

    (fmt/build-catchup-response
     {:scopes                    scopes
      :axiom-candidates-meta     (:axiom-candidates-meta metadata)
      :project-name              project-name
      :principles-meta           (:principles-meta metadata)
      :priority-principles-meta  (:priority-principles-meta metadata)
      :recent-wraps              (:recent-wraps metadata)
      :context-refs              context-refs
      :axioms-meta               (:axioms-meta metadata)
      :memory-status             (memory-status-with-writes bundle)
      :priority-meta             (:priority-meta metadata)
      :carto-status              carto-status
      :expiring-meta             (:expiring-meta metadata)
      :git-info                  git-info
      :sessions-meta             (:sessions-meta metadata)
      :snippets-meta             (:snippets-meta metadata)
      :decisions-meta            (:decisions-base metadata)
      :conventions-meta          (:conventions-base metadata)
      :project-id                project-id
      :contributed-blocks        contributed})))

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
  (let [directory  (ctx/resolve-caller-directory args)
        dir-source (ctx/caller-directory-source args)]
    (log/info "native-catchup: querying memory store with project scope"
              {:directory directory :source dir-source})
    ;; Guard: early return if no store registered
    (if-not (mem-proto/store-set?)
      (fmt/store-not-configured-error)
      (try
        (run-catchup-pipeline args)
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

;; =============================================================================
;; Contracts
;; =============================================================================

(def CatchupResponse
  "A catchup answer: the vector of per-block MCP text responses on success, or
   one MCP error response (store not configured, failure)."
  [:or core/McpResponse [:vector core/McpResponse]])

(def SpawnContextOpts
  "spawn-context options; :mode picks :full (default), :ref or :hints."
  [:maybe [:map
           [:mode {:optional true} [:enum :full :ref :hints]]
           [:task {:optional true} [:maybe :string]]
           [:task-id {:optional true} [:maybe :string]]]])

(m/=> spawn-context [:function
                     [:=> [:cat [:maybe :string]] [:maybe :string]]
                     [:=> [:cat [:maybe :string] SpawnContextOpts] [:maybe :string]]])

(m/=> handle-native-catchup [:=> [:cat [:maybe :map]] CatchupResponse])

(m/=> handle-native-wrap [:=> [:cat [:maybe :map]] CatchupResponse])

(ns hive-mcp.tools.memory.crud.write
  "Write operations for memory: add entry with KG edge creation."
  (:require [hive-mcp.tools.memory.core :refer [with-store]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.format :as fmt]
            [hive-mcp.tools.memory.duration :as dur]
            [hive-mcp.tools.memory.classify :as classify]
            [hive-mcp.tools.memory.gaps :as gaps]
            [hive-mcp.tools.core :as core :refer [mcp-json mcp-error coerce-vec!]]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.plan.plans :as plans]
            [hive-mcp.plan.gate :as plan-gate]
            [clojure.edn :as edn]
            [hive-mcp.knowledge-graph.connection :as kg-conn]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.knowledge-graph.schema :as kg-schema]
            [hive-mcp.concurrency.pool :as pool]
            [hive-weave.pool :as wpool]
            [hive-mcp.context.request :as ctx]
            [hive-mcp.crystal.recall :as recall]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive-mcp.vectordb.resilience :refer [with-resilience call-with-resilience]]
            [hive-mcp.memory.type-registry :as type-registry]
            [hive-weave.core :as weave]
            [hive-mcp.memory.write-events :as write-events]
            [hive-mcp.tools.memory.crud.deferred :as deferred]
            [hive-mcp.embeddings.shared-gate :as embed-gate]
            [hive-mcp.tools.memory.crud.edge-outcome :as edge-outcome]
            [hive-mcp.extensions.registry :as ext]
            [malli.core :as m]
            [hive-mcp.memory.write-latency :as write-latency]))

(def ^:const ^:private memory-write-timeout-ms
  "Timeout budget for a single memory write (Chroma add + KG tx + fetch).
   On timeout the write may still land later; caller gets an error."
  30000)

(def ^:dynamic *write-timeout-ms*
  "Effective outer budget `handle-add` waits for `do-add!`. Defaults to
   `memory-write-timeout-ms`; a dynamic var so a test can shrink it without
   waiting 30 s on a stalled store stub."
  memory-write-timeout-ms)

(def ^:const ^:private store-stage-heal-budget-ms
  "Heal-wait budget for a store stage whose backend already wraps its own
   RPCs in a reconnect-and-retry (hive-milvus `resilient`: await + retry).
   `with-resilience` at the default 8 s on top of that double-wraps every
   RPC (about 16 s per stage) and eats the 30 s outer ceiling before the
   caller learns anything. The handler layer keeps a short second chance
   for a store that does not heal itself; cross-store back-edge probes
   (`find-target-store`, `update-target-incoming!`) keep the default budget.
   Kanban 20260529003400-5c19ae4f (b)."
  2000)

(defmacro ^:private with-stage-resilience
  "`with-resilience` at `store-stage-heal-budget-ms`. For store stages the
   backend already protects; see that constant."
  [& body]
  `(call-with-resilience (fn [] ~@body) store-stage-heal-budget-ms))

(defn new-progress
  "A fresh progress tracker for one `handle-add` call: an atom holding the
   stage the write is in and when it entered it. Starts at `:queued`, the
   stage a write is in until a pool thread picks it up.
   Kanban 20260529003400-5c19ae4f (a)."
  []
  (let [now (System/nanoTime)]
    (atom {:stage :queued :started-ns now :stage-since-ns now})))

(defn- mark-stage!
  "Record that the write entered `stage`. A nil `progress` is a no-op, so
   `do-add!` callers outside `handle-add` (tests, kanban) pay nothing."
  [progress stage]
  (when progress
    (swap! progress assoc :stage stage :stage-since-ns (System/nanoTime)))
  nil)

(defn- record-stage-ms!
  "Add `ms` to `stage`'s wall time under :timings on `progress` (a stage may
   run more than once, e.g. :kg-edges per relation). nil `progress` is a no-op.
   Kanban 20260928204307-01ca0962."
  [progress stage ms]
  (when progress
    (swap! progress update-in [:timings stage] (fnil + 0) (long ms)))
  nil)

(defn timeout-diagnostic
  "Pure: the diagnostic map for a write that outran `timeout-ms`.
   `snapshot` is the deref'd progress tracker, `now-ns` a `System/nanoTime`
   reading. Returns {:stage :elapsed-ms :stage-elapsed-ms :timeout-ms :cause}.

   :cause is :pool-queue-wait when no pool thread ever started the write
   (stage :queued), :stage-stalled when a stage was entered and did not
   return in time."
  [snapshot timeout-ms now-ns]
  (let [{:keys [stage started-ns stage-since-ns]} snapshot
        stage (or stage :queued)
        ->ms  (fn [from] (if from (quot (max 0 (- now-ns from)) 1000000) 0))]
    {:stage            stage
     :elapsed-ms       (->ms started-ns)
     :stage-elapsed-ms (->ms stage-since-ns)
     :timeout-ms       timeout-ms
     :cause            (if (= :queued stage) :pool-queue-wait :stage-stalled)}))

(defn timeout-message
  "Human-readable text for a `timeout-diagnostic` map. Keeps the historical
   `memory add timed out after <N>ms` prefix and appends the stage, how long
   the stage ran, the cause, and the EDN of the full diagnostic."
  [{:keys [stage stage-elapsed-ms timeout-ms cause] :as diag}]
  (str "memory add timed out after " timeout-ms "ms in stage " stage
       " (stage elapsed " stage-elapsed-ms "ms, cause " cause "). "
       "The write may still land later. " (pr-str diag)))

(def default-stage-budgets-ms
  "Per-stage wall-clock budget, in ms, for one memory add. A stage that
   outruns its budget fails at once with an error naming it, instead of
   draining the whole outer `*write-timeout-ms*`. The longest path
   (dedup-lookup, index, kg-edges, link-outgoing, fetch) sums to 28 s, under
   the 30 s outer ceiling, so the per-stage error fires first. :dedup-update
   replaces :index on the duplicate path. :kg-edges bounds the back-edge
   fan-out."
  {:dedup-lookup  4000
   :dedup-update  4000
   :index         12000
   :kg-edges      6000
   :link-outgoing 3000
   :fetch         3000})

(def ^:dynamic *stage-budgets-ms*
  "Effective per-stage budgets; `default-stage-budgets-ms` unless rebound.
   A stage absent from the map runs unbounded (only the outer ceiling)."
  default-stage-budgets-ms)

(defn stage-budget-message
  "Human-readable text for a stage that outran its budget. Names the stage,
   its budget and how long it had run, then the EDN of the diagnostic."
  [{:keys [stage budget-ms elapsed-ms] :as diag}]
  (str "memory add stage " stage " exceeded its " budget-ms "ms budget"
       " (stage elapsed " elapsed-ms "ms). The write may still land later. "
       (pr-str diag)))

(defn- run-stage!
  "Run `f` as write stage `stage`: record the stage on `progress`, then, when
   `*stage-budgets-ms*` has a budget for it, wait at most that long. A stage
   that outruns its budget is cancelled and throws ex-info
   {:type :stage-budget-exceeded :stage :budget-ms :elapsed-ms}. A throw from
   `f` itself is rethrown unwrapped. The stage's wall time, success or not, is
   added to `progress` :timings (`record-stage-ms!`) for the latency ring."
  [progress stage f]
  (mark-stage! progress stage)
  (let [t0 (System/nanoTime)]
    (try
      (if-let [budget-ms (get *stage-budgets-ms* stage)]
        (let [start (System/nanoTime)
              fut   (future (f))
              r     (try (deref fut budget-ms ::over-budget)
                         (catch java.util.concurrent.ExecutionException e
                           (throw (or (.getCause e) e))))]
          (if (= r ::over-budget)
            (let [diag {:stage stage :budget-ms budget-ms
                        :elapsed-ms (quot (- (System/nanoTime) start) 1000000)}]
              (future-cancel fut)
              (log/warn "memory add stage over budget:" (pr-str diag))
              (throw (ex-info (stage-budget-message diag)
                              (assoc diag :type :stage-budget-exceeded))))
            r))
        (f))
      (finally
        (record-stage-ms! progress stage (quot (- (System/nanoTime) t0) 1000000))))))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- find-target-store
  "Probe each registered IMemoryStore for `target-id`. Returns the
   first store whose `get-entry` returns a non-nil record, paired
   with the entry itself so callers don't pay a second read.

   Cross-slot routing: a `:default` (milvus) entry that depends-on a
   target living in `:kanban` (qdrant-local) needs its back-edge
   metadata write routed to the qdrant slot. Probing the registry
   resolves the target's actual slot regardless of which slot the
   source entry was written to.

   :default is probed first when present so the common case (axioms,
   principles, conventions all in :default) costs exactly one read.
   Returns `[slot store entry]` on hit, `nil` on miss."
  [target-id]
  (let [registry (mem-proto/registered-stores)
        ordered (concat (when-let [s (:default registry)] [[:default s]])
                        (filter (fn [[k _]] (not= k :default)) registry))]
    (some (fn [[slot store]]
            (when-let [entry (with-resilience (mem-proto/get-entry store target-id))]
              [slot store entry]))
          ordered)))

(defn- update-target-incoming!
  "Append edge-id to target entry's kg-incoming field, preserving the
   existing embedding.

   Cross-slot routing: probes `(registered-stores)` to locate the slot
   that actually owns `target-id`. Without this, a back-edge from a
   :default entry to a target living in :kanban (or vice-versa) would
   read+write the wrong slot — the read returns nil, the write is a
   silent no-op, and the kg-incoming bookkeeping drifts.

   Uses the IMemoryStoreMetadataWrite protocol (`update-metadata!`)
   when the resolved store satisfies it — the no-embed fast path. Falls
   back to the slow `update-entry!` for stores that don't implement
   metadata writes (re-embeds on every field change).

   Each store probe + the final write are wrapped in `with-resilience`
   so KG back-edge bookkeeping survives a transient transport drop."
  [target-id edge-id]
  (when-let [[_slot store target-entry] (find-target-store target-id)]
    (let [existing-incoming (or (:kg-incoming target-entry) [])
          updated-incoming  (conj existing-incoming edge-id)
          updates           {:kg-incoming updated-incoming}]
      (with-resilience
        (if (mem-proto/metadata-write-store? store)
          (mem-proto/update-metadata! store target-id updates)
          (mem-proto/update-entry!    store target-id updates))))))

(defn- create-kg-edges!
  "Create KG edges for the given relationships and update target entries.

   Two-phase to honour the idempotence law for compound writes
   (add(c, refs=[r1...rN]) ≡ add(c) ; for r in refs: edge(r)):

     Phase 1 — KG transact (synchronous, batched). All `kg-edges/add-edge!`
       calls coalesce into a single Datahike tx via `kg-conn/with-tx-batch`.
       Fast (in-process Datahike) so sequential is fine.

     Phase 2 — Milvus back-edge writes (fan-out via hive-weave). The
       :kg-incoming bookkeeping on each target is one Milvus read + one
       upsert; no embed cost (see IMemoryStoreMetadataWrite). Running
       these in parallel under `weave/fork-join` collapses wall-clock
       latency from sum(per-target) → max(per-target). The fan-out is
       bounded by the `:kg-edges` entry of `*stage-budgets-ms*`; a back-edge
       that outruns it is logged naming the stage and the target, and the
       edge ids are still returned (phase 1 already committed them)."
  [entry-id {:keys [kg_implements kg_supersedes kg_depends_on kg_refines]} project-id agent-id]
  (let [created-by   (ctx/attribution-created-by (ctx/current-attribution agent-id)
                                           (when agent-id (str "agent:" agent-id)))
        edge-records
        (kg-conn/with-tx-batch
          (let [add-edges (fn [targets relation]
                            (when (seq targets)
                              (mapv (fn [target-id]
                                      (let [edge-id (kg-edges/add-edge!
                                                      {:from       entry-id
                                                       :to         target-id
                                                       :relation   relation
                                                       :scope      project-id
                                                       :confidence 1.0
                                                       :created-by created-by})]
                                        {:target-id target-id :edge-id edge-id}))
                                    targets)))]
            (vec (concat
                   (add-edges kg_implements :implements)
                   (add-edges kg_supersedes :supersedes)
                   (add-edges kg_depends_on :depends-on)
                   (add-edges kg_refines    :refines)))))
        budget-ms (max 500 (- (get *stage-budgets-ms* :kg-edges 25000) 1000))
        tasks
        (into [] (map-indexed
                   (fn [i {:keys [target-id edge-id]}]
                     [(keyword (str "back-edge-" i))
                      #(do (update-target-incoming! target-id edge-id) ::done)
                      ::over-budget]))
              edge-records)]
    (when (seq tasks)
      (let [results (apply weave/fork-join {:budget-ms budget-ms} tasks)
            missed  (keep-indexed (fn [i {:keys [target-id]}]
                                    (when (= ::over-budget
                                             (get results (keyword (str "back-edge-" i))))
                                      target-id))
                                  edge-records)]
        (when (seq missed)
          (log/warn "memory add stage :kg-edges: back-edge bookkeeping did not finish within"
                    budget-ms "ms for targets" (vec missed)))))
    (mapv :edge-id edge-records)))

(defn- build-entry-tags
  "Build complete tags vector: base, agent, attribution, KG markers, and scope.
   The `agent:<agent-id>` tag is kept as before; `attribution` (see
   hive-mcp.context.request/attribution) adds the session-precise tags beside it."
  [tags-vec agent-id attribution kg-vecs project-id]
  (let [agent-tag (when agent-id (str "agent:" agent-id))
        tags-with-agent (into (if agent-tag (conj tags-vec agent-tag) tags-vec)
                              (ctx/attribution-tags attribution))
        {:keys [kg-implements-vec kg-supersedes-vec kg-depends-on-vec kg-refines-vec]} kg-vecs
        kg-tags (cond-> []
                  (seq kg-implements-vec) (conj "kg:has-implements")
                  (seq kg-supersedes-vec) (conj "kg:has-supersedes")
                  (seq kg-depends-on-vec) (conj "kg:has-depends-on")
                  (seq kg-refines-vec) (conj "kg:has-refines"))
        tags-with-kg (into tags-with-agent kg-tags)]
    (scope/inject-project-scope tags-with-kg project-id)))

(defn- validate-plan-gate!
  "Validate plan content before storage via FSM gate.

   Strategy: delegate the entire decision to `plan-gate/validate-for-storage`,
   which runs parse + schema + deps + cycles and returns a structured
   `{:valid? false :phase ... :hint ...}` for any failure mode (parse,
   schema, dependencies, cycles, empty-steps). Surfacing that result
   directly via `format-gate-error` gives lings actionable messages —
   e.g. an EDN plan with a list-shaped `:steps` or a missing :title now
   sees the FSM-emitted hint, not a generic 'doesn't look like a plan'
   reject.

   Removed: the prior `plan-content?` pre-check. It was redundant with
   the gate (which already covers EDN + markdown detection) and its
   failure path threw a generic message that hid the real parse error
   — the surface area of the EDN/markdown contract bug
   (kanban 20260429185655-1cfb9277)."
  [content]
  (let [gate-result (plan-gate/validate-for-storage content)]
    (when-not (:valid? gate-result)
      (throw (ex-info (plan-gate/format-gate-error gate-result)
                      {:type :plan-gate-rejected
                       :phase (:phase gate-result)
                       :errors (:errors gate-result)})))))

(defn- resolve-role-card-sym
  "Resolve a `hive-spi.role.card` var, or nil when the role SPI leaf is not
   on the classpath. Keeps this ns loadable without a hard compile-time dep."
  [sym]
  (try (requiring-resolve sym) (catch Throwable _ nil)))

(defonce ^:private role-card-validator (atom nil))

(defn set-role-card-validator!
  "Inject the RoleCard validator as {:valid? fn :explain fn}.

   nil restores resolution from the hive-spi role leaf. Returns the value set."
  [v]
  (reset! role-card-validator v))

(def role-card-validator-extension-key
  "Extension-registry key under which a provider (e.g. the addon that owns the
   RoleCard contract) registers its validator map {:valid? fn :explain fn}.
   The provider registers INTO hive-mcp, so no private namespace name appears
   in this public source (dependency inversion over the registry seam)."
  :role/card-validator)

(defn select-role-card-validator
  "Pure: pick the RoleCard validator from candidate sources in precedence order
   :injected > :extension > :spi. Returns {:source kw :validator v-or-nil};
   :source :none means no validator exists and the gate cannot run."
  [{:keys [injected extension spi]}]
  (cond
    injected  {:source :injected  :validator injected}
    extension {:source :extension :validator extension}
    spi       {:source :spi       :validator spi}
    :else     {:source :none      :validator nil}))

(defn- spi-role-card-validator
  "Validator resolved from the hive-spi role leaf, or nil when it is absent."
  []
  (when-let [valid? (resolve-role-card-sym (quote hive-spi.role.card/valid?))]
    {:valid? valid?
     :explain (or (resolve-role-card-sym (quote hive-spi.role.card/explain))
                  (constantly nil))}))

(defn current-role-card-validator
  "The injected validator, else one resolved from hive-spi.role.card, else nil.
   Between the two, a validator registered in the extension registry under
   `role-card-validator-extension-key` is consulted."
  []
  (:validator
   (select-role-card-validator
    {:injected  @role-card-validator
     :extension (ext/get-extension role-card-validator-extension-key)
     :spi       (spi-role-card-validator)})))

(defn- validate-role-gate!
  "Validate :role content against the RoleCard schema before storage.

   Resolves the validator via `current-role-card-validator` — injected first,
   else one registered under `role-card-validator-extension-key`, else the
   hive-spi role leaf, else nil (gate skipped). FAIL-LOUD otherwise:
   throws `:role-gate-rejected` when the content is unreadable EDN, not a map,
   or a non-conformant RoleCard. A validator whose :valid? is not invokable
   (e.g. a bare fn registered instead of a {:valid? :explain} map) is rejected
   loudly rather than failing with an opaque NPE; a missing :explain explains
   nothing.

   A skipped gate is no longer silent: it logs a warning naming the
   extension key a provider must register under."
  [content]
  (if-let [{:keys [valid? explain]} (current-role-card-validator)]
    (let [explain (if (ifn? explain) explain (constantly nil))
          _ (when-not (ifn? valid?)
              (throw (ex-info (str "RoleCard validator is malformed: expected a map "
                                   "{:valid? fn :explain fn}")
                              {:type :role-gate-rejected
                               :explanation :role-gate/malformed-validator})))
          card (try
                 (edn/read-string content)
                 (catch Exception e
                   (throw (ex-info (str "RoleCard content is not readable EDN: "
                                        (.getMessage e))
                                   {:type :role-gate-rejected}))))]
      (when-not (and (map? card) (valid? card))
        (throw (ex-info (str "RoleCard validation failed. Required: :role/id "
                             "(keyword) and :role/name (string). Explanation: "
                             (pr-str (explain card)))
                        {:type :role-gate-rejected
                         :explanation (explain card)}))))
    (log/warn "RoleCard gate skipped: no validator injected, none registered under"
              role-card-validator-extension-key
              "and hive-spi.role.card is absent; the :role entry is stored unvalidated")))

(defn- add-with-optional-receipt!
  "Use the optional SPI only when the registered (possibly decorated) store
   itself implements it. Never peel decorators: their get-entry may decrypt,
   filter or reshape the backend's receipt. Older SPI versions use add-entry!."
  [store entry]
  (let [protocol (try
                   (some-> (requiring-resolve
                             'hive-spi.memory.receipt/IMemoryStoreAddReceipt) deref)
                   (catch java.io.FileNotFoundException _ nil))]
    (if (and protocol (satisfies? protocol store))
      ((requiring-resolve 'hive-spi.memory.receipt/-add-entry-returning!) store entry)
      (mem-proto/add-entry! store entry))))

(defn- index-entry!
  "Index via the registered store; optional receipt support is negotiated on
   the outermost store. Embedding failures still park in the durable outbox."
  [{:keys [type content tags-with-scope content-hash duration-str
           expires project-id abstraction-level knowledge-gaps store-key]
    :or {store-key :default}}]
  (let [base-entry {:id (mem-proto/generate-id)
                    :type type :content content :tags tags-with-scope
                    :content-hash content-hash :duration duration-str
                    :expires (or expires "") :project-id project-id
                    :abstraction-level abstraction-level
                    :knowledge-gaps knowledge-gaps}
        entry (if (= type "plan")
                (cond-> (assoc base-entry :plan-status "draft")
                  (plans/count-plan-steps content)
                  (assoc :steps-count (plans/count-plan-steps content)))
                base-entry)
        park! (fn [failure attempts]
                (log/error "Embedding failed; entry parked for reembed (NOT vectorized):"
                           (:id entry) "type:" type "store:" store-key
                           "attempts:" attempts "cause:" (pr-str failure))
                (deferred/park!
                  (deferred/with-failure
                    (deferred/with-store-key entry store-key)
                    failure (str (java.time.Instant/now)))))
        {:keys [ok failure attempts]}
        (binding [embed-gate/*lane* :interactive]
          (deferred/call-with-embed-retry
            #(with-stage-resilience
               (add-with-optional-receipt! (mem-proto/get-store store-key) entry))
            deferred/default-retry
            #(Thread/sleep (long %))))]
    (if failure
      (park! failure attempts)
      ok)))

(def ^:private ^:const read-after-write-attempts 6)
(def ^:private ^:const read-after-write-base-ms 40)

(defn- fetch-with-retry
  "Read an entry by id with bounded exponential-ish backoff. Addresses
   read-after-write consistency lag on some IMemoryStore backends
   (e.g. Milvus's HTTP transport where `:consistency-level :strong` on
   get doesn't always surface the just-written row on the first poll).

   Attempts: `read-after-write-attempts`, waits grow linearly from
   `read-after-write-base-ms`. Total worst-case budget ≈ 40+80+120+160+200
   = 600 ms, which stays well under the 30 s memory-write-timeout-ms
   and is invisible to callers when the first read already succeeds.

   Each store read is wrapped in `with-stage-resilience` (the short
   `store-stage-heal-budget-ms`, since the backend's own `resilient` already
   heals) so a transport drop during the read-after-write window triggers a
   heal-and-retry. All types — plans included — go through the same
   IMemoryStore path."
  [store entry-id]
  (loop [attempt 1]
    (let [fetched (with-stage-resilience (mem-proto/get-entry store entry-id))]
      (cond
        (some? fetched) fetched
        (>= attempt read-after-write-attempts) nil
        :else (do (Thread/sleep (* attempt read-after-write-base-ms))
                  (recur (inc attempt)))))))

(defn- finalize-entry!
  "Wire KG edges, fetch created entry, notify the write, and format response.

   `:store-key` (passed via `entry-ctx`) routes the read-after-write +
   `:kg-outgoing` link write to the same slot the entry was indexed in.
   Defaults to `:default`. Kanban writes thread `:kanban`.

   The entry is already stored when this runs, so a failed edge write never
   fails the add: the response carries the entry id plus `:kg_edge_errors`
   [{:relation :to :error}], and the :added notify still fires. A :kg-edges
   stage over its budget is reported the same way, the error naming the stage.

   A deferred entry (embed failed, parked in the outbox) is answered from
   its outbox record; its :kg-outgoing link is amended into that record so
   the drain stores it with the entry. The response names the cause as
   `:embedding_error` so the caller sees why the entry is not searchable yet.

   `:progress` (optional, from `new-progress`) is told which stage the write
   is in (`:kg-edges`, `:link-outgoing`, `:fetch`) so a timeout can name it;
   each stage runs under its `*stage-budgets-ms*` budget (`run-stage!`).
   A canonical receipt replaces read-after-write only with no KG requests."
  [entry-id kg-params project-id agent-id
   {:keys [tags-with-scope type knowledge-gaps store-key progress receipt]
    :or {store-key :default}}]
  (let [store (mem-proto/get-store store-key)
        pending (deferred/lookup entry-id)
        requests (edge-outcome/edge-requests kg-params)
        {:keys [edge-ids edge-errors]}
        (if (seq requests)
          (edge-outcome/attempt-edges
           #(run-stage! progress :kg-edges
                        (fn [] (create-kg-edges! entry-id kg-params project-id agent-id)))
           requests)
          {:edge-ids [] :edge-errors []})
        pending (if (and pending (seq edge-ids))
                  (do (deferred/amend! entry-id {:kg-outgoing edge-ids})
                      (deferred/lookup entry-id))
                  pending)
        _ (when (and (seq edge-ids) (not pending))
            (run-stage! progress :link-outgoing
                        #(with-stage-resilience
                           (mem-proto/update-entry! store entry-id {:kg-outgoing edge-ids}))))
        created (or pending
                    (when (and (empty? requests) (map? receipt)
                               (= entry-id (:id receipt)))
                      receipt)
                    (run-stage! progress :fetch #(fetch-with-retry store entry-id)))]
    (when-not created
      (log/error "Failed to retrieve entry after indexing:" entry-id))
    (log/info "Created memory entry:" entry-id
              (when (seq edge-ids) (str " with " (count edge-ids) " KG edges"))
              (when (seq edge-errors) (str " edge-errors:" (count edge-errors)))
              (when (seq knowledge-gaps) (str " gaps:" (count knowledge-gaps))))
    (write-events/notify! :added {:id entry-id :memory-type type
                                  :tags tags-with-scope :project-id project-id})
    (if created
      (let [requested (type-registry/requested-type-of tags-with-scope)]
        (mcp-json (cond-> (assoc (fmt/entry->receipt created) :id entry-id)
                    pending (assoc :embedding_deferred true :reembed_id entry-id
                                   :embedding_error (some-> pending :deferred/last-error :error str))
                    (seq edge-ids) (assoc :kg_edges_created edge-ids)
                    (seq edge-errors) (assoc :kg_edge_errors edge-errors)
                    requested
                    (assoc :queued_for_review
                           {:requested_type requested
                            :parked_as      type
                            :reason         (:reason (type-registry/gate-of requested))}))))
      (mcp-error (str "Entry indexed as " entry-id " but retrieval failed. Check memory store connectivity."
                      (when (seq edge-errors)
                        (str " KG edges also failed: " (pr-str edge-errors))))))))

(defn- do-add!
  "Inner core of handle-add. Runs the memory-store/KG IO path through
   the IMemoryStore registered under `:store-key` (default `:default`).

   `:store-key` propagates to dedup find/update, primary index, and
   finalize. Kanban callers thread `:kanban` (or `:dual-read`-resolved)
   to land in the dedicated qdrant collection without disturbing the
   generic memory-add surface used by axiom/principle/snippet writes.

   Both the duplicate-detection read and the duplicate-tag-merge write
   are wrapped in `with-stage-resilience`: if the Milvus HTTP transport
   drops between the find-duplicate call and the dedup update, the heal
   loop fires and the call retries once — otherwise transient failures here
   would leak through `with-store`'s generic try/catch as errors.

   `:progress` (optional, from `new-progress`) is told which stage the write
   is in (`:dedup-lookup`, `:dedup-update`, `:index`, then finalize's stages)
   so `handle-add` can name the stage a timeout happened in. Each store
   stage runs under its `*stage-budgets-ms*` budget (`run-stage!`); a stage
   over budget answers an error naming that stage."
  [{:keys [type content tags duration directory agent_id
           kg_implements kg_supersedes kg_depends_on kg_refines abstraction_level
           knowledge_gaps store-key progress]
    :or   {store-key :default}}]
  (let [tags-vec (coerce-vec! tags :tags [])
        kg-vecs {:kg-implements-vec (coerce-vec! kg_implements :kg_implements [])
                 :kg-supersedes-vec (coerce-vec! kg_supersedes :kg_supersedes [])
                 :kg-depends-on-vec (coerce-vec! kg_depends_on :kg_depends_on [])
                 :kg-refines-vec    (coerce-vec! kg_refines :kg_refines [])}
        directory (or directory (ctx/current-directory))
        abstraction-level (or abstraction_level
                              (classify/classify-abstraction-level type content tags-vec))
        ;; Callers that write structured/serialized content (e.g. kanban JSON)
        ;; pass an explicit :knowledge_gaps to bypass regex gap-extraction,
        ;; which otherwise scrapes literals like "status":"todo" out of the
        ;; JSON. An explicit vector (incl. empty) wins; absent/nil auto-extracts.
        knowledge-gaps (or knowledge_gaps (gaps/extract-knowledge-gaps content))]
    (log/info "mcp-memory-add:" type "directory:" directory "agent_id:" agent_id
              "store-key:" store-key)
    (with-store
      (let [;; Caller-explicit `scope:project:<id>` tag wins over the project
            ;; inferred from directory/session (kanban 20260519173408-30f0a6a6,
            ;; rule A); see scope/explicit-project-id.
            project-id (or (scope/explicit-project-id tags-vec)
                           (scope/get-current-project-id directory))
            agent-id (or agent_id (ctx/current-agent-id)
                         (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
            tags-with-scope (build-entry-tags tags-vec agent-id (ctx/current-attribution agent-id) kg-vecs project-id)
            store (mem-proto/get-store store-key)
            content-hash (mem-proto/content-hash content)
            duration-str (or duration "long")
            expires (dur/calculate-expires duration-str)
            existing (run-stage! progress :dedup-lookup
                                 #(with-stage-resilience
                                    (mem-proto/find-duplicate store type content-hash {:project-id project-id})))]
        (if existing
          (let [merged-tags (distinct (concat (:tags existing) tags-with-scope))
                updated (run-stage! progress :dedup-update
                                    #(with-stage-resilience
                                       (mem-proto/update-entry! store (:id existing) {:tags merged-tags})))]
            (log/info "Duplicate found, merged tags:" (:id existing))
            (write-events/notify! :updated {:id (:id existing) :memory-type type
                                            :tags merged-tags :project-id project-id
                                            :fields [:tags]})
            (mcp-json (fmt/entry->receipt updated)))
          (let [_ (when (= type "plan") (validate-plan-gate! content))
                _ (when (= type "role") (validate-role-gate! content))
                entry-ctx {:type type :content content :tags-with-scope tags-with-scope
                           :content-hash content-hash :duration-str duration-str
                           :expires expires :project-id project-id
                           :abstraction-level abstraction-level
                           :knowledge-gaps knowledge-gaps :agent-id agent-id
                           :store-key store-key :progress progress}
                add-result (run-stage! progress :index #(index-entry! entry-ctx))
                ;; Only a receipt carrying the canonical entry can bypass fetch.
                ;; Queued/failure maps (and malformed receipts) remain errors.
                entry-id (if (map? add-result) (:id add-result) add-result)
                receipt (when (and (map? add-result)
                                   (map? (:entry add-result))
                                   (= entry-id (get-in add-result [:entry :id])))
                          (:entry add-result))]
            (if-not (and (string? entry-id) (not (str/blank? entry-id))
                         (or (not (map? add-result)) receipt))
              (mcp-error (str "Memory store add-entry! did not return an id "
                              "(got " (pr-str add-result) "). "
                              "Backend is likely degraded or offline — "
                              "check store health and retry."))
              (let [_ (recall/register-created-id! entry-id project-id type)
                    kg-params {:kg_implements (:kg-implements-vec kg-vecs)
                               :kg_supersedes (:kg-supersedes-vec kg-vecs)
                               :kg_depends_on (:kg-depends-on-vec kg-vecs)
                               :kg_refines    (:kg-refines-vec kg-vecs)}]
                (finalize-entry! entry-id kg-params project-id agent-id
                                 (assoc entry-ctx :receipt receipt))))))))))

(defn handle-add
  "Add an entry to project memory with optional KG edge creation.
   Runs the IO-heavy core on the dedicated memory-pool so slow Chroma
   or KG calls cannot saturate the shared io-pool.

   A write that outruns `*write-timeout-ms*` is answered with a
   stage-attributed error (see `timeout-diagnostic`): which stage was
   running, how long the write and that stage had run, and whether the
   write never left the pool queue.

   Every add that reaches the pool, successful or not, records its per-stage
   wall times (dedup, embed, upsert, kg, read-after-write, total) into the
   `write-latency` ring that catchup's memory-status reads
   (kanban 20260928204307-01ca0962)."
  [{:keys [type abstraction_level] :as args}]
  (try

    (cond
      (not (type-registry/valid-type? type))
      (mcp-error (str "Invalid memory type: " (pr-str type)
                      ". Type must be a safe token — letters, digits, '_' or '-', "
                      "starting with a letter, max " type-registry/max-type-length
                      " chars (e.g. axiom, decision, pattern, my-custom-type)."))

      (and abstraction_level (not (kg-schema/valid-abstraction-level? abstraction_level)))
      (mcp-error (str "Invalid abstraction_level: " abstraction_level))

      :else
      (let [;; Write gate: a request for a human-gated type (:axiom) is PARKED
            ;; as the gate's queue type and tagged pending, never honoured
            ;; outright and never dropped. Ungated types pass through untouched.
            {eff-type :type :keys [queued? gate requested]}
            (type-registry/resolve-write-type type)

            args (cond-> args
                   queued? (update :tags type-registry/queued-tags gate requested))

            ;; Canonicalize + auto-register the (possibly novel) type with sane
            ;; defaults, so unknown-but-safe types persist and stay consistent.
            args (assoc args :type (type-registry/ensure-type! eff-type))
            timeout-sentinel ::timeout
            timeout-ms *write-timeout-ms*
            progress (new-progress)
            embed-ms (atom 0)
            args (assoc args :progress progress)
            ;; Only this (the caller's) thread records, once, after await!
            ;; answered: the outcome is decided by what await! returned, so a
            ;; worker interrupted by the timeout can never record :error over
            ;; the :timeout the caller saw.
            record! (fn [outcome]
                      (let [{:keys [started-ns timings]} @progress]
                        (write-latency/record! {:timings  timings
                                                :embed-ms @embed-ms
                                                :total-ms (quot (- (System/nanoTime) started-ns) 1000000)
                                                :outcome  outcome})))
            result (wpool/await!
                    (pool/memory-pool)
                    (let [budgets *stage-budgets-ms*]
                      #(binding [*stage-budgets-ms* budgets
                                 write-latency/*embed-ms* embed-ms]
                         (try (do-add! args)
                              (catch Throwable t {::caught t}))))
                    {:timeout-ms timeout-ms
                     :fallback   timeout-sentinel
                     :name       "memory-add"})]
        (cond
          (= result timeout-sentinel)
          (let [diag (timeout-diagnostic @progress timeout-ms (System/nanoTime))]
            (record! :timeout)
            (log/warn "memory add timed out:" (pr-str diag))
            (mcp-error (timeout-message diag)))

          (and (map? result) (contains? result ::caught))
          (do (record! :error)
              (throw (::caught result)))

          :else
          (do (record! (if (:isError result) :error :ok))
              result))))
    (catch clojure.lang.ExceptionInfo e
      (if (#{:coercion-error :embedding-too-long :plan-gate-rejected :role-gate-rejected} (:type (ex-data e)))
        (mcp-error (.getMessage e))
        (throw e)))))

;;; =============================================================================
;;; Contracts
;;; =============================================================================

(def RoleCardValidator
  "A RoleCard validator as injected or registered: {:valid? fn :explain fn}.
   The selection itself is source-agnostic, so a candidate is any non-nil
   value; a malformed one is rejected later by the role gate."
  :some)

(def ValidatorSources
  [:maybe
   [:map
    [:injected {:optional true} :any]
    [:extension {:optional true} :any]
    [:spi {:optional true} :any]]])

(def ValidatorChoice
  [:or
   [:map [:source [:enum :injected :extension :spi]] [:validator RoleCardValidator]]
   [:map [:source [:= :none]] [:validator :nil]]])

(def WriteStage
  "The stage a memory add is in; :queued until a pool thread takes it."
  :keyword)

(def ProgressSnapshot
  [:map
   [:stage {:optional true} [:maybe WriteStage]]
   [:started-ns {:optional true} [:maybe :int]]
   [:stage-since-ns {:optional true} [:maybe :int]]])

(def ProgressAtom
  "The progress tracker: an atom holding a ProgressSnapshot."
  [:fn {:gen/schema [:map [:stage WriteStage] [:started-ns :int] [:stage-since-ns :int]]
        :gen/fmap atom}
   #(and (instance? clojure.lang.IAtom %) (map? @%))])

(def TimeoutDiagnostic
  [:map
   [:stage WriteStage]
   [:elapsed-ms [:int {:min 0}]]
   [:stage-elapsed-ms [:int {:min 0}]]
   [:timeout-ms :any]
   [:cause [:enum :pool-queue-wait :stage-stalled]]])

(def StageBudgetDiagnostic
  [:map
   [:stage WriteStage]
   [:budget-ms [:int {:min 0}]]
   [:elapsed-ms [:int {:min 0}]]])

(m/=> stage-budget-message [:=> [:cat StageBudgetDiagnostic] :string])

(m/=> select-role-card-validator [:=> [:cat ValidatorSources] ValidatorChoice])

(m/=> set-role-card-validator! [:=> [:cat :any] :any])

(m/=> current-role-card-validator [:=> [:cat] [:maybe RoleCardValidator]])

(m/=> new-progress [:=> [:cat] ProgressAtom])

(m/=> timeout-diagnostic [:=> [:cat ProgressSnapshot :any :int] TimeoutDiagnostic])

(m/=> timeout-message [:=> [:cat [:maybe :map]] :string])

(m/=> handle-add [:=> [:cat [:maybe :map]] core/McpResponse])
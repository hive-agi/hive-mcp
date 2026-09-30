(ns hive-mcp.tools.kg.synthetics
  "Cleanup scaffolding for synthetic-pattern nodes.

   Synthetic pattern nodes (IDs prefixed `synth-`) exist only as nodes in
   the KG — they project onto raw memory entry IDs via outgoing
   `:projects-to` edges.

   When most of a synthetic's `:projects-to` targets point at
   expired/missing memory entries, the synthetic is dead scaffolding that
   pollutes KG insights (emergent-pattern review, 2026-04-23).

   **Spec:** live-ratio is computed over `:projects-to` edges *only*.
   Other outgoing relation types (e.g. `:depends-on`, `:co-accessed`) do
   not contribute to the freshness decision — a synthetic is fresh iff
   the raw memories it projects onto are still live.

   Actions:
   - `:demote` — set `:projects-to` edge confidence to 0.1 (other
     relations untouched)
   - `:delete` — remove the synthetic node via
     `edges/remove-edges-for-node!`, which cleans up ALL connected edges
     (not just `:projects-to`), leaving no orphan edges behind.

   This ns provides `cleanup-synthetics!` — a bounded per-cycle pass that:
     1. Enumerates distinct source nodes whose ID starts with `synth-`
     2. For each, counts live-vs-expired `:projects-to` targets via
        `mem-proto/get-entry`
     3. Classifies by live-ratio against a configurable :threshold
     4. Applies the selected action

   Uses the same fetch/sort/limit/tally pattern as
   `edges/decay-unverified-edges!`."
  (:require [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.edges :as edges]
            [hive-mcp.knowledge-graph.edge-cycle :as edge-cycle]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Defaults
;; =============================================================================

(def ^:const default-threshold
  "Live-ratio below which a synthetic is considered dead scaffolding.
   0.2 = act when 80%+ of targets are missing/expired (matches the
   2026-04-23 emergent-pattern review observation)."
  0.2)

(def ^:const default-min-live-members
  "Minimum LIVE `:projects-to` targets a synthetic must retain to still be
   a cluster. Below this it names fewer than two things that exist, which
   is not a pattern regardless of what fraction of the original set
   survived. Matches the rule `retract-dangling-synthetics!` already
   applies to registered synthetics."
  2)

(def ^:const default-limit
  "Maximum synthetics to evaluate per cycle. Bounded to prevent
   unbounded scans on large graphs."
  50)

(def ^:const demote-confidence
  "Confidence score applied to `:projects-to` edges when :action is :demote.
   Non-`:projects-to` edges on a synthetic (if any) are left alone."
  0.1)

(def ^:const synth-id-prefix "synth-")

(def ^:const synth-target-relation
  "Relation type considered when computing a synthetic's live-ratio and
   when the `:demote` action adjusts confidence. Synthetic emergent
   patterns project onto raw memories via this relation; other relation
   types (if any) are deliberately excluded from freshness decisions."
  :projects-to)

;; =============================================================================
;; Enumeration
;; =============================================================================

(defn list-synthetic-source-nodes
  "Return distinct source node IDs from `:kg-edge/from` whose ID starts
   with `synth-`. The prefix test runs as a Datalog predicate clause, so
   the engine filters during the join instead of shipping every
   `:kg-edge/from` value to the client.

   Returns a sorted vector of strings for deterministic per-cycle
   ordering."
  []
  (let [q '[:find [?from ...]
            :in $ ?prefix
            :where
            [?e :kg-edge/from ?from]
            [(clojure.string/starts-with? ?from ?prefix)]]
        synth-from (conn/query q synth-id-prefix)]
    (->> (or synth-from [])
         (filter string?)
         distinct
         sort
         vec)))

;; =============================================================================
;; Record visibility: the orphaned-record selection class
;; =============================================================================
;;
;; A synthetic whose :kg-synthetic/id record is gone while its edges survive
;; is edge residue, not a pattern. Measured 2026-08-30: 3,885 of 4,432 synth-
;; edge sources had no record, carrying 674k of 677k :projects-to edges
;; (memory 20260830173421-2baa1fb0). Classifying those on TARGET liveness
;; scored them healthy whenever their targets were alive, so they were never
;; collected. They are now their own class, decided on the record alone.
;;
;; The class is only decidable when the record layer is observable. With no
;; record at all while synth- sources exist, "no record" cannot be told apart
;; from "record schema not loaded", and reading it as the former would delete
;; the whole synthetic layer. That case is reported BLIND, never collected.

(defn synthetic-record-ids
  "Set of every :kg-synthetic/id that has a node record. Throws when the
   store cannot answer; callers decide what an unanswerable query means."
  []
  (set (conn/query '[:find [?id ...] :where [_ :kg-synthetic/id ?id]])))

(defn count-projection-edges
  "Count of every :projects-to edge, whatever its source. The non-vacuity
   reference: if this is positive, an empty synth- source universe is a
   broken derivation, not an empty graph."
  []
  (or (conn/query '[:find (count ?e) .
                    :where [?e :kg-edge/relation :projects-to]])
      0))

(defn record-visibility
  "Can the orphaned-record class be decided over `sources`?

   Returns {:visible? bool :records #{...}|nil :reason str?}."
  [sources]
  (let [records (try (synthetic-record-ids) (catch Exception e e))]
    (cond
      (instance? Exception records)
      {:visible? false :records nil
       :reason (str "record query failed: " (.getMessage ^Exception records))}

      (and (empty? records) (seq sources))
      {:visible? false :records #{}
       :reason (str "no :kg-synthetic/id record exists while " (count sources)
                    " synth- edge sources do; the record layer is absent or "
                    "unloaded, so a missing record proves nothing")}

      :else {:visible? true :records records})))

(defn partition-sources
  "Split synth- edge sources into {:registered [...] :orphans [...]} against a
   record-visibility result. When the orphan class is not visible, every
   source is :registered (the conservative reading) and :orphans is empty."
  [sources {:keys [visible? records]}]
  (if visible?
    {:registered (filterv #(contains? records %) sources)
     :orphans    (filterv #(not (contains? records %)) sources)}
    {:registered (vec sources) :orphans []}))

;; =============================================================================
;; Bounded retraction
;; =============================================================================

(def ^:const default-edge-budget
  "Max edges one cleanup call may retract. The one-off manual reap moved 674k
   edges in ~25 min of sustained transacting; no single call gets near that."
  20000)

(def ^:const default-chunk-size
  "Edges retracted per transaction."
  500)

(defn- node-edge-ids
  "Every :kg-edge/id touching node-id, outgoing and incoming."
  [node-id]
  (let [out (conn/query '[:find [?id ...] :in $ ?n
                          :where [?e :kg-edge/from ?n] [?e :kg-edge/id ?id]]
                        node-id)
        in  (conn/query '[:find [?id ...] :in $ ?n
                          :where [?e :kg-edge/to ?n] [?e :kg-edge/id ?id]]
                        node-id)]
    (vec (distinct (concat out in)))))

(defn- retract-record!
  "Retract the :kg-synthetic node record for synth-id, if one exists, so a
   collected synthetic does not leave the inverse ghost (record, no edges)."
  [synth-id]
  (when-let [eid (try (conn/query '[:find ?e . :in $ ?id
                                    :where [?e :kg-synthetic/id ?id]]
                                  synth-id)
                      (catch Exception _ nil))]
    (conn/transact! [[:db/retractEntity eid]])
    true))

(defn retract-node-bounded!
  "Retract up to `budget` edges touching node-id in chunks of `chunk-size`,
   asking `continue?` before every chunk (nil = go on, anything else = the
   reason to stop). The node record is retracted only once no edge remains.

   Returns {:removed n :remaining n :complete? bool :stopped reason?}."
  [node-id {:keys [budget chunk-size continue?]
            :or   {chunk-size default-chunk-size continue? (constantly nil)}}]
  (let [ids    (node-edge-ids node-id)
        quota  (vec (take (max 0 (or budget (count ids))) ids))]
    (loop [chunks (partition-all chunk-size quota) removed 0]
      (let [stop (when (seq chunks) (continue?))]
        (if (and (seq chunks) (nil? stop))
          (let [c (first chunks)]
            (conn/with-tx-batch
              (doseq [id c] (edges/remove-edge! id)))
            (recur (rest chunks) (+ removed (count c))))
          (let [remaining (- (count ids) removed)
                complete? (zero? remaining)]
            (when complete? (retract-record! node-id))
            (cond-> {:removed removed :remaining remaining :complete? complete?}
              stop                          (assoc :stopped stop)
              (and (not stop) (pos? remaining)) (assoc :stopped :edge-budget))))))))

(defn reap-orphans!
  "Collect synthetics whose node record is gone while their edges survive.
   No liveness lookups: the class is decided on the record alone.

   Opts: :limit (orphans per call, default 200), :edge-budget, :chunk-size,
   :continue?, :dry-run?, :sources (pre-fetched universe).

   Returns {:visible? bool :reason str? :orphans-total n :reaped n
            :partial [ids] :edges-removed n :stopped reason? :dry-run? bool}.
   A partially reaped orphan stays an orphan and is resumed next call."
  [& [{:keys [limit edge-budget chunk-size continue? dry-run? sources]
       :or   {limit 200 edge-budget default-edge-budget
              chunk-size default-chunk-size continue? (constantly nil)}}]]
  (let [sources (or sources (list-synthetic-source-nodes))
        vis     (record-visibility sources)
        {:keys [orphans]} (partition-sources sources vis)
        base    (cond-> {:visible?      (:visible? vis)
                         :orphans-total (count orphans)
                         :reaped 0 :partial [] :edges-removed 0
                         :dry-run?      (boolean dry-run?)}
                  (:reason vis) (assoc :reason (:reason vis)))]
    (if (or dry-run? (not (:visible? vis)))
      base
      (loop [todo (take limit orphans) acc base]
        (let [left (- edge-budget (:edges-removed acc))]
          (cond
            (empty? todo) acc
            (not (pos? left)) (assoc acc :stopped :edge-budget)
            :else
            (let [id (first todo)
                  {:keys [removed complete? stopped]}
                  (retract-node-bounded! id {:budget left :chunk-size chunk-size
                                             :continue? continue?})
                  acc (cond-> (update acc :edges-removed + removed)
                        complete?       (update :reaped inc)
                        (not complete?) (update :partial conj id))]
              (if stopped
                (assoc acc :stopped stopped)
                (recur (rest todo) acc)))))))))

;; =============================================================================
;; Live-ratio Classification
;; =============================================================================

(defn- memory-entry-live?
  "Check whether a memory entry id resolves to a non-nil entry via
   `mem-proto/get-entry`. Returns false if no memory store is registered
   (cleanup is conservative — treat unknown as not live)."
  [store entry-id]
  (try
    (some? (mem-proto/get-entry store entry-id))
    (catch Exception _ false)))

(defn- live-target-ids
  "Return the subset of `target-ids` that resolve to a live memory entry.

   Uses the store's batched read (IMemoryStoreBatch, projected to `id`)
   when available: one round-trip for the whole target set instead of one
   per target. Falls back to per-id reads when the store is not batched,
   and also when a batch read throws, so a transport failure can never be
   read as 'every target is dead'.

   Returns #{} when no store is registered."
  [store target-ids]
  (let [ids (vec (distinct target-ids))]
    (cond
      (nil? store)  #{}
      (empty? ids)  #{}
      (mem-proto/batch-store? store)
      (or (try (into #{} (keep #(or (:id %) (get % "id")))
                     (mem-proto/get-entries-projected store ids {:output-fields ["id"]}))
               (catch Exception _ nil))
          (into #{} (filter #(memory-entry-live? store %)) ids))
      :else
      (into #{} (filter #(memory-entry-live? store %)) ids))))

(defn- classify-synthetic
  "Inspect outgoing `:projects-to` edges for a synthetic node and compute
   live stats.

   Live-ratio is computed over `:projects-to` targets only — other edge
   types (e.g. `:depends-on`, `:co-accessed`) on a synthetic, if they
   exist, don't count toward 'is this pattern still supported by live
   evidence?'. The 2026-04-23 emergent-pattern review framed synthetic
   freshness specifically in terms of the projection onto raw memories,
   and this function enforces that contract.

   Target liveness resolves through `live-target-ids`, one batched store
   read per synthetic rather than one read per target.

   `:edges` in the returned map is the filtered vector (what demote acts
   on); `:edge-count` is the count of those filtered edges. Callers that
   need the full outgoing set can call `edges/get-edges-from` directly."
  [store synth-id]
  (let [all-out       (edges/get-edges-from synth-id)
        projects-edges (filterv #(= synth-target-relation (:kg-edge/relation %))
                                all-out)
        total         (count projects-edges)
        ;; Distinct targets — a synth may repeat a target via separate
        ;; edges; we only care about unique raw-memory ids for the ratio.
        targets       (distinct (map :kg-edge/to projects-edges))
        live          (count (live-target-ids store targets))
        target-ct     (count targets)
        ratio         (if (zero? target-ct) 0.0 (double (/ live target-ct)))]
    {:synth-id     synth-id
     :edges        projects-edges
     :edge-count   total
     :target-count target-ct
     :live-count   live
     :live-ratio   ratio}))

;; =============================================================================
;; Actions
;; =============================================================================

(defn- demote-synthetic!
  "Set outgoing-edge confidence to `demote-confidence` for the given edges.
   Returns the count of edges demoted."
  [out-edges]
  (reduce
   (fn [n edge]
     (if-let [eid (:kg-edge/id edge)]
       (do (edges/update-edge-confidence! eid demote-confidence) (inc n))
       n))
   0
   out-edges))

;; =============================================================================
;; Cycle Step
;; =============================================================================

(defn- delete-bounded!
  "Delete synth-id against the shared per-call edge budget. Returns the
   removed-edge count; a node the budget could not finish is recorded in
   :partial and picked up next call."
  [{:keys [budget-atom chunk-size continue? partial-atom]} synth-id]
  (let [{:keys [removed complete? stopped]}
        (retract-node-bounded! synth-id {:budget     (max 0 @budget-atom)
                                         :chunk-size (or chunk-size default-chunk-size)
                                         :continue?  (or continue? (constantly nil))})]
    (swap! budget-atom - removed)
    (when-not complete? (swap! partial-atom conj {:synth-id synth-id :stopped stopped}))
    {:removed removed :complete? complete?}))

(defn- step!
  "Per-synthetic step invoked by `edge-cycle/run-cycle!`.
   Returns :orphaned, :pruned, :demoted, or :preserved. When dry-run? is
   true never mutates, just classifies.

   An ORPHANED synthetic (edges survive, node record gone) is dead by
   definition and is decided on the record alone: its targets are not read,
   because live targets say nothing about whether the synthetic exists. The
   :demote action does not apply to it; residue is not demoted, it is removed.

   A registered synthetic is dead scaffolding when it retains fewer than
   `:min-live-members` live targets, or (when `:threshold` is given) when
   its live-RATIO falls below it."
  [{:keys [threshold min-live-members action dry-run? store details-atom orphan-set]
    :as ctx} synth-id]
  (if (contains? orphan-set synth-id)
    (let [edge-count (count (edges/get-edges-from synth-id))
          {:keys [removed complete?]}
          (if dry-run? {:removed 0 :complete? false} (delete-bounded! ctx synth-id))]
      (swap! details-atom conj
             {:synth-id synth-id :edge-count edge-count :orphaned-record? true
              :outcome :orphaned :effect-count removed :complete? complete?
              :dry-run? dry-run?})
      :orphaned)
    (let [{:keys [edge-count target-count live-count live-ratio edges]}
          (classify-synthetic store synth-id)
          min-live (or min-live-members default-min-live-members)
          below? (or (< live-count min-live)
                     (and threshold (< live-ratio threshold)))
          outcome (cond
                    (not below?)               :preserved
                    (= action :demote)         :demoted
                    :else                      :pruned)
          {effect-count :removed complete? :complete?}
          (cond
            dry-run?               {:removed 0 :complete? false}
            (= outcome :preserved) {:removed 0 :complete? false}
            (= outcome :demoted)   {:removed (demote-synthetic! edges) :complete? true}
            :else                  (delete-bounded! ctx synth-id))]
      (swap! details-atom conj
             {:synth-id     synth-id
              :edge-count   edge-count
              :target-count target-count
              :live-count   live-count
              :live-ratio   live-ratio
              :min-live     min-live
              :outcome      outcome
              :effect-count effect-count
              :complete?    complete?
              :dry-run?     dry-run?})
      outcome)))

;; =============================================================================
;; Public Entry Point
;; =============================================================================

(defn rotate-after
  "Sorted candidates strictly after cursor `after`, then wrapping to the
   start, so a bounded :limit walks the whole universe across calls instead
   of re-reading the same first N ids forever."
  [ids after]
  (if (nil? after)
    (vec ids)
    (let [later? #(pos? (compare % after))]
      (into (filterv later? ids) (remove later?) ids))))

(defn cleanup-synthetics!
  "Scan synthetic-pattern nodes and act on those that are dead.

   Two selection classes over the EDGE-derived universe (distinct synth-
   :kg-edge/from values, which is the only enumeration that can see a node
   whose record is gone):
     :orphaned  edges survive, :kg-synthetic/id record gone. Removed whole,
                decided on the record alone, no liveness reads.
     :pruned / :demoted
                registered synthetic that no longer names a cluster of live
                raw memory entries (live-count / live-ratio rule below).

   Options:
     :min-live-members - Act when a registered synthetic retains fewer than
                         this many LIVE :projects-to targets (default 2).
     :threshold        - Additional live-RATIO criterion (default 0.2).
     :action           - :delete or :demote (registered class only).
     :limit            - Max synthetics per call (default 50).
     :after            - Resume cursor: start with ids sorted after it,
                         wrapping. Returned as :next-cursor.
     :edge-budget      - Max edges retracted per call (default 20000).
     :chunk-size       - Edges per retraction transaction (default 500).
     :continue?        - 0-arg fn asked before every chunk; non-nil return is
                         the reason to stop (RAM guard, deadline).
     :dry-run?         - Classify only; no mutations. Default false.

   :orphaned/:pruned/:demoted count SELECTED synthetics. A deletion the edge
   budget or :continue? cut short is listed in :partial and its detail row
   carries :complete? false; it is finished on a later call.

   Returns
     {:scanned :orphaned :pruned :demoted :preserved :errors
      :universe {:sources n :orphans n :registered n :projection-edges n}
      :blind [{:class kw :reason str} ...]   what this call could NOT see
      :partial [...] :edge-budget-left n :next-cursor id
      :details [...] ...opts}

   Refuses to classify the registered class when no memory store is
   registered (every target would read dead). Orphans are still decided,
   since they need no store. Errors are tallied, never thrown."
  [& [{:keys [threshold min-live-members action limit dry-run? after
              edge-budget chunk-size continue?]
       :or {threshold        default-threshold
            min-live-members default-min-live-members
            action           :delete
            limit            default-limit
            edge-budget      default-edge-budget
            chunk-size       default-chunk-size
            dry-run?         false}}]]
  ;; Drain any pending write-coalesced edges so the scan sees the
  ;; authoritative state. No-op if writer isn't running.
  (conn/flush-pending!)
  (let [store      (try (mem-proto/get-store) (catch Exception _ nil))
        action-kw  (keyword action)
        sources    (list-synthetic-source-nodes)
        proj-count (try (count-projection-edges) (catch Exception _ -1))
        vis        (record-visibility sources)
        {:keys [registered orphans]} (partition-sources sources vis)
        orphan-set (set orphans)
        blind      (cond-> []
                     (and (empty? sources) (pos? proj-count))
                     (conj {:class  :universe
                            :reason (str proj-count " :projects-to edges exist but no "
                                         "synth- source was derived")})
                     (neg? proj-count)
                     (conj {:class :universe :reason "projection-edge count query failed"})
                     (not (:visible? vis))
                     (conj {:class :orphaned-record :reason (:reason vis)})
                     (and (nil? store) (seq registered))
                     (conj {:class  :liveness
                            :reason (str "no memory store registered; "
                                         (count registered)
                                         " registered synthetics not classified")}))
        ;; With no store only orphans are decidable.
        candidates (if store sources orphans)
        base {:dry-run?         (boolean dry-run?)
              :action           action-kw
              :threshold        threshold
              :min-live-members min-live-members
              :limit            limit
              :universe         {:sources          (count sources)
                                 :orphans          (count orphans)
                                 :registered       (count registered)
                                 :projection-edges proj-count}
              :blind            blind}]
    (if (and (nil? store) (empty? orphans))
      (do (log/warn "cleanup-synthetics: no memory store registered, refusing to classify")
          (merge base {:scanned 0 :orphaned 0 :pruned 0 :demoted 0 :preserved 0
                       :errors 1 :details [] :partial []
                       :error "no memory store registered"}))
      (let [details  (atom [])
            partial  (atom [])
            budget   (atom edge-budget)
            ordered  (rotate-after candidates after)
            taken    (vec (take limit ordered))
            tally (edge-cycle/run-cycle!
                   {:fetch        (constantly taken)
                    :sort-key     nil
                    :limit        nil
                    :outcome-keys [:orphaned :pruned :demoted :preserved]
                    :step!        #(step! {:threshold        threshold
                                           :min-live-members min-live-members
                                           :action           action-kw
                                           :dry-run?         (boolean dry-run?)
                                           :store            store
                                           :orphan-set       orphan-set
                                           :budget-atom      budget
                                           :chunk-size       chunk-size
                                           :continue?        continue?
                                           :partial-atom     partial
                                           :details-atom     details}
                                          %)
                    :error-log-fn (fn [synth-id err]
                                    (log/debug "cleanup-synthetics step failed for"
                                               synth-id ":" (:message err)))
                    :log-fn       (fn [t]
                                    (when (some pos? ((juxt :orphaned :pruned :demoted) t))
                                      (log/info "cleanup-synthetics:"
                                                (:orphaned t) "orphaned,"
                                                (:pruned t) "pruned,"
                                                (:demoted t) "demoted,"
                                                (:preserved t) "preserved"
                                                (when dry-run? " (dry-run)"))))})]
        ;; Flush post-cycle mutations so callers see committed state.
        (conn/flush-pending!)
        (merge base
               {:scanned          (:evaluated tally)
                :orphaned         (:orphaned tally)
                :pruned           (:pruned tally)
                :demoted          (:demoted tally)
                :preserved        (:preserved tally)
                :errors           (:errors tally)
                :partial          @partial
                :edge-budget-left @budget
                :next-cursor      (peek taken)
                :details          @details})))))

;; =============================================================================
;; MCP Handler
;; =============================================================================

(defn- parse-action
  "Accept :delete/:demote as keyword or string. Defaults to :delete."
  [action]
  (cond
    (nil? action) :delete
    (keyword? action) action
    (string? action) (keyword action)
    :else :delete))

(defn- valid-action? [a] (contains? #{:delete :demote} a))

(defn handle-kg-cleanup-synthetics
  "MCP boundary handler. Accepts MCP-style params (strings/ints/booleans)
   and returns an MCP JSON response."
  [{:keys [threshold action limit dry_run min_live_members]}]
  (log/info "kg_cleanup_synthetics"
            {:threshold threshold :action action :limit limit
             :dry-run dry_run :min-live-members min_live_members})
  (try
    (let [action-kw (parse-action action)]
      (cond
        (not (valid-action? action-kw))
        (mcp-error (str "Invalid action '" action
                        "'. Valid: 'delete' or 'demote'."))

        (and threshold
             (or (not (number? threshold))
                 (< threshold 0.0) (> threshold 1.0)))
        (mcp-error "threshold must be a number in [0.0, 1.0]")

        (and limit (or (not (integer? limit)) (neg? limit)))
        (mcp-error "limit must be a non-negative integer")

        (and min_live_members
             (or (not (integer? min_live_members)) (neg? min_live_members)))
        (mcp-error "min_live_members must be a non-negative integer")

        :else
        (let [result (cleanup-synthetics!
                      (cond-> {:action action-kw
                               :dry-run? (boolean dry_run)}
                        threshold        (assoc :threshold threshold)
                        limit            (assoc :limit limit)
                        min_live_members (assoc :min-live-members min_live_members)))]
          (mcp-json (assoc result :success true)))))
    (catch Exception e
      (log/error e "kg_cleanup_synthetics failed")
      (mcp-error (str "cleanup-synthetics failed: " (.getMessage e))))))

(def tool-def
  {:name "kg_cleanup_synthetics"
   :description (str "Scan synthetic-pattern nodes (IDs prefixed 'synth-') "
                     "and delete or demote those that no longer name a "
                     "cluster of live memory entries. Synthetics whose node "
                     "record is gone but whose edges survive are removed as "
                     "their own :orphaned class. Reports :blind for any class "
                     "it could not decide. "
                     "Dead-scaffolding cleanup for KG insights.")
   :inputSchema {:type "object"
                 :properties {"min_live_members" {:type "integer"
                                                  :description "Act when fewer than this many targets are still live (default 2)"}
                              "threshold" {:type "number"
                                           :description "Additional live-ratio criterion; act below it (default 0.2)"}
                              "action" {:type "string"
                                        :enum ["delete" "demote"]
                                        :description "Action on sub-threshold synthetics (default 'delete')"}
                              "limit" {:type "integer"
                                       :description "Max synthetics per cycle (default 50)"}
                              "dry_run" {:type "boolean"
                                         :description "Preview without mutating (default false)"}}
                 :required []}
   :handler handle-kg-cleanup-synthetics})

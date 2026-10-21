(ns hive-mcp.system.sweepers.orphan-edge
  "ISweepable impl that finds auto-generated KG edges whose source memory
   entry no longer exists and retracts them.

   An edge qualifies when ALL of these hold:
     - its :kg-edge/source-type is automated or co-access (never manual or
       inferred, which a human or an explicit call put there);
     - its :kg-edge/from has the shape of a memory entry id (14 digits, a
       dash, 8 hex digits), so nodes that are not memory entries are never
       judged against the memory store;
     - the memory store answers definitively that no such entry exists. A
       lookup that throws is :unknown and keeps the edge.

   Each sweep reads one window of `default-scan-window` edges starting at a
   rotating cursor and retracts at most `default-prune-cap` orphans from it,
   so a multi-million edge graph is walked a window at a time.

   The auto-registered instance is DRY-RUN: it reports :would-prune and
   retracts nothing until the JVM property hive.orphan-edge-sweep.retract is
   true. `backfill` is the one-time pass over the whole graph; it is dry-run
   unless called with :retract? true, so the count is known before anything
   is retracted.

   The collaborators (window scan, entry lookup, edge removal) are
   constructor-injected ports, nil in production so each is resolved per
   sweep through requiring-resolve and this namespace never loads the
   knowledge-graph or vectordb namespaces itself.

   Auto-registers with hive-mcp.system.registry on ns load."
  ;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
  ;;
  ;; SPDX-License-Identifier: AGPL-3.0-or-later
  (:require [hive-mcp.protocols.lifecycle :as lifecycle]
            [hive-mcp.system.registry :as reg]
            [taoensso.timbre :as log]
            [malli.core :as m]))

(def ^:const default-scan-window
  "Edges read per sweep."
  2000)

(def ^:const default-prune-cap
  "Orphans retracted per sweep."
  500)

(def ^:private auto-source-types #{"automated" "co-access"})

;;; ---------------------------------------------------------------------------
;;; Schemas
;;; ---------------------------------------------------------------------------

(def MemoryId
  "The shape of a memory entry id: 14 digits, a dash, 8 lowercase hex."
  [:string {:gen/schema [:tuple [:int {:min 0 :max 99999999999999}]
                         [:int {:min 0 :max 4294967295}]]
            :gen/fmap (fn [[d h]] (format "%014d-%08x" d h))}])

(def OrphanEdge
  "The edge keys the sweep reads. Open map; every key may be absent, and an
   unknown :kg-edge/source-type or a non-memory :kg-edge/from only means the
   edge is not a candidate."
  [:map
   [:kg-edge/id {:optional true} [:maybe :string]]
   [:kg-edge/from {:optional true}
    [:maybe [:string {:gen/schema [:or MemoryId [:enum "not-a-memory-id" "x"]]}]]]
   [:kg-edge/source-type {:optional true}
    [:maybe [:or [:keyword {:gen/elements [:automated :co-access :manual :inferred]}]
             [:string {:gen/elements ["automated" "co-access" "manual"]}]]]]])

(def EntryState
  "Whether a memory entry exists. Only :absent makes an edge an orphan."
  [:enum :present :absent :unknown])

(def EntryStateFn
  "Port: memory id -> EntryState."
  [:=> [:cat :string] EntryState])

(def ScanWindow
  "One window of edges; :taken counts the edge entity ids enumerated."
  [:map [:edges [:sequential OrphanEdge]] [:taken [:int {:min 0}]]])

(def ScanFn
  "Port: (offset, limit) -> ScanWindow."
  [:=> [:cat [:int {:min 0}] [:int {:min 0}]] ScanWindow])

(def RemoveFn
  "Port: edge id -> truthy when the edge was removed."
  [:=> [:cat [:maybe :string]] :boolean])

(def RemoveError
  "One failed retraction."
  [:map [:edge-id [:maybe :string]] [:error [:maybe :string]]])

(def SweepOpts
  "Port overrides and knobs accepted by `config`; nil keys take the default."
  [:maybe
   [:map
    [:scan-fn {:optional true} [:maybe ScanFn]]
    [:entry-state-fn {:optional true} [:maybe EntryStateFn]]
    [:remove-fn {:optional true} [:maybe RemoveFn]]
    [:retract? {:optional true} [:maybe :boolean]]
    [:scan-window {:optional true} [:maybe [:int {:min 1}]]]
    [:prune-cap {:optional true} [:maybe [:int {:min 0}]]]]])

(def SweepConfig
  "`config`'s result: every port resolved, every knob set."
  [:map
   [:scan-fn ScanFn]
   [:entry-state-fn EntryStateFn]
   [:remove-fn RemoveFn]
   [:retract? :boolean]
   [:scan-window [:int {:min 1}]]
   [:prune-cap [:int {:min 0}]]])

(def WindowResult
  "`sweep-window!`'s result."
  [:map
   [:next-offset [:int {:min 0}]]
   [:scanned [:int {:min 0}]]
   [:orphans [:int {:min 0}]]
   [:swept [:int {:min 0}]]
   [:errors [:vector RemoveError]]])

(def BackfillResult
  "`backfill`'s result."
  [:map
   [:scanned [:int {:min 0}]]
   [:orphans [:int {:min 0}]]
   [:swept [:int {:min 0}]]
   [:errors [:vector RemoveError]]
   [:dry-run? :boolean]])

(defn auto-kg-edge?
  "True iff the edge was established by the system, not by a caller."
  [edge]
  (let [st (:kg-edge/source-type edge)]
    (boolean (and (or (keyword? st) (string? st))
                  (contains? auto-source-types (name st))))))

(m/=> auto-kg-edge? [:=> [:cat [:maybe OrphanEdge]] :boolean])

(defn- hex-char? [c]
  (or (Character/isDigit (char c))
      (contains? #{\a \b \c \d \e \f} (Character/toLowerCase (char c)))))

(defn memory-id?
  "True iff `id` has the shape of a memory entry id: 14 digits, a dash, 8 hex."
  [id]
  (boolean
   (and (string? id)
        (= 23 (count id))
        (= \- (nth id 14))
        (every? #(Character/isDigit (char %)) (subs id 0 14))
        (every? hex-char? (subs id 15)))))

(m/=> memory-id? [:=> [:cat [:any {:gen/schema [:or MemoryId :string :nil :int]}]] :boolean])

(defn- resolve-var
  "The var at `sym`, or nil when its namespace is not loadable."
  [sym]
  (try (requiring-resolve sym)
       (catch Throwable _ nil)))

(defn- default-scan
  "One window of edges: {:edges [...] :taken n}. :taken counts edge entity ids
   enumerated, so a window shorter than `limit` marks the end of the graph."
  [offset limit]
  (let [eids-by-attr (resolve-var 'hive-mcp.knowledge-graph.connection/eids-by-attr)
        pull-batch   (resolve-var 'hive-mcp.knowledge-graph.edges.queries/pull-edge-batch)]
    (if (and eids-by-attr pull-batch)
      (let [eids (into [] (comp (drop offset) (take limit))
                       (eids-by-attr :kg-edge/id))]
        {:edges (pull-batch eids) :taken (count eids)})
      {:edges [] :taken 0})))

(defn- default-entry-state
  "Whether the memory entry `id` exists: :present, :absent or :unknown.
   Only a nil answer from the store is :absent."
  [id]
  (if-let [lookup (resolve-var 'hive-mcp.vectordb.facade/get-entry-any-store)]
    (try
      (if (nil? (lookup id)) :absent :present)
      (catch Throwable t
        (log/warn t "orphan-edge sweep: entry lookup threw; keeping edge" {:id id})
        :unknown))
    :unknown))

(defn- default-remove-edge!
  [edge-id]
  (if-let [remove! (resolve-var 'hive-mcp.knowledge-graph.edges/remove-edge!)]
    (remove! edge-id)
    false))

(defn find-orphans
  "Orphaned auto-kg edges among `edges`, judged by `entry-state-fn`.
   Looks each distinct source id up once."
  [entry-state-fn edges]
  (let [candidates (filterv #(and (auto-kg-edge? %) (memory-id? (:kg-edge/from %)))
                            edges)
        states     (into {} (map (juxt identity entry-state-fn))
                         (distinct (map :kg-edge/from candidates)))]
    (filterv #(= :absent (get states (:kg-edge/from %))) candidates)))

(m/=> find-orphans [:=> [:cat EntryStateFn [:maybe [:sequential OrphanEdge]]]
                    [:vector OrphanEdge]])

(defn- retract-orphans!
  "Remove up to `cap` of `orphans`. Returns {:pruned n :errors [...]}."
  [remove-fn cap orphans]
  (reduce (fn [acc edge]
            (let [id (:kg-edge/id edge)]
              (try
                (if (remove-fn id)
                  (update acc :pruned inc)
                  acc)
                (catch Throwable t
                  (log/error t "orphan-edge sweep: remove failed" {:edge-id id})
                  (update acc :errors conj {:edge-id id :error (.getMessage t)})))))
          {:pruned 0 :errors []}
          (take cap orphans)))

(defn sweep-window!
  "Scan one window starting at `offset`. Returns
   {:next-offset n :scanned n :orphans n :swept n :errors [...]}.
   Retracts only when `retract?`; otherwise :swept is 0."
  [{:keys [scan-fn entry-state-fn remove-fn retract? scan-window prune-cap]} offset]
  (let [{:keys [edges taken]} (scan-fn offset scan-window)
        orphans (find-orphans entry-state-fn edges)
        {:keys [pruned errors]} (if retract?
                                  (retract-orphans! remove-fn prune-cap orphans)
                                  {:pruned 0 :errors []})]
    {:next-offset (if (< taken scan-window) 0 (+ offset taken))
     :scanned     (count edges)
     :orphans     (count orphans)
     :swept       pruned
     :errors      errors}))

(m/=> sweep-window! [:=> [:cat SweepConfig [:int {:min 0}]] WindowResult])

(defn config
  "Sweep configuration with production defaults for every port left nil."
  [{:keys [scan-fn entry-state-fn remove-fn retract? scan-window prune-cap]}]
  {:scan-fn        (or scan-fn default-scan)
   :entry-state-fn (or entry-state-fn default-entry-state)
   :remove-fn      (or remove-fn default-remove-edge!)
   :retract?       (boolean retract?)
   :scan-window    (or scan-window default-scan-window)
   :prune-cap      (or prune-cap default-prune-cap)})

(m/=> config [:=> [:cat SweepOpts] SweepConfig])

(defrecord OrphanEdgeSweep [opts cursor]
  lifecycle/ISweepable
  (sweep-interval-s [_] 600)
  (sweep-name [_] "kg/orphan-edge")
  (sweep! [_ _ctx]
    (let [cfg (config opts)
          r   (sweep-window! cfg @cursor)]
      (reset! cursor (:next-offset r))
      (when (pos? (:orphans r))
        (log/info "orphan-edge sweep:" (:orphans r) "orphans in window,"
                  (:swept r) "retracted" {:dry-run? (not (:retract? cfg))}))
      {:swept       (:swept r)
       :errors      (:errors r)
       :scanned     (:scanned r)
       :would-prune (:orphans r)
       :dry-run?    (not (:retract? cfg))})))

(defn ->sweep
  "An OrphanEdgeSweep over `opts` (see `config`) with its cursor at 0."
  [opts]
  (->OrphanEdgeSweep opts (atom 0)))

(m/=> ->sweep [:=> [:cat SweepOpts]
               [:fn {:gen/schema [:map [:retract? :boolean]]
                     :gen/fmap #(->OrphanEdgeSweep % (atom 0))}
                #(instance? OrphanEdgeSweep %)]])

(defn backfill
  "One-time pass over the whole graph, a window at a time. Dry-run unless
   `:retract? true`, so the orphan count is reported before anything is
   retracted. Accepts the same port overrides as `config`.
   Returns {:scanned n :orphans n :swept n :errors [...] :dry-run? bool}."
  [& [opts]]
  (let [cfg (config opts)]
    (loop [offset 0
           acc    {:scanned 0 :orphans 0 :swept 0 :errors []}]
      (let [r   (sweep-window! cfg offset)
            acc (-> acc
                    (update :scanned + (:scanned r))
                    (update :orphans + (:orphans r))
                    (update :swept + (:swept r))
                    (update :errors into (:errors r)))]
        (if (zero? (:next-offset r))
          (assoc acc :dry-run? (not (:retract? cfg)))
          (recur (:next-offset r) acc))))))

(m/=> backfill [:=> [:cat [:? SweepOpts]] BackfillResult])

(def ^:private -registered?
  "Auto-registration. `def`, deliberately NOT `defonce`, for the reason given
   on orphan-channel's registration: re-registering the same sweep-name
   overwrites, and a defonce would leave the old record in the registry after
   a hot reload."
  (do (reg/register-sweep!
       (->sweep {:retract? (Boolean/getBoolean "hive.orphan-edge-sweep.retract")}))
      true))

(ns hive-mcp.tools.kg.derived-excess
  "Guarded one-off removal of EXISTING derived-edge excess [NATIVE-RSS-0.4].

   The writers now cap new derived edges (record-co-access! per node,
   add-projection-edges-batch! per synthetic). This ns removes what was
   written before those caps existed:

     :co-accessed  edges past `max-degree` on either endpoint. Selection is
                   greedy by confidence, highest first, so a node keeps its
                   strongest co-access edges and loses the weakest.
     :projects-to  edge residue of synthetics whose node record is gone,
                   delegated to `synthetics/reap-orphans!` (memory
                   20260830173421-2baa1fb0: 674k of 677k edges).

   DRY-RUN BY DEFAULT. Deletion needs ALL of: :dry-run? false, :i-mean-it
   literally true, and a snapshot that answered success BEFORE the first
   retraction. A snapshot that throws or fails refuses the whole call.
   Deletion is bounded by :edge-budget per call."
  (:require [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.edges :as edges]
            [hive-mcp.tools.core :as core :refer [mcp-json mcp-error]]
            [hive-mcp.tools.kg.synthetics :as synthetics]
            [taoensso.timbre :as log]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const default-edge-budget
  "Max edges one apply call may retract, across both classes."
  20000)

(def ^:const default-chunk-size
  "Edges retracted per transaction."
  500)

(def CoEdge
  [:map
   [:kg-edge/id :string]
   [:kg-edge/from :string]
   [:kg-edge/to :string]
   [:kg-edge/confidence {:optional true} [:maybe number?]]])

;; =============================================================================
;; Pure selection
;; =============================================================================

(defn excess-co-access-ids
  "Ids of the :co-accessed edges to drop so no node keeps more than
   `max-degree`. Greedy over edges by confidence descending (ties by id):
   an edge is kept when, with it added, both endpoints stay within the cap
   (a self-loop counts twice on its node), else it is excess. Pure."
  [co-edges max-degree]
  (->> co-edges
       (sort-by (fn [e] [(- (double (or (:kg-edge/confidence e) 0.0))) (:kg-edge/id e)]))
       (reduce (fn [{:keys [deg] :as acc} {:kg-edge/keys [id from to]}]
                 (let [deg' (-> deg
                                (update from (fnil inc 0))
                                (update to (fnil inc 0)))]
                   (if (and (<= (get deg' from) max-degree)
                            (<= (get deg' to) max-degree))
                     (assoc acc :deg deg')
                     (update acc :excess conj id))))
               {:deg {} :excess []})
       :excess))

(m/=> excess-co-access-ids [:=> [:cat [:sequential CoEdge] nat-int?] [:vector :string]])

;; =============================================================================
;; Store reads
;; =============================================================================

(defn- co-access-edges
  "Every :co-accessed edge as {:kg-edge/id :from :to :confidence}."
  []
  (vec (or (conn/query '[:find [(pull ?e [:kg-edge/id :kg-edge/from :kg-edge/to
                                          :kg-edge/confidence]) ...]
                         :where [?e :kg-edge/relation :co-accessed]])
           [])))

(defn- default-snapshot!
  "Full KG backup through the migration tool. Resolved lazily: tools.kg must
   not take a load-time dependency on tools.migration."
  []
  ((requiring-resolve 'hive-mcp.tools.migration/cmd-backup) {:scope :kg}))

(defn- snapshot-ok?
  [r]
  (and (map? r) (or (true? (:success r)) (some? (:path r)))))

;; =============================================================================
;; Guarded entry point
;; =============================================================================

(defn- retract-ids!
  "Retract up to `budget` of `ids` in chunks. Returns the count retracted."
  [ids budget chunk-size]
  (let [quota (take (max 0 budget) ids)]
    (reduce (fn [n chunk]
              (conn/with-tx-batch
                (doseq [id chunk] (edges/remove-edge! id)))
              (+ n (count chunk)))
            0
            (partition-all chunk-size quota))))

(defn prune-derived-excess!
  "Report, and only under the full guard remove, derived-edge excess.

   Opts:
     :max-degree   per-node :co-accessed cap (default
                   edges/default-co-access-max-degree)
     :dry-run?     DEFAULT true; anything but literal false is a dry run
     :i-mean-it    must be literal true to delete
     :snapshot-fn  0-arg fn run BEFORE any retraction; must answer
                   {:success true} or a {:path ...}. Default: KG backup.
     :edge-budget  max edges retracted this call (default 20000)
     :chunk-size   edges per transaction (default 500)

   Returns {:dry-run? :refused? :reason? :plan {...} :snapshot? :deleted?}."
  [& [{:keys [max-degree dry-run? i-mean-it snapshot-fn edge-budget chunk-size]
       :or   {max-degree  edges/default-co-access-max-degree
              dry-run?    true
              edge-budget default-edge-budget
              chunk-size  default-chunk-size}}]]
  (conn/flush-pending!)
  (let [co-edges   (co-access-edges)
        co-excess  (excess-co-access-ids co-edges max-degree)
        orphans    (synthetics/reap-orphans! {:dry-run? true})
        plan       {:max-degree         max-degree
                    :co-accessed-total  (count co-edges)
                    :co-accessed-excess (count co-excess)
                    :orphan-synthetics  (:orphans-total orphans)
                    :orphans-visible?   (:visible? orphans)
                    :edge-budget        edge-budget}
        apply?     (and (false? dry-run?) (true? i-mean-it))
        base       {:dry-run? (not apply?) :refused? false :plan plan}]
    (log/warn "prune-derived-excess! invoked"
              {:dry-run? dry-run? :i-mean-it (boolean i-mean-it) :plan plan})
    (cond
      (and (false? dry-run?) (not (true? i-mean-it)))
      (assoc base :refused? true :reason "dry-run? false without :i-mean-it true; nothing deleted")

      (not apply?)
      base

      :else
      (let [snap (try ((or snapshot-fn default-snapshot!))
                      (catch Exception e {:error (.getMessage e)}))]
        (if-not (snapshot-ok? snap)
          (assoc base :dry-run? true :refused? true :snapshot snap
                 :reason "snapshot did not succeed; nothing deleted")
          (let [co-removed (retract-ids! co-excess edge-budget chunk-size)
                reap       (synthetics/reap-orphans!
                            {:edge-budget (max 0 (- edge-budget co-removed))
                             :chunk-size  chunk-size})]
            (conn/flush-pending!)
            (log/warn "prune-derived-excess! applied"
                      {:co-accessed-removed co-removed
                       :orphan-edges-removed (:edges-removed reap)})
            (assoc base
                   :snapshot snap
                   :deleted {:co-accessed-removed  co-removed
                             :orphans-reaped       (:reaped reap)
                             :orphan-edges-removed (:edges-removed reap)
                             :stopped              (:stopped reap)})))))))

(m/=> prune-derived-excess! [:=> [:cat [:? [:maybe :map]]] :map])

;; =============================================================================
;; MCP boundary
;; =============================================================================

(defn handle-kg-prune-derived-excess
  "kg prune-derived-excess. Dry run unless dry_run is literal false AND
   i_mean_it is literal true; the apply path snapshots the KG first."
  [{:keys [dry_run i_mean_it max_degree limit]}]
  (try
    (cond
      (and max_degree (or (not (integer? max_degree)) (neg? max_degree)))
      (mcp-error "max_degree must be a non-negative integer")

      (and limit (or (not (integer? limit)) (neg? limit)))
      (mcp-error "limit (edge budget) must be a non-negative integer")

      :else
      (mcp-json (assoc (prune-derived-excess!
                        (cond-> {:dry-run? (if (false? dry_run) false true)
                                 :i-mean-it (true? i_mean_it)}
                          max_degree (assoc :max-degree max_degree)
                          limit      (assoc :edge-budget limit)))
                       :success true)))
    (catch Exception e
      (log/error e "kg prune-derived-excess failed")
      (mcp-error (str "prune-derived-excess failed: " (.getMessage e))))))

(m/=> handle-kg-prune-derived-excess [:=> [:cat :map] core/McpResponse])

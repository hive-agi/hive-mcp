(ns hive-mcp.tools.agent.reconcile
  "Reconciles elisp buffer-introspection rows against agent liveness evidence.

   Elisp reports every unregistered `*swarm-<name>*` buffer as a synthetic
   `swarm-<name>-orphan` row. A row survives here only when no evidence says
   the agent is alive: a phantom of a registered agent is dropped, one the
   hivemind has heard from is relabelled `:unregistered`, and the rest keep
   `:orphan` and are hidden unless stale rows are requested.

   Elisp also keeps its own slave hash, which survives a hive JVM restart.
   A row from it whose registry row was retired (boot reconciliation marks
   every rehydrated in-JVM ling `:zombie`) has no live loop in this JVM: it
   becomes `:orphaned` with a reason and the time of its last event, so it
   can never pass for a `:working` ling."
  (:require [clojure.string :as str]
            [hive-dsl.result :refer [rescue]]
            [hive-mcp.hivemind.state :as hm-state]
            [hive-mcp.swarm.datascript.queries :as queries]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Port
;; =============================================================================

(defprotocol ILivenessEvidence
  (evidence [this agent-id]
    "What is known about `agent-id`: :registered (a registry row exists),
     :heard (the hivemind has seen it shout) or :none."))

(defprotocol IRetirementEvidence
  (retired-row [this agent-id]
    "The registry row of `agent-id` when that row is retired (`:alive?`
     false or a dead status), else nil."))

(def dead-statuses
  "Registry statuses that mean no loop backs the agent."
  #{:zombie :terminated :dead :orphaned})

(defn retired?
  "True when a registry row says nothing is alive behind it."
  [row]
  (boolean (and row (or (false? (:slave/alive? row))
                        (contains? dead-statuses (:slave/status row))))))

(defrecord KnownAgents [registered heard retired]
  ILivenessEvidence
  (evidence [_ agent-id]
    (cond
      (contains? registered agent-id) :registered
      (contains? heard agent-id)      :heard
      :else                           :none))
  IRetirementEvidence
  (retired-row [_ agent-id]
    (get retired agent-id)))

(defn ->known-agents
  "Evidence over two id sets and an optional id -> retired registry row map."
  ([registered heard] (->known-agents registered heard {}))
  ([registered heard retired]
   (->KnownAgents (set registered) (set heard) (or retired {}))))

;; =============================================================================
;; Promote (pure)
;; =============================================================================

(def ^:private synthetic-prefix "swarm-")
(def ^:private synthetic-suffix "-orphan")

(defn orphan-row?
  "True for a row elisp minted from buffer introspection."
  [row]
  (= :orphan (:slave/status row)))

(defn- strip-synthetic-id
  [id]
  (when (and (string? id)
             (str/starts-with? id synthetic-prefix)
             (str/ends-with? id synthetic-suffix))
    (subs id (count synthetic-prefix) (- (count id) (count synthetic-suffix)))))

(defn phantom-name
  "The agent id an orphan row stands for: its name, else its synthetic id
   with the `swarm-`/`-orphan` wrapping removed."
  [row]
  (or (not-empty (:slave/name row))
      (strip-synthetic-id (:slave/id row))
      (:slave/id row)))

(defn- row-evidence
  [probe row]
  (let [candidates (distinct (remove nil? [(phantom-name row) (:slave/id row)]))]
    (or (some #{:registered} (map #(evidence probe %) candidates))
        (some #{:heard} (map #(evidence probe %) candidates))
        :none)))

(def orphaned-reason
  "Why an elisp row is reported `:orphaned`."
  "restored from previous JVM, no live loop")

(defn- last-event-ms
  [row]
  (some #(when (number? %) %)
        ((juxt :slave/status-changed-at :slave/last-active-at :slave/created-at) row)))

(defn- ->iso
  [v]
  (cond
    (number? v)               (str (java.time.Instant/ofEpochMilli (long v)))
    (instance? java.util.Date v) (str (.toInstant ^java.util.Date v))
    :else                     v))

(defn- probe-retired-row
  [probe row]
  (when (satisfies? IRetirementEvidence probe)
    (some #(retired-row probe %)
          (distinct (remove nil? [(:slave/id row) (:slave/name row)])))))

(defn orphan-ghost
  "`row`, a live-looking elisp row whose registry row `dead` is retired, as
   an `:orphaned` row carrying the reason and its last event time. The
   registry row fills in the cwd and project elisp lost."
  [row dead]
  (let [last-event (or (last-event-ms dead) (:slave/created-at dead))]
    (cond-> (assoc row
                   :slave/status :orphaned
                   :slave/orphan-reason orphaned-reason)
      (nil? (:slave/cwd row))        (assoc :slave/cwd (:slave/cwd dead))
      (nil? (:slave/project-id row)) (assoc :slave/project-id (:slave/project-id dead))
      last-event                     (assoc :slave/last-event-at (->iso last-event)))))

(defn orphaned-row?
  "True for a row reconciled to `:orphaned`."
  [row]
  (= :orphaned (:slave/status row)))

(defn reconcile-row
  "nil when the row duplicates a registered agent; otherwise the row with a
   status that reflects liveness. A non-orphan row whose registry row is
   retired becomes `:orphaned`; other non-orphan rows pass unchanged."
  [probe row]
  (if-not (orphan-row? row)
    (if-let [dead (probe-retired-row probe row)]
      (orphan-ghost row dead)
      row)
    (case (row-evidence probe row)
      :registered nil
      :heard      (assoc row :slave/status :unregistered)
      row)))

(defn reconcile
  "Reconciled elisp rows. Dead orphans are kept only when `include-stale?`;
   `:orphaned` ghosts are always kept, so status names them as dead."
  [probe rows {:keys [include-stale?]}]
  (->> rows
       (keep #(reconcile-row probe %))
       (filter #(or include-stale? (not (orphan-row? %))))
       vec))

;; =============================================================================
;; Boundary
;; =============================================================================

(defn- registered-ids
  []
  (rescue #{}
          (into #{} (mapcat (juxt :slave/id :slave/name))
                (queries/get-all-slaves :include-stale? true))))

(defn- retired-rows
  []
  (rescue {}
          (into {}
                (comp (filter retired?)
                      (mapcat (fn [r] (for [k (distinct (remove nil? [(:slave/id r) (:slave/name r)]))]
                                        [k r]))))
                (queries/get-all-slaves :include-stale? true))))

(defn- heard-ids
  []
  (rescue #{} (set (keys @(:atom hm-state/agent-registry)))))

(defn live-evidence
  "Evidence read from the swarm registry (every row, stale included), its
   retired rows, and the hivemind agent registry sixth-sense listens to."
  []
  (->known-agents (disj (registered-ids) nil) (heard-ids) (retired-rows)))

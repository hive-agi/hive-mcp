(ns hive-mcp.tools.agent.reconcile
  "Reconciles elisp buffer-introspection rows against agent liveness evidence.

   Elisp reports every unregistered `*swarm-<name>*` buffer as a synthetic
   `swarm-<name>-orphan` row. A row survives here only when no evidence says
   the agent is alive: a phantom of a registered agent is dropped, one the
   hivemind has heard from is relabelled `:unregistered`, and the rest keep
   `:orphan` and are hidden unless stale rows are requested."
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

(defrecord KnownAgents [registered heard]
  ILivenessEvidence
  (evidence [_ agent-id]
    (cond
      (contains? registered agent-id) :registered
      (contains? heard agent-id)      :heard
      :else                           :none)))

(defn ->known-agents
  "Evidence over two id sets."
  [registered heard]
  (->KnownAgents (set registered) (set heard)))

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

(defn reconcile-row
  "nil when the row duplicates a registered agent; otherwise the row with a
   status that reflects liveness. Non-orphan rows pass through unchanged."
  [probe row]
  (if-not (orphan-row? row)
    row
    (case (row-evidence probe row)
      :registered nil
      :heard      (assoc row :slave/status :unregistered)
      row)))

(defn reconcile
  "Reconciled elisp rows. Dead orphans are kept only when `include-stale?`."
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

(defn- heard-ids
  []
  (rescue #{} (set (keys @(:atom hm-state/agent-registry)))))

(defn live-evidence
  "Evidence read from the swarm registry (every row, stale included) and the
   hivemind agent registry sixth-sense listens to."
  []
  (->known-agents (disj (registered-ids) nil) (heard-ids)))

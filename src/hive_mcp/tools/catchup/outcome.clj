(ns hive-mcp.tools.catchup.outcome
  (:require [malli.core :as m]))

(def Status [:enum :ok :degraded :timeout :error])

(def Warning
  [:map
   [:label :string]
   [:status [:enum :degraded :timeout :error]]
   [:message {:optional true} :string]])

(def QueryOutcome
  [:map
   [:status Status]
   [:value {:optional true} :any]
   [:warnings [:vector Warning]]])

(defn ok [value]
  {:status :ok :value value :warnings []})

(defn degraded [value warnings]
  {:status :degraded :value value :warnings (vec warnings)})

(defn failure [status label message]
  {:status status
   :warnings [(cond-> {:status status :label label}
                message (assoc :message message))]})

(defn outcome? [candidate]
  (m/validate QueryOutcome candidate))

(defn available? [outcome]
  (contains? #{:ok :degraded} (:status outcome)))

(defn value-or [outcome fallback]
  (if (available? outcome) (:value outcome) fallback))

(defn summary [outcome]
  (select-keys outcome [:status :warnings]))

(defn with-write-health
  "Pure: fold a write-path health map {:status :warnings :write-path} into a
   memory-status summary. A reachable store (:ok) whose writes are slow
   becomes :degraded; a :timeout/:error status is never softened. The
   :write-path detail (per-stage p50/p95/max, embed-gate and memory-pool
   gauges) rides along. nil write health leaves the summary unchanged.
   Kanban 20260928204307-01ca0962."
  [summary write-health]
  (if-not write-health
    summary
    (let [rank {:ok 0 :degraded 1 :timeout 2 :error 3}
          worse (max-key rank (:status summary :ok) (:status write-health :ok))]
      (cond-> (assoc summary
                     :status worse
                     :warnings (into (vec (:warnings summary)) (:warnings write-health)))
        (:write-path write-health) (assoc :write-path (:write-path write-health))))))

(m/=> with-write-health [:=> [:cat :map [:maybe [:map [:status {:optional true} Status] [:warnings {:optional true} [:sequential Warning]] [:write-path {:optional true} :map]]]] [:map [:status Status] [:warnings [:vector Warning]]]])

(defn map-value [outcome f]
  (if (available? outcome)
    (update outcome :value f)
    outcome))

(m/=> ok [:=> [:cat :any] QueryOutcome])
(m/=> degraded [:=> [:cat :any [:sequential Warning]] QueryOutcome])
(m/=> failure [:=> [:cat [:enum :timeout :error] :string [:maybe :string]] QueryOutcome])
(m/=> outcome? [:=> [:cat :any] :boolean])
(m/=> available? [:=> [:cat QueryOutcome] :boolean])
(m/=> value-or [:=> [:cat QueryOutcome :any] :any])
(m/=> summary [:=> [:cat QueryOutcome] [:map [:status Status] [:warnings [:vector Warning]]]])
(m/=> map-value [:=> [:cat QueryOutcome ifn?] QueryOutcome])
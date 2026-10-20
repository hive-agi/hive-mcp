(ns hive-mcp.saa.registry.planners
  "planner-id → IPlanSynthesizer registry.

   Mirrors registry.phase-providers shape (SRP). Owner = addon-id keyword.
   Synthetic owner :saa/core seeds the NoopPlanSynthesizer at boot.

   Conflict policy:
     same id + same owner      → idempotent silent replace
     same id + different owner  → :saa/registry-conflict warn-log,
                                   first-write-wins"
  (:require [taoensso.timbre :as log]
            [malli.core :as m]
            [hive-mcp.saa.types :as types]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private state
  (atom {:by-id    {}    ;; :saa/default → {:planner rec :owner kw :registered-at inst}
         :by-owner {}})) ;; kw → #{:saa/default}

(defn register!
  "Register an IPlanSynthesizer under owner. Returns :ok | :replaced | :conflict.

   entry shape: {:planner <IPlanSynthesizer>}"
  [owner planner-id entry]
  (let [now (java.time.Instant/now)
        v   (assoc entry :owner owner :registered-at now)]
    (let [outcome (atom :ok)]
      (swap! state
             (fn [{:keys [by-id by-owner]}]
               (let [existing (get by-id planner-id)]
                 (cond
                   (nil? existing)
                   (do (reset! outcome :ok)
                       {:by-id    (assoc by-id planner-id v)
                        :by-owner (update by-owner owner (fnil conj #{}) planner-id)})

                   (= owner (:owner existing))
                   (do (reset! outcome :replaced)
                       {:by-id    (assoc by-id planner-id v)
                        :by-owner by-owner})

                   :else
                   (do (reset! outcome :conflict)
                       {:by-id by-id :by-owner by-owner})))))
      (when (= :conflict @outcome)
        (log/warn "[saa.registry.planners] :saa/registry-conflict"
                  {:planner-id planner-id
                   :existing-owner (:owner (get-in @state [:by-id planner-id]))
                   :rejected-owner owner}))
      @outcome)))

;; owner and id are stored as given; see registry.scorers/register!
(m/=> register! [:=> [:cat :any :any :map] types/RegisterOutcome])

(defn deregister-by-owner!
  "Remove every planner registered by `owner`. Returns set of removed ids."
  [owner]
  (let [removed (atom #{})]
    (swap! state
           (fn [{:keys [by-id by-owner]}]
             (let [ids (get by-owner owner #{})]
               (reset! removed ids)
               {:by-id    (apply dissoc by-id ids)
                :by-owner (dissoc by-owner owner)})))
    @removed))

(m/=> deregister-by-owner! [:=> [:cat :any] [:set :any]])

(defn lookup
  "Return the registered entry for a planner-id, or nil."
  [planner-id]
  (get-in @state [:by-id planner-id]))

(m/=> lookup [:=> [:cat :any] [:maybe types/RegistryRecord]])

(defn all-ids
  "Sorted vector of all registered planner-ids."
  []
  (vec (sort (keys (:by-id @state)))))

(m/=> all-ids [:=> [:cat] [:vector :any]])

(defn snapshot []
  (let [s @state]
    {:version (hash s) :data s}))

(m/=> snapshot [:=> [:cat] types/RegistrySnapshot])

(defn restore!
  "Replace the registry state with a prior `snapshot` value. Test-only."
  [{:keys [data]}]
  (reset! state (or data {:by-id {} :by-owner {}})))

(m/=> restore! [:=> [:cat types/RestoreArg] :map])

(defn reset-for-test! []
  (reset! state {:by-id {} :by-owner {}}))

(m/=> reset-for-test! [:=> [:cat] types/RegistryState])

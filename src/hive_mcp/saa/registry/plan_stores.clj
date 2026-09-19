(ns hive-mcp.saa.registry.plan-stores
  "store-id → plan-store fn registry. The kernel seeds NO plan store: absence
   means SAA runs never persist plans.

   Mirrors registry.planners shape (SRP). Owner = addon-id keyword.

   Store fn contract: (fn [plan agent-id directory])
                      => {:memory-id _ :kanban-ids _ :kg-edges _}

   Conflict policy:
     same id + same owner      → idempotent silent replace
     same id + different owner → :saa/registry-conflict warn-log,
                                 first-write-wins"
  (:require [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private state
  (atom {:by-id    {}    ;; :saa/default → {:store fn :owner kw :registered-at inst}
         :by-owner {}})) ;; kw → #{:saa/default}

(defn register!
  "Register a plan-store fn under owner. Returns :ok | :replaced | :conflict.

   entry shape: {:store (fn [plan agent-id directory])}"
  [owner store-id entry]
  (let [v       (assoc entry :owner owner :registered-at (java.time.Instant/now))
        outcome (atom :ok)]
    (swap! state
           (fn [{:keys [by-id by-owner]}]
             (let [existing (get by-id store-id)]
               (cond
                 (nil? existing)
                 (do (reset! outcome :ok)
                     {:by-id    (assoc by-id store-id v)
                      :by-owner (update by-owner owner (fnil conj #{}) store-id)})

                 (= owner (:owner existing))
                 (do (reset! outcome :replaced)
                     {:by-id    (assoc by-id store-id v)
                      :by-owner by-owner})

                 :else
                 (do (reset! outcome :conflict)
                     {:by-id by-id :by-owner by-owner})))))
    (when (= :conflict @outcome)
      (log/warn "[saa.registry.plan-stores] :saa/registry-conflict"
                {:store-id store-id
                 :existing-owner (:owner (get-in @state [:by-id store-id]))
                 :rejected-owner owner}))
    @outcome))

(defn deregister-by-owner!
  "Remove every plan store registered by `owner`. Returns set of removed ids."
  [owner]
  (let [removed (atom #{})]
    (swap! state
           (fn [{:keys [by-id by-owner]}]
             (let [ids (get by-owner owner #{})]
               (reset! removed ids)
               {:by-id    (apply dissoc by-id ids)
                :by-owner (dissoc by-owner owner)})))
    @removed))

(defn lookup
  "Return the registered entry for a store-id, or nil."
  [store-id]
  (get-in @state [:by-id store-id]))

(defn all-ids
  "Sorted vector of all registered store-ids."
  []
  (vec (sort (keys (:by-id @state)))))

(defn snapshot
  "Immutable value of the registry plus a :version hash."
  []
  (let [s @state]
    {:version (hash s) :data s}))

(defn restore!
  "Replace the registry state with a prior `snapshot` value. Test-only."
  [{:keys [data]}]
  (reset! state (or data {:by-id {} :by-owner {}})))

(defn reset-for-test! []
  (reset! state {:by-id {} :by-owner {}}))

(ns hive-mcp.saa.registry.dispatch-modes
  "execution-mode keyword → Act dispatch fn registry. Dispatch modes are an
   OPEN set: every mode, including the kernel's own :dag-wave, arrives as an
   owner-tagged entry.

   Mirrors registry.planners shape (SRP). Owner = addon-id keyword.
   Synthetic owner :saa/core seeds :dag-wave at boot.

   Dispatch fn contract: (fn [plan agent-id ctx]) => {:wave-id _ :result _}
   where ctx carries :run-id, :plan-memory-id and :directory.

   Conflict policy:
     same mode + same owner      → idempotent silent replace
     same mode + different owner → :saa/registry-conflict warn-log,
                                   first-write-wins"
  (:require [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private state
  (atom {:by-id    {}    ;; :dag-wave → {:dispatch fn :owner kw :registered-at inst}
         :by-owner {}})) ;; kw → #{:dag-wave}

(defn register!
  "Register a dispatch fn for `mode` under owner. Returns :ok | :replaced | :conflict.

   entry shape: {:dispatch (fn [plan agent-id ctx])}"
  [owner mode entry]
  (let [v       (assoc entry :owner owner :registered-at (java.time.Instant/now))
        outcome (atom :ok)]
    (swap! state
           (fn [{:keys [by-id by-owner]}]
             (let [existing (get by-id mode)]
               (cond
                 (nil? existing)
                 (do (reset! outcome :ok)
                     {:by-id    (assoc by-id mode v)
                      :by-owner (update by-owner owner (fnil conj #{}) mode)})

                 (= owner (:owner existing))
                 (do (reset! outcome :replaced)
                     {:by-id    (assoc by-id mode v)
                      :by-owner by-owner})

                 :else
                 (do (reset! outcome :conflict)
                     {:by-id by-id :by-owner by-owner})))))
    (when (= :conflict @outcome)
      (log/warn "[saa.registry.dispatch-modes] :saa/registry-conflict"
                {:mode mode
                 :existing-owner (:owner (get-in @state [:by-id mode]))
                 :rejected-owner owner}))
    @outcome))

(defn deregister-by-owner!
  "Remove every dispatch mode registered by `owner`. Returns set of removed modes."
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
  "Return the registered entry for a mode, or nil."
  [mode]
  (get-in @state [:by-id mode]))

(defn all-ids
  "Sorted vector of all registered modes."
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

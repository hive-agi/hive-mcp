(ns hive-mcp.scheduler.task-dispatch
  "Task dispatch through the :task-dispatch TransportChain.

   The open scheduler hands a task to `dispatch!` and never names how the agent
   is spawned. Selection is the chain from config.edn
   [:transports :task-dispatch :chain], default
   [:hive-agent-swarm :local-headless]:

     :hive-agent-swarm  the dispatcher the closed hive-agent addon registers in
                        hive-spi.swarm.dispatch (grants, budgets, park/resume)
     :local-headless    the open default in this namespace, wrapping
                        hive-mcp.agent.ling/create-ling!, so the DAG works with
                        no hive-agent loaded

   The Result of `dispatch!` is the chain's annotated Result: :via names the
   dispatcher that served the task, :resolved-by says :primary or :fallback and
   :skipped lists what was passed over and why.

   Composition runs from hive-agent to hive-spi only; nothing here requires
   closed code."
  (:require [clojure.string :as str]
            [hive-dsl.result :as r]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.transport.registry :as registry]
            [malli.core :as m]
            [hive-spi.swarm.dispatch :as port]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Pure
;; =============================================================================

(defn ling-id
  "The ling id for a task titled TITLE started at NOW-MS."
  [title now-ms]
  (let [alnum? (fn [c] (or (<= (int \a) (int c) (int \z)) (<= (int \0) (int c) (int \9))))
        safe   (->> (str/lower-case (str (or title "task")))
                    (partition-by alnum?)
                    (map (fn [run] (if (alnum? (first run)) (apply str run) "-")))
                    (apply str))]
    (str "swarm-dag-" (subs safe 0 (min 30 (count safe))) "-" now-ms)))

(defn- blank-role? [role]
  (or (nil? role) (and (string? role) (str/blank? role))))

(defn create-ling-opts
  "The create-ling! opts for TASK under OPTS. :prompt of the task is the
   ling's task text. The task's :role, :tier and :work-class form the
   :spawn/request the :spawn/opts-overlay contributors read (spawn-ctx
   :request), the same request map `agent spawn` hands them, so a DAG spawn
   is routed exactly like a direct one. No such key, no :spawn/request."
  [task opts]
  (let [{:keys [cwd presets project-id prefer-lightweight? run-id max-budget-usd
                parent spawned-by]
         :or   {prefer-lightweight? true}} opts
        role    (:role task)
        request (cond-> {}
                  (not (blank-role? role)) (assoc :role role)
                  (some? (:tier task))       (assoc :tier (:tier task))
                  (some? (:work-class task)) (assoc :work-class (:work-class task)))]
    (cond-> {:cwd           cwd
             :presets       (or presets ["ling"])
             :project-id    project-id
             :kanban-task-id (:task-id task)
             :task          (:prompt task)}
      parent                                  (assoc :parent parent)
      (string? spawned-by)                    (assoc :spawned-by spawned-by)
      run-id                                  (assoc :run-id run-id)
      (and max-budget-usd (pos? max-budget-usd)) (assoc :max-budget-usd max-budget-usd)
      prefer-lightweight?                     (assoc :spawn-mode :headless)
      (seq request)                           (assoc :spawn/request request))))

;; =============================================================================
;; Open default: :local-headless
;; =============================================================================

(defonce ^:private last-id-time (atom 0))

(defn- next-id-time!
  "Wall-clock ms, forced strictly increasing so two dispatches in the same
   millisecond never share a ling id."
  []
  (swap! last-id-time (fn [prev] (max (inc prev) (System/currentTimeMillis)))))

(defrecord LocalHeadlessDispatcher []
  port/ITaskDispatcher
  (dispatch-task! [_ task opts]
    (if-not (port/valid-task? task)
      (port/invalid-task task)
      (let [id (ling-id (:title task) (next-id-time!))]
        (try
          (ling/create-ling! id (create-ling-opts task opts))
          (r/ok {:agent-id id :run-id (:run-id opts)})
          (catch Exception e
            ;; :addressing keeps the chain from marking :local-headless
            ;; degraded: a refused spawn is a verdict on this task, not on
            ;; the dispatcher's availability
            (r/err :dag/spawn-failed
                   {:error/kind :addressing
                    :message    (ex-message e)
                    :agent-id   id}))))))
  (capabilities [_] #{}))

(def local-headless
  "The open :local-headless dispatcher."
  (->LocalHeadlessDispatcher))

;; =============================================================================
;; Chain adapters
;; =============================================================================

(defn- unavailable
  [dispatcher-id]
  (r/err :dispatch/dispatcher-unavailable
         {:error/kind :unavailable
          :message    (str "no dispatcher registered as " dispatcher-id)}))

(defn- registry-adapter
  "Adapter over the hive-spi dispatcher registered as DISPATCHER-ID, resolved
   on every call so a registration or unregistration after boot is honoured."
  [dispatcher-id]
  {:call  (fn [{:keys [task opts]}]
            (if-let [d (port/dispatcher dispatcher-id)]
              (port/dispatch-task! d task opts)
              (unavailable dispatcher-id)))
   :probe (fn []
            (if-let [d (port/dispatcher dispatcher-id)]
              (port/dispatcher-health d)
              (unavailable dispatcher-id)))})

(defmethod registry/open-adapter [:task-dispatch :hive-agent-swarm]
  [_ _]
  (when (port/registered? :hive-agent-swarm)
    (registry-adapter :hive-agent-swarm)))

(defmethod registry/open-adapter [:task-dispatch :local-headless]
  [_ _]
  {:call  (fn [{:keys [task opts]}]
            (port/dispatch-task! local-headless task opts))
   :probe (fn [] (r/ok {:probe? false}))})

;; =============================================================================
;; Facade
;; =============================================================================

(defn dispatch!
  "Dispatch TASK under OPTS through the :task-dispatch chain. Answers the
   chain's Result: (ok {:value {:agent-id :run-id} :via :resolved-by :skipped})
   or an err carrying :via and :skipped."
  ([task opts] (dispatch! task opts nil))
  ([task opts chain-opts]
   (registry/call! :task-dispatch {:task task :opts opts} chain-opts)))

(m/=> ling-id [:=> [:cat [:maybe :string] :int] :string])
(m/=> create-ling-opts [:=> [:cat :map [:maybe :map]] :map])
(m/=> dispatch! [:function
                 [:=> [:cat :map [:maybe :map]] :map]
                 [:=> [:cat :map [:maybe :map] [:maybe :map]] :map]])

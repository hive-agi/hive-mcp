(ns hive-mcp.tools.kanban.port
  "Core's provider for the hive-contracts kanban ports.

   `CoreKanban` implements IKanbanRead and IKanbanWrite over the kanban
   domain (list plan, facade, move event bus, create handler) and speaks the
   port's public status vocabulary. `register!` installs it under
   :IKanbanRead and :IKanbanWrite in hive-contracts.registry; consumers
   resolve the provider through that registry on every call."
  (:require [clojure.data.json :as json]
            [clojure.set :as set]
            [clojure.string :as str]
            [hive-contracts.kanban :as kanban]
            [hive-contracts.registry :as contracts]
            [hive-dsl.result :as r :refer [rescue]]
            [hive-mcp.tools.kanban.events :as kanban-events]
            [hive-mcp.tools.kanban.predicates :as kp]
            [hive-mcp.tools.kanban.transitions :as kt]
            [hive-mcp.tools.memory-kanban :as memory-kanban]
            [hive-mcp.tools.memory-kanban.query :as query]
            [hive-mcp.vectordb.kanban-facade :as kanban-facade]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Vocabulary: storage tag -> public status
;; =============================================================================

(def ^:private tag->status
  (set/map-invert kp/status-enum->tag))

(defn- public-status
  "The port's spelling of a stored status; nil for an unknown value."
  [s]
  (when s (get tag->status (str s))))

(defn- ->task
  "A slim board row in the port's Task shape."
  [slim]
  (cond-> slim
    (contains? slim :status) (update :status public-status)))

(defn- entry->task
  "A full store entry in the port's Task shape."
  [entry]
  (let [content (:content entry)]
    (-> (kt/task->slim entry true)
        ->task
        (assoc :description (kt/content-val content :description nil)
               :tags (vec (:tags entry))))))

;; =============================================================================
;; Read side
;; =============================================================================

(defn- list-params
  "ListQuery -> the list handler's parameter map."
  [{:keys [status directory project-id include-descendants? scope tags limit]}]
  (cond-> {}
    status                      (assoc :status status)
    directory                   (assoc :directory directory)
    project-id                  (assoc :project_id project-id)
    (some? include-descendants?) (assoc :include_descendants include-descendants?)
    scope                       (assoc :scope scope)
    (seq tags)                  (assoc :tags (vec tags))
    limit                       (assoc :limit limit)))

;; =============================================================================
;; Write side
;; =============================================================================

(defn- write-error
  [default-code res]
  (let [code (:error res)]
    {:err {:error   (if (keyword? code) code default-code)
           :message (str (or (:message res) code default-code))}}))

(defn- parse-create-text
  "CreateResult from the create handler's text payload: JSON with an :id, a
   {:success? false} rejection, or the bare id an idempotent hit returns."
  [text]
  (let [parsed (when (string? text) (rescue nil (json/read-str text :key-fn keyword)))
        id     (when (map? parsed) (or (:id parsed) (get parsed "id")))]
    (cond
      (and (map? parsed) (false? (:success? parsed)))
      {:err {:error   :kanban/backend-rejected
             :message (str (:error parsed)
                           (when-let [retry (:retry-after parsed)]
                             (str " (retry after " retry "ms)")))}}

      id
      {:ok {:id (str id)}}

      (and (nil? parsed) (string? text) (not (str/blank? text)))
      {:ok {:id text}}

      :else
      {:err {:error :kanban/no-id :message (pr-str text)}})))

(defn- create-params
  "CreateRequest -> the create handler's parameter map."
  [{:keys [title description priority status tags directory agent-id]}]
  (cond-> {:title title}
    description (assoc :description description)
    priority    (assoc :priority priority)
    status      (assoc :status status)
    (seq tags)  (assoc :tags (vec tags))
    directory   (assoc :directory directory)
    agent-id    (assoc :agent_id agent-id)))

;; =============================================================================
;; Provider
;; =============================================================================

(defrecord CoreKanban []
  kanban/IKanbanRead
  (list-tasks [_ query]
    (rescue []
      (mapv ->task (query/list-slim-data (list-params query)))))
  (get-task [_ id]
    (rescue nil
      (when-let [entry (kanban-facade/get-entry-by-id id)]
        (entry->task (assoc entry :id (or (:id entry) id))))))

  kanban/IKanbanWrite
  (transition! [_ {:keys [task-id new-status directory]}]
    (try
      (let [res (kanban-events/dispatch-move! {:task-id    task-id
                                               :new-status new-status
                                               :directory  directory})]
        (if (r/ok? res)
          (let [{:keys [content tags]} (get-in res [:ok :kanban/facade-update :payload])]
            {:ok (->task (kt/task->slim {:id task-id :content content :tags tags}))})
          (write-error :kanban/move-failed res)))
      (catch Throwable t
        {:err {:error :kanban/move-failed :message (or (ex-message t) (str (class t)))}})))
  (create-task! [_ request]
    (try
      (let [res (memory-kanban/handle-mem-kanban-create (create-params request))]
        (if (:isError res)
          {:err {:error :kanban/create-failed :message (str (:text res))}}
          (parse-create-text (:text res))))
      (catch Throwable t
        {:err {:error :kanban/create-failed :message (or (ex-message t) (str (class t)))}}))))

(defn register!
  "Install core's provider under :IKanbanRead and :IKanbanWrite. Returns it."
  []
  (let [impl (->CoreKanban)]
    (contracts/register! :IKanbanRead impl)
    (contracts/register! :IKanbanWrite impl)
    impl))

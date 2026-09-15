(ns hive-mcp.swarm.datascript.coordination.session-registry
  "Session-scoped registry of what a run produced, for wrap to harvest.

   SCOPING: every row carries the session that wrote it. A reader passes a
   `:session-ref` (plus `:parent-of`) and gets back only the rows that session
   owns under the HCR rules in hive-mcp.session.identity -- its own, its
   descendants', and any ad-hoc session it adopted. Reads WITHOUT a
   :session-ref keep the old unscoped behaviour so existing callers are
   unaffected, but clearing is never unscoped by accident: clear-* takes the
   explicit ids the caller actually harvested."
  (:require [datascript.core :as d]
            [hive-mcp.session.identity :as sid]
            [hive-mcp.swarm.datascript.connection :as conn]
            [taoensso.timbre :as log]))

(declare register-completed-task! get-completed-task get-completed-tasks-this-session clear-completed-tasks! register-kanban-movement! get-kanban-movements-this-session clear-kanban-movements!)

(defn- scope-rows
  "Keep only the rows `session-ref` owns, per hive-mcp.session.identity. With no
   session-ref the seq passes through unchanged (unscoped legacy read)."
  [{:keys [session-ref parent-of]} session-key rows]
  (if-not (sid/valid? session-ref)
    rows
    (sid/harvestable {:parent-of    (or parent-of {})
                      :row->session session-key}
                     session-ref
                     rows)))

(defn register-completed-task!
  "Register a completed task for wrap to harvest.

   Called by on-kanban-done hook when tasks move to DONE.
   Tasks are session-scoped and cleared after wrap.

   Arguments:
     task-id - Unique identifier (e.g., kanban task ID)
     opts    - Map with optional keys:
               :title      - Task title/description
               :agent-id   - ID of completing agent (auto-detected if not provided)
               :project-id - Project scope
               :session-id - Session that recorded it; without it the row is
                             unowned and no wrap will clear it

   Returns:
     Transaction report"

  [task-id {:keys [title agent-id project-id session-id]}]
  {:pre [(string? task-id)]}
  (let [c (conn/ensure-conn)
        ;; Auto-detect agent-id from environment if not provided
        auto-agent-id (or agent-id (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
        tx-data (cond-> {:completed-task/id task-id
                         :completed-task/completed-at (conn/now)}
                  title (assoc :completed-task/title title)
                  auto-agent-id (assoc :completed-task/agent-id auto-agent-id)
                  session-id (assoc :completed-task/session-id session-id)
                  project-id (assoc :completed-task/project-id project-id))]
    (log/debug "Registering completed task:" task-id "title:" title "project-id:" project-id)
    (d/transact! c [tx-data])))

(defn get-completed-task
  "Get a completed task by ID.

   Returns:
     Map with completed-task attributes or nil if not found"
  [task-id]
  (let [c (conn/ensure-conn)
        db @c]
    (when-let [e (d/entity db [:completed-task/id task-id])]
      (-> (into {} e)
          (dissoc :db/id)))))

(defn get-completed-tasks-this-session
  "Get all completed tasks registered this session.

   Used by wrap to harvest task completions.

   Options:
   - :agent-id    - Filter by specific agent
   - :project-id  - Filter by project scope
   - :session-ref - SessionRef of the caller; when given, only rows this
                    session OWNS come back (its own subtree plus adopted
                    ad-hoc sessions). Without it the read is unscoped.
   - :parent-of   - session-id -> parent session-id, for the ownership walk

   Returns:
     Seq of completed-task maps sorted by completion time (most recent first)"
  [& {:keys [agent-id project-id session-ref parent-of]}]
  (let [c (conn/ensure-conn)
        db @c
        ;; Query all completed tasks
        all-tasks (d/q '[:find [(pull ?e [*]) ...]
                         :where [?e :completed-task/id _]]
                       db)]
    (->> all-tasks
         (scope-rows {:session-ref session-ref :parent-of parent-of}
                     :completed-task/session-id)
         ;; Filter by agent-id if provided
         (filter (fn [task]
                   (or (nil? agent-id)
                       (= agent-id (:completed-task/agent-id task)))))
         ;; Filter by project-id if provided (HCR scope isolation)
         (filter (fn [task]
                   (or (nil? project-id)
                       (= project-id (:completed-task/project-id task)))))
         ;; Sort by completion time (most recent first)
         (sort-by :completed-task/completed-at #(compare %2 %1))
         ;; Clean up output format
         (map (fn [task]
                (dissoc task :db/id))))))

(defn clear-completed-tasks!
  "Clear completed tasks from the registry after wrap has harvested them.

   PREFER the 1-arity: pass the ids this wrap actually harvested, so a
   concurrent session's unharvested rows survive. The 0-arity clears EVERY row
   in the store regardless of who wrote it -- it is the reset button, not the
   end of a wrap.

   Returns:
     Number of tasks cleared"
  ([task-ids]
   (let [c (conn/ensure-conn)
         db @c
         ids (set (remove nil? task-ids))
         eids (when (seq ids)
                (d/q '[:find [?e ...]
                       :in $ ?ids
                       :where
                       [?e :completed-task/id ?id]
                       [(contains? ?ids ?id)]]
                     db ids))
         count-cleared (count eids)]
     (when (seq eids)
       (log/debug "Clearing" count-cleared "harvested completed tasks")
       (d/transact! c (mapv (fn [eid] [:db/retractEntity eid]) eids)))
     count-cleared))
  ([]
   (let [c (conn/ensure-conn)
         db @c
         eids (d/q '[:find [?e ...]
                     :where [?e :completed-task/id _]]
                   db)
         count-cleared (count eids)]
     (when (seq eids)
       (log/debug "Clearing" count-cleared "completed tasks (UNSCOPED)")
       (d/transact! c (mapv (fn [eid] [:db/retractEntity eid]) eids)))
     count-cleared)))

(defn register-kanban-movement!
  "Register a kanban status transition for wrap to harvest.

   Called by kanban create/move handlers to track all status changes,
   not just completions. Session-scoped and cleared after wrap.

   Arguments:
     opts - Map with keys:
            :task-id    - Kanban task ID
            :title      - Task title
            :from       - Previous status (nil for creation)
            :to         - New status
            :agent-id   - Agent that triggered the move (auto-detected)
            :project-id - Project scope
            :session-id - Session that recorded it (see register-completed-task!)

   Returns:
     Transaction report"
  [{:keys [task-id title from to agent-id project-id session-id]}]
  {:pre [(string? task-id) (string? to)]}
  (let [c (conn/ensure-conn)
        auto-agent-id (or agent-id (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
        movement-id (str "mv-" (System/currentTimeMillis) "-" (subs (str (java.util.UUID/randomUUID)) 0 8))
        tx-data (cond-> {:kanban-movement/id movement-id
                         :kanban-movement/task-id task-id
                         :kanban-movement/to to
                         :kanban-movement/at (conn/now)}
                  title (assoc :kanban-movement/title title)
                  from (assoc :kanban-movement/from from)
                  auto-agent-id (assoc :kanban-movement/agent-id auto-agent-id)
                  session-id (assoc :kanban-movement/session-id session-id)
                  project-id (assoc :kanban-movement/project-id project-id))]
    (log/debug "Registering kanban movement:" task-id from "->" to "project-id:" project-id)
    (d/transact! c [tx-data])))

(defn get-kanban-movements-this-session
  "Get all kanban movements registered this session.

   Used by wrap to harvest status transitions.

   Options:
   - :agent-id    - Filter by specific agent
   - :project-id  - Filter by project scope
   - :session-ref - SessionRef of the caller; scopes the read to what this
                    session owns (see get-completed-tasks-this-session)
   - :parent-of   - session-id -> parent session-id, for the ownership walk

   Returns:
     Seq of kanban-movement maps sorted by timestamp (chronological)"
  [& {:keys [agent-id project-id session-ref parent-of]}]
  (let [c (conn/ensure-conn)
        db @c
        all-movements (d/q '[:find [(pull ?e [*]) ...]
                             :where [?e :kanban-movement/id _]]
                           db)]
    (->> all-movements
         (scope-rows {:session-ref session-ref :parent-of parent-of}
                     :kanban-movement/session-id)
         (filter (fn [mv]
                   (or (nil? agent-id)
                       (= agent-id (:kanban-movement/agent-id mv)))))
         (filter (fn [mv]
                   (or (nil? project-id)
                       (= project-id (:kanban-movement/project-id mv)))))
         (sort-by :kanban-movement/at)
         (map (fn [mv] (dissoc mv :db/id))))))

(defn clear-kanban-movements!
  "Clear kanban movements after wrap has harvested them.

   PREFER the 1-arity with the ids this wrap harvested; the 0-arity clears the
   whole store regardless of owner (see clear-completed-tasks!).

   Returns:
     Number of movements cleared"
  ([movement-ids]
   (let [c (conn/ensure-conn)
         db @c
         ids (set (remove nil? movement-ids))
         eids (when (seq ids)
                (d/q '[:find [?e ...]
                       :in $ ?ids
                       :where
                       [?e :kanban-movement/id ?id]
                       [(contains? ?ids ?id)]]
                     db ids))
         count-cleared (count eids)]
     (when (seq eids)
       (log/debug "Clearing" count-cleared "harvested kanban movements")
       (d/transact! c (mapv (fn [eid] [:db/retractEntity eid]) eids)))
     count-cleared))
  ([]
   (let [c (conn/ensure-conn)
         db @c
         eids (d/q '[:find [?e ...]
                     :where [?e :kanban-movement/id _]]
                   db)
         count-cleared (count eids)]
     (when (seq eids)
       (log/debug "Clearing" count-cleared "kanban movements (UNSCOPED)")
       (d/transact! c (mapv (fn [eid] [:db/retractEntity eid]) eids)))
     count-cleared)))

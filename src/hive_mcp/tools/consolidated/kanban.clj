(ns hive-mcp.tools.consolidated.kanban
  "Consolidated Kanban CLI tool.

   Loading this namespace is what makes core the kanban CONTRIBUTOR: it
   installs core's IKanbanRead / IKanbanWrite provider in
   hive-spi.kanban.registry and the :kanban catchup block in
   hive-spi.catchup.registry. An addon that owns the board does the same at
   its initialize! and replaces both."
  (:require [clojure.tools.logging :as log]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.cli :refer [make-cli-handler make-batch-handler]]
            [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.tools.kanban.catchup-block :as catchup-block]
            [hive-mcp.tools.kanban.port :as port]
            [hive-mcp.tools.memory-kanban :as mem-kanban]
            [hive-mcp.plan.tool :as plan-tool]
            [hive-mcp.resilience.swallow :as swallow])
  (:import [java.time Instant]))

(port/register!)
(catchup-block/register!)

(def ^:private batch-allowed-handlers
  "Handlers reachable from a kanban batch operation.

   Restricted to ID-keyed mutating commands — read ops (`list`, `status`)
   make no sense per-op. Adding more commands here is a deliberate decision.

   PR4.4 — :create added so batch-create can sweep many task titles at once.
   :retag added — scope-move + ±tags batches preserve entry id + KG edges.

   Stored as VARS so a reload reaches the table (20260817195749-0d407e9c).
   `make-batch-handler` reads `(keys handlers)` for its deprecation warning,
   routes through `resolve-handler`, and calls what it finds, so all three
   sites take a var."
  {:update #'mem-kanban/handle-mem-kanban-move
   :delete #'mem-kanban/handle-mem-kanban-delete
   :create #'mem-kanban/handle-mem-kanban-create
   :retag  #'mem-kanban/handle-mem-kanban-retag})

(defn- with-default-command
  "Set :command on each op only when missing. Never overwrites a caller's
   explicit value — that was the silent-misroute bug (audit 20260429203443)."
  [operations default-cmd]
  (mapv (fn [op] (update op :command #(or % default-cmd))) operations))

(defn- handle-batch-update
  "Batch handler defaulting omitted :command to \"update\".

   Per-op :command is respected when supplied — pass :command \"delete\" to
   mix delete ops in. Unknown commands fail loudly per-op via the inner
   make-batch-handler (no silent misroute to update).

   Backward-compatible: callers passing only :task_id + :new_status keep
   working — :update fills in."
  [{:keys [operations] :as params}]
  (let [inner (make-batch-handler batch-allowed-handlers)]
    (inner (assoc params :operations (with-default-command operations "update")))))

(defn- handle-batch-delete
  "Batch handler defaulting omitted :command to \"delete\".

   Mirror of handle-batch-update. Use when sweeping many task ids:

     {:command \"batch-delete\"
      :operations [{:task_id \"id-1\"} {:task_id \"id-2\"} ...]}"
  [{:keys [operations] :as params}]
  (let [inner (make-batch-handler batch-allowed-handlers)]
    (inner (assoc params :operations (with-default-command operations "delete")))))

(defn- handle-batch-create
  "Batch handler defaulting omitted :command to \"create\".

   Sweep many titles at once:

     {:command \"batch-create\"
      :operations [{:title \"task-1\" :description \"...\"}
                   {:title \"task-2\"} ...]}"
  [{:keys [operations] :as params}]
  (let [inner (make-batch-handler batch-allowed-handlers)]
    (inner (assoc params :operations (with-default-command operations "create")))))

(defn- handle-batch-retag
  "Batch handler defaulting omitted :command to \"retag\".

   Sweep many task scope-moves in one call:

     {:command \"batch-retag\"
      :operations [{:task_id \"id-1\" :new_project_id \"probe\"}
                   {:task_id \"id-2\" :new_project_id \"probe\"
                    :add_tags [\"epic:adapter\"]}]}"
  [{:keys [operations] :as params}]
  (let [inner (make-batch-handler batch-allowed-handlers)]
    (inner (assoc params :operations (with-default-command operations "retag")))))

;; =============================================================================
;; Timeline / evolution — thin, var-backed forwarders onto the hive-knowledge
;; extensions (:tl/timeline, :tl/evolution).
;;
;; The read model lives in the addon; this side owns only what a caller sees:
;; the parameter shape, the compact row projection and the one error a
;; caller can act on (the extension is not mounted). Both handlers re-resolve
;; the extension per call, so mounting or evicting the addon is picked up
;; without a reload.
;; =============================================================================

(def ^:private timeline-unavailable
  "timeline and evolution are provided by the hive-knowledge addon; it is not mounted.")

(defn- extension
  "The registered extension fn under `k`, or nil."
  [k]
  (swallow/attempt (ext/get-extension k)))

(defn- iso
  "Epoch ms -> ISO-8601 UTC, or nil."
  [ms]
  (when (integer? ms)
    (str (Instant/ofEpochMilli (long ms)))))

(defn- compact-event
  "One timeline event, projected: instants as ISO text, keywords as names,
   absent sides dropped."
  [{:keys [at kind from to source]}]
  (cond-> {:at (iso at) :kind (name kind)}
    from   (assoc :from (name from))
    to     (assoc :to (name to))
    source (assoc :source (name source))))

(defn- compact-timeline
  "A schema/Timeline as the caller sees it: the task identity, the ordered
   events and the derived metrics. The title is kept — a timeline with no
   title is unreadable — the scope is not (the caller named it)."
  [{:keys [task events derived]}]
  {:id          (:id task)
   :title       (:title task)
   :status      (some-> (:status task) name)
   :events      (mapv compact-event events)
   :lead_time_ms      (:lead-time-ms derived)
   :cycle_time_ms     (:cycle-time-ms derived)
   :reopened          (:reopened? derived)
   :touches           (:touches derived)})

(defn- compact-evolution
  "A schema/Evolution as the caller sees it: scope, window, totals and the
   non-empty months. A dense all-zero month is noise over six months."
  [{:keys [scope from to tasks months totals]}]
  {:scope  (if (set? scope) (vec (sort scope)) scope)
   :from   from
   :to     to
   :tasks  tasks
   :totals totals
   :months (into [] (remove #(every? zero? ((juxt :created :started :done) %))) months)})

(defn- forward
  "Call extension `k` with `params`; an absent extension, a refusal
   ({:error kw :message s}) and a throw all become one MCP error."
  [k params compact]
  (if-let [f (extension k)]
    (let [result (try (f params)
                      (catch Throwable t
                        {:error :tl/failed :message (or (ex-message t) (str (class t)))}))]
      (if (:error result)
        (mcp-error (str "kanban " (name k) ": " (str (:error result)) " — " (:message result)))
        (mcp-json (compact result))))
    (mcp-error timeline-unavailable)))

(defn- handle-kanban-timeline
  "[timeline] One task's history: created/started/moved/done/deleted/archived
   events with their source, plus lead and cycle time. Points at the
   hive-knowledge :tl/timeline extension."
  [params]
  (forward :tl/timeline
           (cond-> {:task_id (:task_id params)}
             (some? (:dedupe_window_ms params))
             (assoc :dedupe-window-ms (:dedupe_window_ms params)))
           compact-timeline))

(defn- handle-kanban-evolution
  "[evolution] A scope's per-month created/started/done histogram over a
   window (from/to as YYYY-MM, or window like \"6m\"). Points at the
   hive-knowledge :tl/evolution extension."
  [params]
  (forward :tl/evolution
           {:project_id (:project_id params)
            :from       (:from params)
            :to         (:to params)
            :window     (:window params)
            :zone       (:zone params)}
           compact-evolution))

(def ^:private canonical-handlers
  "The `kanban` verbs, stored as VARS so a reload reaches this table
   (20260817195749-0d407e9c). The deprecated aliases below read their target
   out of here and close over it; closing over a var is what makes an alias
   forward to the CURRENT handler rather than to the one this load interned.

   `:sync` is the one entry with nothing to quote: an inline `fn` returning a
   constant, because this backend has nothing to sync."
  {:list           #'mem-kanban/handle-mem-kanban-list-slim
   :get            #'mem-kanban/handle-mem-kanban-get
   :create         #'mem-kanban/handle-mem-kanban-create
   :update         (fn [params]
                     (if (contains? params :operations)
                       (handle-batch-update params)
                       (mem-kanban/handle-mem-kanban-move params)))
   :delete         #'mem-kanban/handle-mem-kanban-delete
   :history        #'mem-kanban/handle-mem-kanban-history
   :restore        #'mem-kanban/handle-mem-kanban-restore
   :status         #'mem-kanban/handle-mem-kanban-stats
   :retag          #'mem-kanban/handle-mem-kanban-retag
   :timeline       #'handle-kanban-timeline
   :evolution      #'handle-kanban-evolution
   :sync           (fn [_] {:success true :message "Memory kanban is single-backend, no sync needed"})
   :plan-to-kanban #'plan-tool/handle-plan-to-kanban
   :plan-schema    #'plan-tool/handle-plan-schema
   :batch-update   #'handle-batch-update
   :batch-delete   #'handle-batch-delete
   :batch-create   #'handle-batch-create
   :batch-retag    #'handle-batch-retag})

(def ^:private deprecated-aliases
  {:move     :update
   :roadmap  :status
   :my-tasks :list})

(defn- wrap-deprecated
  [alias-key canonical-key handler-fn]
  (fn [params]
    (log/warn {:event :deprecation-warning
               :command (name alias-key)
               :canonical (name canonical-key)
               :message (str "DEPRECATED: '" (name alias-key)
                             "' is deprecated. Use '" (name canonical-key)
                             "' instead.")})
    (handler-fn params)))

(def handlers
  (merge canonical-handlers
         (reduce-kv (fn [m alias-key canonical-key]
                      (assoc m alias-key
                             (wrap-deprecated alias-key canonical-key
                                              (get canonical-handlers canonical-key))))
                    {}
                    deprecated-aliases)))

(def handle-kanban
  (make-cli-handler #'handlers))

(def tool-def
  {:name "kanban"
   :consolidated true
   :description "Kanban task management: list (all/filtered tasks), get (single task/entry by id — unified across the kanban store AND the default memory store, since they are separate backends; surfaces full fields + KG edges; on miss returns semantic-search suggestions), create (new task), update (move status via new_status and/or edit title/description/priority and/or add_tags/remove_tags, all in ONE commit; a replaced description is kept in the card's description-history, append_description adds a note instead of replacing), history (a task's earlier descriptions, numbered rev 1.. oldest first, with replaced-at and agent), restore (put revision rev from history back as the description; the replaced text joins the history), delete (hard-remove task by id; no archival, no completion — use for duplicates/cancellations), retag (scope-move via project_id + optional ±tags; preserves entry id + KG edges, no re-embed), status (board overview + milestones), sync (backends), plan-to-kanban (convert plan to tasks, supports plan_id or plan_path; plan_id must name a type=plan memory, an already-converted active plan answers already-converted, force re-converts a spent one), plan-schema (the plan-memory EDN contract: JSON-schema + example + how-to, so a type=plan memory can be authored without reading source), batch-update (bulk status changes; per-op :command respected — pass :command \"delete\" inside an op to mix delete in), batch-delete (sweep many task_ids; mirror of batch-update), batch-retag (sweep many scope-moves). Aliases (deprecated): move→update, roadmap→status, my-tasks→list. Use command='help' to list all. HCR: a list shows its own scope + ANCESTORS (parent tasks, always — 'child sees parent'); include_descendants=true (default) also aggregates DESCENDANT (child) project tasks; scope=\"all\" lifts the project filter entirely for a cross-workspace whole-board view. List filters: status (one status, or \"all\" for every status; without status done cards are hidden), query (substring), tags (extra required tags), tag_match (any|all), created_after / updated_after (ISO-8601), limit / offset (pagination), fields (projection). Session todos: cards tagged session-todo (an agent's private todo_write step list) are HIDDEN by default from list, status and the catchup kanban block; opt in with include_session_todos=true or by naming the session-todo tag in tags. timeline (ONE task's history: created/started/moved/done/deleted/archived events with their source, plus lead and cycle time; task_id), evolution (a scope's per-month created/started/done histogram over a window; project_id + from/to as YYYY-MM or window like \"6m\"). Both come from the hive-knowledge addon and error when it is not mounted."   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["list" "get" "create" "move" "status" "update" "delete" "history" "restore" "retag" "roadmap" "my-tasks" "sync" "plan-to-kanban" "plan-schema" "batch-update" "batch-delete" "batch-create" "batch-retag" "timeline" "evolution" "help"]
                                         :description "Kanban operation to perform"}
                              "status" {:type "string"
                                        :enum ["todo" "inprogress" "inreview" "done" "all"]
                                        :description "[list] Filter by task status. status=\"all\" returns EVERY status, done included: with tags, one call is a run's result table (list tags=[run:<id>] status=all). Without status the board hides done cards. [create] Initial status (not all)."}
                              "title" {:type "string"
                                       :description "[create|update] Task title. On update a non-blank value replaces the current title in place — entry id, KG edges, status and scope are preserved."}
                              "steal_claim" {:type "boolean"
                                             :description "[update] A move to inprogress claims the card for you (owner + since on the card); a card another live agent or coordinator claims is refused, naming the holder. true takes it over. todo/done clear the claim."}
                              "description" {:type "string"
                                             :description "[create|update] Task description. On update a non-blank value replaces the current description; the old text is kept in the card's description-history (see history/restore). To add a note, use append_description."}
                              "append_description" {:type "string" :description "[update] Append text to the existing description after a blank line, without replacing it. Applied after description when both are given."}
                              "rev" {:type "integer" :description "[restore] Revision number from `history` (1 = oldest) to put back as the description"}
                              "task_id" {:type "string"
                                         :description "Task ID to get/move/update/retag/delete"}
                              "id" {:type "string"
                                    :description "[get] Alias for task_id — entry id to fetch (kanban store or default memory store)"}
                              "new_status" {:type "string"
                                            :enum ["todo" "inprogress" "inreview" "done"]
                                            :description "Target status for move"}
                              "new_project_id" {:type "string"
                                                :description "[retag] Target project scope (replaces existing scope:project:* tag, preserves entry id + KG edges)"}
                              "add_tags" {:type "array" :items {:type "string"}
                                          :description "[retag|update] Extra tags to add (deduplicated). On update they are written in the SAME commit as new_status/title/description/priority, so a verdict close is one call: update new_status=done add_tags=[verdict:pass]"}
                              "remove_tags" {:type "array" :items {:type "string"}
                                             :description "[retag|update] Tags to remove (applied after add; on update, in the same commit as new_status)"}
                              "plan_id" {:type "string"
                                         :description "Memory entry ID containing the plan (for plan-to-kanban). Call command='plan-schema' for the plan EDN contract (schema + example + how-to)."}
                              "plan_path" {:type "string"
                                           :description "File path to a plan file (alternative to plan_id for plan-to-kanban). Slurps file content directly — zero-token plan loading for large plans."}
                              "force" {:type "boolean"
                                       :description "[plan-to-kanban] Convert even when the plan is done: a plan that is completed or superseded refuses without it, and an ACTIVE plan with task ids answers {already-converted true} with the ids it already has instead of posting a duplicate board. A plan_path has no entry to remember, so it converts every time."}
                              "operations" {:type "array"
                                            :items {:type "object"
                                                    :properties {"command"        {:type "string"
                                                                                   :enum ["update" "delete" "create" "retag"]
                                                                                   :description "Per-op command override; defaults to the wrapper's verb"}
                                                                 "task_id"        {:type "string"}
                                                                 "new_status"     {:type "string"
                                                                                   :enum ["todo" "inprogress" "inreview" "done"]}
                                                                 "new_project_id" {:type "string"}
                                                                 "add_tags"       {:type "array" :items {:type "string"}}
                                                                 "remove_tags"    {:type "array" :items {:type "string"}}
                                                                 "title"          {:type "string"}
                                                                 "description"    {:type "string"}
                                                                 "append_description" {:type "string"}
                                                                 "priority"       {:type "string"
                                                                                   :enum ["high" "medium" "low"]}}}
                                            :description "Array of operations for batch-update / batch-delete / batch-create / batch-retag. Each op may specify :command to mix verbs in one batch; otherwise the wrapper's default applies."}
                              "directory" {:type "string"
                                           :description "Working directory for project scope (auto-detected if not provided)"}
                              "include_descendants" {:type "boolean"
                                                     :description "Include DESCENDANT (child) project tasks in results (HCR Wave 4). Default true — set false to restrict to current scope + ancestors only. Ancestor (parent) tasks are ALWAYS included regardless of this flag ('child sees parent')."}
                              "include_ancestors" {:type "boolean"
                                                   :description "[list/status] Include ANCESTOR (parent) project tasks. Default true ('child sees parent'). Set false from a leaf scope to list only its own board (+ descendants when include_descendants)."}
                              "scope" {:type "string"
                                       :enum ["all"]
                                       :description "[list/status] scope=\"all\" lifts the project filter entirely — returns the whole board across EVERY workspace (opt-in cross-workspace view). Omit for the default scoped view (current project + ancestors [+ descendants])."}
                              "project_id" {:type "string"
                                            :description "[list] Exact-match project filter (overrides directory-derived scope). [create] Project the card lands in (overrides directory; a conflicting scope:project tag is refused). [retag] Alias for new_project_id."}
                              "query" {:type "string"
                                       :description "[list] Case-insensitive substring match on title + description. [get] Fallback semantic-search term when the id misses in both stores (defaults to the id itself)"}
                              "tags" {:type "array" :items {:type "string"}
                                      :description "[list] Extra required tags beyond ['kanban' status]"}
                              "tag_match" {:type "string"
                                           :enum ["any" "all"]
                                           :description "[list] Tag match semantics for `tags` (default 'all')"}
                              "include_session_todos" {:type "boolean"
                                                       :description "[list|status] Include cards tagged `session-todo` (an agent's private todo_write step list, mirrored by hive-agent). Default false: they are hidden from list, status and the catchup kanban block. Naming the `session-todo` tag in `tags` also opts in. Only true (or the text \"true\") opts in."}
                              "priority" {:type "string"
                                          :enum ["high" "medium" "low"]
                                          :description "[list] Filter by exact priority. [create|update] Set the task priority — an update rewrites the `priority-*` tag together with the content field, so `list` and `get` cannot disagree."}
                              "created_after" {:type "string"
                                               :description "[list] ISO-8601 timestamp; only entries with content :created >= this"}
                              "updated_after" {:type "string"
                                               :description "[list] ISO-8601 timestamp; only entries with :updated >= this"}
                              "limit" {:type "integer"
                                       :description "[list] Cap result count after sort"}
                              "offset" {:type "integer"
                                        :description "[list] Skip first N results after sort"}
                              "fields" {:type "array" :items {:type "string"}
                                        :description "[list] Project each result to a subset of fields (e.g. ['id' 'title'])"}
                              "parallel" {:type "boolean"
                                          :description "Run batch operations in parallel (default: false)"}
                              "dedupe_window_ms" {:type "integer"
                                                  :description "[timeline] Collapse consecutive same-kind events this many ms apart (default 120000). 0 keeps every event."}
                              "from" {:type "string"
                                      :description "[evolution] First month of the window, YYYY-MM (inclusive). Omit to let `window` decide."}
                              "to" {:type "string"
                                    :description "[evolution] Last month of the window, YYYY-MM (inclusive). Omit to end at the current month."}
                              "window" {:type "string"
                                        :description "[evolution] Window BACK FROM NOW instead of from/to, e.g. \"6m\", \"30d\", \"2w\", \"1y\"."}
                              "zone" {:type "string"
                                      :description "[evolution] IANA zone the months are bucketed in (default UTC)."}}
                 :required ["command"]}
   :handler #'handle-kanban})

(def tools [tool-def])
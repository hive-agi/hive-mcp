(ns hive-mcp.plan.tool
  "MCP tool for converting plan memory entries to kanban tasks with KG edges.

   Implements the exploration-to-kanban pipeline:
   1. Fetch plan from memory by ID
   2. Parse plan steps using plan.parser (EDN or markdown)
   3. Create kanban tasks via memory system
   4. Create KG edges: plan --depends-on--> tasks
   5. Create KG edges: task --depends-on--> task (from step dependencies)"

  (:require [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [hive-spi.kanban.registry :as kanban-port]
            [hive-mcp.plan.fsm :as plan-fsm]
            [hive-mcp.plan.kg-degraded :as kg-degraded]
            [hive-mcp.vectordb.facade :as facade]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.context.request :as ctx]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive-mcp.plan.schema :as schema]
            [hive-mcp.plan.plans :as plans]
            [hive-mcp.plan.status :as status]
            [hive-spi.schema.derive :as derive]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- normalize-user-waves
  "Convert a user-supplied :waves map into the same step-id → wave-number
   shape produced by `compute-waves`. Accepted shapes:

     {:A {:parallel [\"s1\" \"s2\"]}    ; explicit grouping
      :B {:parallel [\"s3\"]}}

     {:A [\"s1\" \"s2\"] :B [\"s3\"]}    ; bare seq under each key

   Wave numbers are assigned by lexicographic key order so :A → 0, :B → 1, etc.

   Returns nil for absent/empty/unrecognized shapes — callers fall back
   to the auto-computed waves (audit kanban 20260429203429: hand-written
   :waves was silently dropped)."
  [waves-map]
  (when (and (map? waves-map) (seq waves-map))
    (let [ordered-keys (sort-by str (keys waves-map))]
      (into {}
            (mapcat (fn [wave-num k]
                      (let [v (get waves-map k)
                            parallel (cond
                                       (and (map? v) (sequential? (:parallel v)))
                                       (:parallel v)
                                       (sequential? v)
                                       v
                                       :else [])]
                        (mapv (fn [sid] [(str sid) wave-num]) parallel)))
                    (range) ordered-keys)))))

(defn- merge-waves
  "Merge user-supplied :waves over auto-computed waves. User wins per step.
   Steps not mentioned by user keep their auto-derived wave."
  [auto-waves user-waves-map]
  (if-let [user (normalize-user-waves user-waves-map)]
    (merge auto-waves user)
    auto-waves))

(defn compute-waves
  "Compute DAG wave numbers for plan steps based on dependencies.

   Returns a map of step-id -> wave-number where:
   - Wave 0: Steps with no dependencies
   - Wave N: Steps whose dependencies are all in waves < N

   This enables parallel execution within each wave and sequential
   ordering across waves for the DAG-Wave swarm pattern.

   Example:
     Steps: A (no deps), B (deps: A), C (no deps), D (deps: B, C)
     Result: {\"A\" 0, \"C\" 0, \"B\" 1, \"D\" 2}"
  [steps]
  (let [step-ids (set (map :id steps))
        step-index (into {} (map (juxt :id identity) steps))

        ;; Iteratively assign wave numbers
        compute-wave (fn [assigned step-id]
                       (let [step (get step-index step-id)
                             deps (filter step-ids (:depends-on step []))]
                         (if (empty? deps)
                           0
                           (inc (apply max (map #(get assigned % 0) deps))))))

        ;; Kahn's algorithm: assign waves in dependency order
        assign-waves (fn [assigned remaining]
                       (if (empty? remaining)
                         assigned
                         (let [;; Find steps whose deps are all assigned
                               ready (filter (fn [sid]
                                               (let [deps (:depends-on (get step-index sid) [])]
                                                 (every? #(contains? assigned %) deps)))
                                             remaining)
                               ;; Assign waves to ready steps
                               new-assigned (reduce (fn [acc sid]
                                                      (assoc acc sid (compute-wave acc sid)))
                                                    assigned
                                                    ready)
                               new-remaining (remove (set ready) remaining)]
                           (if (empty? ready)
                             ;; No progress - cycle detection should have caught this
                             (reduce #(assoc %1 %2 0) assigned remaining)
                             (recur new-assigned new-remaining)))))]
    (assign-waves {} (map :id steps))))

(defn- create-kanban-task!
  "Create a kanban task for a plan step.

   Optional `wave` (non-negative int, or nil): when supplied a `wave:N`
   tag is added so `kanban list :tags [\"wave:1\"]` filters to a wave
   (audit kanban 20260429203429 + 20260429203455 — wave-aware kanban).

   Creates through the IKanbanWrite port resolved at call time.

   Returns {:ok task-id} or {:error message}"
  [{:keys [id title description priority tags files execution]} directory & {:keys [wave deps]}]
  (try
    (let [priority-str (if (keyword? priority) (name priority) (str priority))
          task-context (cond-> {:plan-step-id id}
                         (seq files) (assoc :files files)
                         execution (assoc :execution execution))
          task-tags    (vec (distinct (cond-> (vec tags)
                                        (some? wave) (conj (str "wave:" wave))
                                        true         (into (schema/routing-tags {:execution execution}))
                                        (seq deps)   (into (map #(str "dep:" %) (sort deps))))))
          request      (cond-> {:title title
                                :priority priority-str
                                :directory directory
                                :context task-context}
                         description     (assoc :description description)
                         (seq task-tags) (assoc :tags task-tags))
          {:keys [ok err]} (kanban-port/create-task! request)]
      (cond
        (:id ok) {:ok (:id ok)}
        err      {:error (str "kanban backend rejected: " (:message err)
                              " (" (some-> (:error err) name) ")")}
        :else    {:error "Failed to get task ID from kanban create response"}))
    (catch Exception e
      {:error (str "Failed to create kanban task: " (.getMessage e))})))

(defn- create-plan-decision-edge!
  "Create KG edge: Plan --derived-from--> Decision.

   Links the plan memory entry to its source decision (exploration result).
   Returns the edge ID or nil if no decision-id provided."
  [plan-id decision-id scope agent-id]
  (when decision-id
    (kg-edges/add-edge!
     {:from plan-id
      :to decision-id
      :relation :derived-from
      :scope scope
      :confidence 1.0
      :source-type :manual
      :created-by (str "plan_to_kanban" (when agent-id (str ":" agent-id)))})))

(defn- create-plan-task-edges!
  "Create KG edges from plan to its tasks.
   plan --depends-on--> task (plan depends on task completion)

   Arguments:
     plan-id  - Plan memory entry ID
     task-ids - Vector of kanban task IDs
     scope    - Project scope
     agent-id - Creating agent
     waves    - Map of step-id -> wave-number (unused; kept for the call shape)
     step-id-to-task-id - Map of step-id -> task-id (unused; same reason)

   Returns vector of created edge IDs.

   The edges go through `add-edges!` on the write-coalescing queue: the call
   enqueues and returns the ids, it does not wait on the store. A synchronous
   transaction here ran past the kg-degraded budget on a busy store, so the
   response reported the edges as skipped while the transaction went on to
   write them."
  [plan-id task-ids scope agent-id _waves _step-id-to-task-id]
  (kg-edges/add-edges!
   (mapv (fn [task-id]
           {:from plan-id
            :to task-id
            :relation :depends-on
            :scope scope
            :confidence 1.0
            :source-type :automated
            :created-by (str "plan_to_kanban" (when agent-id (str ":" agent-id)))})
         task-ids)))

(defn- create-task-dependency-edges!
  "Create KG edges for task-to-task dependencies.
   task-B --depends-on--> task-A (B depends on A completing first)

   `step-id-to-task-id` maps step ID to kanban task ID; `steps` carry
   :depends-on. Returns vector of created edge IDs, enqueued in one
   `add-edges!` call for the reason given on `create-plan-task-edges!`."
  [step-id-to-task-id steps scope agent-id _waves]
  (kg-edges/add-edges!
   (vec
    (for [step steps
          dep-id (:depends-on step)
          :when (and dep-id (contains? step-id-to-task-id dep-id))]
      {:from (get step-id-to-task-id (:id step))
       :to (get step-id-to-task-id dep-id)
       :relation :depends-on
       :scope scope
       :confidence 1.0
       :source-type :automated
       :created-by (str "plan_to_kanban" (when agent-id (str ":" agent-id)))}))))

(defn- create-step-cards!
  "Create one kanban card per plan step, in wave order so a step's
   dependencies already have task ids when it is created, and write those ids
   on the card as `dep:<task-id>` tags in the same load-bearing create. The
   scheduler reads the card first, so a slow or unreachable KG cannot turn a
   dependent into 'no dependencies'. Answers [{:step-id :task-id} | {:step-id
   :error}] in creation order; a step whose dependency failed to create still
   creates, with the deps it has."
  [steps waves directory]
  (let [ordered (sort-by #(get waves (:id %) 0) steps)]
    (loop [todo ordered, made {}, out []]
      (if-let [step (first todo)]
        (let [deps (keep #(get made %) (:depends-on step))
              {:keys [ok error]} (create-kanban-task! step directory
                                                      :wave (get waves (:id step))
                                                      :deps (set deps))]
          (if error
            (recur (rest todo) made (conj out {:step-id (:id step) :error error}))
            (recur (rest todo) (assoc made (:id step) ok)
                   (conj out {:step-id (:id step) :task-id ok}))))
        (let [by-step (into {} (map (juxt :step-id identity)) out)]
          (mapv #(get by-step (:id %)) steps))))))

(defn- build-execute-fn
  "Build the execute function for the Plan FSM.

   Closes over directory/plan-id/project-id/agent-id to create
   kanban tasks and KG edges during the :approved → :executing transition.

   Wave handling (audit kanban 20260429203429): merges any user-supplied
   :waves map from the plan over the auto-derived waves (Kahn over
   :depends-on). User pins win per step; unmentioned steps keep auto.
   Each created kanban task gets a `wave:N` tag so `kanban list` filters
   by wave (closes 20260429203455).

   Args (closed over):
     directory      - Working directory for kanban task creation
     plan-memory-id - Plan memory entry ID for KG edge source
     project-id     - Project scope for KG edges
     agent-id       - Creating agent ID

   Returns: (fn [data] -> {:task-ids [...] :kg-edges [...] :waves {...} ...})
   Throws: ExceptionInfo if task creation fails."
  [directory plan-memory-id project-id agent-id]
  (fn [{:keys [plan]}]
    (let [steps (:steps plan)
          ;; Compute wave numbers — auto from :depends-on, then merge
          ;; user-supplied :waves over (user wins per step).
          auto-waves (compute-waves steps)
          waves      (merge-waves auto-waves (:waves plan))

          ;; Create kanban tasks for each step, threading wave-number through
          task-results (create-step-cards! steps waves directory)
          errors (filter :error task-results)
          successes (remove :error task-results)]

      (when (seq errors)
        (throw (ex-info "Failed to create some kanban tasks"
                        {:errors (mapv #(str (:step-id %) ": " (:error %)) errors)})))

      (let [step-id-to-task-id (into {} (map (juxt :step-id :task-id) successes))
            task-ids (mapv :task-id successes)
            decision-id (:decision-id plan)

            ;; KG enrichment is best-effort. Each batch is wrapped in a
            ;; hard timeout + rescue via hive-weave (see kg-degraded ns).
            ;; A wedged KG store cannot block kanban task creation —
            ;; tasks are load-bearing, edges are backfillable.
            kg-result (kg-degraded/apply-kg-calls
                       [["decision-edge"
                         #(create-plan-decision-edge!
                           plan-memory-id decision-id project-id agent-id)]
                        ["plan-task-edges"
                         #(create-plan-task-edges!
                           plan-memory-id task-ids project-id agent-id
                           waves step-id-to-task-id)]
                        ["task-dep-edges"
                         #(create-task-dependency-edges!
                           step-id-to-task-id steps project-id agent-id waves)]])
            ;; The edge calls only enqueue: wait, bounded, until the edges
            ;; read back so the response says readable or still queued.
            kg-result (kg-degraded/with-durability kg-result kg-edges/get-edge)]

        (when (:degraded? kg-result)
          (kg-degraded/log-degradation!
           kg-result {:plan-id plan-memory-id :task-count (count task-ids)}))

        {:task-ids task-ids
         :kg-edges (:edges kg-result)
         :waves waves
         :step-mapping step-id-to-task-id
         :decision-id decision-id
         :kg-degraded? (:degraded? kg-result)
         :kg-pending? (:pending? kg-result)
         :kg-warnings (:warnings kg-result)}))))

(def ^:private terminal-plan-statuses
  "Statuses a plan is spent at: converting again would duplicate a board that
   already finished (completed) or was replaced (superseded)."
  #{"completed" "superseded"})

(defn- plan-entry?
  "True when `entry` is a plan record. The type may arrive as a keyword."
  [entry]
  (= "plan" (some-> (:type entry) name)))

(defn- plan-status
  "The entry's plan-status through the tag/field reader."
  [entry]
  (status/status-of entry))

(defn- already-converted-response
  "Answer a repeat convert of an ACTIVE plan with what the first one made.

   Returning the existing ids instead of creating a duplicate set is the point:
   a re-run (a retried tool call, an explorer ling completing twice) must be
   safe. `:already-converted` marks the response so a caller can tell the noop
   from a conversion."
  [entry]
  (let [task-ids (status/task-ids-of entry)]
    (mcp-json {:success true
               :already-converted true
               :plan-id (:id entry)
               :plan-source :memory
               :plan-status "active"
               :task-ids task-ids
               :task-count (count task-ids)
               :waves (status/waves-of entry)})))

(defn- mark-plan-active!
  "Stamp `active` on the plan entry BEFORE any task is created.

   Order matters: the write lands first, so a conversion that then dies
   half-way still reads as `active` — a partial board the caller can see —
   rather than `draft`, which looks unconverted and invites a duplicate. Both
   stores are written: the memory store is what a later convert reads back, the
   plans index is what `plan list` and a `plan-status` filter read."
  [plan-id]
  (let [entry (facade/get-entry-by-id plan-id)]
    (facade/update-entry! plan-id {:plan-status "active"
                                   :tags (status/with-status (:tags entry) "active")}))
  ;; The index mirror is best-effort: the store is authoritative and has the
  ;; status, and a degraded index must not abort a conversion the board needs.
  (try
    (plans/update-plan-status! plan-id "active")
    (catch Throwable t
      (log/warn "Plan index status mirror failed for" plan-id ":" (ex-message t)))))

(defn- plan-index-payload
  "The plans-index document for ENTRY with the conversion folded in.

   :wave-count is the only conversion fact the index metadata carries, so it is
   derived here rather than left off: the index is derived state and must not
   go on advertising a draft, wave-less plan after a conversion."
  [entry task-ids waves]
  (cond-> {:id (:id entry)
           :type "plan"
           :content (:content entry)
           :tags (:tags entry)
           :project-id (:project-id entry)
           :duration (:duration entry)
           :expires (:expires entry)
           :content-hash (:content-hash entry)
           :plan-status "active"
           :wave-count (count waves)
           :abstraction-level (:abstraction-level entry)
           :knowledge-gaps (:knowledge-gaps entry)
           :agent-id (:agent-id entry)}
    (seq task-ids) (assoc :steps-count (count task-ids))))

(defn- persist-conversion!
  "Record what the conversion produced on the plan entry — the ids it created
   and the waves it laid out — then re-index.

   Without the ids the next convert cannot tell a converted plan from an
   untouched one, and the double-convert noop above has nothing to answer with.
   The store write propagates on failure (the board exists; a silently dropped
   record is exactly what makes the next convert duplicate it)."
  [entry task-ids waves]
  (let [task-ids (vec task-ids)
        waves (or waves {})]
    (facade/update-entry! (:id entry)
                          {:task-ids task-ids :waves waves
                           :tags (status/with-status
                                     (status/with-conversion (:tags entry) task-ids waves)
                                     "active")})
    (try
      (plans/index-plan! (plan-index-payload entry task-ids waves))
      (catch Throwable t
        (log/warn "Plan re-index after conversion failed for" (:id entry)
                  "(conversion recorded, index stale):" (ex-message t))))))

;; -----------------------------------------------------------------------------
;; plan-to-kanban stages (CPPB): guard -> collect source -> pipeline -> respond
;; -----------------------------------------------------------------------------

(def ^:private PlanSource
  "Where the plan content came from and the ids the conversion runs under."
  [:map
   [:content [:maybe :string]]
   [:project-id [:maybe :string]]
   [:plan-id [:maybe :string]]
   [:source [:enum :file :memory]]])

(defn- conversion-guard
  "Promote (pure): the response that stops a memory-route conversion before
   it starts, or nil to proceed. A non-plan entry is refused, a spent plan
   refuses unless `force`, and an active plan that already holds task ids
   answers what the first conversion made."
  [plan-memory-id entry force]
  (when entry
    (cond
      (not (plan-entry? entry))
      (mcp-error (str "Memory entry " plan-memory-id " is type="
                      (pr-str (:type entry)) ", not plan. "
                      "Store the plan as a type=plan memory."))

      (and (contains? terminal-plan-statuses (plan-status entry)) (not force))
      (mcp-error (str "Plan " plan-memory-id " is " (plan-status entry)
                      " — already converted. Pass force=true to convert again."))

      (and (= "active" (plan-status entry))
           (seq (status/task-ids-of entry)) (not force))
      (already-converted-response entry))))

(m/=> conversion-guard [:=> [:cat :any [:maybe :map] :any] [:maybe :map]])

(defn- plan-source
  "Collect: the plan content and the ids it converts under. A plan_path is
   read from disk and scoped to the request's project; otherwise the
   resolved memory entry supplies both."
  [plan-memory-id plan-path entry]
  (if plan-path
    (do (log/info "plan_to_kanban: loading plan from file" {:path plan-path})
        {:content (slurp plan-path)
         :project-id (ctx/current-project-id)
         :plan-id (str "file:" plan-path)
         :source :file})
    {:content (:content entry)
     :project-id (:project-id entry)
     :plan-id plan-memory-id
     :source :memory}))

(m/=> plan-source [:=> [:cat :any [:maybe :string] [:maybe :map]] PlanSource])

(defn- plan-not-found
  "The error for a request whose plan content resolved to nothing."
  [plan-memory-id plan-path]
  (mcp-error (str "Plan not found. Provide plan_id (memory entry) or plan_path (file). "
                  (when plan-memory-id (str "Tried memory ID: " plan-memory-id))
                  (when plan-path (str "Tried file: " plan-path)))))

(m/=> plan-not-found [:=> [:cat :any [:maybe :string]] :map])

(defn- conversion-response
  "Pipeline (pure): the success payload for a finished FSM run."
  [plan-id source result]
  (let [{:keys [task-ids waves kg-edges]} result]
    {:success true
     :plan-id plan-id
     :plan-title (get-in result [:plan :title])
     :plan-source source
     :decision-id (:decision-id result)
     :task-ids task-ids
     :task-count (count task-ids)
     :waves waves
     :max-wave (when (seq waves) (apply max (vals waves)))
     :kg-edges kg-edges
     :edge-count (count kg-edges)
     :kg-degraded? (boolean (:kg-degraded? result))
     :kg-pending? (boolean (:pending? result))
     :kg-warnings (:kg-warnings result)
     :step-mapping (:step-mapping result)}))

(m/=> conversion-response [:=> [:cat [:maybe :string] [:enum :file :memory] :map] :map])

(defn- run-conversion!
  "Boundary: mark the entry active, run the plan FSM with the kanban/KG
   executor, and record the conversion back on the entry."
  [{:keys [plan-id project-id content source]} entry directory agent-id]
  (when entry (mark-plan-active! plan-id))
  (let [result (plan-fsm/run-plan-fsm
                {:content content :plan-id plan-id}
                {:execute-fn (build-execute-fn directory plan-id project-id agent-id)
                 :directory directory})]
    (when entry
      (persist-conversion! entry (:task-ids result) (:waves result)))
    (mcp-json (conversion-response plan-id source result))))

(m/=> run-conversion! [:=> [:cat PlanSource [:maybe :map] [:maybe :string] [:maybe :string]] :map])

(defn- conversion-failure
  "The error envelope for a conversion that threw. An ex-info carries the
   FSM's validation and errors into the message."
  [e]
  (if (instance? clojure.lang.ExceptionInfo e)
    (let [data (ex-data e)]
      (log/error e "plan_to_kanban FSM failed" data)
      (mcp-error (str "Plan conversion failed: " (ex-message e)
                      (when-let [v (:validation data)]
                        (str "\nValidation: " (pr-str v)))
                      (when-let [errs (:errors data)]
                        (str "\nErrors: " (str/join ", " errs))))))
    (do (log/error e "plan_to_kanban failed")
        (mcp-error (str "Failed to convert plan to kanban: " (ex-message e))))))

(m/=> conversion-failure [:=> [:cat [:fn #(instance? Throwable %)]] :map])

(defn plan-to-kanban
  "Convert a plan memory entry (or file) to kanban tasks with KG edges.

   Uses the Plan FSM (plan.fsm) to drive the lifecycle:
     draft → validated → approved → executing

   The FSM handles parsing, validation (schema + deps + cycles),
   and delegates execution to an injected execute-fn that creates
   kanban tasks and KG edges.

   Only the memory route carries lifecycle. A plan_id must name a type=plan
   entry (anything else parses as prose and posts a junk board), a spent plan
   refuses unless `force`, and the conversion is recorded on the entry. A
   plan_path is a bare file — no entry to read a status from or write one to —
   so it converts every time.

   Arguments:
     plan-memory-id - Memory entry ID containing the plan (nil when using plan-path)
     :directory     - Working directory for project scope (optional)
     :plan-path     - File path to slurp plan content from (alternative to memory ID)
     :auto-assign?  - Auto-assign tasks to lings (optional, not yet implemented)
     :force         - Convert even when the plan is spent or already converted

   Returns:
     {:task-ids [...] :kg-edges [...] :plan-id ...}, or
     {:success true :already-converted true :task-ids [...] :waves {...}} on a
     noop, or an :isError envelope.

   Side effects:
     - Marks the plan entry `active`, then records task-ids/waves on it
     - Creates kanban tasks in memory system
     - Creates KG edges: plan --depends-on--> tasks
     - Creates KG edges: task --depends-on--> task (based on step dependencies)

   Stages: `conversion-guard` -> `plan-source` -> `run-conversion!`
   (-> `conversion-response`); a throw answers `conversion-failure`."
  [plan-memory-id & {:keys [directory plan-path force]}]
  (log/info "plan_to_kanban" {:plan-id plan-memory-id :plan-path plan-path
                              :directory directory :force force})
  (try
    (let [directory (or directory (ctx/current-directory))
          ;; Resolve the entry once: the memory route takes content, project and
          ;; status off it, and writes the conversion back to it.
          entry (when-not (str/blank? (str plan-memory-id))
                  (facade/get-entry-by-id plan-memory-id))]
      (or (conversion-guard plan-memory-id entry force)
          (let [source (plan-source plan-memory-id plan-path entry)]
            (if-not (:content source)
              (plan-not-found plan-memory-id plan-path)
              (run-conversion! source entry directory
                               (System/getenv "CLAUDE_SWARM_SLAVE_ID"))))))
    (catch Exception e
      (conversion-failure e))))

(defn handle-plan-to-kanban
  "MCP tool handler for plan_to_kanban.
   Accepts plan_id (memory entry) OR plan_path (file path). At least one required.
   force=true re-converts a plan that is already active, completed or superseded."
  [{:keys [plan_id plan_path directory auto_assign force]}]
  (if (and (str/blank? plan_id) (str/blank? plan_path))
    (mcp-error "Either plan_id or plan_path is required")
    (plan-to-kanban plan_id
                    :directory directory
                    :plan-path plan_path
                    :auto-assign? auto_assign
                    :force (boolean force))))

(defn- json-get
  "Read k from a JSON-Schema map whose keys may be keywords or strings."
  [m k]
  (when (map? m)
    (if (contains? m k) (get m k) (get m (name k)))))

(m/=> json-get [:=> [:cat :any :keyword] :any])

(defn- json-deref
  "Follow a local $ref (#/definitions/X or #/$defs/X) against the root schema;
   any other node is returned unchanged."
  [root node]
  (if-let [ref (json-get node :$ref)]
    (let [[_ section def-name] (str/split (str ref) #"/" 3)]
      (json-deref root (json-get (json-get root (keyword section)) (keyword def-name))))
    node))

(defn- json-prop
  "The dereferenced property schema `prop` of object schema `node`."
  [root node prop]
  (json-deref root (json-get (json-get (json-deref root node) :properties) prop)))

(defn- json-required
  "The :required names of an object schema, as a vector of strings."
  [root node]
  (mapv name (json-get (json-deref root node) :required)))

(defn- json-enum
  "The :enum values of a (possibly $ref'd or nullable) schema, as strings."
  [root node]
  (let [node (json-deref root node)]
    (or (some->> (json-get node :enum) (mapv name))
        (some (fn [alt] (some->> (json-get (json-deref root alt) :enum) (mapv name)))
              (concat (json-get node :anyOf) (json-get node :oneOf))))))

(defn plan-contract
  "Derive the plan-schema verb's :required / :step-required / :step-enums from
   the projected JSON-Schema `input-schema`, so the response can never restate
   a list that drifted from schema/Plan. Shapes are the historical ones:
   vectors of strings, and {:priority [..] :estimate [..]}."
  [input-schema]
  (let [step (json-get (json-prop input-schema input-schema :steps) :items)]
    {:required      (json-required input-schema input-schema)
     :step-required (json-required input-schema step)
     :step-enums    {:priority (json-enum input-schema (json-prop input-schema step :priority))
                     :estimate (json-enum input-schema (json-prop input-schema step :estimate))}}))

(defn handle-plan-schema
  "MCP handler: surface the plan-memory contract at the tool boundary.

   Returns the malli `Plan` schema projected to JSON-Schema via the single-source
   hive-spi `compile-op` seam — the SAME projection every schema-driven MCP tool
   advertises as its :inputSchema — plus the required keys, the step enums, a
   valid example, and the authoring recipe. Zero-arg. Removes the need to read
   hive-mcp.plan.schema source before authoring a `type=plan` memory.
   :required, :step-required and :step-enums are DERIVED from that projection
   (plan-contract), never restated beside it."
  [_params]
  (let [{:keys [input-schema]} (derive/compile-op schema/Plan)
        {:keys [required step-required step-enums]} (plan-contract input-schema)]
    (mcp-json
     {:success       true
      :for           "type=plan memory content — embed the plan as an ```edn fenced block matching :json-schema"
      :json-schema   input-schema
      :required      required
      :step-required step-required
      :step-enums    step-enums
      :example       schema/example-plan
      :tag-vocabulary (str "Step :tags use a FIXED vocabulary, no synonyms (agentic run ledger, "
                           "convention 20261125100357-6730324c): run:<RUN-ID>, scenario:<card-id> "
                           "(not card:<id>), persona:<p>, verdict:pass|fail-product|fail-product-known|"
                           "blocked-safety|inconclusive, known:<finding-id>, evidence:<sha256-12>, "
                           "cleanup:pending|done, system:fb|pg|ui, needs-pedro. Close a step with "
                           "`kanban update new_status=done add_tags=[verdict:<v>]`; read a run with "
                           "`kanban list tags=[run:<RUN-ID>] status=all`.")
      :how-to        (str "1) Write a memory: type=plan whose content embeds the plan as an "
                          "```edn fenced block matching :json-schema. "
                          "2) Run `kanban plan-to-kanban` with plan_id=<memory-id> (or plan_path=<file>). "
                          "Each :steps entry becomes a kanban task; :depends-on becomes task->task "
                          "KG edges; :decision-id links the plan to its parent decision memory.")})))

(def tools
  "REMOVED: Flat plan_to_kanban tool no longer exposed. Use consolidated `kanban` tool with `plan-to-kanban` command."
  [])
;; =============================================================================
;; Function Contracts (m16 coverage batch)
;; =============================================================================

(m/=> compute-waves
      [:=> [:cat [:sequential :map]]
       [:map-of :string :int]])

;; Lifecycle guards/persistence take an entry map and answer maps or booleans.
;; Any-typed in: a backend entry is an open map.

(m/=> plan-entry?
      [:=> [:cat :any]
       :boolean])

(m/=> plan-status
      [:=> [:cat :any]
       :string])

(m/=> already-converted-response
      [:=> [:cat :map]
       :map])

(m/=> mark-plan-active!
      [:=> [:cat :any]
       :any])

(m/=> plan-index-payload
      [:=> [:cat :map [:sequential :any] :any]
       :map])

(m/=> persist-conversion!
      [:=> [:cat :map [:sequential :any] :any]
       :any])

(m/=> plan-to-kanban
      [:=> [:cat :any [:* :any]] :map])

(m/=> handle-plan-to-kanban
      [:=> [:cat :map]
       :map])

(m/=> plan-contract
      [:=> [:cat :any]
       [:map
        [:required [:vector :string]]
        [:step-required [:vector :string]]
        [:step-enums :map]]])

(m/=> handle-plan-schema
      [:=> [:cat :any]
       :map])

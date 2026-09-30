(ns hive-mcp.scheduler.dag-waves
  "DAGWave scheduler for dependency-ordered task dispatch."
  (:require [hive-mcp.spi.kanban.registry :as kanban-port]
            [hive-mcp.dns.result :as result]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.hivemind.core :as hivemind]
            [hive-mcp.channel.core :as channel]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-mcp.tools.memory.scope :as scope]
            [clojure.core.async :as async :refer [go-loop close!]]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive.events :as ev]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; State
;; =============================================================================

(defonce dag-state
  (atom {:active    false
         :plan-id   nil
         :max-slots 5
         :wave-log  []
         :dispatched {}      ; {kanban-task-id -> ling-id}
         :completed  #{}     ; set of kanban-task-ids
         :failed     #{}     ; set of kanban-task-ids
         :opts       {}}))   ; original start opts

(defn- wave-progress-payload
  "Current DAG parity snapshot for the swarm roster panel."
  []
  (let [s @dag-state]
    {:run-id        (str (:plan-id s))
     :dispatched    (count (:dispatched s))
     :completed     (count (:completed s))
     :failed        (count (:failed s))
     :in-flight     (mapv (fn [[tid lid]] {:task-id tid :ling-id lid :status "dispatched"})
                          (:dispatched s))
     :completed-ids (vec (:completed s))
     :failed-ids    (vec (:failed s))}))

(defn- emit-wave!
  "Fail-soft emit of a swarm wave event on the generic hive.events multi-bus."
  [variant]
  (result/rescue nil (ev/dispatch-multi [variant (wave-progress-payload)])))

;; =============================================================================
;; Kanban Helpers
;; =============================================================================

(defn- get-kanban-todos
  "Every task with status todo for the given project, through the kanban
   read port resolved at call time."
  [directory]
  (result/rescue []
                 (vec (kanban-port/list-tasks {:status "todo" :directory directory}))))

(defn- get-kanban-task
  "The task with TASK-ID through the kanban read port, or nil."
  [task-id]
  (result/rescue nil (kanban-port/get-task task-id)))

(defn- kanban-task-done?
  "Check if a kanban task has been completed."
  [task-id completed-set]
  (or (contains? completed-set task-id)
      (nil? (get-kanban-task task-id))))

(defn- move-kanban-done!
  "Move a kanban task to 'done' status."
  [task-id directory]
  (result/rescue nil
                 (kanban-port/transition!
                  {:task-id task-id :new-status "done" :directory directory})))


;; =============================================================================
;; KG Dependency Helpers
;; =============================================================================

(defn- get-task-dependencies
  "Get the task IDs that a given task depends on via KG edges."
  [task-id]
  (result/rescue #{}
                 (let [edges (kg-edges/get-edges-from task-id)
                       depends-on-edges (filter #(= :depends-on (:kg-edge/relation %)) edges)]
                   (set (map :kg-edge/to depends-on-edges)))))

;; =============================================================================
;; Core Pure Functions
;; =============================================================================

(defn- task-role
  "Extract a role for a task, fail-soft. Reads an explicit :role, else a
   `role:<name>` tag from :tags. Returns nil when absent."
  [task]
  (or (:role task)
      (some (fn [t]
              (let [s (str t)]
                (when (str/starts-with? s "role:")
                  (subs s (count "role:")))))
            (:tags task))))

(defn find-ready-tasks
  "Find tasks whose all dependencies have been completed."
  [directory completed dispatched failed]
  (let [todos (get-kanban-todos directory)
        already-handled (into (set (keys dispatched))
                              (into completed failed))]
    (->> todos
         (remove #(contains? already-handled (:id %)))
         (keep (fn [task]
                 (let [task-id (:id task)
                       deps (get-task-dependencies task-id)
                       ;; A dep is satisfied if it's completed or no longer exists
                       all-deps-done? (every? #(kanban-task-done? % completed) deps)]
                   (when all-deps-done?
                     {:task-id task-id
                      :title   (:title task)
                      :role    (task-role task)
                      :deps    deps
                      :dep-count (count deps)}))))
         vec)))

;; =============================================================================
;; Stateful Dispatch Functions
;; =============================================================================

(defn- normalize-role
  "Fail-soft role coercion: nil for absent/blank, else the role value."
  [role]
  (when (and (some? role)
             (not (and (string? role) (str/blank? role))))
    role))

(defn- default-ling-budget
  "Per-ling USD cap applied when a caller supplies none.

   Read from hive-mcp.agent.hooks.budget at call time rather than copied, so
   there is one definition of the default. nil only if that ns cannot be
   resolved, in which case no guardrail is registered and the ling runs
   uncapped — the pre-existing behaviour, now the exception rather than the
   rule."
  []
  (try
    (some-> (requiring-resolve 'hive-mcp.agent.hooks.budget/default-max-budget-usd)
            deref)
    (catch Throwable _ nil)))

(defn- spawn-ling-for-task
  "Attempt to spawn a ling for a single DAG task. Returns Result.
   Threads the task :role (when present) into create-ling! via the
   :spawn/request seam so Stage-A role overlay applies. Fail-soft: a
   nil/blank role adds no request.
   When :prefer-lightweight? is true (default), uses :headless spawn mode
   which routes to TransparentAgenticLoop (~2MB vs ~280MB ProcessBuilder).
   Threads :run-id (when present) into create-ling! so the ling's headless
   completion publishes on hive.v1.wave.<run-id>.completed.<task-id>.
   Threads :max-budget-usd so each dispatched ling carries a spend guardrail:
   this scheduler auto-advances waves on completion, so an uncapped ling here
   is an unbounded spend loop with no operator in it."
  [task-id title role {:keys [cwd presets project-id prefer-lightweight? run-id max-budget-usd]
                       :or {prefer-lightweight? true}}]
  (let [safe-title (-> (or title "task")
                       str/lower-case
                       (str/replace #"[^a-z0-9]+" "-"))
        truncated (subs safe-title 0 (min 30 (count safe-title)))
        ling-id (str "swarm-dag-" truncated "-" (System/currentTimeMillis))
        role* (normalize-role role)]
    (result/try-effect* :dag/spawn-failed
                        (ling/create-ling! ling-id
                                           (cond-> {:cwd cwd
                                                    :presets (or presets ["ling"])
                                                    :project-id project-id
                                                    :kanban-task-id task-id
                                                    :task title}
                                             run-id
                                             (assoc :run-id run-id)
                                             (and max-budget-usd (pos? max-budget-usd))
                                             (assoc :max-budget-usd max-budget-usd)
                                             prefer-lightweight?
                                             (assoc :spawn-mode :headless)
                                             role*
                                             (assoc :spawn/request {:role role*})))
                        ling-id)))

(defn dispatch-wave!
  "Dispatch lings for ready tasks up to available slots."
  [ready-tasks max-slots opts]
  (let [current-dispatched (:dispatched @dag-state)
        available-slots (max 0 (- max-slots (count current-dispatched)))
        tasks-to-dispatch (take available-slots ready-tasks)
        skipped (- (count ready-tasks) (count tasks-to-dispatch))]

    (when (pos? (count tasks-to-dispatch))
      (log/info "DAGWaves dispatching" (count tasks-to-dispatch)
                "tasks (slots:" available-slots "ready:" (count ready-tasks) ")"))

    (let [dispatched-results
          (doall
           (for [{:keys [task-id title role]} tasks-to-dispatch]
             (let [r (spawn-ling-for-task task-id title role opts)]
               (if-let [ling-id (:ok r)]
                 (do
                   ;; Track dispatch in state
                   (swap! dag-state update :dispatched assoc task-id ling-id)
                   ;; Log wave progress
                   (swap! dag-state update :wave-log conj
                          {:event :dispatched
                           :task-id task-id
                           :ling-id ling-id
                           :title title
                           :timestamp (System/currentTimeMillis)})
                   {:task-id task-id :ling-id ling-id :status :dispatched})
                 (do
                   ;; Mark as failed so we don't keep retrying
                   (swap! dag-state update :failed conj task-id)
                   {:task-id task-id :status :failed :error (:message r)})))))]

      (when (pos? (count tasks-to-dispatch))
        (emit-wave! :workflow/wave-dispatched))

      {:dispatched-count (count (filter #(= :dispatched (:status %)) dispatched-results))
       :dispatched-tasks dispatched-results
       :skipped-count skipped})))

;; =============================================================================
;; Ling Outcome (data)
;; =============================================================================

(def ^:private ling-event-outcomes
  "Ling event -> how the DAG reads it. Rows marked :terminal? are the events
   a ling ends its run with; the listener subscribes to exactly those. A new
   terminal event is a new row here, nothing else.
   :blocked is not terminal (the ling may resume) but, when handed to
   `on-ling-complete`, still counts as a failure."
  {:completed     {:outcome :succeeded :terminal? true}
   :truncated     {:outcome :failed    :terminal? true}
   :error         {:outcome :failed    :terminal? true}
   :context-death {:outcome :failed    :terminal? true}
   :blocked       {:outcome :failed    :terminal? false}})

(defn terminal-event-types
  "The ling event types that end a run, i.e. the ones the scheduler listens to."
  []
  (->> ling-event-outcomes
       (keep (fn [[event-type {:keys [terminal?]}]] (when terminal? event-type)))
       sort
       vec))

(defn event-channel-topic
  "The hive-mcp.channel.core topic a hivemind event of EVENT-TYPE is published
   on — the same `hivemind-<name>` naming the delivery channels use."
  [event-type]
  (keyword (str "hivemind-" (name event-type))))

(defn ling-outcome
  "Pure: :succeeded or :failed for a ling event of EVENT-TYPE carrying DATA.
   An explicit `:result \"failure\"` in the data fails any event; an unknown
   event type reads as success, as the completed channel always did."
  [event-type data]
  (if (= "failure" (str (:result data)))
    :failed
    (get-in ling-event-outcomes [event-type :outcome] :succeeded)))

;; =============================================================================
;; Completion Handler
;; =============================================================================

(defn record-outcome
  "Pure: STATE after TASK-ID, run by LING-ID, ended with OUTCOME
   (:succeeded or :failed) at TS. The task leaves :dispatched, joins
   :completed or :failed, and the wave log gains the matching event."
  [state task-id ling-id outcome ts]
  (let [event (if (= :failed outcome) :failed :completed)]
    (-> state
        (update :dispatched dissoc task-id)
        (update event conj task-id)
        (update :wave-log conj {:event     event
                                :task-id   task-id
                                :ling-id   ling-id
                                :timestamp ts}))))

(defn plan-finished?
  "Pure: a plan is finished when nothing is ready and nothing is in flight.
   Dependents of a failed task never become ready, so they do not hold it open."
  [ready in-flight]
  (and (empty? ready) (empty? in-flight)))

(defn- depends-on-any?
  "True when some id in DEPS is in UNSATISFIABLE."
  [unsatisfiable deps]
  (boolean (some #(contains? unsatisfiable %) deps)))

(defn blocked-task-ids
  "Pure: ids in DEP-GRAPH {task-id -> #{dep-id}} that depend, directly or
   transitively, on a task in FAILED. These are the plan's skipped tasks."
  [dep-graph failed]
  (loop [blocked #{}]
    (let [unsatisfiable (into (set failed) blocked)
          grown (into blocked
                      (keep (fn [[task-id deps]]
                              (when (depends-on-any? unsatisfiable deps) task-id)))
                      dep-graph)]
      (if (= grown blocked) blocked (recur grown)))))

(defn plan-summary
  "Pure: the final report of a finished plan STATE with SKIPPED task ids."
  [{:keys [plan-id completed failed opts]} skipped]
  (let [n-ok (count completed) n-failed (count failed) n-skipped (count skipped)]
    {:plan-id     plan-id
     :run-id      (:run-id opts)
     :result      (if (and (zero? n-failed) (zero? n-skipped)) "success" "failure")
     :completed   n-ok
     :failed      n-failed
     :skipped     n-skipped
     :failed-ids  (vec (sort failed))
     :skipped-ids (vec (sort skipped))
     :message     (str "All tasks complete. " n-ok " succeeded, " n-failed " failed, "
                       n-skipped " skipped.")}))

(defn- remaining-dep-graph
  "{task-id -> #{dep-id}} for every todo task the plan never handled."
  [directory {:keys [completed failed dispatched]}]
  (let [handled (into (set (keys dispatched)) (into completed failed))]
    (into {}
          (comp (map :id)
                (remove handled)
                (map (juxt identity get-task-dependencies)))
          (get-kanban-todos directory))))

(defn- announce-plan-complete!
  "Shout the finished plan's summary (with failed and skipped counts) and
   deactivate the scheduler."
  [directory]
  (let [state   @dag-state
        skipped (blocked-task-ids (remaining-dep-graph directory state) (:failed state))
        summary (plan-summary state skipped)]
    (log/info "DAGWaves: plan" (:plan-id state) "finished:" (:message summary))
    (hivemind/shout! "coordinator" :completed
                     (merge {:task (str "DAGWaves plan " (:plan-id state))
                             :project-id (get-in state [:opts :project-id])}
                            summary))
    (swap! dag-state assoc :active false)
    summary))

(defn- advance-dag!
  "Dispatch every task made ready by the latest outcome within the slot limit,
   then announce the plan when nothing is ready or in flight. A wave whose
   every spawn failed leaves nothing in flight, so the advance repeats; each
   round moves at least one ready task to :failed, so it terminates."
  [directory]
  (loop []
    (let [s         @dag-state
          ready     (find-ready-tasks directory (:completed s) (:dispatched s) (:failed s))
          wave      (when (seq ready) (dispatch-wave! ready (:max-slots s) (:opts s)))
          in-flight (:dispatched @dag-state)]
      (cond
        (plan-finished? ready in-flight)                        (announce-plan-complete! directory)
        (and (empty? in-flight) (seq (:dispatched-tasks wave))) (recur)
        :else                                                   wave))))

(defn- settle-task!
  "Record TASK-ID's OUTCOME. A success moves its kanban card to done; a
   failure leaves the card open, which keeps its dependents unready."
  [task-id ling-id outcome directory]
  (if (= :failed outcome)
    (log/warn "DAGWaves: task" task-id "FAILED via" ling-id)
    (move-kanban-done! task-id directory))
  (swap! dag-state record-outcome task-id ling-id outcome (System/currentTimeMillis)))

(defn on-ling-complete
  "Handle a terminal ling event: settle the task, then advance the plan.
   Success and failure share the advance step, so a failure still dispatches
   independent ready work and a failed last task still finishes the plan.
   EVENT-TYPE is the ling event that arrived (:completed, :truncated, ...);
   an older caller that put it in DATA as :event-type is still honoured."
  [{:keys [agent-id event-type data]}]
  (when (:active @dag-state)
    (let [slave          (ds-queries/get-slave agent-id)
          kanban-task-id (:slave/kanban-task-id slave)
          directory      (or (:slave/cwd slave) (get-in @dag-state [:opts :cwd]))
          event-type*    (or event-type (:event-type data))]
      (cond
        (nil? kanban-task-id)
        (log/debug "DAGWaves: ling" agent-id "ended but has no kanban-task-id (not a DAG task)")

        (contains? (:dispatched @dag-state) kanban-task-id)
        (do (log/info "DAGWaves: ling" agent-id "ended task" kanban-task-id "with" event-type*)
            (settle-task! kanban-task-id agent-id (ling-outcome event-type* data) directory)
            (emit-wave! :workflow/wave-completed)
            (advance-dag! directory))))))

;; =============================================================================
;; Channel Subscription (core.async pub/sub)
;; =============================================================================

(defonce ^:private dag-sub-channels (atom {}))  ; {event-type -> ch}

(defn- start-event-listener!
  "Subscribe to the hivemind channel of every terminal ling event and route
   each arrival to `on-ling-complete`, tagged with the event that arrived.
   ONE go-loop reads every channel (alts!), so arrivals are handled one at a
   time and two lings ending together never advance the plan concurrently.
   The loop ends once every channel is closed."
  []
  (let [subs (into {} (map (fn [event-type]
                             [(channel/subscribe! (event-channel-topic event-type)) event-type]))
                   (terminal-event-types))]
    (reset! dag-sub-channels (into {} (map (fn [[ch et]] [et ch])) subs))
    (go-loop [live subs]
      (when (seq live)
        (let [[event ch] (async/alts! (vec (keys live)))]
          (if (nil? event)
            (recur (dissoc live ch))
            (do (when (:active @dag-state)
                  (result/rescue nil
                                 (on-ling-complete {:agent-id   (:agent-id event)
                                                    :project-id (:project-id event)
                                                    :event-type (get live ch)
                                                    :data       (:data event)})))
                (recur live)))))))
  (log/info "DAGWaves event listener started"))

(defn- stop-event-listener!
  "Stop every terminal-event listener go-loop."
  []
  (doseq [[event-type ch] @dag-sub-channels]
    (let [r (result/try-effect* :dag/unsubscribe-failed
                                (channel/unsubscribe! (event-channel-topic event-type) ch))]
      (when (result/err? r)
        (result/rescue nil (close! ch)))))
  (reset! dag-sub-channels {})
  (log/info "DAGWaves event listener stopped"))

;; =============================================================================
;; Lifecycle Functions
;; =============================================================================

(defn start-dag!
  "Initialize and start the DAG scheduler for a plan.

   Every dispatched ling gets a per-ling USD cap: `:max-budget-usd` from opts,
   else `default-ling-budget`. This scheduler auto-advances waves when lings
   complete and nothing calls `stop-dag!` on failure, so an uncapped run is an
   unbounded spend loop with no operator in it. Pass an explicit
   `:max-budget-usd` to widen or narrow it; pass 0 only if you have another
   guardrail."
  [plan-id opts]
  (when (:active @dag-state)
    (throw (ex-info "DAG already active. Call stop-dag! first."
                    {:current-plan (:plan-id @dag-state)})))

  (let [{:keys [max-slots cwd presets project-id run-id max-budget-usd]
         :or {max-slots 5
              presets ["ling"]}} opts
        effective-project-id (or project-id
                                 (when cwd (scope/get-current-project-id cwd)))
        effective-budget     (if (some? max-budget-usd)
                               max-budget-usd
                               (default-ling-budget))]

    ;; Initialize state
    (reset! dag-state
            {:active     true
             :plan-id    plan-id
             :max-slots  max-slots
             :wave-log   []
             :dispatched {}
             :completed  #{}
             :failed     #{}
             :opts       {:cwd cwd
                          :presets presets
                          :project-id effective-project-id
                          :run-id run-id
                          :max-budget-usd effective-budget}})

    ;; Start event listener for completion detection
    (start-event-listener!)

    ;; Shout start
    (hivemind/shout! "coordinator" :started
                     {:task (str "DAGWaves scheduler for plan " plan-id)
                      :message (str "Max slots: " max-slots " project: " effective-project-id
                                    " budget/ling: " (or effective-budget "UNCAPPED"))})

    ;; Find and dispatch first wave
    (let [ready (find-ready-tasks cwd #{} {} #{})
          result (when (seq ready)
                   (dispatch-wave! ready max-slots (:opts @dag-state)))]

      (log/info "DAGWaves started. Plan:" plan-id
                "Ready tasks:" (count ready)
                "Dispatched:" (or (:dispatched-count result) 0)
                "Budget/ling:" (or effective-budget "UNCAPPED"))

      {:started true
       :plan-id plan-id
       :max-slots max-slots
       :max-budget-usd effective-budget
       :ready-count (count ready)
       :initial-dispatch result})))

(defn stop-dag!
  "Stop the DAG scheduler."
  []
  (let [state @dag-state]
    ;; Stop event listener
    (stop-event-listener!)

    ;; Mark as inactive
    (swap! dag-state assoc :active false)

    (log/info "DAGWaves stopped. Plan:" (:plan-id state)
              "Completed:" (count (:completed state))
              "Failed:" (count (:failed state))
              "Still dispatched:" (count (:dispatched state)))

    {:stopped true
     :plan-id (:plan-id state)
     :completed-count (count (:completed state))
     :failed-count (count (:failed state))
     :dispatched-count (count (:dispatched state))
     :wave-log (:wave-log state)}))

;; =============================================================================
;; Query Functions
;; =============================================================================

(defn dag-status
  "Get current DAG scheduler progress."
  []
  (let [state @dag-state
        directory (get-in state [:opts :cwd])
        ready (when (:active state)
                (result/rescue []
                               (find-ready-tasks directory
                                                 (:completed state)
                                                 (:dispatched state)
                                                 (:failed state))))]
    {:active       (:active state)
     :plan-id      (:plan-id state)
     :max-slots    (:max-slots state)
     :completed    (count (:completed state))
     :failed       (count (:failed state))
     :dispatched   (count (:dispatched state))
     :ready        (count (or ready []))
     :completed-ids (:completed state)
     :failed-ids   (:failed state)
     :dispatched-map (:dispatched state)
     :wave-log     (take-last 20 (:wave-log state))}))

;; =============================================================================
;; Comment / REPL Usage
;; =============================================================================

(comment
  ;; Typical usage:
  ;; 1. Coordinator creates plan and runs plan-to-kanban
  ;; 2. Start the DAG scheduler:
  (start-dag! "plan-memory-id-here"
              {:cwd "/home/lages/PP/hive/hive-mcp"
               :max-slots 5
               :presets ["ling"]})

  ;; 3. Monitor progress:
  (dag-status)

  ;; 4. Stop when done:
  (stop-dag!)

  ;; Manual completion test (simulates a ling finishing):
  (on-ling-complete {:agent-id "swarm-test-123"
                     :project-id "hive-mcp"
                     :data {:result "success"}})

  ;; Find ready tasks manually:
  (find-ready-tasks "/home/lages/PP/hive/hive-mcp" #{} {} #{}))
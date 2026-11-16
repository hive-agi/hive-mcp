(ns hive-mcp.scheduler.dag-waves
  "DAGWave scheduler for dependency-ordered task dispatch."
  (:require [hive-spi.kanban.registry :as kanban-port]
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
            [hive.events :as ev]
            [hive-mcp.scheduler.dag-eligibility :as elig]
            [hive-mcp.scheduler.task-dispatch :as task-dispatch]
            [malli.core :as m]))
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
  "True when task-id completed in this run, its card is gone, or its card
   status is done (completed by an earlier run or by hand)."
  [task-id completed-set]
  (or (contains? completed-set task-id)
      (let [task (get-kanban-task task-id)]
        (or (nil? task)
            (= "done" (some-> (:status task) name))))))

(defn- move-kanban-done!
  "Move a finished task's kanban card to 'inreview'. A ling ending its run is
   not evidence the work landed, so the card waits for review; the scheduler
   counts the task complete through its own state, not the card's status."
  [task-id directory]
  (result/rescue nil
                 (kanban-port/transition!
                  {:task-id task-id :new-status "inreview" :directory directory})))


;; =============================================================================
;; KG Dependency Helpers
;; =============================================================================

(def ^:private dep-tag-prefix "dep:")

(defn card-dependencies
  "Pure: the task ids a card lists as dependencies, from its `dep:<task-id>`
   tags. plan-to-kanban writes them in the same write that creates the card,
   so they survive a slow or unreachable KG."
  [task]
  (into #{}
        (keep (fn [t]
                (let [s (str (if (keyword? t) (name t) t))]
                  (when (str/starts-with? s dep-tag-prefix)
                    (not-empty (subs s (count dep-tag-prefix)))))))
        (:tags task)))

(m/=> card-dependencies [:=> [:cat [:maybe :map]] [:set :string]])

(defn- kg-dependencies
  "Dependencies from KG depends-on edges, or ::unknown when the KG read fails."
  [task-id]
  (let [edges (result/rescue ::unknown (vec (kg-edges/get-edges-from task-id)))]
    (if (= ::unknown edges)
      ::unknown
      (->> edges
           (filter #(= :depends-on (:kg-edge/relation %)))
           (map :kg-edge/to)
           set))))

(defn- get-task-dependencies
  "The task IDs TASK-ID depends on: the union of the card's own `dep:` tags and
   the KG depends-on edges. A KG read that fails is not 'no dependencies': the
   answer is ::unknown unless the card alone already lists some, and a task
   with unknown dependencies is not ready this tick."
  ([task-id] (get-task-dependencies task-id (get-kanban-task task-id)))
  ([task-id card]
   (let [from-card (card-dependencies card)
         from-kg   (kg-dependencies task-id)]
     (if (= ::unknown from-kg)
       (if (seq from-card) from-card ::unknown)
       (into from-card from-kg)))))

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

(def capabilities
  "What this scheduler honours in start-dag! opts. A caller that needs a
   guarantee (e.g. :task-ids — never touch cards outside the plan) checks for it
   here BEFORE starting, because an older scheduler would silently ignore the
   key and drain the whole todo board. :plan-scope = a plan's own cards bound
   the run (and :require-plan-scope? / :board-wide? are honoured);
   :human-guard = human-only cards are never dispatched."
  #{:task-ids :plan-scope :human-guard :rules})

;; =============================================================================
;; Schemas
;; =============================================================================

(def IdColl
  "A collection of task ids (sets in state, any seqable from callers)."
  [:maybe [:or [:set :any] [:sequential :any]]])

(def ReadyTask
  "One ready task find-ready-tasks answers."
  [:map [:task-id :any] [:title :any] [:description :any] [:role :any]
   [:deps [:set :any]] [:dep-count [:int {:min 0}]]])

(def LingOutcome [:enum :succeeded :failed])

(def DagState
  "The scheduler state map (dag-state's value) the pure transitions thread."
  [:map
   [:dispatched [:map-of :any :any]]
   [:completed [:set :any]]
   [:failed [:set :any]]
   [:wave-log [:vector :map]]])

(def PlanSummary
  "The final report of a finished plan."
  [:map
   [:plan-id :any] [:run-id :any] [:result [:enum "success" "failure"]]
   [:completed [:int {:min 0}]] [:failed [:int {:min 0}]] [:skipped [:int {:min 0}]]
   [:failed-ids [:vector :any]] [:skipped-ids [:vector :any]] [:message :string]])

(def StopResult
  "stop-dag!'s report."
  [:map [:stopped [:= true]] [:plan-id :any] [:completed-count :int]
   [:failed-count :int] [:dispatched-count :int] [:wave-log [:maybe [:vector :map]]]])

(defn- in-scope
  "TODOS restricted to TASK-IDS, or every todo when TASK-IDS is nil (the
   unbounded, board-wide mode)."
  [task-ids todos]
  (if (nil? task-ids)
    todos
    (let [allowed (set task-ids)]
      (filterv #(contains? allowed (:id %)) todos))))

(defn find-ready-tasks
  "Find tasks whose all dependencies have been completed.
   With TASK-IDS (a collection) only those cards are candidates, lowest open
   wave first; nil means every todo on the board. Human-only cards are never
   candidates (see dag-eligibility)."
  ([directory completed dispatched failed]
   (find-ready-tasks directory completed dispatched failed nil))
  ([directory completed dispatched failed task-ids]
  (let [todos (->> (get-kanban-todos directory)
                   (in-scope task-ids)
                   (elig/eligible (some? task-ids) (into (set completed) failed)))
        already-handled (into (set (keys dispatched))
                              (into completed failed))]
    (->> todos
         (remove #(contains? already-handled (:id %)))
         (keep (fn [task]
                 (let [task-id (:id task)
                       deps (get-task-dependencies task-id task)
                       ;; A dep is satisfied if it's completed or no longer exists;
                       ;; unreadable deps leave the task unready this tick
                       all-deps-done? (and (set? deps)
                                           (every? #(kanban-task-done? % completed) deps))]
                   (when-not (set? deps)
                     (log/warn "DAGWaves: dependencies of" task-id "unreadable; not ready this tick"))
                   (when all-deps-done?
                     {:task-id task-id
                      :title   (:title task)
                      :description (:description task)
                      :role    (task-role task)
                      :deps    deps
                      :dep-count (count deps)}))))
         vec))))

;; boundary: reads the kanban port and the KG (both rescued); the sequential
;; result is a vector of ReadyTask
(m/=> find-ready-tasks [:function
                        [:=> [:cat [:maybe :string] IdColl [:maybe [:map-of :any :any]] IdColl]
                         [:vector ReadyTask]]
                        [:=> [:cat [:maybe :string] IdColl [:maybe [:map-of :any :any]] IdColl IdColl]
                         [:vector ReadyTask]]])

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

(defn- task-prompt
  "The text a dispatched agent starts from: the plan rules, the card pointer
   and the card description."
  [task-id title description rules]
  (str (when-not (str/blank? rules)
         (str "Rules for every card of this plan (follow them):\n" rules "\n\n"))
       title
       "\n\nKanban card " task-id ": read it in full with"
       " `project kanban get id=" task-id "` before starting."
       (when-not (str/blank? description) (str "\n\n" description))))

(defn- spawn-ling-for-task
  "Dispatch a single DAG task through the :task-dispatch chain
   (hive-mcp.swarm.task-dispatch) and nothing else: this namespace names no
   spawn internals. Returns a Result whose ok value is
   {:agent-id :run-id :via :resolved-by :skipped}; :via names the dispatcher
   that served the task and :resolved-by :fallback says the preferred one did
   not. The dispatch opts carry :run-id (completion lands on
   hive.v1.wave.<run-id>.completed.<task-id>), :max-budget-usd (this scheduler
   auto-advances waves, so an uncapped agent is an unbounded spend loop) and
   :spawned-by, captured by `start-dag!` because later waves run on an event
   thread with no request in flight."
  [task-id title role {:keys [description rules] :as opts}]
  (let [task {:task-id     task-id
              :title       title
              :description description
              :role        (normalize-role role)
              :project-id  (:project-id opts)
              :prompt      (task-prompt task-id title description rules)}
        out  (task-dispatch/dispatch! task (dissoc opts :description :rules))]
    (if (result/ok? out)
      (let [{:keys [value via resolved-by skipped]} (:ok out)]
        (result/ok (assoc value :via via :resolved-by resolved-by :skipped skipped)))
      (assoc out :error (or (:error out) :dag/spawn-failed)))))

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
           (for [{:keys [task-id title role description]} tasks-to-dispatch]
             (let [r (spawn-ling-for-task task-id title role
                                          (assoc opts :description description))]
               (if-let [ling-id (get-in r [:ok :agent-id])]
                 (let [{:keys [via resolved-by skipped]} (:ok r)]
                   (when (= :fallback resolved-by)
                     (log/warn "DAGWaves: task" task-id "dispatched via" via
                               "after the preferred dispatcher was passed over:" (pr-str skipped)))
                   ;; Track dispatch in state
                   (swap! dag-state update :dispatched assoc task-id ling-id)
                   ;; Log wave progress
                   (swap! dag-state update :wave-log conj
                          {:event :dispatched
                           :task-id task-id
                           :ling-id ling-id
                           :via via
                           :resolved-by resolved-by
                           :skipped skipped
                           :title title
                           :timestamp (System/currentTimeMillis)})
                   {:task-id task-id :ling-id ling-id :status :dispatched
                    :via via :resolved-by resolved-by :skipped skipped})
                 (do
                   ;; Mark as failed so we don't keep retrying
                   (swap! dag-state update :failed conj task-id)
                   {:task-id task-id :status :failed :error (:message r)})))))]

      (when (pos? (count tasks-to-dispatch))
        (emit-wave! :workflow/wave-dispatched))

      {:dispatched-count (count (filter #(= :dispatched (:status %)) dispatched-results))
       :dispatched-tasks dispatched-results
       :skipped-count skipped})))

;; boundary: spawns lings and mutates dag-state; max-slots bounds the wave
(m/=> dispatch-wave! [:=> [:cat [:sequential [:map [:task-id :any]]] :int [:maybe :map]]
                      [:map
                       [:dispatched-count [:int {:min 0}]]
                       [:dispatched-tasks [:sequential [:map [:task-id :any]
                                                        [:status [:enum :dispatched :failed]]]]]
                       [:skipped-count [:int {:min 0}]]]])

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

(m/=> terminal-event-types [:=> [:cat] [:vector :keyword]])

(defn event-channel-topic
  "The hive-mcp.channel.core topic a hivemind event of EVENT-TYPE is published
   on — the same `hivemind-<name>` naming the delivery channels use."
  [event-type]
  (keyword (str "hivemind-" (name event-type))))

;; `name` accepts a keyword, symbol or string
(m/=> event-channel-topic [:=> [:cat [:or :keyword :symbol :string]] :keyword])

(defn ling-outcome
  "Pure: :succeeded or :failed for a ling event of EVENT-TYPE carrying DATA.
   An explicit `:result \"failure\"` fails any event, and so does a `:suspect`
   verdict (hive-agent's :no-deliverable or :ended-blocked): a run that ended
   without delivering must not unblock its card's dependents (friction
   20261021123123-70298deb). An unknown event type reads as success."
  [event-type data]
  (let [suspect (:suspect data)]
    (if (or (= "failure" (str (:result data)))
            (and (some? suspect)
                 (not (str/blank? (if (keyword? suspect) (name suspect) (str suspect))))))
      :failed
      (get-in ling-event-outcomes [event-type :outcome] :succeeded))))

(m/=> ling-outcome [:=> [:cat :any [:maybe :map]] LingOutcome])

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

;; any outcome other than :failed records as completed
(m/=> record-outcome [:=> [:cat DagState :any :any :any :any] DagState])

(defn plan-finished?
  "Pure: a plan is finished when nothing is ready and nothing is in flight.
   Dependents of a failed task never become ready, so they do not hold it open."
  [ready in-flight]
  (and (empty? ready) (empty? in-flight)))

(m/=> plan-finished? [:=> [:cat [:maybe [:sequential :any]] [:maybe [:map-of :any :any]]] :boolean])

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

(m/=> blocked-task-ids [:=> [:cat [:maybe [:map-of :any IdColl]] IdColl] [:set :any]])

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

;; failed / skipped are sorted, so their ids must be mutually comparable
(m/=> plan-summary [:=> [:cat [:map [:plan-id {:optional true} :any]
                               [:completed {:optional true} IdColl]
                               [:failed {:optional true} [:maybe [:or [:set :string] [:sequential :string]]]]
                               [:opts {:optional true} [:maybe :map]]]
                         [:maybe [:or [:set :string] [:sequential :string]]]]
                    PlanSummary])

(defn- remaining-dep-graph
  "{task-id -> #{dep-id}} for every in-scope todo task the plan never handled."
  [directory {:keys [completed failed dispatched opts]}]
  (let [handled (into (set (keys dispatched)) (into completed failed))]
    (into {}
          (comp (remove #(contains? handled (:id %)))
                (map (fn [task] [(:id task) (get-task-dependencies (:id task) task)]))
                ;; unreadable deps: the task is neither blocked nor reported skipped
                (filter (fn [[_ deps]] (set? deps))))
          (in-scope (:task-ids opts) (get-kanban-todos directory)))))

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
          ready     (find-ready-tasks directory (:completed s) (:dispatched s) (:failed s)
                                      (get-in s [:opts :task-ids]))
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

;; boundary: answers nil when inactive or not a DAG task, else the advance
;; step's wave report or the finished plan's summary
(m/=> on-ling-complete [:=> [:cat [:map [:agent-id {:optional true} :any]
                                   [:event-type {:optional true} :any]
                                   [:data {:optional true} [:maybe :map]]]]
                        [:maybe :map]])

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

(declare stop-dag!)

(defn start-dag!
  "Initialize and start the DAG scheduler for a plan.

   Every dispatched ling gets a per-ling USD cap: `:max-budget-usd` from opts,
   else `default-ling-budget`. This scheduler auto-advances waves when lings
   complete and nothing calls `stop-dag!` on failure, so an uncapped run is an
   unbounded spend loop with no operator in it. Pass an explicit
   `:max-budget-usd` to widen or narrow it; pass 0 only if you have another
   guardrail.

   The run is bounded to a set of kanban cards, by this wave and every
   auto-advanced one (dag-eligibility/run-scope): `:task-ids` when given, else
   the plan's own cards (its `plan --depends-on--> task` KG edges, written by
   `kanban plan-to-kanban`). Only `:board-wide? true`, or a plan with no
   linked cards and no `:require-plan-scope?`, makes every todo a candidate;
   with `:require-plan-scope?` such a plan is refused. An EMPTY `:task-ids` is
   refused rather than read as \"no bound\".

   A start that leaves nothing in flight (no ready task, or every spawn of the
   first wave failed) stops the scheduler before returning: with no ling
   running, no completion event can ever advance it, so leaving it :active
   would only park a subscribed listener that a later unrelated ling event
   could wake."
  [plan-id opts]
  (when (:active @dag-state)
    (throw (ex-info "DAG already active. Call stop-dag! first."
                    {:current-plan (:plan-id @dag-state)})))

  (when (and (contains? opts :task-ids) (empty? (:task-ids opts)))
    (throw (ex-info "start-dag! :task-ids is empty; refusing to treat it as unbounded."
                    {:plan-id plan-id})))

  (let [{:keys [max-slots cwd presets project-id run-id max-budget-usd task-ids]
         :or {max-slots 5
              presets ["ling"]}} opts
        task-ids (elig/run-scope plan-id opts
                                 (when-not (or (seq task-ids) (:board-wide? opts))
                                   (let [deps (get-task-dependencies plan-id)]
                                     (when (set? deps) deps))))
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
                          :max-budget-usd effective-budget
                          :task-ids task-ids
                          :rules (:rules opts)
                          ;; Captured now, inside the start request: later
                          ;; waves spawn on the event listener thread.
                          :spawned-by (if (string? (:spawned-by opts))
                                        (:spawned-by opts)
                                        (ling/request-spawner))}})

    ;; Start event listener for completion detection
    (start-event-listener!)

    ;; Shout start
    (hivemind/shout! "coordinator" :started
                     {:task (str "DAGWaves scheduler for plan " plan-id)
                      :message (str "Max slots: " max-slots " project: " effective-project-id
                                    " budget/ling: " (or effective-budget "UNCAPPED"))})

    ;; Find and dispatch first wave
    (let [ready (find-ready-tasks cwd #{} {} #{} task-ids)
          result (when (seq ready)
                   (dispatch-wave! ready max-slots (:opts @dag-state)))
          idle?  (empty? (:dispatched @dag-state))]

      (log/info "DAGWaves started. Plan:" plan-id
                "Ready tasks:" (count ready)
                "Dispatched:" (or (:dispatched-count result) 0)
                "Scope:" (if task-ids (str (count task-ids) " task(s)") "WHOLE BOARD")
                "Budget/ling:" (or effective-budget "UNCAPPED"))

      ;; Nothing in flight -> nothing can ever advance this plan. Stop now.
      (when idle?
        (log/warn "DAGWaves: plan" plan-id "has nothing in flight after its first wave; stopping.")
        (stop-dag!))

      {:started true
       :stopped (boolean idle?)
       :plan-id plan-id
       :max-slots max-slots
       :max-budget-usd effective-budget
       :task-ids task-ids
       :ready-count (count ready)
       :initial-dispatch result})))

(def StartOpts
  "start-dag! options (see its docstring). Open map."
  [:maybe
   [:map
    [:max-slots {:optional true} :int]
    [:cwd {:optional true} [:maybe :string]]
    [:presets {:optional true} [:maybe [:sequential :string]]]
    [:project-id {:optional true} [:maybe :string]]
    [:run-id {:optional true} :any]
    [:max-budget-usd {:optional true} [:maybe number?]]
    [:task-ids {:optional true} IdColl]
    [:board-wide? {:optional true} :any]
    [:require-plan-scope? {:optional true} :any]]])

;; throws ex-info when already active, on an empty :task-ids, or when a
;; required plan scope resolves to nothing
(m/=> start-dag! [:=> [:cat :any StartOpts]
                  [:map [:started [:= true]] [:stopped :boolean] [:plan-id :any]
                   [:max-slots :int] [:max-budget-usd [:maybe number?]]
                   [:task-ids [:maybe [:set :any]]] [:ready-count [:int {:min 0}]]
                   [:initial-dispatch [:maybe :map]]]])

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

(m/=> stop-dag! [:=> [:cat] StopResult])

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
                                                 (:failed state)
                                                 (get-in state [:opts :task-ids]))))]
    {:active       (:active state)
     :plan-id      (:plan-id state)
     :task-ids     (get-in state [:opts :task-ids])
     :max-slots    (:max-slots state)
     :completed    (count (:completed state))
     :failed       (count (:failed state))
     :dispatched   (count (:dispatched state))
     :ready        (count (or ready []))
     :completed-ids (:completed state)
     :failed-ids   (:failed state)
     :dispatched-map (:dispatched state)
     :wave-log     (take-last 20 (:wave-log state))}))

(m/=> dag-status [:=> [:cat]
                  [:map [:active :boolean] [:plan-id :any] [:task-ids :any]
                   [:max-slots :int] [:completed :int] [:failed :int]
                   [:dispatched :int] [:ready :int]
                   [:completed-ids [:set :any]] [:failed-ids [:set :any]]
                   [:dispatched-map [:map-of :any :any]]
                   [:wave-log [:maybe [:sequential :map]]]]])

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
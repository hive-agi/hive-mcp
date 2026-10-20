(ns hive-mcp.server.routes.async-tasks
  "Registry of in-flight async tool calls: bounded, inspectable, cancellable.

   Boundary layer. Decisions live in `async-task.state` (pure); shapes live in
   `async-task.schema`. This namespace only holds state and performs effects.

   Execution goes through `ITaskRunner`, so a caller may substitute the
   thread-backed runner. The default is backed by `hive-weave`: a fixed pool
   rather than unbounded `future` threads, and `safe-future-call` for a task
   that carries its own deadline.

   Rationale, incident and the alternative designs considered:
   memory 20260907-async-boundable (see kanban card for this work)."
  (:require [hive-dsl.bounded-atom :refer [bounded-atom bget bput! bkeys bclear!
                                           bounded-swap! register-sweepable!]]
            [hive-weave.pool :as pool]
            [hive-weave.safe :as safe]
            [hive-mcp.server.routes.async-task.state :as st]
            [taoensso.timbre :as log]
            [hive-mcp.server.routes.async-task.lane :as lane])
  (:import [java.util.concurrent Future]
[java.util.concurrent RejectedExecutionException]))

;; =============================================================================
;; Execution port (DIP)
;; =============================================================================

(defprotocol ITaskRunner
  "How a task is started, stopped and observed.

   The seam exists so the lifecycle rules can be exercised without real
   concurrency, and so the execution strategy can change without touching the
   registry above it."
  (-run    [this f]      "Start f. Returns an opaque handle.")
  (-cancel [this handle] "Interrupt the handle's work. Returns true if this call stopped it.")
  (-view   [this handle] "Project the handle into a schema/HandleView."))

(def default-pool-size
  "Concurrent async tool calls; beyond this they QUEUE rather than run.

   These calls are long (an ingest runs for hours), so this bounds genuinely
   slow work rather than tuning throughput."
  8)

(def default-queue-capacity
  "Queued calls before the pool starts REFUSING new ones.

   It used to be the point where the submitting thread ran the task itself.
   That is the right shape for a caller that can afford to wait, and this
   caller cannot: the submitter here is the thread serving the MCP request,
   and the work it would inherit is an async tool call, i.e. the calls that
   run for hours. CallerRuns turned a saturated pool into a request that never
   answers, which is the one failure the async surface exists to prevent.

   Refusing says so in milliseconds and leaves the caller free to retry, so it
   should still take a pathological backlog before it fires."
  256)

(defrecord WeavePoolRunner [pool]
  ITaskRunner
  (-run [_ f] (pool/submit! pool f))
  (-cancel [_ handle] (.cancel ^Future handle true))
  (-view [_ handle]
    (if (nil? handle)
      {:present? false :done? false :cancelled? false}
      {:present?   true
       :done?      (.isDone ^Future handle)
       :cancelled? (.isCancelled ^Future handle)})))

(def short-pool-size
  "Concurrent calls on the :short lane (`async-task.lane`).

   Small on purpose: only bounded verbs are declared short, so a few slots
   drain quickly, and a small pool keeps a misclassified long verb from
   taking many threads."
  4)

(def short-queue-capacity
  "Queued :short calls before that lane refuses new ones."
  64)

(def ^:private lane-pool-specs
  "lane -> hive-weave pool spec. :batch is the pre-lane `mcp-async` pool,
   unchanged, so everything not declared short behaves as before."
  {:batch {:name           "mcp-async"
           :size           default-pool-size
           :queue-capacity default-queue-capacity
           :rejection      :abort}
   :short {:name           "mcp-async-short"
           :size           short-pool-size
           :queue-capacity short-queue-capacity
           :rejection      :abort}})

(defonce ^:private default-runners
  (into {}
        (map (fn [[lane spec]] [lane (delay (->WeavePoolRunner (pool/make-pool spec)))]))
        lane-pool-specs))

(defonce ^{:doc "The active ITaskRunner. Rebind to substitute execution."}
  runner
  (atom nil))

(defonce ^{:doc "lane -> ITaskRunner overriding that lane's default pool. `runner`
   wins over it: a runner set there still takes every lane, as before lanes."}
  lane-runners
  (atom {}))

(defn runner-for
  "The ITaskRunner for `lane` (nil or unknown: `lane/default-lane`)."
  [lane]
  (let [lane (lane/known-lane lane)]
    (or @runner
        (get @lane-runners lane)
        @(get default-runners lane))))

(defn current-runner
  "The default lane's runner. Kept for callers that predate lanes."
  []
  (runner-for lane/default-lane))

;; =============================================================================
;; Registry
;; =============================================================================

(defonce ^{:doc "task-id -> entry. Bounded: a registry that only grows is a leak."}
  tasks
  (bounded-atom {:max-entries     500
                 :ttl-ms          1800000
                 :eviction-policy :lru}))
(register-sweepable! tasks :async-tasks)

(defn- entry-view
  "Registry row with its handle projected, which is what the pure layer takes."
  [task-id]
  (when-let [e (bget tasks task-id)]
    (-> e
        (assoc :handle (-view (runner-for (:lane e)) (:handle-obj e)))
        (dissoc :handle-obj))))

(defn get-task
  [task-id]
  (when-let [e (entry-view task-id)]
    (st/summarize task-id e (System/currentTimeMillis))))

(defn list-tasks
  []
  (let [now (System/currentTimeMillis)]
    (->> (bkeys tasks)
         (keep (fn [id] (when-let [e (entry-view id)] (st/summarize id e now))))
         (sort-by :elapsed-ms)
         vec)))

(defn running-tasks
  []
  (vec (remove (comp st/terminal? :state) (list-tasks))))

(defn- update-row!
  "Apply `f` to field `k` of `task-id`'s row, atomically; no-op for a row
   that is gone.

   Atomic because a row has two writers: `submit!` attaches the handle on the
   submitting thread while the pool thread records :task/running. With a
   bget/bput! read-modify-write each wrote back the row it had read, so the
   later write erased the other's field: a running task listed with no handle
   (cancel! answered :cancel/not-started and never interrupted it) or put
   back to :task/queued (hive-mcp PR #447 CI)."
  [task-id k f]
  (bounded-swap! tasks
                 (fn [m]
                   (if (get m task-id)
                     (update-in m [task-id :data k] f)
                     m))))

(defn- set-state!
  [task-id state]
  (update-row! task-id :state (constantly state)))

(defn- start-state!
  "Record that the body started. Only a :task/queued row is promoted, so a
   cancel that landed first is not overwritten."
  [task-id]
  (update-row! task-id :state #(if (= :task/queued %) :task/running %)))

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defn- bounded-body
  "Wrap `f` so a task carrying `timeout-ms` ends itself at the deadline.

   The wait happens on the POOL thread, never on the caller's: the caller has
   already been acknowledged. `safe-future-call` returns the timeout as a
   value, so the deadline needs no scheduler of its own."
  [task-id f timeout-ms]
  (if (and timeout-ms (pos-int? timeout-ms))
    (fn []
      (let [res (safe/safe-future-call {:timeout-ms timeout-ms :name task-id} f)]
        (when (= :weave/timeout (:error res))
          (log/warn "async task exceeded its bound" {:task-id task-id :timeout-ms timeout-ms})
          (set-state! task-id :task/timed-out))
        res))
    f))

(defn- complete-state!
  "Record that a task's body returned, without overwriting why it stopped.

   Atomic on purpose. A read-modify-write through bget/bput! loses a
   concurrent cancel: the body's `finally` can read :task/running, a cancel
   can then write :task/cancelled, and the `finally` overwrites it with
   :task/done, erasing the only record that someone stopped it. bounded-swap!
   sees the raw entry map, so the decision and the write are one step."
  [task-id]
  (bounded-swap! tasks
                 (fn [m]
                   (if (get m task-id)
                     (update-in m [task-id :data :state] st/completion-state)
                     m))))

(defn submit!
  "Run `f` under `task-id` on `lane`, registering a cancellable handle.
   Returns the handle.

   `lane` (see `async-task.lane`) picks the pool; omitted or unknown, it is
   `lane/default-lane`, the pool every async call used before lanes existed.

   The handle and the :task/running state are written by different threads,
   each as an atomic update of its own field (`update-row!`), so neither
   write can erase the other.

   Throws `RejectedExecutionException` when the lane's pool is saturated. The
   row this function wrote a moment earlier is removed first, because a
   registry that lists a queued task nobody will ever run is worse than no row
   at all: it is a task `list-tasks` reports, `cancel!` accepts, and nothing
   completes."
  [{:keys [task-id tool caller-id timeout-ms f lane]}]
  (let [lane (lane/known-lane lane)]
    (bput! tasks task-id {:tool       tool
                          :caller-id  caller-id
                          :lane       lane
                          :state      :task/queued
                          :started-at (System/currentTimeMillis)
                          :timeout-ms timeout-ms
                          :handle-obj nil})
    (let [body   (bounded-body task-id f timeout-ms)
          handle (try
                   (-run (runner-for lane)
                         (fn []
                           (start-state! task-id)
                           (try
                             (body)
                             (finally
                               (complete-state! task-id)))))
                   (catch RejectedExecutionException e
                     (bounded-swap! tasks #(dissoc % task-id))
                     (log/warn "async task refused: pool saturated"
                               {:task-id task-id :tool tool :lane lane})
                     (throw e)))]
      (update-row! task-id :handle-obj (constantly handle))
      handle)))

(defn cancel!
  "Stop `task-id`. Returns an `AsyncCancelOutcome`; never throws for a stranger.

   Whether to act is decided by the pure layer, so a refusal is a value rather
   than a side effect that did nothing."
  [task-id]
  (let [outcome (st/cancel-outcome task-id (entry-view task-id))]
    (when (st/stops-the-work? outcome)
      (let [e   (bget tasks task-id)
            ok? (-cancel (runner-for (:lane e)) (:handle-obj e))]
        (set-state! task-id :task/cancelled)
        (log/info "async task cancelled" {:task-id task-id :interrupted? ok?})))
    outcome))

(defn forget!
  "Drop finished rows. Running ones are kept."
  []
  (let [gone (->> (bkeys tasks)
                  (filter #(some-> (entry-view %) st/collectable?))
                  vec)]
    ;; bounded-atom has no per-key delete, and `bput! nil` would store a nil
    ;; ROW rather than remove one. bounded-swap! sees the raw entry map, so
    ;; dissoc there is the actual removal.
    (when (seq gone)
      (bounded-swap! tasks #(apply dissoc % gone)))
    {:forgotten (count gone)}))

(defn reset-registry!
  "Clear every row. For tests and for a cold boot, not for operational use."
  []
  (bclear! tasks))

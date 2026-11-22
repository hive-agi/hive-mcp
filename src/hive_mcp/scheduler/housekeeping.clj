(ns hive-mcp.scheduler.housekeeping
  "Periodic global housekeeping: clean up stale JVM-side resources.

   Runs on a scheduled executor (like decay.clj) every 5 minutes.
   Each cleanup task is individually guarded — one failure doesn't
   stop the sweep.

   Targets:
   - Stale callbacks (> 30 min)
   - Completed multi-async batches
   - Event journal old entries
   - Completed SAA states
   - Bounded atom GC sweep (TTL + capacity eviction)
   - Terminal liveness sweep and ledger cold sweep
   - KG GC lane (synthetic cleanup), self-gated to its own interval; see
     hive-mcp.scheduler.kg-gc"
  (:require [hive-mcp.dns.result :as result]
            [hive-mcp.scheduler.gc-gate :as gc-gate]
            [hive-mcp.config.core :as config]
            [taoensso.timbre :as log]
            [clojure.set :as set]
            [malli.core :as m]
            [clojure.string :as str])
  (:import [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; State
;; =============================================================================

(defonce ^:private scheduler-state
  (atom {:executor nil
         :running? false
         :sweep-count 0
         :last-run nil
         :last-result nil}))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- resolve-and-call
  "Resolve sym via requiring-resolve; call f with the resolved fn.

   Three outcomes, each distinguishable in the RETURNED map:
     success       -> the callee's own result
     call threw    -> fallback + {:failed true :error msg}
     unresolvable  -> {:skipped true :reason ...}

   `rescue` parks the cause in metadata, which every consumer of this map
   drops; the merge is what keeps a throwing sweep from reading as a clean one.

   The resolve is itself guarded: requiring-resolve THROWS (not nil) when the
   namespace cannot be loaded, and an unguarded throw here aborts the whole
   sweep -- every later task included."
  [sym call-fn fallback]
  (if-let [f (result/rescue nil (requiring-resolve sym))]
    (let [r (result/rescue fallback (call-fn f))]
      (if-let [err (:hive-dsl.result/error (meta r))]
        (merge (when (map? r) r) {:failed true :error (:message err)})
        r))
    {:skipped true :reason (str (name sym) " not available")}))

;; =============================================================================
;; Housekeeping Sweep (Pure Logic)
;; =============================================================================

(def housekeeping-sweep-names
  "Logical names of tasks owned by the five-minute housekeeping timer. Keep this
   inventory alongside housekeeping-sweep! when adding or removing a task;
   the boot wiring test checks for collisions with registered ISweepable names.
   This is descriptive data, not a second dispatcher."
  #{"swarm/stale-callbacks"
    "tools/completed-async-batches"
    "channels/event-journal"
    "agent/completed-saa-states"
    "system/bounded-atom-gc"
    "lings/terminal-liveness"
    "lings/ledger-cold"
    "knowledge-graph/gc-lane"
    "memory/pending-reembed"
    "system/jvm-gc-hint"})

(defn disjoint-sweep-owners?
  "True when no registered sweep name duplicates a housekeeping-owned task."
  [registered-names]
  (empty? (set/intersection housekeeping-sweep-names
                            (set registered-names))))

;; registered-names is any collection (coerced with `set`)
(m/=> disjoint-sweep-owners? [:=> [:cat [:maybe [:or [:sequential :any] [:set :any]]]] :boolean])

(def ^:private sweep-tasks
  "The guarded tasks of one sweep, in the order they run: result key, the
   symbol resolved at call time, and the fallback a throwing task answers.

   1. stale callbacks (30 min threshold)  2. completed multi-async batches
   3. event journal  4. completed SAA states
   5. bounded atom GC sweep: evicts TTL-expired and over-capacity entries
      from every registered bounded atom (no GC death spiral from unbounded
      atom growth)
   then the terminal liveness sweep; the ledger cold sweep (retracts terminal
   tasks and claim-history from the hot DataScript store once past the retain
   window, durable in the ledger); and the KG GC lane, which ticks every
   sweep but runs only on its own interval (default 60 min) and reports what
   it could not see (see the kg-gc ns)."
  [[:callbacks 'hive-mcp.swarm.callback/cleanup-stale! {:cleaned 0}]
   [:async-batches 'hive-mcp.tools.multi-async/gc-completed! {:cleaned 0}]
   [:event-journal 'hive-mcp.tools.swarm.channel/clear-event-journal! {:cleared true}]
   [:saa-states 'hive-mcp.agent.saa.orchestrator/clear-completed-states! {:cleaned 0}]
   [:gc-sweep 'hive-mcp.gc.bounded-atom/sweep-all!
    {:total-evicted 0 :atom-count 0 :duration-ms 0}]
   [:terminal-liveness 'hive-mcp.swarm.lifecycle.terminal-sweep/sweep-once!
    {:checked 0 :zombified 0 :alive 0 :errors []}]
   [:ledger-cold 'hive-mcp.swarm.datascript.coordination.cleanup/sweep-ledger-cold!
    {:tasks 0 :claim-history 0}]
   [:kg-gc 'hive-mcp.scheduler.kg-gc/run-lane!
    {:skipped true :reason "kg-gc lane failed"}]])

(def ^:private pending-reembed-task
  "Memory entries whose embed failed sit in the durable outbox; each sweep
   reembeds them once (a failure stays parked). It runs AFTER the sweep's
   duration is taken, as it always has."
  ['hive-mcp.tools.memory.crud.reembed/drain-pending!
   {:total 0 :reembedded 0 :not-found 0 :errors 0}])

(def ^:private SweepTask [:tuple :keyword :symbol :any])

(defn- run-sweep-tasks
  "Collect: run each task through CALL (sym fallback -> result), in order,
   into one map keyed by the task's result key. CALL is the port; the sweep
   passes `resolve-and-call`."
  [call tasks]
  (reduce (fn [acc [k sym fallback]] (assoc acc k (call sym fallback))) {} tasks))

(defn- sweep-result
  "Promote: the sweep's result map from what its stages measured."
  [task-results {:keys [gc-hint pending-reembed sweep-number duration-ms timestamp]}]
  (assoc task-results
         :pending-reembed pending-reembed
         :gc-hint gc-hint
         :sweep-number sweep-number
         :duration-ms duration-ms
         :timestamp timestamp))

(defn- sweep-log-events
  "Pipeline: the [level & args] log events a sweep result calls for.
   An eviction by the bounded-atom sweep and a GC hint that freed over 50 MB
   are reported at info. A large committed/used gap that survives an
   explicit GC means committed heap is NOT being returned (the ratchet), so
   that warns rather than stays silent."
  [{:keys [gc-sweep gc-hint]}]
  (cond-> []
    (pos? (get gc-sweep :total-evicted 0))
    (conj [:info "Housekeeping: GC sweep evicted"
           (:total-evicted gc-sweep) "entries from"
           (:atom-count gc-sweep) "atoms"])

    (> (get gc-hint :freed-mb 0) 50)
    (conj [:info "Housekeeping: GC hint freed" (:freed-mb gc-hint) "MB"
           "(before:" (:before-mb gc-hint) "MB)"])

    (and (:gc? gc-hint) (:still-fragmented? gc-hint))
    (conj [:warn "Housekeeping: committed heap not returning to the OS —"
           "committed" (:committed-mb gc-hint) "MB vs used"
           (:used-mb gc-hint) "MB."
           "Check -XX:-ExplicitGCInvokesConcurrent is set."])))

(defn- emit-log-event!
  "Boundary: write one [level & args] event through timbre."
  [[level & args]]
  (let [msg (str/join " " (map #(if (nil? %) "nil" %) args))]
    (case level
      :info (log/info msg)
      :warn (log/warn msg))))

(m/=> run-sweep-tasks [:=> [:cat [:=> [:cat :symbol :any] :any] [:sequential SweepTask]]
                       [:map-of :keyword :any]])
(m/=> sweep-result [:=> [:cat [:map-of :keyword :any] :map] :map])
(m/=> sweep-log-events [:=> [:cat :map] [:vector [:cat [:enum :info :warn] [:* :any]]]])

(defn- call-sweep-task
  [sym fallback]
  (resolve-and-call sym #(%) fallback))

(defn housekeeping-sweep!
  "Run a single housekeeping sweep. Each task is guarded independently.

   Stages: run-sweep-tasks (Collect, over `sweep-tasks`), sweep-result
   (Promote), sweep-log-events (Pipeline), and this fn, the Boundary: the
   sweep counter, the clock, the GC hint and the logs live here only.

   opts :gc-ports are the injected ports of `gc-gate/gc-hint!` (:probe, :gc!,
   :thresholds); omitted, the live heap and Runtime.gc are used.

   JVM GC hint, GATED on real fragmentation (scheduler.gc-gate). An
   unconditional System/gc here was a stop-the-world Old collection every
   sweep under -XX:-ExplicitGCInvokesConcurrent (18 in 83 min, 25.5 s total).
   Below the gate G1PeriodicGCInterval does the uncommit."
  ([] (housekeeping-sweep! {}))
  ([{:keys [gc-ports] :or {gc-ports {}}}]
   (let [start-ms (System/currentTimeMillis)
         sweep-num (-> (swap! scheduler-state update :sweep-count inc)
                       :sweep-count)
         _ (log/debug "Housekeeping: sweep" sweep-num "starting")
         task-results (run-sweep-tasks call-sweep-task sweep-tasks)
         gc-hint-result (gc-gate/gc-hint! gc-ports)
         elapsed-ms (- (System/currentTimeMillis) start-ms)
         result (sweep-result task-results
                              {:pending-reembed (apply call-sweep-task pending-reembed-task)
                               :gc-hint gc-hint-result
                               :sweep-number sweep-num
                               :duration-ms elapsed-ms
                               :timestamp (java.time.Instant/now)})]
     (run! emit-log-event! (sweep-log-events result))
     (swap! scheduler-state assoc
            :last-run (java.time.Instant/now)
            :last-result result)
     (log/info "Housekeeping: sweep" sweep-num "completed in" elapsed-ms "ms")
     result)))

(def SweepResult
  "One sweep's per-task results. Each task answers its callee's value, its
   fallback merged with {:failed true :error _}, or {:skipped true :reason _}."
  [:map
   [:callbacks :any] [:async-batches :any] [:event-journal :any]
   [:saa-states :any] [:gc-sweep :any] [:terminal-liveness :any]
   [:ledger-cold :any] [:kg-gc :any] [:pending-reembed :any]
   [:gc-hint [:map [:freed-mb :int] [:committed-freed-mb :int]]]
   [:sweep-number [:int {:min 1}]] [:duration-ms :int] [:timestamp inst?]])

(m/=> housekeeping-sweep! [:=> [:cat] SweepResult])

;; =============================================================================
;; Scheduler Lifecycle
;; =============================================================================

(defn- get-housekeeping-config
  "Read housekeeping config with defaults applied."
  []
  (let [cfg (config/get-service-config :housekeeping)]
    {:enabled (get cfg :enabled true)
     :interval-minutes (get cfg :interval-minutes 5)}))

(defn- make-sweep-task
  "Create a Runnable that runs a housekeeping sweep."
  []
  (reify Runnable
    (run [_]
      ;; Boundary — MUST catch Throwable: ScheduledExecutorService silently
      ;; stops scheduling future executions if a Runnable throws.
      (try
        (housekeeping-sweep!)
        (catch Throwable t
          (log/error t "Housekeeping: sweep threw (caught at boundary)"))))))

(defn start!
  "Start the periodic housekeeping scheduler."
  []
  (let [{:keys [enabled interval-minutes]} (get-housekeeping-config)]
    (cond
      (not enabled)
      (do
        (log/info "Housekeeping: scheduler disabled via config")
        {:started false :reason "disabled"})

      (:running? @scheduler-state)
      (do
        (log/info "Housekeeping: already running, skipping start")
        {:started false :reason "already-running"})

      :else
      (try
        (let [^ScheduledExecutorService executor
              (Executors/newSingleThreadScheduledExecutor
               (reify java.util.concurrent.ThreadFactory
                 (newThread [_ r]
                   (doto (Thread. r "hive-housekeeping")
                     (.setDaemon true)))))
              task (make-sweep-task)]
          (.scheduleWithFixedDelay executor task
                                   (long interval-minutes)
                                   (long interval-minutes)
                                   TimeUnit/MINUTES)
          (swap! scheduler-state assoc
                 :executor executor
                 :running? true)
          (log/info "Housekeeping: scheduler started"
                    {:interval-minutes interval-minutes})
          {:started true :interval-minutes interval-minutes})
        (catch Exception e
          (log/error e "Housekeeping: failed to start scheduler")
          {:started false :reason (.getMessage e)})))))

(m/=> start! [:=> [:cat]
              [:or
               [:map [:started [:= true]] [:interval-minutes number?]]
               [:map [:started [:= false]] [:reason [:maybe :string]]]]])

(defn stop!
  "Stop the periodic housekeeping scheduler."
  []
  (if-not (:running? @scheduler-state)
    {:stopped false :reason "not-running"}
    (try
      (let [^ScheduledExecutorService executor (:executor @scheduler-state)
            sweeps (:sweep-count @scheduler-state)]
        (.shutdown executor)
        (when-not (.awaitTermination executor 5 TimeUnit/SECONDS)
          (.shutdownNow executor)
          (log/warn "Housekeeping: forced shutdown after 5s timeout"))
        (swap! scheduler-state assoc
               :executor nil
               :running? false)
        (log/info "Housekeeping: scheduler stopped after" sweeps "sweeps")
        {:stopped true :sweeps-completed sweeps})
      (catch Exception e
        (log/error e "Housekeeping: error during shutdown")
        (swap! scheduler-state assoc :executor nil :running? false)
        {:stopped true :error (.getMessage e)}))))

(m/=> stop! [:=> [:cat]
             [:or
              [:map [:stopped [:= false]] [:reason [:= "not-running"]]]
              [:map [:stopped [:= true]] [:sweeps-completed :int]]
              [:map [:stopped [:= true]] [:error [:maybe :string]]]]])

(defn status
  "Return current housekeeping scheduler status."
  []
  (let [state @scheduler-state]
    {:running? (:running? state)
     :sweep-count (:sweep-count state)
     :last-run (:last-run state)
     :last-result (:last-result state)
     :config (get-housekeeping-config)}))

(m/=> status [:=> [:cat]
              [:map
               [:running? :boolean]
               [:sweep-count :int]
               [:last-run [:maybe inst?]]
               [:last-result [:maybe SweepResult]]
               [:config [:map [:enabled :any] [:interval-minutes :any]]]]])

(ns hive-mcp.scheduler.contract-test
  "Contract facets for the schedulers: DAG eligibility, the Vulcan readiness
   and sort rules, the pure DAG-wave transitions, the KG-GC verdict and
   continuation probe, and the lifecycle surfaces of the decay and
   housekeeping schedulers. Every port (deps-fn, state-fn, heap-fn, now-fn,
   cleanup-fn) is injected; nothing reads the live stores."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.scheduler.dag-eligibility :as elig]
            [hive-mcp.scheduler.dag-waves :as dag]
            [hive-mcp.scheduler.decay :as decay]
            [hive-mcp.scheduler.housekeeping :as hk]
            [hive-mcp.scheduler.kg-gc :as kg-gc]
            [hive-mcp.scheduler.vulcan :as vulcan]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ---------------------------------------------------------------------------
;;; dag-eligibility
;;; ---------------------------------------------------------------------------

(def ^:private Card
  [:map
   [:id [:enum "a" "b" "c" "d" "e"]]
   [:title {:optional true} [:enum "Fix it" "PEDRO DECISION: pick" "human only review" "plain"]]
   [:tags {:optional true}
    [:vector [:enum "wave:1" "wave:2" "wave:3" "human-only" "decision" "x" :kw]]]])

(def ^:private Cards [:vector {:max 6} Card])

(defn- human?
  "Oracle: the title regex or a reserved tag, per the ns docstring."
  [{:keys [title tags]}]
  (boolean (or (and title (re-find #"(?i)PEDRO DECISION|HUMAN ONLY" title))
               (some #(contains? elig/human-only-tags (str %)) tags))))

(defn- wave-of [{:keys [tags]}]
  (or (some #(when-let [[_ n] (re-matches #"wave:(\d+)" (str %))] (parse-long n)) tags) 0))

(deftrifecta-from-schema human-only-reads-title-and-tags
  #'elig/human-only?
  {:in [:cat Card]
   :out :boolean
   :rel (fn [[c] out] (= out (human? c)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 200})

(deftrifecta-from-schema task-wave-is-the-first-wave-tag
  #'elig/task-wave
  {:in [:cat Card]
   :out [:int {:min 0}]
   :rel (fn [[c] out] (= out (wave-of c)))
   :golden-path "test/golden/hive-mcp/scheduler/task-wave.edn"
   :num-tests 200})

(deftrifecta-from-schema lowest-open-wave-keeps-one-wave
  #'elig/lowest-open-wave
  {:in [:cat [:set [:enum "a" "b" "c"]] Cards]
   :out [:vector elig/Task]
   :rel (fn [[closed todos] out]
          (let [open (remove #(contains? closed (:id %)) todos)]
            (and (every? #(not (contains? closed (:id %))) out)
                 (if (seq open)
                   (and (seq out)
                        (apply = (map wave-of out))
                        (= (wave-of (first out)) (apply min (map wave-of open))))
                   (empty? out)))))
   :num-tests 200})

(deftrifecta-from-schema eligible-never-hands-a-human-card
  #'elig/eligible
  {:in [:cat :boolean [:set [:enum "a" "b"]] Cards]
   :out [:vector elig/Task]
   :rel (fn [[bounded? closed todos] out]
          (and (not-any? human? out)
               (if bounded?
                 (= out (elig/lowest-open-wave closed (remove human? todos)))
                 (= out (vec (remove human? todos))))))
   :golden-path "test/golden/hive-mcp/scheduler/eligible.edn"
   :num-tests 200})

(deftrifecta-from-schema run-scope-precedence
  #'elig/run-scope
  {:in [:cat [:enum "plan-1"]
        [:map
         [:task-ids {:optional true} [:vector {:max 2} [:enum "a" "b"]]]
         [:board-wide? {:optional true} :boolean]]
        [:vector {:max 2} [:enum "c" "d"]]]
   :out [:maybe [:set :any]]
   ;; :require-plan-scope? is never generated here, so run-scope never throws
   :rel (fn [[_ {:keys [task-ids board-wide?]} plan-tasks] out]
          (cond (seq task-ids)   (= out (set task-ids))
                board-wide?      (nil? out)
                (seq plan-tasks) (= out (set plan-tasks))
                :else            (nil? out)))
   :classify (fn [[_ {:keys [task-ids board-wide?]} plan-tasks] _]
               (cond (seq task-ids) :explicit board-wide? :board-wide
                     (seq plan-tasks) :plan :else :unbounded))
   :classify-domain #{:explicit :board-wide :plan :unbounded}
   :classify-floor 10
   :num-tests 200})

(deftest run-scope-refuses-a-required-empty-plan
  (is (= :no-plan-scope
         (try (elig/run-scope "p" {:require-plan-scope? true} []) nil
              (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))

;;; ---------------------------------------------------------------------------
;;; vulcan — readiness over injected ports
;;; ---------------------------------------------------------------------------

(def ^:private dep-graph {"t1" #{} "t2" #{"t1"} "t3" #{"t2"} "t4" #{"t1" "x"} "t5" #{"t5"}})
(def ^:private deps-fn #(get dep-graph % #{}))
(def ^:private states {"t1" :done "t2" :open "x" :missing "t3" :invalid "t5" :lookup-error})
(def ^:private state-fn #(get states % false))

(def ^:private TaskId [:enum "t1" "t2" "t3" "t4" "t5" "x" "zz"])
(def ^:private Completed [:set [:enum "t1" "t2" "x"]])

(def ^:private RawState
  [:enum :done :open :missing :invalid :lookup-error true false nil :weird "done"])

(deftrifecta-from-schema dependency-state-normalizes-every-lookup
  #'vulcan/dependency-state
  {:in [:cat TaskId Completed [:enum {} (constantly :done) (constantly true) (constantly false)
                                (constantly nil) (constantly :weird) (constantly :lookup-error)
                                state-fn]]
   :out vulcan/EntryState
   :rel (fn [[id completed f] out]
          (if (contains? completed id)
            (= :done out)
            (let [s (f id)]
              (= out (cond (true? s) :open
                           (or (false? s) (nil? s)) :missing
                           (#{:done :open :missing :invalid :lookup-error} s) s
                           :else :invalid)))))
   :classify (fn [_ out] out)
   :classify-domain vulcan/EntryState
   :classify-floor 5
   :num-tests 300})

(deftrifecta-from-schema dep-satisfied-only-by-done
  #'vulcan/dep-satisfied?
  {:in [:cat TaskId Completed [:enum {} state-fn (constantly true)]]
   :out :boolean
   :rel (fn [[id completed f] out] (= out (= :done (vulcan/dependency-state id completed f))))
   :num-tests 200})

(deftrifecta-from-schema task-ready-needs-every-dep
  #'vulcan/task-ready?
  {:in [:cat TaskId Completed [:enum {} deps-fn] [:enum {} state-fn]]
   :out :boolean
   :rel (fn [[id completed df sf] out]
          (= out (every? #(vulcan/dep-satisfied? % completed sf) (df id))))
   ;; no golden: the inputs carry fns (the ports), which EDN cannot snapshot;
   ;; prioritize-tasks-golden pins readiness over the same ports
   :num-tests 200})

(def ^:private VTasks [:vector {:max 5} [:map [:id TaskId]
                                         [:priority {:optional true} [:enum "high" "medium" "low" "priority-high" "odd"]]]])

(deftrifecta-from-schema filter-ready-is-a-subsequence
  #'vulcan/filter-ready-tasks
  {:in [:cat VTasks Completed [:enum {} deps-fn] [:enum {} state-fn]]
   :out [:vector vulcan/VTask]
   :rel (fn [[tasks completed df sf] out]
          (= out (filterv #(vulcan/task-ready? (:id %) completed df sf) tasks)))
   :num-tests 200})

(deftrifecta-from-schema wave-number-is-dependency-depth
  #'vulcan/compute-wave-number
  {:in [:cat TaskId [:enum {} deps-fn]]
   :out [:int {:min 0}]
   :rel (fn [[id _] out]
          (= out (get {"t1" 0, "t2" 1, "t3" 2, "t4" 1, "t5" 1, "x" 0, "zz" 0} id)))
   :num-tests 100})

(deftrifecta-from-schema enrich-assigns-every-wave
  #'vulcan/enrich-with-wave-numbers
  {:in [:cat VTasks [:enum {} deps-fn]]
   :out [:vector [:map [:wave-number [:int {:min 0}]]]]
   :rel (fn [[tasks df] out]
          (and (= (count tasks) (count out))
               (every? (fn [[t o]] (= (:wave-number o) (vulcan/compute-wave-number (:id t) df)))
                       (map vector tasks out))))
   :num-tests 100})

(def ^:private rank {"high" 0 "priority-high" 0 "medium" 1 "low" 2})

(deftrifecta-from-schema sort-vulcan-orders-by-priority-then-wave
  #'vulcan/sort-vulcan
  {:in [:cat [:vector {:max 6} [:map [:id TaskId]
                               [:priority {:optional true} [:enum "high" "medium" "low" "priority-high" "odd"]]
                               [:wave-number {:optional true} [:int {:min 0 :max 3}]]]]]
   :out [:vector vulcan/VTask]
   :rel (fn [[tasks] out]
          (and (= (frequencies tasks) (frequencies out))
               (let [k (fn [t] [(get rank (or (:priority t) "medium") 1) (or (:wave-number t) 0) (str (:id t))])]
                 (= (map k out) (sort (map k tasks))))))
   :golden-path "test/golden/hive-mcp/scheduler/sort-vulcan.edn"
   :num-tests 200})

(golden/deftest-golden prioritize-tasks-golden
  "test/golden/hive-mcp/scheduler/prioritize-tasks.edn"
  (let [tasks [{:id "t1" :priority "low"} {:id "t2" :priority "high"} {:id "t3"}
               {:id "t4"} {:id "t5"} {:id "zz" :priority "high"}]
        throwing (fn [id] (if (= id "t5") (throw (ex-info "kg down" {})) (deps-fn id)))]
    {:ports    (vulcan/prioritize-tasks tasks #{"t1"} {:deps-fn deps-fn :state-fn state-fn})
     :legacy   (vulcan/prioritize-tasks tasks #{} {:deps-fn deps-fn :exists-fn (constantly true)})
     :scoped   (vulcan/prioritize-tasks tasks #{} {:deps-fn deps-fn :state-fn state-fn
                                                   :task_ids ["t1" "zz"]})
     :throwing (vulcan/prioritize-tasks tasks #{} {:deps-fn throwing :state-fn state-fn})}))

(deftest entry-state-classifies-kanban-content
  (testing "every documented arm"
    (is (= :missing (vulcan/entry-state nil)))
    (is (= :done (vulcan/entry-state {:content "{\"task-type\":\"kanban\",\"status\":\"done\"}"})))
    (is (= :open (vulcan/entry-state {:content {:task-type "kanban" :status :inprogress}})))
    (is (= :invalid (vulcan/entry-state {:content {:task-type "note" :status "done"}})))
    (is (= :invalid (vulcan/entry-state {:content "{not json"})))
    (is (= :invalid (vulcan/entry-state {:content {"task-type" "kanban" "status" "parked"}})))))

;;; ---------------------------------------------------------------------------
;;; dag-waves — the pure transitions
;;; ---------------------------------------------------------------------------

(deftrifecta-from-schema ling-outcome-reads-the-event-table
  #'dag/ling-outcome
  {:in [:cat [:enum :completed :truncated :error :context-death :blocked :unknown]
        [:maybe [:map [:result {:optional true} [:enum "failure" "success" :failure]]]]]
   :out dag/LingOutcome
   :rel (fn [[et data] out]
          (= out (cond (= "failure" (str (:result data))) :failed
                       (#{:completed :unknown} et) :succeeded
                       :else :failed)))
   :classify (fn [_ out] out)
   :classify-domain dag/LingOutcome
   :num-tests 200})

(deftrifecta-from-schema topic-prefixes-hivemind
  #'dag/event-channel-topic
  {:in [:cat [:enum :completed :error "truncated"]]
   :out :keyword
   :rel (fn [[et] out] (= out (keyword (str "hivemind-" (name et)))))
   :num-tests 20})

(def ^:private SmallState
  [:map
   [:dispatched [:map-of [:enum "a" "b"] [:enum "l1" "l2"]]]
   [:completed [:set [:enum "c"]]]
   [:failed [:set [:enum "d"]]]
   [:wave-log [:vector [:map [:event :keyword]]]]])

(deftrifecta-from-schema record-outcome-moves-the-task
  #'dag/record-outcome
  {:in [:cat SmallState [:enum "a" "b" "z"] [:enum "l1"] dag/LingOutcome [:int {:min 0 :max 100}]]
   :out dag/DagState
   :rel (fn [[s tid lid outcome ts] out]
          (let [bucket (if (= :failed outcome) :failed :completed)]
            (and (not (contains? (:dispatched out) tid))
                 (contains? (get out bucket) tid)
                 (= (inc (count (:wave-log s))) (count (:wave-log out)))
                 (= {:event (if (= :failed outcome) :failed :completed)
                     :task-id tid :ling-id lid :timestamp ts}
                    (peek (:wave-log out))))))
   :golden-path "test/golden/hive-mcp/scheduler/record-outcome.edn"
   :num-tests 200})

(deftrifecta-from-schema finished-means-empty-both
  #'dag/plan-finished?
  {:in [:cat [:enum [] [] [1]] [:enum {} {} {} {"t" "l"}]]
   :out :boolean
   :rel (fn [[ready in-flight] out] (= out (and (empty? ready) (empty? in-flight))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 200})

(deftrifecta-from-schema blocked-is-the-transitive-closure
  #'dag/blocked-task-ids
  {:in [:cat [:map-of [:enum "a" "b" "c" "d" "e"] [:set [:enum "a" "b" "c" "d" "e" "f"]]]
        [:set [:enum "e" "f"]]]
   :out [:set :any]
   :rel (fn [[graph failed] out]
          (let [bad (into (set failed) out)]
            (and (every? (fn [t] (some bad (get graph t))) out)
                 (every? (fn [[t deps]] (or (contains? out t) (not-any? bad deps))) graph))))
   :golden-path "test/golden/hive-mcp/scheduler/blocked-task-ids.edn"
   :num-tests 200})

(deftrifecta-from-schema plan-summary-counts-and-verdict
  #'dag/plan-summary
  {:in [:cat [:map [:plan-id [:enum "p"]] [:completed [:set [:enum "a" "b"]]]
              [:failed [:set [:enum "c"]]] [:opts [:map [:run-id {:optional true} [:enum "r"]]]]]
        [:set [:enum "d" "e"]]]
   :out dag/PlanSummary
   :rel (fn [[{:keys [completed failed opts]} skipped] out]
          (and (= (count completed) (:completed out))
               (= (vec (sort failed)) (:failed-ids out))
               (= (vec (sort skipped)) (:skipped-ids out))
               (= (:run-id opts) (:run-id out))
               (= (:result out) (if (and (empty? failed) (empty? skipped)) "success" "failure"))))
   :num-tests 100})

(golden/deftest-golden dag-surface-when-idle-golden
  "test/golden/hive-mcp/scheduler/dag-idle-surface.edn"
  ;; the scheduler is inactive in the test JVM: on-ling-complete is a no-op,
  ;; stop-dag! reports the idle state, and the listen set is the event table's
  ;; terminal rows
  (let [stopped (dag/stop-dag!)]
    {:terminal (dag/terminal-event-types)
     :capabilities dag/capabilities
     :on-complete (dag/on-ling-complete {:agent-id "nobody" :event-type :completed})
     :stopped (select-keys stopped [:stopped :completed-count :failed-count :dispatched-count])
     :status (select-keys (dag/dag-status) [:active :completed :failed :dispatched :ready])
     :dispatch-empty (dag/dispatch-wave! [] 3 {})}))

(deftest start-dag-refuses-an-empty-task-ids
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"empty"
                        (dag/start-dag! "plan-x" {:task-ids []}))))

;;; ---------------------------------------------------------------------------
;;; kg-gc — verdict and the continuation probe
;;; ---------------------------------------------------------------------------

(def ^:private CleanupProbe
  ;; enums weighted toward clean inputs so the :ok and :vacuous arms are
  ;; reached as often as the :blind one
  [:map
   [:universe [:map [:sources [:enum 0 1 2 3]]]]
   [:blind [:enum [] [] [] [{:class :edges :reason "io"}]]]
   [:scanned [:int {:min 0 :max 9}]]
   [:orphaned [:int {:min 0 :max 3}]]
   [:errors [:enum 0 0 0 1]]
   [:partial [:vector {:max 2} [:map [:stopped {:optional true} [:enum :deadline]]]]]
   [:details [:vector {:max 4} [:map [:complete? :boolean]
                                [:outcome [:enum :orphaned :pruned :demoted]]
                                [:effect-count [:int {:min 0 :max 5}]]]]]])

(deftrifecta-from-schema verdict-reports-what-it-cannot-see
  #'kg-gc/verdict
  {:in [:cat CleanupProbe]
   :out kg-gc/Verdict
   :rel (fn [[r] out]
          (let [vacuous? (and (zero? (get-in r [:universe :sources])) (not (:error r)))
                done (filter :complete? (:details r))]
            (and (= vacuous? (:vacuous? out))
                 (= (:ok? out) (and (empty? (:blind out)) (zero? (:errors r))))
                 (= (count (:blind out))
                    (+ (count (:blind r)) (if vacuous? 1 0) (if (:error r) 1 0)))
                 (= (get-in out [:collected :total])
                    (count (filter #(#{:orphaned :pruned :demoted} (:outcome %)) done)))
                 (= (get-in out [:coverage :partial]) (count (:partial r))))))
   :classify (fn [_ out] (cond (:vacuous? out) :vacuous (:ok? out) :ok :else :blind))
   :classify-domain #{:vacuous :ok :blind}
   :golden-path "test/golden/hive-mcp/scheduler/kg-gc-verdict.edn"
   :num-tests 200})

(deftrifecta-from-schema continue-probe-stops-on-heap-or-deadline
  #'kg-gc/make-continue?
  {:in [:cat [:map [:max-heap-ratio [:enum 0.5]] [:deadline-ms [:enum 100]]]
        [:int {:min 0 :max 10}]
        [:map [:heap-fn [:enum {} (constantly 0.1) (constantly 0.9)]]
         [:now-fn [:enum {} (constantly 50) (constantly 500)]]]]
   :out kg-gc/ContinueFn
   :rel (fn [[cfg start {:keys [heap-fn now-fn]}] probe]
          (= (probe)
             (cond (> (heap-fn) (:max-heap-ratio cfg)) :heap-pressure
                   (> (- (now-fn) start) (:deadline-ms cfg)) :deadline
                   :else nil)))
   ;; the output is a fn: no data mutants to derive from it
   :mutation false
   :num-tests 50})

(golden/deftest-golden kg-gc-lane-golden
  "test/golden/hive-mcp/scheduler/kg-gc-lane.edn"
  ;; The lane keeps process-global bookkeeping (cursor, last run, cycle count)
  ;; that other tests in the same JVM tick. The test owns it: it resets the
  ;; lane before driving it, snapshots only the cycle DELTA it caused, and
  ;; resets again on the way out. The cleanup fn and the heap probe are
  ;; injected stubs, so no KG or store is read.
  (let [_ (kg-gc/reset-state!)
        cycles-before (:cycles (kg-gc/status))
        seen (atom nil)
        cleanup (fn [opts] (reset! seen (dissoc opts :continue?))
                  {:universe {:sources 2} :scanned 2 :orphaned 1
                   :details [{:complete? true :outcome :orphaned :effect-count 3}]})
        ran (kg-gc/run-lane! {:force? true :cleanup-fn cleanup :heap-fn (constantly 0.1)})
        not-due (kg-gc/run-lane! {:cleanup-fn cleanup :heap-fn (constantly 0.1)})
        heavy (kg-gc/run-lane! {:force? true :cleanup-fn cleanup :heap-fn (constantly 0.99)})
        disabled (kg-gc/run-lane! {:enabled false})
        st (kg-gc/status)]
    (kg-gc/reset-state!)
    {:ran (dissoc ran :duration-ms) :cleanup-opts @seen
     :not-due not-due :heavy (dissoc heavy :heap-ratio) :disabled disabled
     :status {:running? (:running? st) :cycles-run (- (:cycles st) cycles-before)}
     :config-validates (m/validate kg-gc/LaneConfig (kg-gc/lane-config))
     :heap-ratio-sane (<= 0.0 (kg-gc/heap-ratio))}))

;;; ---------------------------------------------------------------------------
;;; housekeeping / decay — lifecycle surfaces, never started here
;;; ---------------------------------------------------------------------------

(deftrifecta-from-schema disjoint-owners-is-set-disjointness
  #'hk/disjoint-sweep-owners?
  {:in [:cat [:vector {:max 3} [:enum "swarm/stale-callbacks" "x/other" "system/jvm-gc-hint" "y"]]]
   :out :boolean
   :rel (fn [[names] out] (= out (not-any? hk/housekeeping-sweep-names names)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 100})

(golden/deftest-golden scheduler-lifecycles-when-stopped-golden
  "test/golden/hive-mcp/scheduler/lifecycles-stopped.edn"
  ;; the test JVM never starts either executor; stop! on a stopped scheduler
  ;; reports :not-running, and status validates against its contract
  {:decay-stop (decay/stop!)
   :decay-status (select-keys (decay/status) [:running?])
   :hk-stop (hk/stop!)
   :hk-status (select-keys (hk/status) [:running? :config])})

(deftest sweep-and-cycle-contracts-name-their-outputs
  (testing "the cycle / sweep result schemas generate and validate an example"
    (is (m/validate decay/StartResult {:started false :reason "disabled"}))
    (is (m/validate decay/StopResult {:stopped false :reason "not-running"}))
    (is (m/validate hk/SweepResult (mg/generate hk/SweepResult {:seed 1})))))

;;; ---------------------------------------------------------------------------
;;; Coverage gate
;;; ---------------------------------------------------------------------------

(coverage/deftest-contract-coverage scheduler-contracts
  {:paths ["src/hive_mcp/scheduler"]})

(deftest scheduler-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths ["src/hive_mcp/scheduler"]})]
    (is (= 47 (count (:universe report))) "every public defn of the tree")
    (is (= 47 (count (:covered report))))))

(deftest every-scheduler-contract-generates-both-sides
  (doseq [ns-sym '[hive-mcp.scheduler.dag-eligibility hive-mcp.scheduler.dag-waves
                   hive-mcp.scheduler.decay hive-mcp.scheduler.housekeeping
                   hive-mcp.scheduler.kg-gc hive-mcp.scheduler.vulcan]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

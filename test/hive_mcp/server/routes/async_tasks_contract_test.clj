(ns hive-mcp.server.routes.async-tasks-contract-test
  "Contract facets for the async-task registry boundary and the process-role
   guard shim, plus the coverage gate over both files.

   Every facet runs the subject for real and checks what it answered against
   the output side of its own registered `m/=>` contract, so a contract that
   drifts from the code fails here rather than under instrumentation in
   production. The registry runs on an inline runner (no threads), and the
   guard state, which is process global, is put back after each test."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.server.guards :as guards]
            [hive-mcp.server.routes.async-tasks :as at]
            [hive-schemas.coverage :as coverage]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private inline-runner
  (reify at/ITaskRunner
    (-run [_ f] (f) ::handle)
    (-cancel [_ _] false)
    (-view [_ h] {:present? (some? h) :done? true :cancelled? false})))

(defn- own-state
  "Inline runner and an empty registry for the test; prior runner, guard
   flags and coordinator mark restored after."
  [f]
  (let [runner  @at/runner
        mode    (:mode (guards/guard-status))
        enabled (:enabled? (guards/guard-status))
        running (guards/coordinator-running?)]
    (try
      (reset! at/runner inline-runner)
      (at/reset-registry!)
      (f)
      (finally
        (at/reset-registry!)
        (reset! at/runner runner)
        (guards/set-enforcement-mode! mode)
        (if enabled (guards/enable-guards!) (guards/disable-guards!))
        (if running (guards/mark-coordinator-running!) (guards/mark-coordinator-stopped!))))))

(use-fixtures :each own-state)

(defn- output-schema
  "The output side of the single-arity contract registered for `qsym`."
  [qsym]
  (let [schema (get-in (m/function-schemas) [(symbol (namespace qsym)) (symbol (name qsym)) :schema])]
    (last (m/children schema))))

(defn- conforms?
  "True iff `value` satisfies the output contract of `qsym`."
  [qsym value]
  (let [out (output-schema qsym)]
    (or (m/validate out value)
        (do (println qsym (m/explain out value)) false))))

;;; ---------------------------------------------------------------------------
;;; async-tasks
;;; ---------------------------------------------------------------------------

(deftest current-runner-answers-the-installed-runner
  (is (identical? inline-runner (at/current-runner)))
  (is (conforms? `at/current-runner (at/current-runner))))

(deftest submit-registers-a-row-that-get-task-and-list-tasks-render
  (let [ran (atom false)]
    (at/submit! {:task-id "c-1" :tool "memory" :caller-id "test"
                 :f #(reset! ran true)})
    (is @ran)
    (is (conforms? `at/get-task (at/get-task "c-1")))
    (is (= :task/done (:state (at/get-task "c-1"))))
    (is (conforms? `at/get-task (at/get-task "c-absent")) "a miss is nil")
    (is (conforms? `at/list-tasks (at/list-tasks)))
    (is (= ["c-1"] (mapv :task-id (at/list-tasks))))))

(deftest submit-accepts-the-timeouts-the-middleware-forwards
  (testing "nil, zero and a positive bound are all accepted; only a positive one is recorded"
    (doseq [[id t recorded] [["c-nil" nil nil] ["c-zero" 0 nil] ["c-pos" 60000 60000]]]
      (at/submit! {:task-id id :tool "memory" :caller-id "test" :timeout-ms t :f (fn [] :ok)})
      (is (conforms? `at/get-task (at/get-task id)) id)
      (is (= recorded (:timeout-ms (at/get-task id))) id))))

(deftest running-tasks-excludes-terminal-rows
  (at/submit! {:task-id "c-done" :tool "memory" :caller-id "test" :f (fn [] nil)})
  (is (conforms? `at/running-tasks (at/running-tasks)))
  (is (= [] (at/running-tasks)) "an inline task has finished by the time submit! returns"))

(deftest cancel-answers-a-cancel-outcome-for-every-case
  (at/submit! {:task-id "c-fin" :tool "memory" :caller-id "test" :f (fn [] nil)})
  (doseq [[id variant] [["c-fin" :cancel/already-finished]
                        ["c-stranger" :cancel/unknown-task]]]
    (let [out (at/cancel! id)]
      (is (conforms? `at/cancel! out) id)
      (is (= variant (:adt/variant out)) id))))

(deftest forget-reports-how-many-rows-it-dropped
  (at/submit! {:task-id "c-a" :tool "memory" :caller-id "test" :f (fn [] nil)})
  (at/submit! {:task-id "c-b" :tool "memory" :caller-id "test" :f (fn [] nil)})
  (let [out (at/forget!)]
    (is (conforms? `at/forget! out))
    (is (= {:forgotten 2} out)))
  (is (= {:forgotten 0} (at/forget!)) "a second sweep finds nothing"))

(deftest reset-registry-empties-and-answers-nil
  (at/submit! {:task-id "c-r" :tool "memory" :caller-id "test" :f (fn [] nil)})
  (is (conforms? `at/reset-registry! (at/reset-registry!)))
  (is (= [] (at/list-tasks))))

(deftest every-async-tasks-contract-generates-both-sides
  (doseq [[fn-sym {:keys [schema]}] (get (m/function-schemas) 'hive-mcp.server.routes.async-tasks)
          side (take 2 (m/children schema))]
    (testing (str fn-sym)
      (is (= 3 (count (mg/sample side {:size 3 :seed 7})))))))

;;; ---------------------------------------------------------------------------
;;; guards shim
;;; ---------------------------------------------------------------------------

(deftest role-readers-agree-with-each-other
  (let [child? (guards/child-ling?)]
    (is (conforms? `guards/child-ling? child?))
    (is (conforms? `guards/coordinator? (guards/coordinator?)))
    (is (= (not child?) (guards/coordinator?)))
    (is (conforms? `guards/get-role (guards/get-role)))
    (is (= child? (= "child-ling" (guards/get-role))))
    (is (conforms? `guards/ling-depth (guards/ling-depth)))))

(deftest child-ling-env-is-one-level-deeper
  (let [env (guards/child-ling-env)]
    (is (conforms? `guards/child-ling-env env))
    (is (= (inc (guards/ling-depth)) (parse-long (get env "HIVE_LING_DEPTH"))))))

(deftest configuration-setters-answer-nil-and-take-effect
  (is (conforms? `guards/set-enforcement-mode! (guards/set-enforcement-mode! :block)))
  (is (= :block (:mode (guards/guard-status))))
  (is (nil? (guards/set-enforcement-mode! :bogus)) "an unknown mode is ignored")
  (is (= :block (:mode (guards/guard-status))))
  (is (conforms? `guards/disable-guards! (guards/disable-guards!)))
  (is (false? (:enabled? (guards/guard-status))))
  (is (conforms? `guards/enable-guards! (guards/enable-guards!)))
  (is (true? (:enabled? (guards/guard-status))))
  (is (conforms? `guards/guard-status (guards/guard-status))))

(deftest coordinator-mark-round-trips
  (is (conforms? `guards/mark-coordinator-running! (guards/mark-coordinator-running!)))
  (is (true? (guards/coordinator-running?)))
  (is (conforms? `guards/mark-coordinator-stopped! (guards/mark-coordinator-stopped!)))
  (is (conforms? `guards/coordinator-running? (guards/coordinator-running?)))
  (is (false? (guards/coordinator-running?))))

;;; ---------------------------------------------------------------------------
;;; Coverage gate
;;; ---------------------------------------------------------------------------
;;; guards.clj holds only def re-exports and a macro, so on its own the scan
;;; would be vacuous; it is gated together with async_tasks.clj. Its
;;; contracts are asserted directly below instead.

(def ^:private gated-paths
  ["src/hive_mcp/server/routes/async_tasks.clj" "src/hive_mcp/server/guards.clj"])

(coverage/deftest-contract-coverage async-tasks-and-guards-contracts
  {:paths gated-paths})

(deftest async-tasks-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths gated-paths})]
    (is (= 8 (count (:universe report))))
    (is (= 8 (count (:covered report))))))

(deftest every-guards-reexport-is-contracted
  (is (= '#{child-ling? coordinator? get-role ling-depth child-ling-env
            set-enforcement-mode! enable-guards! disable-guards! guard-status
            coordinator-running? mark-coordinator-running! mark-coordinator-stopped!}
         (set (keys (get (m/function-schemas) 'hive-mcp.server.guards))))))

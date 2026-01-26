(ns hive-mcp.tdd.loop
  "TDD Loop - Autonomous Ling-Orchestrated Test-Driven Development.

   Implements ADR 20260126163927-77493f81: Ling TDD Loop.

   Architecture:
   - FSM states: :idle -> :implementing -> :testing -> [:passed | :failed] -> :fixing
   - Bounded iterations (max-tdd-iterations = 5)
   - Event-driven execution with consensus signals

   Key Axiom: 'Drones get ONE shot - no internal retry loop.'
   The TDD loop handles retries EXTERNALLY via ling orchestration.

   Integration:
   - Uses validated_wave.clj pattern for dispatch
   - Uses agora/consensus.clj signals for pass/fail

   SOLID-S: SRP - TDD loop orchestration only
   CLARITY-A: Bounded iterations prevent infinite loops
   CLARITY-T: Event emissions for observability"
  (:require [hive-mcp.tdd.protocol :as proto]
            [hive-mcp.events.core :as ev]
            [hive-mcp.channel.websocket :as ws]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants (CLARITY-A: Architectural Guarantees)
;; =============================================================================

(def ^:const max-tdd-iterations
  "Maximum number of TDD loop iterations.
   Prevents infinite loops - fails fast if tests don't pass after N attempts."
  5)

(def ^:const default-test-command
  "Default test command for Clojure projects."
  "clojure -M:test")

;; =============================================================================
;; FSM States
;; =============================================================================

(def tdd-states
  "Valid TDD loop states.

   State machine:
   :idle -> :implementing -> :testing -> [:passed | :failed] -> :fixing
                                                      |
                                                      v
                                              :testing (loop)"
  #{:idle :implementing :testing :passed :failed :fixing :max-iterations :error})

(def state-transitions
  "Valid state transitions."
  {:idle         #{:implementing}
   :implementing #{:testing :error}
   :testing      #{:passed :failed :error}
   :passed       #{}  ; Terminal
   :failed       #{:fixing :max-iterations}
   :fixing       #{:testing :error}
   :max-iterations #{} ; Terminal
   :error        #{}}) ; Terminal

;; =============================================================================
;; Loop State Management
;; =============================================================================

(defonce ^{:doc "Active TDD loops: {loop-id -> {:state :iteration :participants ...}}"}
  tdd-loops
  (atom {}))

(defn- generate-loop-id
  "Generate unique loop ID."
  []
  (str "tdd-" (System/currentTimeMillis) "-" (rand-int 10000)))

(defn- get-loop
  "Get TDD loop state by ID."
  [loop-id]
  (get @tdd-loops loop-id))

(defn- update-loop!
  "Update TDD loop state."
  [loop-id updates]
  (swap! tdd-loops update loop-id merge updates))

(defn- transition-state!
  "Transition loop to new state with validation."
  [loop-id new-state]
  (let [current-state (get-in @tdd-loops [loop-id :state])]
    (when-not (contains? (get state-transitions current-state #{}) new-state)
      (log/warn "Invalid state transition" {:from current-state :to new-state}))
    (update-loop! loop-id {:state new-state
                           :state-history (conj (or (get-in @tdd-loops [loop-id :state-history]) [])
                                                {:state new-state
                                                 :timestamp (java.time.Instant/now)})})))

(defn- cleanup-loop!
  "Remove loop state after completion."
  [loop-id]
  (swap! tdd-loops dissoc loop-id))

;; =============================================================================
;; Test Result Parsing
;; =============================================================================

(def test-pass-signals
  "Signals indicating test pass (Nash equilibrium)."
  #{:pass :passed :success :ok})

(def test-fail-signals
  "Signals indicating test failure (reset needed)."
  #{:fail :failed :error})

(defn parse-test-result
  "Parse test execution result into structured format.

   Attempts to parse JSON from result, falls back to text analysis.

   Returns: {:status :pass|:fail
             :failures [{:file :line :message}]
             :summary string}"
  [result]
  (try
    ;; Try JSON parsing first
    (let [json-str (if (str/includes? (or result "") "```")
                     (-> result
                         (str/replace #"```json\s*" "")
                         (str/replace #"```\s*" "")
                         str/trim)
                     (str/trim (or result "")))
          parsed (json/read-str json-str :key-fn keyword)
          status-kw (keyword (str/lower-case (or (:status parsed) "fail")))]
      {:status (if (test-pass-signals status-kw) :pass :fail)
       :failures (or (:failures parsed) [])
       :summary (or (:summary parsed) "")
       :raw result})
    (catch Exception _
      ;; Fallback: text-based analysis
      (let [lower-result (str/lower-case (or result ""))
            ;; Check for "0 failures" or "0 failed" - indicates pass despite containing "fail"
            zero-failures (or (re-find #"0 fail" lower-result)
                              (re-find #"no fail" lower-result))
            ;; Actual failure indicators (exclude zero-failure patterns)
            has-fail-indicators (and (not zero-failures)
                                     (or (str/includes? lower-result "fail")
                                         (str/includes? lower-result "error")
                                         (re-find #"assertion.*fail" lower-result)))
            has-pass-indicators (or zero-failures
                                    (and (str/includes? lower-result "pass")
                                         (not has-fail-indicators)))]
        {:status (if has-pass-indicators :pass :fail)
         :failures []
         :summary (str "Text analysis: " (if has-pass-indicators "PASS" "FAIL"))
         :raw result}))))

(defn format-failures-for-fix
  "Format test failures into a prompt for fix drone."
  [{:keys [failures summary raw]}]
  (str "## Test Failures\n\n"
       (if (seq failures)
         (str/join "\n" (map (fn [{:keys [file line message]}]
                               (format "- %s:%s - %s" (or file "unknown") (or line "?") (or message "no message")))
                             failures))
         (str "Raw test output:\n" raw))
       "\n\n"
       (when summary (str "Summary: " summary))))

;; =============================================================================
;; Event Emissions (CLARITY-T: Telemetry First)
;; =============================================================================

(defn- emit-tdd-event!
  "Emit TDD event to both WebSocket and event system."
  [event-type data]
  (let [event-kw (keyword "tdd" (name event-type))]
    ;; WebSocket for Emacs UI
    (ws/emit! event-kw data)
    ;; Event system for handlers
    (try
      (ev/dispatch [event-kw data])
      (catch Exception e
        (log/warn "Event dispatch failed for" event-kw (.getMessage e))))))

;; =============================================================================
;; Core TDD Loop Implementation
;; =============================================================================

(defn- execute-implement-phase!
  "Execute implementation phase with impl-drone.

   Arguments:
     loop-id     - Loop identifier
     impl-drone  - ITDDParticipant for implementation
     task        - Implementation task spec

   Returns: {:status :completed|:failed :result|:error}"
  [loop-id impl-drone task]
  (log/info "TDD loop" loop-id "- Implementing" {:task (:prompt task)})
  (transition-state! loop-id :implementing)
  (emit-tdd-event! :implementing {:loop-id loop-id
                                  :task (:prompt task)
                                  :participant (proto/participant-id impl-drone)})

  (let [result (proto/execute-task! impl-drone task)]
    (update-loop! loop-id {:last-impl-result result})
    result))

(defn- execute-test-phase!
  "Execute test phase with test-drone.

   Arguments:
     loop-id     - Loop identifier
     test-drone  - ITDDParticipant for testing
     test-cmd    - Test command to execute

   Returns: Parsed test result {:status :pass|:fail :failures [...] :summary}"
  [loop-id test-drone test-cmd]
  (log/info "TDD loop" loop-id "- Testing")
  (transition-state! loop-id :testing)
  (emit-tdd-event! :testing {:loop-id loop-id
                             :test-cmd test-cmd
                             :participant (proto/participant-id test-drone)})

  (let [exec-result (proto/execute-task! test-drone {:prompt test-cmd})
        parsed (if (= :completed (:status exec-result))
                 (parse-test-result (:result exec-result))
                 {:status :fail :failures [] :summary (str "Test execution failed: " (:error exec-result))})]
    (update-loop! loop-id {:last-test-result parsed})
    parsed))

(defn- execute-fix-phase!
  "Execute fix phase with fix-drone.

   Arguments:
     loop-id     - Loop identifier
     fix-drone   - ITDDParticipant for fixing
     test-result - Previous test result with failures
     impl-task   - Original implementation task for context

   Returns: {:status :completed|:failed :result|:error}"
  [loop-id fix-drone test-result impl-task]
  (log/info "TDD loop" loop-id "- Fixing failures")
  (transition-state! loop-id :fixing)
  (emit-tdd-event! :fixing {:loop-id loop-id
                            :failure-count (count (:failures test-result))
                            :participant (proto/participant-id fix-drone)})

  (let [fix-context (format-failures-for-fix test-result)
        result (proto/execute-task! fix-drone {:prompt (:prompt impl-task)
                                               :files (:files impl-task)
                                               :context fix-context})]
    (update-loop! loop-id {:last-fix-result result})
    result))

;; =============================================================================
;; Main TDD Loop (Bounded Iterations)
;; =============================================================================

(defn tdd-loop!
  "Execute a complete TDD loop with bounded iterations.

   Flow:
     IMPLEMENT -> TEST -> [PASS] -> DONE
                    |
                 [FAIL]
                    |
                    v
             FIX -> TEST (repeat up to max-tdd-iterations)

   Arguments:
     impl-task   - Map with :prompt and :files for implementation
     test-cmd    - Test command string (default: clojure -M:test)
     opts        - Options:
                   :impl-drone   - ITDDParticipant for implementation (default: create-impl-drone)
                   :test-drone   - ITDDParticipant for testing (default: create-test-drone)
                   :fix-drone    - ITDDParticipant for fixing (default: create-fix-drone)
                   :max-iterations - Override max iterations (default: 5)
                   :trace        - Emit events (default: true)

   Returns:
     {:status      :passed|:failed|:max-iterations|:error
      :iterations  Number of iterations executed
      :loop-id     Loop identifier
      :test-result Final test result
      :history     [{:iteration N :phase :state :result}]}

   Throws:
     ex-info if max iterations exceeded and no pass.

   CLARITY-A: Bounded iterations - fails fast if tests don't pass after N attempts.
   AXIOM: Drones get ONE shot - retries handled by THIS loop, not internally."
  [impl-task test-cmd & [{:keys [impl-drone test-drone fix-drone max-iterations trace]
                          :or {max-iterations max-tdd-iterations
                               trace true}}]]
  {:pre [(map? impl-task) (contains? impl-task :prompt)]}

  (let [loop-id (generate-loop-id)
        test-cmd (or test-cmd default-test-command)
        impl-drone (or impl-drone (proto/create-impl-drone))
        test-drone (or test-drone (proto/create-test-drone))
        fix-drone (or fix-drone (proto/create-fix-drone))]

    ;; Initialize loop state
    (swap! tdd-loops assoc loop-id
           {:state :idle
            :iteration 0
            :max-iterations max-iterations
            :impl-task impl-task
            :test-cmd test-cmd
            :participants {:impl (proto/participant-id impl-drone)
                           :test (proto/participant-id test-drone)
                           :fix (proto/participant-id fix-drone)}
            :history []
            :state-history []
            :started-at (java.time.Instant/now)})

    (when trace
      (emit-tdd-event! :start {:loop-id loop-id
                               :impl-task (:prompt impl-task)
                               :test-cmd test-cmd
                               :max-iterations max-iterations}))

    (log/info "Starting TDD loop" {:loop-id loop-id :max-iterations max-iterations})

    (try
      ;; Phase 1: Initial Implementation
      (let [impl-result (execute-implement-phase! loop-id impl-drone impl-task)]
        (when (= :failed (:status impl-result))
          (transition-state! loop-id :error)
          (when trace
            (emit-tdd-event! :error {:loop-id loop-id
                                     :phase :implementing
                                     :error (:error impl-result)}))
          (throw (ex-info "Implementation failed"
                          {:loop-id loop-id :phase :implementing :error (:error impl-result)}))))

      ;; Phase 2: Test-Fix Loop
      (loop [iteration 1]
        (update-loop! loop-id {:iteration iteration})

        (when (> iteration max-iterations)
          (log/warn "TDD loop exceeded max iterations" {:loop-id loop-id :max iteration})
          (transition-state! loop-id :max-iterations)
          (when trace
            (emit-tdd-event! :max-iterations {:loop-id loop-id
                                              :iterations iteration
                                              :last-test-result (get-in @tdd-loops [loop-id :last-test-result])}))
          (let [final-state (get-loop loop-id)]
            (cleanup-loop! loop-id)
            (throw (ex-info "TDD loop exceeded max iterations"
                            {:loop-id loop-id
                             :max max-iterations
                             :status :max-iterations
                             :iterations (dec iteration)
                             :last-test-result (:last-test-result final-state)}))))

        ;; Emit iteration event
        (when trace
          (emit-tdd-event! :iteration {:loop-id loop-id
                                       :n iteration
                                       :remaining (- max-iterations iteration)}))

        ;; Execute test phase
        (let [test-result (execute-test-phase! loop-id test-drone test-cmd)]

          ;; Record iteration history
          (update-loop! loop-id
                        {:history (conj (get-in @tdd-loops [loop-id :history] [])
                                        {:iteration iteration
                                         :test-status (:status test-result)
                                         :failure-count (count (:failures test-result))
                                         :timestamp (java.time.Instant/now)})})

          ;; Check test result (consensus signals)
          (if (= :pass (:status test-result))
            ;; SUCCESS - Nash equilibrium reached
            (do
              (log/info "TDD loop PASSED" {:loop-id loop-id :iterations iteration})
              (transition-state! loop-id :passed)
              (when trace
                (emit-tdd-event! :test-result {:loop-id loop-id
                                               :status :pass
                                               :iteration iteration})
                (emit-tdd-event! :consensus {:loop-id loop-id
                                             :iterations iteration
                                             :status :passed}))
              (let [final-state (get-loop loop-id)]
                (cleanup-loop! loop-id)
                {:status :passed
                 :iterations iteration
                 :loop-id loop-id
                 :test-result test-result
                 :history (:history final-state)}))

            ;; FAIL - Need fix
            (do
              (log/info "TDD loop iteration" iteration "FAILED, attempting fix"
                        {:failures (count (:failures test-result))})
              (transition-state! loop-id :failed)
              (when trace
                (emit-tdd-event! :test-result {:loop-id loop-id
                                               :status :fail
                                               :iteration iteration
                                               :failures (:failures test-result)}))

              ;; Execute fix phase
              (let [fix-result (execute-fix-phase! loop-id fix-drone test-result impl-task)]
                (if (= :failed (:status fix-result))
                  ;; Fix failed - try next iteration anyway
                  (do
                    (log/warn "Fix phase failed, continuing to next iteration"
                              {:error (:error fix-result)})
                    (recur (inc iteration)))
                  ;; Fix completed - re-test
                  (recur (inc iteration))))))))

      (catch clojure.lang.ExceptionInfo e
        ;; Expected control flow exception (max iterations, etc.)
        (let [data (ex-data e)]
          (if (= :max-iterations (:status data))
            ;; Return max-iterations result
            (let [final-state (get-loop loop-id)]
              (cleanup-loop! loop-id)
              {:status :max-iterations
               :iterations (:iterations data)
               :loop-id loop-id
               :test-result (:last-test-result data)
               :history (or (:history final-state) [])
               :message (ex-message e)})
            ;; Re-throw non-max-iterations errors
            (throw e))))

      (catch Exception e
        ;; Unexpected error
        (log/error e "TDD loop failed with unexpected error")
        (transition-state! loop-id :error)
        (when trace
          (emit-tdd-event! :error {:loop-id loop-id
                                   :error (.getMessage e)}))
        (let [final-state (get-loop loop-id)]
          (cleanup-loop! loop-id)
          {:status :error
           :iterations (or (:iteration final-state) 0)
           :loop-id loop-id
           :error (.getMessage e)
           :history (or (:history final-state) [])})))))

;; =============================================================================
;; Async TDD Loop (Non-Blocking)
;; =============================================================================

(defn tdd-loop-async!
  "Start a TDD loop asynchronously (returns immediately).

   Returns: {:loop-id string :future future}

   Use deref on the future to get the result."
  [impl-task test-cmd & [opts]]
  (let [loop-id (generate-loop-id)
        f (future (tdd-loop! impl-task test-cmd (assoc opts :loop-id loop-id)))]
    {:loop-id loop-id
     :future f}))

;; =============================================================================
;; Query Functions
;; =============================================================================

(defn get-tdd-loop-status
  "Get current status of a TDD loop.

   Returns: {:loop-id :state :iteration :history} or nil if not found."
  [loop-id]
  (when-let [loop-state (get-loop loop-id)]
    (select-keys loop-state [:state :iteration :history :participants
                             :started-at :last-test-result])))

(defn list-active-tdd-loops
  "List all active TDD loops."
  []
  (->> @tdd-loops
       (map (fn [[id state]]
              {:loop-id id
               :state (:state state)
               :iteration (:iteration state)
               :started-at (:started-at state)}))
       (vec)))

(defn cancel-tdd-loop!
  "Cancel an active TDD loop."
  [loop-id]
  (when (get-loop loop-id)
    (emit-tdd-event! :cancelled {:loop-id loop-id})
    (cleanup-loop! loop-id)
    {:cancelled loop-id}))

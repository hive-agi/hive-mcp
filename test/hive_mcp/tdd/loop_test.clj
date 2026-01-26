(ns hive-mcp.tdd.loop-test
  "TDD tests for hive-mcp.tdd.loop - Autonomous TDD loop orchestration.

   Tests the TDD loop logic using mock participants.
   Key axiom under test: 'Drones get ONE shot - retries handled externally.'"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as string]
            [hive-mcp.tdd.protocol :as proto]
            [hive-mcp.tdd.loop :as tdd]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn cleanup-loops
  "Cleanup active loops before and after each test."
  [f]
  (reset! tdd/tdd-loops {})
  (f)
  (reset! tdd/tdd-loops {}))

(use-fixtures :each cleanup-loops)

;; =============================================================================
;; Mock Helpers
;; =============================================================================

(defn mock-pass-impl
  "Create impl drone that always succeeds."
  []
  (proto/create-mock-participant :impl-drone
                                 [{:status :completed :result "Implemented feature"}]))

(defn mock-fail-impl
  "Create impl drone that always fails."
  []
  (proto/create-mock-participant :impl-drone
                                 [{:status :failed :error "Implementation error"}]))

(defn mock-pass-test
  "Create test drone that always passes."
  []
  (proto/create-mock-participant :test-drone
                                 [{:status :completed
                                   :result "{\"status\": \"pass\", \"failures\": [], \"summary\": \"All tests passed\"}"}]))

(defn mock-fail-test
  "Create test drone that always fails."
  []
  (proto/create-mock-participant :test-drone
                                 [{:status :completed
                                   :result "{\"status\": \"fail\", \"failures\": [{\"file\": \"test.clj\", \"line\": 10, \"message\": \"Expected 1, got 2\"}], \"summary\": \"1 failure\"}"}]))

(defn mock-fail-then-pass-test
  "Create test drone that fails N times then passes."
  [fail-count]
  (proto/create-mock-participant
   :test-drone
   (concat
    (repeat fail-count {:status :completed
                        :result "{\"status\": \"fail\", \"failures\": [{\"file\": \"test.clj\", \"line\": 10, \"message\": \"Expected 1, got 2\"}], \"summary\": \"1 failure\"}"})
    [{:status :completed
      :result "{\"status\": \"pass\", \"failures\": [], \"summary\": \"All tests passed\"}"}])))

(defn mock-pass-fix
  "Create fix drone that always succeeds."
  []
  (proto/create-mock-participant :fix-drone
                                 ;; Enough responses for max iterations
                                 (repeat 10 {:status :completed :result "Fixed the issue"})))

(defn mock-fail-fix
  "Create fix drone that always fails."
  []
  (proto/create-mock-participant :fix-drone
                                 (repeat 10 {:status :failed :error "Fix error"})))

;; =============================================================================
;; Protocol Tests
;; =============================================================================

(deftest participant-type-constants-test
  (testing "participant types are defined"
    (is (map? proto/participant-types))
    (is (contains? proto/participant-types :impl-drone))
    (is (contains? proto/participant-types :test-drone))
    (is (contains? proto/participant-types :fix-drone))))

(deftest create-impl-drone-test
  (testing "creates impl drone with defaults"
    (let [drone (proto/create-impl-drone)]
      (is (proto/valid-participant? drone))
      (is (= :impl-drone (proto/participant-type drone)))
      (is (string? (proto/participant-id drone)))
      (is (= "drone-worker" (proto/get-preset drone)))))

  (testing "creates impl drone with custom options"
    (let [drone (proto/create-impl-drone {:id "custom-id" :preset "custom-preset"})]
      (is (= "custom-id" (proto/participant-id drone)))
      (is (= "custom-preset" (proto/get-preset drone))))))

(deftest create-test-drone-test
  (testing "creates test drone with defaults"
    (let [drone (proto/create-test-drone)]
      (is (proto/valid-participant? drone))
      (is (= :test-drone (proto/participant-type drone)))
      (is (= "tester" (proto/get-preset drone))))))

(deftest create-fix-drone-test
  (testing "creates fix drone with defaults"
    (let [drone (proto/create-fix-drone)]
      (is (proto/valid-participant? drone))
      (is (= :fix-drone (proto/participant-type drone)))
      (is (= "fixer" (proto/get-preset drone))))))

(deftest mock-participant-test
  (testing "mock participant returns configured responses"
    (let [responses [{:status :completed :result "first"}
                     {:status :failed :error "second"}
                     {:status :completed :result "third"}]
          mock (proto/create-mock-participant :impl-drone responses)]
      (is (= {:status :completed :result "first"}
             (proto/execute-task! mock {:prompt "task 1"})))
      (is (= {:status :failed :error "second"}
             (proto/execute-task! mock {:prompt "task 2"})))
      (is (= {:status :completed :result "third"}
             (proto/execute-task! mock {:prompt "task 3"}))))))

(deftest participant-summary-test
  (testing "returns summary for valid participant"
    (let [drone (proto/create-impl-drone {:id "test-drone" :preset "my-preset"})
          summary (proto/participant-summary drone)]
      (is (= "test-drone" (:id summary)))
      (is (= :impl-drone (:type summary)))
      (is (= "my-preset" (:preset summary)))))

  (testing "returns nil for invalid participant"
    (is (nil? (proto/participant-summary "not a participant")))))

;; =============================================================================
;; Loop State Constants Tests
;; =============================================================================

(deftest tdd-states-test
  (testing "TDD states are defined"
    (is (set? tdd/tdd-states))
    (is (contains? tdd/tdd-states :idle))
    (is (contains? tdd/tdd-states :implementing))
    (is (contains? tdd/tdd-states :testing))
    (is (contains? tdd/tdd-states :passed))
    (is (contains? tdd/tdd-states :failed))
    (is (contains? tdd/tdd-states :fixing))
    (is (contains? tdd/tdd-states :max-iterations))
    (is (contains? tdd/tdd-states :error))))

(deftest state-transitions-test
  (testing "valid transitions from :idle"
    (is (contains? (tdd/state-transitions :idle) :implementing)))

  (testing "valid transitions from :implementing"
    (is (contains? (tdd/state-transitions :implementing) :testing))
    (is (contains? (tdd/state-transitions :implementing) :error)))

  (testing "valid transitions from :testing"
    (is (contains? (tdd/state-transitions :testing) :passed))
    (is (contains? (tdd/state-transitions :testing) :failed)))

  (testing "valid transitions from :failed"
    (is (contains? (tdd/state-transitions :failed) :fixing))
    (is (contains? (tdd/state-transitions :failed) :max-iterations)))

  (testing ":passed is terminal"
    (is (empty? (tdd/state-transitions :passed)))))

(deftest max-tdd-iterations-test
  (testing "max iterations constant is defined"
    (is (number? tdd/max-tdd-iterations))
    (is (= 5 tdd/max-tdd-iterations))))

;; =============================================================================
;; Test Result Parsing Tests
;; =============================================================================

(deftest parse-test-result-json-test
  (testing "parses JSON pass result"
    (let [result (tdd/parse-test-result "{\"status\": \"pass\", \"failures\": [], \"summary\": \"All passed\"}")]
      (is (= :pass (:status result)))
      (is (empty? (:failures result)))
      (is (= "All passed" (:summary result)))))

  (testing "parses JSON fail result with failures"
    (let [result (tdd/parse-test-result "{\"status\": \"fail\", \"failures\": [{\"file\": \"test.clj\", \"line\": 10, \"message\": \"Assert failed\"}], \"summary\": \"1 failure\"}")]
      (is (= :fail (:status result)))
      (is (= 1 (count (:failures result))))
      (is (= "test.clj" (get-in result [:failures 0 :file])))))

  (testing "handles markdown code blocks"
    (let [result (tdd/parse-test-result "```json\n{\"status\": \"pass\", \"failures\": []}\n```")]
      (is (= :pass (:status result))))))

(deftest parse-test-result-text-fallback-test
  (testing "falls back to text analysis on invalid JSON"
    (let [result (tdd/parse-test-result "Tests: 5 passed, 0 failed")]
      (is (= :pass (:status result)))
      (is (string? (:summary result)))))

  (testing "detects failure indicators in text"
    (let [result (tdd/parse-test-result "FAILURE: Expected 1 but got 2")]
      (is (= :fail (:status result)))))

  (testing "detects error indicators in text"
    (let [result (tdd/parse-test-result "ERROR in (my-test) - NullPointerException")]
      (is (= :fail (:status result))))))

(deftest format-failures-for-fix-test
  (testing "formats failures into fix prompt"
    (let [test-result {:failures [{:file "src/foo.clj" :line 42 :message "Expected true"}]
                       :summary "1 failure"
                       :raw "raw output"}
          formatted (tdd/format-failures-for-fix test-result)]
      (is (string? formatted))
      (is (clojure.string/includes? formatted "src/foo.clj"))
      (is (clojure.string/includes? formatted "42"))
      (is (clojure.string/includes? formatted "Expected true"))))

  (testing "uses raw output when no structured failures"
    (let [test-result {:failures [] :summary "unknown" :raw "some raw output"}
          formatted (tdd/format-failures-for-fix test-result)]
      (is (clojure.string/includes? formatted "some raw output")))))

;; =============================================================================
;; TDD Loop Tests - Happy Path
;; =============================================================================

(deftest tdd-loop-pass-first-try-test
  (testing "TDD loop passes on first iteration"
    (let [result (tdd/tdd-loop!
                  {:prompt "Implement add function" :files ["src/math.clj"]}
                  "clojure -M:test"
                  {:impl-drone (mock-pass-impl)
                   :test-drone (mock-pass-test)
                   :fix-drone (mock-pass-fix)
                   :trace false})]
      (is (= :passed (:status result)))
      (is (= 1 (:iterations result)))
      (is (string? (:loop-id result)))
      (is (= :pass (get-in result [:test-result :status]))))))

;; =============================================================================
;; TDD Loop Tests - Fix Iterations
;; =============================================================================

(deftest tdd-loop-pass-after-fixes-test
  (testing "TDD loop passes after 2 fix iterations"
    (let [result (tdd/tdd-loop!
                  {:prompt "Implement subtract function" :files ["src/math.clj"]}
                  "clojure -M:test"
                  {:impl-drone (mock-pass-impl)
                   :test-drone (mock-fail-then-pass-test 2)
                   :fix-drone (mock-pass-fix)
                   :trace false})]
      (is (= :passed (:status result)))
      (is (= 3 (:iterations result)))  ; 2 fails + 1 pass
      (is (= 3 (count (:history result)))))))

(deftest tdd-loop-pass-at-max-iteration-test
  (testing "TDD loop passes on last allowed iteration"
    (let [result (tdd/tdd-loop!
                  {:prompt "Implement divide function" :files ["src/math.clj"]}
                  "clojure -M:test"
                  {:impl-drone (mock-pass-impl)
                   :test-drone (mock-fail-then-pass-test 4)  ; Fail 4 times, pass on 5th
                   :fix-drone (mock-pass-fix)
                   :max-iterations 5
                   :trace false})]
      (is (= :passed (:status result)))
      (is (= 5 (:iterations result))))))

;; =============================================================================
;; TDD Loop Tests - Max Iterations Exceeded
;; =============================================================================

(deftest tdd-loop-max-iterations-test
  (testing "TDD loop fails after max iterations"
    (let [result (tdd/tdd-loop!
                  {:prompt "Implement impossible function" :files ["src/impossible.clj"]}
                  "clojure -M:test"
                  {:impl-drone (mock-pass-impl)
                   :test-drone (mock-fail-test)
                   :fix-drone (mock-pass-fix)
                   :max-iterations 3
                   :trace false})]
      (is (= :max-iterations (:status result)))
      (is (= 3 (:iterations result)))
      (is (string? (:message result))))))

(deftest tdd-loop-custom-max-iterations-test
  (testing "TDD loop respects custom max-iterations"
    (let [result (tdd/tdd-loop!
                  {:prompt "Implement test" :files ["test.clj"]}
                  "clojure -M:test"
                  {:impl-drone (mock-pass-impl)
                   :test-drone (mock-fail-test)
                   :fix-drone (mock-pass-fix)
                   :max-iterations 2
                   :trace false})]
      (is (= :max-iterations (:status result)))
      (is (= 2 (:iterations result))))))

;; =============================================================================
;; TDD Loop Tests - Error Cases
;; =============================================================================

(deftest tdd-loop-impl-failure-test
  (testing "TDD loop errors on implementation failure"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Implementation failed"
         (tdd/tdd-loop!
          {:prompt "Implement broken" :files ["broken.clj"]}
          "clojure -M:test"
          {:impl-drone (mock-fail-impl)
           :test-drone (mock-pass-test)
           :fix-drone (mock-pass-fix)
           :trace false})))))

(deftest tdd-loop-continues-after-fix-failure-test
  (testing "TDD loop continues even when fix fails (tries next iteration)"
    ;; With failed fix, it should still attempt next iteration
    (let [result (tdd/tdd-loop!
                  {:prompt "Implement resilient" :files ["resilient.clj"]}
                  "clojure -M:test"
                  {:impl-drone (mock-pass-impl)
                   :test-drone (mock-fail-then-pass-test 2)  ; Fails 2x then passes
                   :fix-drone (mock-fail-fix)  ; Fix always fails
                   :max-iterations 5
                   :trace false})]
      ;; Should still pass because test eventually passes
      (is (= :passed (:status result))))))

;; =============================================================================
;; TDD Loop History Tests
;; =============================================================================

(deftest tdd-loop-records-history-test
  (testing "TDD loop records iteration history"
    (let [result (tdd/tdd-loop!
                  {:prompt "Test history" :files ["history.clj"]}
                  "clojure -M:test"
                  {:impl-drone (mock-pass-impl)
                   :test-drone (mock-fail-then-pass-test 2)
                   :fix-drone (mock-pass-fix)
                   :trace false})]
      (is (= 3 (count (:history result))))
      (is (every? #(contains? % :iteration) (:history result)))
      (is (every? #(contains? % :test-status) (:history result)))
      (is (every? #(contains? % :timestamp) (:history result)))
      ;; First two should be :fail, last should be :pass
      (is (= :fail (get-in result [:history 0 :test-status])))
      (is (= :fail (get-in result [:history 1 :test-status])))
      (is (= :pass (get-in result [:history 2 :test-status]))))))

;; =============================================================================
;; Query Function Tests
;; =============================================================================

(deftest list-active-tdd-loops-test
  (testing "lists active loops when none active"
    (is (empty? (tdd/list-active-tdd-loops))))

  (testing "active loops are cleaned up after completion"
    (tdd/tdd-loop!
     {:prompt "Quick test" :files ["quick.clj"]}
     "clojure -M:test"
     {:impl-drone (mock-pass-impl)
      :test-drone (mock-pass-test)
      :fix-drone (mock-pass-fix)
      :trace false})
    ;; Loop should be cleaned up after completion
    (is (empty? (tdd/list-active-tdd-loops)))))

;; =============================================================================
;; Edge Case Tests
;; =============================================================================

(deftest tdd-loop-preconditions-test
  (testing "requires impl-task to be a map"
    (is (thrown? AssertionError
                 (tdd/tdd-loop! "not a map" "test-cmd" {:trace false}))))

  (testing "requires impl-task to have :prompt"
    (is (thrown? AssertionError
                 (tdd/tdd-loop! {:files ["test.clj"]} "test-cmd" {:trace false})))))

(deftest tdd-loop-default-test-command-test
  (testing "uses default test command when nil provided"
    ;; This test verifies the code path, not actual execution
    (is (= "clojure -M:test" tdd/default-test-command))))

;; =============================================================================
;; Axiom Verification Tests
;; =============================================================================

(deftest axiom-drones-get-one-shot-test
  (testing "AXIOM: Each drone is called exactly once per phase/iteration"
    ;; We verify this by counting how many times each mock is called
    (let [impl-calls (atom 0)
          test-calls (atom 0)
          fix-calls (atom 0)

          counting-impl (reify proto/ITDDParticipant
                          (participant-id [_] "counting-impl")
                          (participant-type [_] :impl-drone)
                          (execute-task! [_ _]
                            (swap! impl-calls inc)
                            {:status :completed :result "done"})
                          (get-preset [_] "mock"))

          counting-test (reify proto/ITDDParticipant
                          (participant-id [_] "counting-test")
                          (participant-type [_] :test-drone)
                          (execute-task! [_ _]
                            (swap! test-calls inc)
                            (if (< @test-calls 3)
                              {:status :completed
                               :result "{\"status\": \"fail\", \"failures\": [], \"summary\": \"fail\"}"}
                              {:status :completed
                               :result "{\"status\": \"pass\", \"failures\": [], \"summary\": \"pass\"}"}))
                          (get-preset [_] "mock"))

          counting-fix (reify proto/ITDDParticipant
                         (participant-id [_] "counting-fix")
                         (participant-type [_] :fix-drone)
                         (execute-task! [_ _]
                           (swap! fix-calls inc)
                           {:status :completed :result "fixed"})
                         (get-preset [_] "mock"))]

      (tdd/tdd-loop!
       {:prompt "Test axiom" :files ["axiom.clj"]}
       "test"
       {:impl-drone counting-impl
        :test-drone counting-test
        :fix-drone counting-fix
        :trace false})

      ;; Impl called exactly once (at start)
      (is (= 1 @impl-calls) "Impl drone should be called exactly once")
      ;; Test called 3 times (2 fails + 1 pass)
      (is (= 3 @test-calls) "Test drone called once per iteration")
      ;; Fix called 2 times (for each failure)
      (is (= 2 @fix-calls) "Fix drone called once per failure"))))

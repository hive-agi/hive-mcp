(ns hive-mcp.resilience.contract-test
  "Contract facets for the resilience kernel: the breaker state machine, the
   ErrorClass classifier, the retry policy table, the thunk runner, and the
   store-facing predicates. The runner and kick predicates drive thunks that
   always throw, so the exercised paths stay off the live memory store."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.resilience.breaker :as breaker]
            [hive-mcp.resilience.classify :as classify]
            [hive-mcp.resilience.policy :as policy]
            [hive-mcp.resilience.protocol :as proto]
            [hive-mcp.resilience.runner :as runner]
            [hive-mcp.resilience.store :as store]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ---------------------------------------------------------------------------
;;; Shared schemas
;;; ---------------------------------------------------------------------------

(def ^:private Breaker
  "Breaker records the generator can produce — same map shape as
   breaker/Breaker, with a bounded enum for :state."
  breaker/Breaker)

(def ^:private Policy
  [:map
   [:max-failures [:int {:min 1 :max 10}]]
   [:initial-cooldown-ms [:int {:min 0 :max 60000}]]
   [:max-cooldown-ms [:int {:min 0 :max 600000}]]])

(def ^:private AThrowable
  "Generated throwables across the classification domain: ex-infos carrying
   the tags `classify` dispatches on, IOExceptions, and bare RuntimeExceptions
   (with and without a transient message marker)."
  [:enum
   (ex-info "Milvus HTTP returned code=1804 message=dim mismatch"
            {:milvus-clj.client/transport :http :code 1804 :message "dim"})
   (ex-info "auth" {:milvus-clj.client/transport :http :status 401})
   (ex-info "transport drop" {:cause :io})
   (ex-info "too large" {:err/tag :embedder/input-too-large})
   (ex-info "dim mismatch" {:err/tag :embedder/dim-mismatch})
   (java.io.IOException. "Connection reset")
   (RuntimeException. "selector manager closed")
   (RuntimeException. "something else entirely")])

(def ^:private VariantOf
  "The :adt/variant of a generated ErrorClass."
  (comp :adt/variant))

;;; ---------------------------------------------------------------------------
;;; classify — every generated throwable lands on a variant, and the
;;; classification agrees with the pinned precedence rules
;;; ---------------------------------------------------------------------------

(deftrifecta-from-schema classify-total
  #'classify/classify
  {:in [:cat AThrowable]
   :out proto/ErrorClassValue
   :rel (fn [[t] out]
          (let [v (:adt/variant out)
                data (ex-data t)]
            (cond
              (= :io (:cause data))                     (= :err/transient v)
              (= :embedder/dim-mismatch (:err/tag data)) (= :err/schema-mismatch v)
              (= :embedder/input-too-large (:err/tag data)) (= :err/validation v)
              (= 401 (:status data))                    (= :err/auth v)
              (= 1804 (:code data))                     (= :err/schema-mismatch v)
              (instance? java.io.IOException t)         (= :err/transient v)
              (= "selector manager closed" (.getMessage t)) (= :err/transient v)
              :else                                     (= :err/unknown v))))
   :classify (fn [_ out] (:adt/variant out))
   :classify-domain proto/ErrorClassVariant
   :classify-floor 5
   :num-tests 200})

(golden/deftest-golden classify-variants-golden
  "test/golden/hive-mcp/resilience/classify.edn"
  ;; Throwables do not round-trip through EDN, so the snapshot keys each
  ;; generated throwable by class + message and pins the classified value.
  (into (sorted-map)
        (map (fn [^Throwable t]
               [(str (.getName (class t)) ": " (.getMessage t))
                (dissoc (classify/classify t) :details)]))
        (m/children (m/schema AThrowable))))

;;; ---------------------------------------------------------------------------
;;; policy — the decision table is total and consistent with retry?
;;; ---------------------------------------------------------------------------

(deftrifecta-from-schema decide-total-and-consistent
  #'policy/decide
  {:in [:cat proto/ErrorClassValue]
   :out policy/RetryDecision
   :rel (fn [[err] out]
          (and (= (:retry? out) (policy/retry? err))
               (= (:retry? out) (= :err/transient (:adt/variant err)))
               (= (not (:retry? out)) (:escalate? out))))
   :classify (fn [_ out] (if (:retry? out) :retry :escalate))
   :classify-domain #{:retry :escalate}
   :golden-path "test/golden/hive-mcp/resilience/policy-decide.edn"
   :num-tests 200})

(deftrifecta-from-schema retry-predicate-agrees
  #'policy/retry?
  {:in [:cat proto/ErrorClassValue]
   :out :boolean
   :rel (fn [[err] out] (= out (:retry? (policy/decide err))))
   :num-tests 100})

(deftrifecta-from-schema schema-mismatch-predicates-agree
  #'proto/schema-mismatch?
  {:in [:cat proto/ErrorClassValue]
   :out :boolean
   :rel (fn [[err] out]
          (and (= out (= :err/schema-mismatch (:adt/variant err)))
               (= out (policy/schema-mismatch? err))))
   :num-tests 100})

(deftrifecta-from-schema transient-predicate-agrees
  #'proto/transient?
  {:in [:cat proto/ErrorClassValue]
   :out :boolean
   :rel (fn [[err] out] (= out (= :err/transient (:adt/variant err))))
   :num-tests 100})

;;; ---------------------------------------------------------------------------
;;; breaker — the state machine transitions stay inside Breaker
;;; ---------------------------------------------------------------------------

(deftrifecta-from-schema fresh-is-a-closed-breaker
  #'breaker/fresh
  {:in [:cat]
   :out [:and breaker/Breaker
         [:map [:state [:= :closed]] [:failures [:= 0]]
          [:opened-at :nil] [:cooldown-ms [:= 0]]]]
   ;; a record with no history never blocks
   :rel (fn [_ out] (= :pass (breaker/decision out)))
   :num-tests 20})

(deftrifecta-from-schema on-success-always-resets
  #'breaker/on-success
  {:in [:cat [:maybe Breaker]]
   :out breaker/Breaker
   :rel (fn [_ out] (= out (breaker/fresh)))
   :num-tests 50})

(deftrifecta-from-schema on-failure-stays-a-breaker
  #'breaker/on-failure
  {:in [:cat Breaker Policy [:int {:min 0 :max 1000000000}]]
   :out breaker/Breaker
   ;; failures strictly increase; :open is the only state that carries a
   ;; timestamp, and a fresh record trips open exactly at the threshold
   :rel (fn [[b policy _ts] out]
          (and (= (:failures out) (inc (:failures b)))
               (if (= :open (:state out))
                 (some? (:opened-at out))
                 true)
               (if (and (not= :half-open (:state b))
                        (>= (inc (:failures b)) (:max-failures policy)))
                 (= :open (:state out))
                 true)))
   :golden-path "test/golden/hive-mcp/resilience/breaker-on-failure.edn"
   :num-tests 200})

(deftrifecta-from-schema maybe-recover-preserves-shape
  #'breaker/maybe-recover
  {:in [:cat Breaker [:int {:min 0 :max 1000000000}]]
   :out breaker/Breaker
   ;; only :open may move, and only to :half-open, exactly when the cooldown
   ;; has elapsed; every other key is untouched
   :rel (fn [[b ts] out]
          (and (= (dissoc b :state) (dissoc out :state))
               (if (and (= :open (:state b))
                        (some? (:opened-at b))
                        (>= ts (+ (:opened-at b) (:cooldown-ms b))))
                 (= :half-open (:state out))
                 (= (:state b) (:state out)))))
   :num-tests 200})

(deftrifecta-from-schema decision-reads-open-only
  #'breaker/decision
  {:in [:cat Breaker]
   :out [:enum :pass :block]
   :rel (fn [[b] out] (= out (if (= :open (:state b)) :block :pass)))
   :num-tests 100})

(deftest breaker-atom-orchestration
  (testing "attempt / record-failure! / record-success! / snapshot / reset!"
    (let [a (atom {})
          p breaker/default-policy]
      (is (= :closed (:state (breaker/attempt a :slot/x p))))
      (is (= 1 (:failures (breaker/record-failure! a :slot/x p))))
      (is (= :closed (:state (breaker/record-failure! a :slot/x p)))
          "the second failure of a max-failures 3 policy stays closed")
      (let [b (breaker/record-failure! a :slot/x p)]
        (is (= :open (:state b)) "the third failure trips the breaker")
        (is (= (:initial-cooldown-ms p) (:cooldown-ms b))))
      (is (= :block (breaker/decision (breaker/attempt a :slot/x p)))
          "an open breaker blocks while its cooldown runs")
      (is (= #{:slot/x} (set (keys (breaker/snapshot a)))))
      (is (= :closed (:state (get (breaker/record-success! a :slot/x) :slot/x))))
      (is (= {} (breaker/reset! a)))
      (is (= {} (breaker/snapshot a))))))

;;; ---------------------------------------------------------------------------
;;; runner — thunks that always throw or always succeed, no store touched
;;; ---------------------------------------------------------------------------

(deftest run-classifies-failures
  (testing "a successful thunk rides {:ok _}"
    (is (= {:ok 42} (runner/run (fn [] 42))))
    (is (= {:ok 42} (runner/run (fn [] 42) {})))
    (is (= {:ok 42} (runner/run (fn [] 42) {:kick! (constantly nil) :budget-ms 1}))))
  (testing "validation failure is terminal — no retry"
    (let [calls (atom 0)
          res (runner/run (fn [] (swap! calls inc)
                            (throw (ex-info "boom" {:err/tag :embedder/input-too-large}))))]
      (is (= :runner/fatal (:error res)))
      (is (= 1 @calls) "fatal classes are not retried")))
  (testing "schema-mismatch is terminal with the dim-mismatch tag"
    (let [res (runner/run (fn [] (throw (ex-info "1804" {:milvus-clj.client/transport :http
                                                         :code 1804}))))]
      (is (= :embedder/dim-mismatch (:error res)))))
  (testing "transient kicks once and retries once"
    (let [kicks (atom 0)
          calls (atom 0)
          res (runner/run (fn [] (swap! calls inc)
                            (throw (java.io.IOException. "Connection reset")))
                          {:kick! (fn [] (swap! kicks inc))})]
      (is (= :runner/retry-failed (:error res)))
      (is (= 1 @kicks) "kick! runs once")
      (is (= 2 @calls) "the thunk runs twice: first attempt + one retry")))
  (testing "transient with no kick! still retries once"
    (let [calls (atom 0)
          res (runner/run (fn [] (swap! calls inc)
                            (throw (java.io.IOException. "Broken pipe"))))]
      (is (= :runner/retry-failed (:error res)))
      (is (= 2 @calls))))
  (testing "a retry that succeeds returns ok"
    (let [calls (atom 0)
          res (runner/run (fn [] (if (= 1 (swap! calls inc))
                                   (throw (java.io.IOException. "flake"))
                                   :recovered)))]
      (is (= {:ok :recovered} res)))))

(deftrifecta-from-schema decide-only-matches-policy
  #'runner/decide-only
  {:in [:cat AThrowable]
   :out policy/RetryDecision
   :rel (fn [[t] out] (= out (policy/decide (classify/classify t))))
   :num-tests 100})

;;; ---------------------------------------------------------------------------
;;; store — the pure surface; kick-and-wait! with no store set
;;; ---------------------------------------------------------------------------

(deftest store-predicates-and-kick
  (testing "transient-failure? tracks classify"
    (is (true? (store/transient-failure? (java.io.IOException. "Connection reset"))))
    (is (false? (store/transient-failure? (ex-info "1804" {:milvus-clj.client/transport :http
                                                           :code 1804})))))
  (testing "kick-and-wait! is false when no liveness store is active"
    ;; memory store slot is unset in this test JVM — the fn must not throw
    (is (false? (store/kick-and-wait!)))
    (is (false? (store/kick-and-wait! 1)))))

(deftest call-with-resilience-result-railway
  (testing "ok / fatal / retry-failed shapes"
    (is (= {:ok :fine} (store/call-with-resilience-result (fn [] :fine))))
    (let [res (store/call-with-resilience-result
               (fn [] (throw (ex-info "boom" {:err/tag :embedder/input-too-large}))))]
      (is (= :resilience/fatal (:error res))))
    (let [res (store/call-with-resilience-result
               (fn [] (throw (java.io.IOException. "Broken pipe"))) 1)]
      (is (= :resilience/retry-failed (:error res))))))

(deftest call-with-resilience-legacy-throwing-surface
  (testing "success returns the value"
    (is (= :fine (store/call-with-resilience (fn [] :fine)))))
  (testing "schema-mismatch re-throws as :embedder/dim-mismatch ex-info"
    (let [ex (try (store/call-with-resilience
                   (fn [] (throw (ex-info "1804" {:milvus-clj.client/transport :http
                                                  :code 1804}))))
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
      (is (= :embedder/dim-mismatch (:err/tag (ex-data ex))))))
  (testing "fatal re-throws the original throwable"
    (let [orig (ex-info "boom" {:err/tag :embedder/input-too-large})
          ex (try (store/call-with-resilience (fn [] (throw orig)))
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
      (is (identical? orig ex)))))

;;; ---------------------------------------------------------------------------
;;; default classifier construction
;;; ---------------------------------------------------------------------------

(deftest default-classifier-satisfies-the-port
  (let [c (classify/default-classifier)]
    (is (satisfies? proto/IErrorClassifier c))
    (is (= :err/unknown (:adt/variant (proto/classify c (RuntimeException. "zz")))))))

;;; ---------------------------------------------------------------------------
;;; Coverage gate — every public defn of src/hive_mcp/resilience contracted
;;; ---------------------------------------------------------------------------

(coverage/deftest-contract-coverage resilience-contracts
  {:paths ["src/hive_mcp/resilience"]})

(deftest resilience-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths ["src/hive_mcp/resilience"]})]
    (is (= 23 (count (:universe report))) "every public defn of the tree")
    (is (= 23 (count (:covered report))))
    (is (empty? (:missing report)))))

(def ^:private gated-namespaces
  '[hive-mcp.resilience.breaker hive-mcp.resilience.classify
    hive-mcp.resilience.policy hive-mcp.resilience.protocol
    hive-mcp.resilience.runner hive-mcp.resilience.store])

(deftest every-resilience-contract-generates-both-sides
  (doseq [ns-sym gated-namespaces
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

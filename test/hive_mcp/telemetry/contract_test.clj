(ns hive-mcp.telemetry.contract-test
  "Contract facets for the telemetry tree. The Prometheus registry is a
   process-global delay; every test here writes only to label values namespaced
   `m23-contract-*`, so no shared series another test reads is touched, and
   the delay is never re-created. Logging is captured through a timbre
   appender, never redefined."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.telemetry.core :as telemetry]
            [hive-mcp.telemetry.health :as health]
            [hive-mcp.telemetry.prometheus :as prom]
            [hive-mcp.test.stub.log-capture :as cap :refer [with-captured-logs]]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [iapetos.core :as iapetos]
            [malli.core :as m]
            [malli.generator :as mg]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ---------------------------------------------------------------------------
;;; telemetry.core — log-shaping fns, observed through a capture appender
;;; ---------------------------------------------------------------------------

(defn- request-event
  "Subject wrapper is NOT what the trifecta targets; it only reads the event
   the contracted log-eval-request emitted."
  [arg]
  (with-captured-logs logs
    (telemetry/log-eval-request arg)
    (cap/event-data logs :eval-request)))

(deftrifecta-from-schema log-eval-request-answers-nil
  #'telemetry/log-eval-request
  {:in [:map [:code [:string {:max 80}]] [:mode [:enum :elisp :cider-silent]]]
   :out :nil
   ;; logging is the effect; the relation checks the logged preview
   :rel (fn [{:keys [code]} _]
          (let [ev (request-event {:code code :mode :elisp})]
            (and (= (count code) (:code-length ev))
                 (= (:code-preview ev)
                    (if (> (count code) 50) (str (subs code 0 50) "...") code)))))
   ;; :out is :nil, which has no mutants to derive
   :mutation false
   :num-tests 50})

(golden/deftest-golden log-eval-shapes-golden
  "test/golden/hive-mcp/telemetry/log-eval-shapes.edn"
  (with-captured-logs logs
    (telemetry/log-eval-request {:code (apply str (repeat 60 "x")) :mode :elisp :metadata {:u "a"}})
    (telemetry/log-eval-request {:code "(+ 1 2)" :mode :cider-silent})
    (telemetry/log-eval-result {:success true :duration-ms 4 :result-length 9 :metadata {:s 1}})
    (telemetry/log-eval-result {:success false :error "boom" :duration-ms 7})
    (telemetry/log-eval-exception {:exception (ex-info "kaput" {}) :operation "eval-x"})
    (mapv (fn [e] [(:level e) (:event e)
                   (dissoc (:data e) :exception-type)])
          (cap/events logs))))

(deftest configure-logging-answers-the-merged-config
  (let [saved log/*config*]
    (try
      (let [cfg (telemetry/configure-logging! {:level :warn
                                               :appenders {:println {:enabled? false}}})]
        (is (map? cfg))
        (is (= :warn (:min-level cfg))))
      (finally (log/set-config! saved)))))

(deftest core-facade-aliases-point-at-health
  (is (= health/severities telemetry/health-severities))
  (is (= health/error-types telemetry/health-error-types))
  (is (= #'health/emit-health-event! telemetry/emit-health-event!)))

;;; ---------------------------------------------------------------------------
;;; telemetry.health — validation refuses before any store is reached
;;; ---------------------------------------------------------------------------

(defn- refused? [thunk]
  ;; malli :invalid-input (instrumented) and validate-event!'s ExceptionInfo
  ;; are the same refusal; either counts
  (try (thunk) false (catch Throwable _ true)))

(deftest emit-health-event-refuses-unknown-type-and-severity
  (is (refused? #(health/emit-health-event! {:type :not-a-type :severity :error :message "x"})))
  (is (refused? #(health/emit-health-event! {:type :harvest-failed :severity :loud :message "x"}))))

(deftest health-schemas-accept-the-documented-shapes
  (is (m/validate health/HealthEvent {:type :harvest-failed :severity :error :message "m"
                                      :context {:a 1} :recoverable? true}))
  (is (not (m/validate health/HealthEvent {:severity :error})))
  (is (m/validate health/HealthRow {:health-event/id "health-event-1"
                                    :health-event/type :nrepl-timeout
                                    :health-event/timestamp (java.util.Date.)})))

;;; ---------------------------------------------------------------------------
;;; telemetry.prometheus — counters observed by value, labels namespaced
;;; ---------------------------------------------------------------------------

(defn- events-value [type]
  (iapetos/value (@prom/registry :hive-mcp/events-total {:type (name type) :severity "info"})))

(deftrifecta-from-schema inc-events-total-bumps-by-one
  #'prom/inc-events-total!
  {:in [:cat [:enum :m23-contract-a :m23-contract-b "m23-contract-c"] [:= :info]]
   :out prom/CounterChild
   :rel (fn [[type _] out]
          (let [before (events-value type)]
            (iapetos/inc out)
            (= (inc before) (events-value type))))
   :num-tests 30})

(deftest every-collector-answers-its-child
  (prom/init!)
  (is (m/validate prom/CounterChild (prom/inc-errors-total! :m23-contract true)))
  (is (m/validate prom/CounterChild (prom/inc-mcp-requests! "m23-contract")))
  (is (m/validate prom/CounterChild (prom/inc-memory-ops! :m23-contract :success)))
  (is (m/validate prom/CounterChild (prom/inc-hivemind-shouts! :m23-contract)))
  (is (m/validate prom/CounterChild (prom/inc-wave-items! :m23-contract)))
  (is (m/validate prom/CounterChild (prom/inc-wave-failures! "m23-contract" nil)))
  (is (m/validate prom/HistogramChild (prom/observe-request-duration! "m23-contract" 0.01)))
  (is (m/validate prom/HistogramChild (prom/observe-chroma-query! :m23-contract 0.01)))
  (is (nil? (prom/observe-wave-duration! nil)))
  (testing "gauges are set back to what they held"
    (let [g (fn [k] (iapetos/value (@prom/registry k)))
          lings (g :hive-mcp/lings-active) ws (g :hive-mcp/ws-clients)
          rate (g :hive-mcp/wave-success-rate)]
      (is (m/validate prom/GaugeChild (prom/set-lings-active! 7)))
      (is (= 7.0 (g :hive-mcp/lings-active)))
      (is (m/validate prom/GaugeChild (prom/set-ws-clients! 2)))
      (is (m/validate prom/GaugeChild (prom/set-wave-success-rate! 1/2)))
      (is (= 0.5 (g :hive-mcp/wave-success-rate)))
      (prom/set-lings-active! lings) (prom/set-ws-clients! ws) (prom/set-wave-success-rate! rate))))

(deftest prometheus-effect-routes-wave-failures
  (let [v #(iapetos/value (@prom/registry :hive-mcp/wave-failures-total
                                          {:wave-id "m23-contract-w" :reason "timeout"}))
        before (v)]
    (is (nil? (prom/handle-prometheus-effect!
               {:counter :wave_failure :labels {:wave_id "m23-contract-w" :reason :timeout}})))
    (is (= (inc before) (v))))
  (let [before (events-value :m23-contract-unknown)]
    (is (nil? (prom/handle-prometheus-effect! {:counter :m23-contract-unknown
                                               :histogram {:name :x}})))
    (is (= (inc before) (events-value :m23-contract-unknown)))))

(deftest metrics-export-is-the-text-format
  (let [resp (prom/metrics-handler {})]
    (is (= 200 (:status resp)))
    (is (str/includes? (:body resp) "hive_mcp_events_total"))
    (is (string? (prom/metrics-response)))
    (is (true? (prom/init!)))))

;;; ---------------------------------------------------------------------------
;;; Coverage gate
;;; ---------------------------------------------------------------------------

(coverage/deftest-contract-coverage telemetry-contracts
  {:paths ["src/hive_mcp/telemetry"]})

(deftest telemetry-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths ["src/hive_mcp/telemetry"]})]
    (is (= 24 (count (:universe report))))
    (is (= 24 (count (:covered report))))))

(deftest every-telemetry-contract-generates-both-sides
  (doseq [ns-sym '[hive-mcp.telemetry.core hive-mcp.telemetry.health hive-mcp.telemetry.prometheus]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

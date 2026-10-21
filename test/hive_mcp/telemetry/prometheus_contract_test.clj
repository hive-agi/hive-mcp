(ns hive-mcp.telemetry.prometheus-contract-test
  "Contract facets for hive-mcp.telemetry.prometheus. The registry is a
   process-global delay; every test here writes only to label values namespaced
   `m23-contract-*`, so no shared series another test reads is touched, and the
   delay is never re-created."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.telemetry.prometheus :as prom]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [iapetos.core :as iapetos]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

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

(defn- delta
  "How far `thunk` moves the series `k` / `labels`."
  [k labels thunk]
  (let [v #(iapetos/value (@prom/registry k labels))
        before (v)]
    (thunk)
    (- (v) before)))

(golden/deftest-golden prometheus-effect-routing-golden
  "test/golden/hive-mcp/telemetry/prometheus-effect-routing.edn"
  (do
    (prom/init!)
    {:wave-failure (delta :hive-mcp/wave-failures-total {:wave-id "m23-contract-g" :reason "timeout"}
                          #(prom/handle-prometheus-effect!
                            {:counter :wave_failure :labels {:wave_id "m23-contract-g" :reason :timeout}}))
     :unknown-counter (delta :hive-mcp/events-total {:type "m23-contract-g" :severity "info"}
                             #(prom/handle-prometheus-effect! {:counter :m23-contract-g}))
     :nothing-named (prom/handle-prometheus-effect! {})
     :wave-items (delta :hive-mcp/wave-items-total {:status "m23-contract-g"}
                        #(prom/inc-wave-items! :m23-contract-g))
     :wave-duration-nil (prom/observe-wave-duration! nil)
     :handler-status (:status (prom/metrics-handler {}))
     :handler-headers (:headers (prom/metrics-handler {}))}))

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

(deftest metrics-export-is-the-text-format
  (let [resp (prom/metrics-handler {})]
    (is (= 200 (:status resp)))
    (is (str/includes? (:body resp) "hive_mcp_events_total"))
    (is (string? (prom/metrics-response)))
    (is (true? (prom/init!)))))

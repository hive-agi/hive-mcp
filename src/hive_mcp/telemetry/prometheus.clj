(ns hive-mcp.telemetry.prometheus
  "Prometheus metrics registry and collectors.


   Metrics naming convention:
   - All metrics prefixed with 'hive_mcp_'
   - Counters: *_total (e.g., hive_mcp_events_total)
   - Gauges: no suffix (e.g., hive_mcp_lings_active)
   - Histograms: *_seconds or *_bytes (e.g., hive_mcp_request_duration_seconds)

   Usage:
     (require '[hive-mcp.telemetry.prometheus :as prom])

     ;; Increment counter
     (prom/inc-events-total! :progress :info)

     ;; Set gauge
     (prom/set-lings-active! 3)

     ;; Observe histogram
     (prom/observe-request-duration! :mcp-memory-query 0.045)

     ;; Get Prometheus exposition format
     (prom/metrics-response)  ; Returns text/plain for /metrics endpoint"
  (:require [iapetos.core :as prometheus]
            [iapetos.collector.jvm :as jvm]
            [iapetos.export :as export]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


(def ^:private events-total-name :hive-mcp/events-total)
(def ^:private errors-total-name :hive-mcp/errors-total)
(def ^:private lings-active-name :hive-mcp/lings-active)
(def ^:private ws-clients-name :hive-mcp/ws-clients)
(def ^:private mcp-requests-total-name :hive-mcp/mcp-requests-total)
(def ^:private request-duration-seconds-name :hive-mcp/request-duration-seconds)
(def ^:private memory-ops-total-name :hive-mcp/memory-ops-total)
(def ^:private hivemind-shouts-total-name :hive-mcp/hivemind-shouts-total)
(def ^:private chroma-query-seconds-name :hive-mcp/chroma-query-seconds)

;; Wave-specific metrics
(def ^:private wave-success-rate-name :hive-mcp/wave-success-rate)
(def ^:private wave-items-total-name :hive-mcp/wave-items-total)
(def ^:private wave-duration-seconds-name :hive-mcp/wave-duration-seconds)
(def ^:private wave-failures-total-name :hive-mcp/wave-failures-total)


;; Counter: Events processed (labels: :type, :severity)
(def ^:private events-total-collector
  (prometheus/counter
   events-total-name
   {:description "Total events by type and severity"
    :labels [:type :severity]}))

;; Counter: Errors by type (labels: :error-type, :recoverable)
(def ^:private errors-total-collector
  (prometheus/counter
   errors-total-name
   {:description "Total errors by type"
    :labels [:error-type :recoverable]}))

;; Gauge: Active lings (no labels)
(def ^:private lings-active-collector
  (prometheus/gauge
   lings-active-name
   {:description "Currently active swarm lings"}))

;; Gauge: WebSocket clients connected (no labels)
(def ^:private ws-clients-collector
  (prometheus/gauge
   ws-clients-name
   {:description "Connected WebSocket clients"}))

;; Counter: MCP tool invocations (labels: :tool)
(def ^:private mcp-requests-total-collector
  (prometheus/counter
   mcp-requests-total-name
   {:description "MCP tool invocations"
    :labels [:tool]}))

;; Histogram: Request duration (labels: :tool, buckets: 10ms to 10s)
(def ^:private request-duration-seconds-collector
  (prometheus/histogram
   request-duration-seconds-name
   {:description "Request latency distribution"
    :labels [:tool]
    :buckets [0.01 0.025 0.05 0.1 0.25 0.5 1.0 2.5 5.0 10.0]}))

;; Counter: Memory operations (labels: :operation, :result)
(def ^:private memory-ops-total-collector
  (prometheus/counter
   memory-ops-total-name
   {:description "Memory operations by type"
    :labels [:operation :result]}))

;; Counter: Hivemind shouts (labels: :event-type)
(def ^:private hivemind-shouts-total-collector
  (prometheus/counter
   hivemind-shouts-total-name
   {:description "Hivemind shouts by event type"
    :labels [:event-type]}))

;; Histogram: Chroma query latency (labels: :operation)
(def ^:private chroma-query-seconds-collector
  (prometheus/histogram
   chroma-query-seconds-name
   {:description "Chroma query latency"
    :labels [:operation]
    :buckets [0.01 0.05 0.1 0.25 0.5 1.0 2.5 5.0]}))

;; Gauge: Wave success rate (0.0 to 1.0)
(def ^:private wave-success-rate-collector
  (prometheus/gauge
   wave-success-rate-name
   {:description "Success rate of last wave execution (0.0-1.0)"}))

;; Counter: Wave items processed (labels: :status)
(def ^:private wave-items-total-collector
  (prometheus/counter
   wave-items-total-name
   {:description "Total wave items processed by status"
    :labels [:status]}))

;; Histogram: Wave execution duration (buckets: 1s to 30m)
(def ^:private wave-duration-seconds-collector
  (prometheus/histogram
   wave-duration-seconds-name
   {:description "Wave execution duration distribution"
    :buckets [1.0 5.0 10.0 30.0 60.0 120.0 300.0 600.0 1800.0]}))

;; Counter: Wave failures (labels: :wave-id :reason)
(def ^:private wave-failures-total-collector
  (prometheus/counter
   wave-failures-total-name
   {:description "Total wave failures by reason"
    :labels [:wave-id :reason]}))


;; Global Prometheus collector registry.
;; Initialized lazily on first access to avoid startup ordering issues.
(defonce registry
  (delay
    (-> (prometheus/collector-registry)
        ;; JVM metrics - standard for observability dashboards
        (jvm/initialize)
        ;; Register all custom metrics
        (prometheus/register
         events-total-collector
         errors-total-collector
         lings-active-collector
         ws-clients-collector
         mcp-requests-total-collector
         request-duration-seconds-collector
         memory-ops-total-collector
         hivemind-shouts-total-collector
         chroma-query-seconds-collector
         ;; Wave orchestration metrics
         wave-success-rate-collector
         wave-items-total-collector
         wave-duration-seconds-collector
         wave-failures-total-collector))))


(defn inc-events-total!
  "Increment events counter.
   type: :progress :completed :error :blocked :started
   severity: :info :warn :error :fatal"
  [type severity]
  (prometheus/inc
   (@registry events-total-name {:type (name type)
                                 :severity (name severity)})))

(defn inc-errors-total!
  "Increment errors counter.
   error-type: keyword from health/error-types
   recoverable?: boolean"
  [error-type recoverable?]
  (prometheus/inc
   (@registry errors-total-name {:error-type (name error-type)
                                 :recoverable (str recoverable?)})))

(defn set-lings-active!
  "Set number of active lings."
  [n]
  (prometheus/set (@registry lings-active-name) n))

(defn set-ws-clients!
  "Set number of connected WebSocket clients."
  [n]
  (prometheus/set (@registry ws-clients-name) n))

(defn inc-mcp-requests!
  "Increment MCP tool request counter.
   tool: string tool name"
  [tool]
  (prometheus/inc
   (@registry mcp-requests-total-name {:tool (str tool)})))

(defn observe-request-duration!
  "Observe request duration.
   tool: string tool name
   seconds: duration in seconds (double)"
  [tool seconds]
  (prometheus/observe
   (@registry request-duration-seconds-name {:tool (str tool)})
   seconds))

(defn inc-memory-ops!
  "Increment memory operation counter.
   operation: :add :query :search
   result: :success :failure"
  [operation result]
  (prometheus/inc
   (@registry memory-ops-total-name {:operation (name operation)
                                     :result (name result)})))

(defn inc-hivemind-shouts!
  "Increment hivemind shouts counter.
   event-type: :progress :completed :error :blocked :started"
  [event-type]
  (prometheus/inc
   (@registry hivemind-shouts-total-name {:event-type (name event-type)})))

(defn observe-chroma-query!
  "Observe Chroma query duration.
   operation: :query :search :add
   seconds: duration in seconds (double)"
  [operation seconds]
  (prometheus/observe
   (@registry chroma-query-seconds-name {:operation (name operation)})
   seconds))


(defn set-wave-success-rate!
  "Set wave success rate gauge (0.0 to 1.0).
   rate: success ratio (completed / total)"
  [rate]
  (prometheus/set (@registry wave-success-rate-name) (double rate)))

(defn inc-wave-items!
  "Increment wave items counter.
   status: :success or :failed"
  [status]
  (prometheus/inc
   (@registry wave-items-total-name {:status (name status)})))

(defn observe-wave-duration!
  "Observe wave execution duration.
   seconds: total wave duration in seconds (double)"
  [seconds]
  (when seconds
    (prometheus/observe
     (@registry wave-duration-seconds-name)
     seconds)))

(defn inc-wave-failures!
  "Increment wave failures counter.
   wave-id: wave identifier
   reason: failure reason keyword (:conflict :timeout :validation :execution)"
  [wave-id reason]
  (prometheus/inc
   (@registry wave-failures-total-name {:wave-id (str wave-id)
                                        :reason (name (or reason :unknown))})))

(defmacro with-wave-timing
  "Execute body and record duration in wave-duration-seconds histogram.
   Also updates wave-success-rate gauge based on results.
   Returns the result of body execution.

   Usage:
     (with-wave-timing
       (execute-wave! plan-id opts))"
  [& body]
  `(let [start# (System/nanoTime)
         result# (do ~@body)
         duration# (/ (- (System/nanoTime) start#) 1e9)]
     (observe-wave-duration! duration#)
     result#))


(defn handle-prometheus-effect!
  "Handle a :prometheus effect from the event system.

   Effect shape: {:counter kw :labels {...} :histogram {:name kw :value n}}

   Known counters:
   - :wave_failure (with :wave_id and :reason labels)
   Any other counter increments the generic events counter.

   No histogram names are currently known; a :histogram entry is logged
   at debug level and otherwise ignored."

  [{:keys [counter labels histogram]}]
  (let [wave-id (get labels :wave_id)
        reason (get labels :reason)]
    ;; Handle counter
    (case counter
      :wave_failure (inc-wave-failures! wave-id reason)
      ;; Fallback for unknown counters - log and increment generic events
      (do
        (log/debug "Unknown prometheus counter:" counter "- using generic events")
        (inc-events-total! (or counter :unknown) :info)))

    ;; Handle histogram if present
    (when-let [{:keys [name]} histogram]
      (log/debug "Unknown prometheus histogram:" name))))


(defmacro with-request-timing
  "Execute body and record duration in request-duration-seconds histogram.
   Returns the result of body execution.

   Usage:
     (with-request-timing \"mcp-memory-query\"
       (query-memory ...))"
  [tool & body]
  `(let [start# (System/nanoTime)
         result# (do ~@body)
         duration# (/ (- (System/nanoTime) start#) 1e9)]
     (observe-request-duration! ~tool duration#)
     result#))

(defmacro with-store-timing
  "Execute body and record duration in chroma-query-seconds histogram.
   Returns the result of body execution.

   Usage:
     (with-store-timing :search
       (chroma/search ...))"
  [operation & body]
  `(let [start# (System/nanoTime)
         result# (do ~@body)
         duration# (/ (- (System/nanoTime) start#) 1e9)]
     (observe-chroma-query! ~operation duration#)
     result#))


(defn metrics-response
  "Export all metrics in Prometheus exposition format.
   Returns string suitable for /metrics HTTP endpoint.

   Format: text/plain; version=0.0.4; charset=utf-8"
  []
  (export/text-format @registry))

(defn metrics-handler
  "Ring-compatible handler for /metrics endpoint.
   Returns {:status 200 :headers {...} :body <metrics>}"
  [_request]
  {:status 200
   :headers {"Content-Type" "text/plain; version=0.0.4; charset=utf-8"}
   :body (metrics-response)})


(defn init!
  "Initialize the Prometheus registry and all collectors.
   Safe to call multiple times - collectors are idempotent.

   Returns true on success."
  []
  (try
    ;; Force initialization of registry (includes all collectors)
    @registry
    (log/info "Prometheus metrics initialized")
    true
    (catch Exception e
      (log/error e "Failed to initialize Prometheus metrics")
      false)))

(comment
  ;; REPL examples

  ;; Initialize (safe to call multiple times)
  (init!)

  ;; Record some metrics
  (inc-events-total! :progress :info)
  (inc-events-total! :completed :info)
  (inc-errors-total! :chroma-unavailable true)
  (set-lings-active! 3)
  (inc-mcp-requests! "mcp_memory_query")
  (observe-request-duration! "mcp_memory_query" 0.042)

  ;; Timing macro
  (with-request-timing "test-operation"
    (Thread/sleep 100)
    {:result "done"})

  ;; Get Prometheus format
  (println (metrics-response))

  ;; Ring handler
  (metrics-handler {}))

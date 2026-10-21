(ns hive-mcp.telemetry.resources-test
  "JVM resource sampler: sample math over injected MX readings, ring bounds,
   growth rates, start/stop idempotency and gauge registration."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.telemetry.metrics-tool :as tool]
            [hive-mcp.telemetry.prometheus :as prom]
            [hive-mcp.telemetry.resources :as res])
  (:import [io.prometheus.client CollectorRegistry]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (res/stop!) (res/reset-buffer!) (try (f) (finally (res/stop!) (res/reset-buffer!)))))

(def ^:private mb 1048576)

(defn- raw
  "A synthetic MX reading at minute `t` with the given overrides."
  [t & {:keys [meta classes total unloaded rss cpu threads gc]
        :or {meta (* 800 mb) classes 189000 total 190000 unloaded 1000
             rss (* 4300 mb) cpu 0 threads {} gc [{:name "G1 Young Generation" :count 145 :time-ms 4500}]}}]
  {:at-ms (* t 60000)
   :process-cpu-ns cpu
   :process-load 0.137 :system-cpu-load 0.2 :system-load-avg 1.5 :processors 16
   :heap {:used (* 1600 mb) :committed (* 2000 mb) :max (* 10240 mb)}
   :old-gen-after-gc (* 900 mb)
   :non-heap-used (* 1000 mb) :metaspace meta :compressed-class (* 120 mb)
   :classes {:loaded classes :total-loaded total :unloaded unloaded}
   :threads {:live 264 :peak 355 :daemon 200}
   :thread-cpu threads
   :gc gc
   :rss-bytes rss :open-fds 512})

(deftest first-sample-has-no-deltas
  (let [s (res/derive-sample nil (raw 0) 5)]
    (is (nil? (:interval-ms s)))
    (is (nil? (get-in s [:classes :loaded-delta])))
    (is (= [] (get-in s [:threads :groups])))
    (is (nil? (get-in s [:gc 0 :count-delta])))
    (is (= 189000 (get-in s [:classes :loaded])))
    (is (= (* 900 mb) (get-in s [:heap :old-gen-after-gc])))))

(deftest sample-math-over-injected-readings
  (let [a (raw 0 :cpu 0
               :threads {1 {:name "clojure-agent-send-off-pool-1" :cpu-ns 0}
                         2 {:name "clojure-agent-send-off-pool-2" :cpu-ns 0}
                         3 {:name "main" :cpu-ns 5}})
        b (raw 1 :cpu 30000000000 :total 190600 :unloaded 1100
               :gc [{:name "G1 Young Generation" :count 150 :time-ms 4700}]
               :threads {1 {:name "clojure-agent-send-off-pool-1" :cpu-ns 6000000000}
                         2 {:name "clojure-agent-send-off-pool-2" :cpu-ns 3000000000}
                         3 {:name "main" :cpu-ns 5}
                         4 {:name "nrepl-session-9" :cpu-ns 1000000000}})
        s (res/derive-sample a b 1)]
    (testing "interval and process CPU"
      (is (= 60000 (:interval-ms s)))
      (is (= 30000000000 (get-in s [:cpu :process-cpu-delta-ns])))
      (is (= 0.5 (get-in s [:cpu :process-cores]))))
    (testing "class deltas"
      (is (= 600 (get-in s [:classes :loaded-delta])))
      (is (= 100 (get-in s [:classes :unloaded-delta]))))
    (testing "gc deltas per collector"
      (is (= 5 (get-in s [:gc 0 :count-delta])))
      (is (= 200 (get-in s [:gc 0 :time-ms-delta]))))
    (testing "thread groups use observe-threads naming, top-n bounded"
      (is (= [{:group "clojure-agent-send-off-pool-*" :threads 2 :cpu-ms 9000.0 :core-ratio 0.15}]
             (get-in s [:threads :groups]))))))

(deftest reused-thread-id-counts-from-zero-never-negative
  (let [a (raw 0 :threads {1 {:name "old" :cpu-ns 9000000000}})
        b (raw 1 :threads {1 {:name "new" :cpu-ns 1000000}})]
    (is (= [{:group "new" :threads 1 :cpu-ms 1.0 :core-ratio 0.0}]
           (get-in (res/derive-sample a b 3) [:threads :groups])))))

(deftest ring-buffer-is-bounded
  (let [ring (reduce (fn [r i] (res/push-ring r {:at-ms i} 3)) [] (range 10))]
    (is (= 3 (count ring)))
    (is (= [7 8 9] (map :at-ms ring))))
  (is (= 1 (count (res/push-ring [{:at-ms 1}] {:at-ms 2} 1))))
  (testing "step keeps capacity"
    (let [st (reduce (fn [st t] (res/step st (raw t) {:capacity 4})) {} (range 9))]
      (is (= 4 (count (:ring st))))
      (is (= (* 8 60000) (:at-ms (:last st)))))))

(deftest growth-rates
  (testing "Metaspace and classes per hour, RSS slope"
    (let [st (reduce (fn [st t]
                       (res/step st (raw t :meta (+ (* 800 mb) (* t mb))
                                         :classes (+ 189000 (* 10 t))
                                         :rss (+ (* 4300 mb) (* 2 t mb)))
                                 {}))
                     {} (range 31))
          t (res/trend (:ring st))]
      (is (= 31 (:samples t)))
      (is (= (* 30 60000) (:window-ms t)))
      (is (== (* 60 mb) (:metaspace-bytes-per-hour t)))
      (is (== 60.0 (:metaspace-mb-per-hour t)))
      (is (== 600.0 (:classes-per-hour t)))
      (is (< (Math/abs (- (:rss-bytes-per-hour t) (* 120.0 mb))) 1.0))
      (is (== 0.0 (:old-gen-after-gc-mb-per-hour t)))))
  (testing "under two samples the rates are nil"
    (let [t (res/trend [(res/derive-sample nil (raw 0) 5)])]
      (is (= 1 (:samples t)))
      (is (nil? (:metaspace-bytes-per-hour t)))
      (is (nil? (:rss-bytes-per-hour t)))))
  (testing "rss nil off Linux leaves the slope nil"
    (let [st (reduce (fn [st t] (res/step st (raw t :rss nil) {})) {} (range 3))]
      (is (nil? (:rss-bytes-per-hour (res/trend (:ring st))))))))

(deftest start-stop-idempotent
  (let [calls (atom 0)
        read-fn (fn [] (raw (swap! calls inc)))]
    (is (= {:started? true :running? true :interval-ms 10}
           (res/start! {:interval-ms 10 :read-fn read-fn})))
    (is (= {:started? false :running? true :interval-ms 10}
           (res/start! {:interval-ms 10 :read-fn read-fn})))
    (is (res/running?))
    (let [deadline (+ (System/currentTimeMillis) 5000)]
      (while (and (< @calls 3) (< (System/currentTimeMillis) deadline)) (Thread/sleep 5)))
    (is (<= 3 @calls))
    (is (= {:stopped? true} (res/stop!)))
    (is (= {:stopped? false} (res/stop!)))
    (is (not (res/running?)))
    (testing "only one sampler thread ever ran"
      (is (>= 1 (count (filter #(= "hive-resource-sampler" (.getName ^Thread %))
                               (keys (Thread/getAllStackTraces))))))
      (Thread/sleep 50)
      (let [n @calls] (Thread/sleep 50) (is (= n @calls))))
    (testing "the buffer survives stop and the report reads it"
      (let [r (res/report)]
        (is (false? (:running? r)))
        (is (<= 3 (get-in r [:trend :samples])))
        (is (some? (:latest r)))))
    (is (:started? (res/start! {:interval-ms 10 :read-fn read-fn})) "restart after stop")))

(deftest a-failing-read-never-kills-the-sampler
  (is (nil? (res/sample-once! {:read-fn #(throw (ex-info "boom" {}))}))))

(deftest real-mx-reading-derives
  (let [a (res/read-mx) b (res/read-mx)
        s (res/derive-sample a b 5)]
    (is (pos? (get-in s [:classes :loaded])))
    (is (pos? (get-in s [:threads :live])))
    (is (pos? (get-in s [:heap :used])))
    (is (seq (:gc s)))
    (is (<= (count (get-in s [:threads :groups])) 5))))

(deftest gauge-registration
  (let [reg (CollectorRegistry.)
        gs (res/register-gauges! reg)]
    (is (= (count res/gauge-specs) (count gs)))
    (is (identical? gs (res/register-gauges! reg)) "second registration answers the same gauges")
    (is (nil? (res/register-gauges! :not-a-registry)))
    (let [st (reduce (fn [st t] (res/step st (raw t :threads {1 {:name "w-1" :cpu-ns (* t 6000000000)}}) {}))
                     {} (range 3))]
      (res/publish! gs (:last st) (res/trend (:ring st))))
    (is (== 189000.0 (.getSampleValue reg "hive_mcp_resource_classes_loaded")))
    (is (== 0.1 (.getSampleValue reg "hive_mcp_resource_thread_group_cpu_ratio"
                                 (into-array String ["group"]) (into-array String ["w-*"]))))
    (is (== 0.137 (.getSampleValue reg "hive_mcp_resource_process_cpu_load")))))

(deftest gauges-do-not-duplicate-the-iapetos-jvm-collectors
  (prom/init!)
  (let [before (prom/metrics-response)]
    (testing "the iapetos JVM collectors are already registered"
      (is (str/includes? before "jvm_memory_bytes_used"))
      (is (str/includes? before "jvm_threads_current"))
      (is (str/includes? before "process_cpu_seconds_total")))
    (testing "no resource gauge reuses an exported name"
      (doseq [n (res/metric-names)]
        (is (not (str/includes? before (str n " "))) n)))
    (testing "start-default! registers the gauges in the hive registry once"
      (is (map? (res/start-default!)))
      (res/stop!)
      (is (map? (res/start-default!)))
      (res/stop!)
      (let [after (prom/metrics-response)]
        (doseq [n (res/metric-names)]
          (is (= 1 (count (re-seq (re-pattern (str "# TYPE " n " ")) after))) n))))))

(deftest resources-verb-answers-the-report
  (res/step {} (raw 0) {})
  (let [r (tool/handle-resources {})]
    (is (= "text" (:type r)))
    (is (str/includes? (:text r) ":trend"))
    (is (contains? (get-in tool/contribution ["metrics" :handler]) :resources))))

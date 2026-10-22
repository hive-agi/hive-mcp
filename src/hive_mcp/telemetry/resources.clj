(ns hive-mcp.telemetry.resources
  "JVM resource-usage sampler: one daemon thread reads the platform MX beans
   on a fixed interval (default 15 s) into a bounded ring buffer, publishes
   gauges into the iapetos registry, and answers a trend report.

   Contract:
   - Nothing runs per MCP call. The only work is one sample per interval on
     the thread `hive-resource-sampler`.
   - `read-mx` is the ONLY fn that touches the running JVM; it answers a raw
     reading (plain data). `derive-sample`, `push-ring`, `step` and
     `trend` are pure, so tests inject readings.
   - `start!` / `stop!` are idempotent and survive a namespace reload (the
     runner and buffer are defonce; the scheduled task calls through a var).
   - Gauges cover only what the iapetos JVM collectors (Standard, GC,
     MemoryPools, Thread exports, registered by
     hive-mcp.telemetry.prometheus) do not already export: CPU load, old gen
     after GC, class loading, per-thread-group CPU and the trend slopes.
     Heap, threads live/peak/daemon, process CPU seconds, RSS and open fds are
     already exported there, so they stay in the report only.
   - Thread groups are named with hive-observability.jvm.threads/normalize-name
     (the grouping behind `observe threads`) when that addon is on the
     classpath, else with a local fallback of the same rule."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [malli.core :as m]
            [taoensso.timbre :as log])
  (:import [io.prometheus.client CollectorRegistry Gauge]
           [java.lang.management GarbageCollectorMXBean ManagementFactory
            MemoryPoolMXBean MemoryUsage ThreadInfo ThreadMXBean]
           [java.util.concurrent Executors ScheduledExecutorService ThreadFactory TimeUnit]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-opts
  "Sampler defaults: 15 s interval, 960 samples (4 h), top 10 thread groups."
  {:interval-ms 15000 :capacity 960 :top-n 10})

;; =============================================================================
;; Schemas
;; =============================================================================

(def Opts
  "Sampler options; every key optional."
  [:map
   [:interval-ms {:optional true} [:int {:min 1}]]
   [:capacity {:optional true} [:int {:min 1}]]
   [:top-n {:optional true} [:int {:min 1}]]
   [:read-fn {:optional true} ifn?]
   [:registry {:optional true} :any]])

(def Raw
  "One raw MX reading. Cumulative counters, wall clock ms."
  [:map
   [:at-ms :int]
   [:thread-cpu {:optional true} [:map-of :int [:map [:name :string] [:cpu-ns :int]]]]
   [:gc {:optional true} [:sequential [:map [:name :string] [:count :int] [:time-ms :int]]]]])

(def Sample
  "One derived sample (deltas against the previous reading)."
  [:map [:at-ms :int]])

(def Ring
  "The bounded sample buffer, oldest first."
  [:vector Sample])

;; =============================================================================
;; Adapter: the running JVM
;; =============================================================================

(defn- usage [^MemoryUsage u]
  (if u
    {:used (.getUsed u) :committed (.getCommitted u) :max (.getMax u)}
    {:used 0 :committed 0 :max -1}))

(defn- pool-named [re]
  (some (fn [^MemoryPoolMXBean p] (when (re-find re (.getName p)) p))
        (ManagementFactory/getMemoryPoolMXBeans)))

(defn- rss-bytes
  "VmRSS from /proc/self/status in bytes; nil off Linux or on any failure."
  []
  (try
    (let [f (io/file "/proc/self/status")]
      (when (.exists f)
        (some (fn [line]
                (when (str/starts-with? line "VmRSS:")
                  (some-> (re-find #"\d+" line) parse-long (* 1024))))
              (str/split-lines (slurp f)))))
    (catch Throwable _ nil)))

(defn- open-fds []
  (let [os (ManagementFactory/getOperatingSystemMXBean)]
    (when (instance? com.sun.management.UnixOperatingSystemMXBean os)
      (.getOpenFileDescriptorCount ^com.sun.management.UnixOperatingSystemMXBean os))))

(defn- thread-cpu []
  (let [^ThreadMXBean b (ManagementFactory/getThreadMXBean)]
    (when (and (.isThreadCpuTimeSupported b) (not (.isThreadCpuTimeEnabled b)))
      (.setThreadCpuTimeEnabled b true))
    (if (.isThreadCpuTimeEnabled b)
      (into {}
            (keep (fn [^ThreadInfo info]
                    (when info
                      (let [id (.getThreadId info)
                            cpu (.getThreadCpuTime b id)]
                        (when (>= cpu 0)
                          [id {:name (.getThreadName info) :cpu-ns cpu}])))))
            (.getThreadInfo b (.getAllThreadIds b)))
      {})))

(defn read-mx
  "One raw reading of the running JVM: CPU, memory, classes, threads, GC and
   process (RSS nil off Linux, open fds nil off Unix)."
  []
  (let [os  (ManagementFactory/getOperatingSystemMXBean)
        sun (when (instance? com.sun.management.OperatingSystemMXBean os) os)
        mem (ManagementFactory/getMemoryMXBean)
        cl  (ManagementFactory/getClassLoadingMXBean)
        ^ThreadMXBean th (ManagementFactory/getThreadMXBean)
        ^MemoryPoolMXBean old (pool-named #"(?i)old gen|tenured|old space")
        ^MemoryPoolMXBean meta (pool-named #"^Metaspace$")
        ^MemoryPoolMXBean ccs (pool-named #"(?i)compressed class")]
    {:at-ms            (System/currentTimeMillis)
     :process-cpu-ns   (if sun (max 0 (.getProcessCpuTime ^com.sun.management.OperatingSystemMXBean sun)) 0)
     :process-load     (when sun (.getProcessCpuLoad ^com.sun.management.OperatingSystemMXBean sun))
     :system-cpu-load  (when sun (.getCpuLoad ^com.sun.management.OperatingSystemMXBean sun))
     :system-load-avg  (.getSystemLoadAverage os)
     :processors       (.availableProcessors (Runtime/getRuntime))
     :heap             (usage (.getHeapMemoryUsage mem))
     :old-gen-after-gc (some-> old .getCollectionUsage .getUsed)
     :non-heap-used    (.getUsed (.getNonHeapMemoryUsage mem))
     :metaspace        (some-> meta .getUsage .getUsed)
     :compressed-class (some-> ccs .getUsage .getUsed)
     :classes          {:loaded       (.getLoadedClassCount cl)
                        :total-loaded (.getTotalLoadedClassCount cl)
                        :unloaded     (.getUnloadedClassCount cl)}
     :threads          {:live (.getThreadCount th) :peak (.getPeakThreadCount th)
                        :daemon (.getDaemonThreadCount th)}
     :thread-cpu       (thread-cpu)
     :gc               (mapv (fn [^GarbageCollectorMXBean b]
                               {:name (.getName b)
                                :count (max 0 (.getCollectionCount b))
                                :time-ms (max 0 (.getCollectionTime b))})
                             (ManagementFactory/getGarbageCollectorMXBeans))
     :rss-bytes        (rss-bytes)
     :open-fds         (open-fds)}))

(m/=> read-mx [:=> [:cat] Raw])

;; =============================================================================
;; Pure: grouping, deltas, ring, trend
;; =============================================================================

(defn- fallback-normalize
  "Same rule as hive-observability.jvm.threads/normalize-name: fold hex ids
   and free-standing digit runs into *."
  [thread-name]
  (let [n (-> (str thread-name)
              (str/replace #"(?i)\b(?=[0-9a-f-]*[0-9])[0-9a-f]{6,}(?:-[0-9a-f]+)*\b" "*")
              (str/replace #"(?<![A-Za-z0-9])\d+" "*")
              (str/replace #"\*(?:[-_.:]?\*)+" "*")
              str/trim)]
    (if (str/blank? n) "<unnamed>" n)))

(def ^:private normalizer
  (delay (or (try (some-> (requiring-resolve 'hive-observability.jvm.threads/normalize-name) deref)
                  (catch Throwable _ nil))
             fallback-normalize)))

(defn group-name
  "The thread-group name of `thread-name`, as `observe threads` groups it."
  [thread-name]
  (@normalizer (str thread-name)))

(m/=> group-name [:=> [:cat :string] :string])

(defn- round3 [x] (/ (Math/round (* 1000.0 (double x))) 1000.0))

(defn- thread-groups
  "Top-n thread groups by CPU spent between two readings. A thread whose id
   was reused under another name counts from zero; counters never go negative."
  [prev-cpu cpu interval-ms top-n]
  (if (and prev-cpu interval-ms (pos? interval-ms))
    (->> cpu
         (map (fn [[id {:keys [name cpu-ns]}]]
                (let [p (get prev-cpu id)
                      base (if (and p (= name (:name p))) (:cpu-ns p) 0)]
                  [(group-name name) (max 0 (- cpu-ns base))])))
         (reduce (fn [acc [g ns]] (-> acc (update-in [g :cpu-ns] (fnil + 0) ns)
                                      (update-in [g :threads] (fnil inc 0))))
                 {})
         (map (fn [[g {:keys [cpu-ns threads]}]]
                {:group g :threads threads
                 :cpu-ms (round3 (/ cpu-ns 1e6))
                 :core-ratio (round3 (/ cpu-ns (* 1e6 interval-ms)))}))
         (remove (comp zero? :cpu-ms))
         (sort-by (juxt (comp - :cpu-ms) :group))
         (take top-n)
         vec)
    []))

(defn- delta [a b] (when (and a b) (max 0 (- a b))))

(defn derive-sample
  "Fold reading `raw` against the previous reading `prev` (nil on the first
   sample) into one sample: gauges as read plus deltas over the interval."
  [prev raw top-n]
  (let [interval (when prev (max 1 (- (:at-ms raw) (:at-ms prev))))
        prev-gc (into {} (map (juxt :name identity)) (:gc prev))
        cpu-delta (when prev (delta (:process-cpu-ns raw) (:process-cpu-ns prev)))]
    {:at-ms (:at-ms raw)
     :interval-ms interval
     :cpu {:process-load (:process-load raw)
           :system-cpu-load (:system-cpu-load raw)
           :system-load-avg (:system-load-avg raw)
           :process-cpu-ns (:process-cpu-ns raw)
           :process-cpu-delta-ns cpu-delta
           :process-cores (when (and cpu-delta interval) (round3 (/ cpu-delta (* 1e6 interval))))}
     :heap (assoc (:heap raw) :old-gen-after-gc (:old-gen-after-gc raw))
     :non-heap {:used (:non-heap-used raw) :metaspace (:metaspace raw)
                :compressed-class (:compressed-class raw)}
     :classes {:loaded (get-in raw [:classes :loaded])
               :loaded-delta (when prev (delta (get-in raw [:classes :total-loaded])
                                               (get-in prev [:classes :total-loaded])))
               :unloaded-delta (when prev (delta (get-in raw [:classes :unloaded])
                                                 (get-in prev [:classes :unloaded])))}
     :threads (assoc (:threads raw)
                     :groups (thread-groups (:thread-cpu prev) (:thread-cpu raw) interval
                                            (or top-n (:top-n default-opts))))
     :gc (mapv (fn [{:keys [name count time-ms]}]
                 (let [p (get prev-gc name)]
                   {:name name :count count :time-ms time-ms
                    :count-delta (when p (delta count (:count p)))
                    :time-ms-delta (when p (delta time-ms (:time-ms p)))}))
               (:gc raw))
     :process {:rss-bytes (:rss-bytes raw) :open-fds (:open-fds raw)}}))

(m/=> derive-sample [:=> [:cat [:maybe Raw] Raw [:maybe [:int {:min 1}]]] Sample])

(defn push-ring
  "Append sample `s` to `ring`, dropping the oldest so at most `capacity`
   samples remain."
  [ring s capacity]
  (let [r (conj (vec ring) s)
        over (- (count r) (max 1 capacity))]
    (if (pos? over) (subvec r over) r)))

(m/=> push-ring [:=> [:cat [:maybe Ring] Sample [:int {:min 1}]] Ring])

(defn- per-hour
  "Growth of `f` per hour between the first and last sample that carry it."
  [samples f]
  (let [xs (filter (comp some? f) samples)
        a (first xs) b (last xs)
        dt (when (and a b) (- (:at-ms b) (:at-ms a)))]
    (when (and dt (pos? dt))
      (/ (* 3.6e6 (double (- (f b) (f a)))) dt))))

(defn- slope-per-hour
  "Least-squares slope of `f` over time, per hour; nil under two points."
  [samples f]
  (let [pts (keep (fn [s] (when-let [y (f s)] [(double (:at-ms s)) (double y)])) samples)
        n (count pts)]
    (when (>= n 2)
      (let [mx (/ (reduce + (map first pts)) n)
            my (/ (reduce + (map second pts)) n)
            sxx (reduce + (map (fn [[x _]] (* (- x mx) (- x mx))) pts))
            sxy (reduce + (map (fn [[x y]] (* (- x mx) (- y my))) pts))]
        (when (pos? sxx) (* 3.6e6 (/ sxy sxx)))))))

(defn- mb [x] (when x (round3 (/ x 1048576.0))))

(defn trend
  "Trend over the ring: Metaspace and class growth per hour (first to last),
   RSS and old-gen-after-GC least-squares slope per hour. nil rates under two
   samples."
  [ring]
  (let [ring (vec ring)
        meta-h (per-hour ring #(get-in % [:non-heap :metaspace]))
        cls-h (per-hour ring #(get-in % [:classes :loaded]))
        rss-h (slope-per-hour ring #(get-in % [:process :rss-bytes]))
        old-h (slope-per-hour ring #(get-in % [:heap :old-gen-after-gc]))]
    {:samples (count ring)
     :window-ms (if (>= (count ring) 2) (- (:at-ms (peek ring)) (:at-ms (first ring))) 0)
     :metaspace-bytes-per-hour meta-h
     :metaspace-mb-per-hour (mb meta-h)
     :classes-per-hour (some-> cls-h round3)
     :rss-bytes-per-hour rss-h
     :rss-mb-per-hour (mb rss-h)
     :old-gen-after-gc-mb-per-hour (mb old-h)}))

(m/=> trend [:=> [:cat [:maybe Ring]]
             [:map [:samples :int] [:window-ms :int]
              [:metaspace-bytes-per-hour [:maybe number?]]
              [:classes-per-hour [:maybe number?]]
              [:rss-bytes-per-hour [:maybe number?]]]])

(defn step
  "Advance sampler state {:prev raw :ring [...]} by one reading."
  [state raw opts]
  (let [{:keys [capacity top-n]} (merge default-opts opts)
        s (derive-sample (:prev state) raw top-n)]
    {:prev raw :ring (push-ring (:ring state) s capacity) :last s}))

(m/=> step [:=> [:cat [:maybe :map] Raw [:maybe Opts]] [:map [:prev Raw] [:ring Ring] [:last Sample]]])

;; =============================================================================
;; Gauges
;; =============================================================================

(def gauge-specs
  "name -> [help labels]. Every name is prefixed hive_mcp_resource_."
  {"process_cpu_load"              ["Process CPU load, 0..1 of all cores" []]
   "system_cpu_load"               ["System CPU load, 0..1" []]
   "system_load_average"           ["System load average (1 min)" []]
   "old_gen_after_gc_bytes"        ["Old generation occupancy after the last collection" []]
   "compressed_class_space_bytes"  ["Compressed class space used" []]
   "classes_loaded"                ["Currently loaded classes" []]
   "classes_loaded_delta"          ["Classes loaded during the last interval" []]
   "classes_unloaded_delta"        ["Classes unloaded during the last interval" []]
   "thread_group_cpu_ratio"        ["CPU of a thread group over the last interval, cores (top N groups)" ["group"]]
   "metaspace_growth_bytes_per_hour" ["Metaspace growth per hour over the sample ring" []]
   "classes_growth_per_hour"       ["Loaded-class growth per hour over the sample ring" []]
   "rss_slope_bytes_per_hour"      ["RSS least-squares slope per hour over the sample ring" []]})

(defn metric-names
  "Exported Prometheus metric names, sorted."
  []
  (vec (sort (map #(str "hive_mcp_resource_" %) (keys gauge-specs)))))

(m/=> metric-names [:=> [:cat] [:vector :string]])

(defonce ^:private gauges-by-registry (atom {}))

(defn register-gauges!
  "Register the resource gauges in the raw CollectorRegistry `reg` once;
   later calls answer the same gauges. nil when registration fails (e.g. the
   names are already taken by gauges this namespace no longer holds)."
  [reg]
  (when (instance? CollectorRegistry reg)
    (locking gauges-by-registry
      (or (get @gauges-by-registry reg)
          (try
            (let [gs (into {}
                           (map (fn [[n [help labels]]]
                                  [n (-> (Gauge/build)
                                         (.name (str "hive_mcp_resource_" n))
                                         (.help help)
                                         (.labelNames (into-array String labels))
                                         (.register ^CollectorRegistry reg))]))
                           gauge-specs)]
              (swap! gauges-by-registry assoc reg gs)
              gs)
            (catch Throwable e
              (log/warn "resources: gauge registration failed:" (ex-message e))
              nil))))))

(m/=> register-gauges! [:=> [:cat :any] [:maybe [:map-of :string :any]]])

(defn- set-gauge! [gs n v]
  (when-let [^Gauge g (get gs n)]
    (.set g (double (if (number? v) v Double/NaN)))))

(defn publish!
  "Write `sample` and trend `t` into gauges `gs`. The thread-group gauge is
   cleared first, so it never holds more than top-n children. Returns nil."
  [gs sample t]
  (when (seq gs)
    (set-gauge! gs "process_cpu_load" (get-in sample [:cpu :process-load]))
    (set-gauge! gs "system_cpu_load" (get-in sample [:cpu :system-cpu-load]))
    (set-gauge! gs "system_load_average" (get-in sample [:cpu :system-load-avg]))
    (set-gauge! gs "old_gen_after_gc_bytes" (get-in sample [:heap :old-gen-after-gc]))
    (set-gauge! gs "compressed_class_space_bytes" (get-in sample [:non-heap :compressed-class]))
    (set-gauge! gs "classes_loaded" (get-in sample [:classes :loaded]))
    (set-gauge! gs "classes_loaded_delta" (get-in sample [:classes :loaded-delta]))
    (set-gauge! gs "classes_unloaded_delta" (get-in sample [:classes :unloaded-delta]))
    (set-gauge! gs "metaspace_growth_bytes_per_hour" (:metaspace-bytes-per-hour t))
    (set-gauge! gs "classes_growth_per_hour" (:classes-per-hour t))
    (set-gauge! gs "rss_slope_bytes_per_hour" (:rss-bytes-per-hour t))
    (when-let [^Gauge g (get gs "thread_group_cpu_ratio")]
      (.clear g)
      (doseq [{:keys [group core-ratio]} (get-in sample [:threads :groups])]
        (.set ^io.prometheus.client.Gauge$Child (.labels g (into-array String [group]))
              (double core-ratio)))))
  nil)

(m/=> publish! [:=> [:cat [:maybe :map] Sample [:maybe :map]] :nil])

;; =============================================================================
;; Runner
;; =============================================================================

(defonce ^:private state (atom {:prev nil :ring []}))

(defonce ^:private runner (atom nil))

(defn sample-once!
  "Take one reading with `read-fn`, advance the buffer and publish gauges.
   Never throws (a throw would cancel the scheduled task); answers the
   sample, or nil on failure."
  [opts]
  (try
    (let [opts (merge default-opts opts)
          raw ((or (:read-fn opts) read-mx))
          st (swap! state step raw opts)]
      (publish! (:gauges @runner) (:last st) (trend (:ring st)))
      (:last st))
    (catch Throwable e
      (log/debug "resources: sample failed:" (ex-message e))
      nil)))

(m/=> sample-once! [:=> [:cat [:maybe Opts]] [:maybe Sample]])

(defn running?
  "True while the sampler thread is scheduled."
  []
  (some? (:executor @runner)))

(m/=> running? [:=> [:cat] :boolean])

(defn- daemon-factory ^ThreadFactory []
  (reify ThreadFactory
    (newThread [_ r]
      (doto (Thread. ^Runnable r "hive-resource-sampler") (.setDaemon true)))))

(defn start!
  "Start the sampler (idempotent). opts: :interval-ms :capacity :top-n,
   :read-fn (tests), :registry (a raw CollectorRegistry for the gauges; nil
   keeps gauges off). Answers {:started? bool :running? true :interval-ms n}."
  [opts]
  (locking runner
    (if (running?)
      {:started? false :running? true :interval-ms (get-in @runner [:opts :interval-ms])}
      (let [opts (merge default-opts (into {} (remove (comp nil? val)) opts))
            gs (some-> (:registry opts) register-gauges!)
            ^ScheduledExecutorService ex (Executors/newSingleThreadScheduledExecutor (daemon-factory))
            task-opts (dissoc opts :registry)]
        (swap! state assoc :prev nil)
        (reset! runner {:executor ex :opts task-opts :gauges gs})
        (.scheduleWithFixedDelay ex ^Runnable (fn [] (#'sample-once! task-opts))
                                 0 (long (:interval-ms opts)) TimeUnit/MILLISECONDS)
        (log/info "resources: sampler started" {:interval-ms (:interval-ms opts)
                                                :gauges (count gs)})
        {:started? true :running? true :interval-ms (:interval-ms opts)}))))

(m/=> start! [:=> [:cat [:maybe Opts]]
              [:map [:started? :boolean] [:running? :boolean] [:interval-ms [:maybe :int]]]])

(defn stop!
  "Stop the sampler (idempotent). The buffer is kept, so `report` still
   answers. Answers {:stopped? bool}."
  []
  (locking runner
    (if-let [^ScheduledExecutorService ex (:executor @runner)]
      (do (.shutdownNow ex)
          (.awaitTermination ex 2 TimeUnit/SECONDS)
          (reset! runner nil)
          (log/info "resources: sampler stopped")
          {:stopped? true})
      {:stopped? false})))

(m/=> stop! [:=> [:cat] [:map [:stopped? :boolean]]])

(defn reset-buffer!
  "Drop every buffered sample. Returns nil."
  []
  (reset! state {:prev nil :ring []})
  nil)

(m/=> reset-buffer! [:=> [:cat] :nil])

(defn report
  "The resource report: sampler status, trend over the ring, the latest
   sample and the exported metric names."
  []
  (let [{:keys [ring last]} @state]
    {:running? (running?)
     :interval-ms (get-in @runner [:opts :interval-ms])
     :trend (trend ring)
     :latest last
     :metrics (metric-names)}))

(m/=> report [:=> [:cat] [:map [:running? :boolean] [:trend :map]]])

(defn config-opts
  "Sampler opts from the environment: HIVE_RESOURCE_SAMPLE_MS, then config
   key telemetry.resources.interval-ms; capacity from
   telemetry.resources.capacity. Unset keys are absent."
  []
  (let [cfg (fn [k] (try (some-> (requiring-resolve 'hive-mcp.config.core/get-config-value)
                                 (apply [k]))
                         (catch Throwable _ nil)))
        int-of (fn [v] (cond (int? v) v (string? v) (parse-long v) :else nil))
        interval (or (int-of (System/getenv "HIVE_RESOURCE_SAMPLE_MS"))
                     (int-of (cfg "telemetry.resources.interval-ms")))
        capacity (int-of (cfg "telemetry.resources.capacity"))]
    (cond-> {}
      (and interval (pos? interval)) (assoc :interval-ms interval)
      (and capacity (pos? capacity)) (assoc :capacity capacity))))

(defn- prom-raw-registry
  "The raw CollectorRegistry behind hive-mcp.telemetry.prometheus/registry,
   or nil when Prometheus is unavailable."
  []
  (try
    (when ((requiring-resolve 'hive-mcp.telemetry.prometheus/init!))
      (let [reg @@(requiring-resolve 'hive-mcp.telemetry.prometheus/registry)]
        ((requiring-resolve 'iapetos.registry/raw) reg)))
    (catch Throwable _ nil)))

(defn start-default!
  "Start the sampler with `config-opts` and the gauges in the hive-mcp
   Prometheus registry. Idempotent; never throws (nil on failure)."
  []
  (try
    (start! (assoc (config-opts) :registry (prom-raw-registry)))
    (catch Throwable e
      (log/warn "resources: sampler start failed (non-fatal):" (ex-message e))
      nil)))

(m/=> start-default! [:=> [:cat] [:maybe :map]])

(m/=> config-opts [:=> [:cat] Opts])

(ns hive-mcp.tools.consolidated.hot-memory-gate
  "Refuse a heavy hot reload when the host is short of memory.

   A forced reload re-reads every file under an addon's roots and cascades to
   its dependents; reload-all does that for every addon. On 2026-11-18 a
   forced reload at ~7 GB available with swap full hung and the kernel
   OOM-killed the hive-mcp JVM, taking every ling with it. A plain
   changed-only reload of one addon stays allowed: it is the small one.

   Pure except `read-meminfo`, which callers may replace."
  (:require [clojure.string :as str]))

(def default-floor-gb
  "MemAvailable below this refuses a heavy reload."
  15)

(def default-min-swap-free-ratio
  "SwapFree below this fraction of SwapTotal refuses a heavy reload."
  0.10)

(def ^:private meminfo-keys
  {"MemAvailable" :mem-available-kb
   "SwapFree"     :swap-free-kb
   "SwapTotal"    :swap-total-kb})

(defn- line-entry
  "One /proc/meminfo line, \"Key:   1234 kB\", to [k kb], or nil."
  [line]
  (when-let [i (str/index-of line ":")]
    (when-let [k (meminfo-keys (subs line 0 i))]
      (let [v (-> (subs line (inc i)) (str/replace "kB" "") str/trim)]
        (when-let [n (parse-long v)]
          [k n])))))

(defn parse-meminfo
  "Pure. /proc/meminfo text to {:mem-available-kb :swap-free-kb :swap-total-kb};
   a key the text lacks is absent."
  [text]
  (into {} (keep line-entry) (str/split-lines (str text))))

(defn read-meminfo
  "The host's memory figures, or nil where /proc/meminfo cannot be read
   (not Linux): an unreadable host is not refused."
  []
  (try (parse-meminfo (slurp "/proc/meminfo"))
       (catch java.io.IOException _no-proc-meminfo-on-this-host nil)))

(defn verdict
  "Pure. nil when a heavy reload may run, else a refusal map naming the
   numbers and the override. `mem` is a `parse-meminfo` map."
  ([mem] (verdict mem {}))
  ([{:keys [mem-available-kb swap-free-kb swap-total-kb]}
    {:keys [floor-gb min-swap-free-ratio]
     :or   {floor-gb default-floor-gb min-swap-free-ratio default-min-swap-free-ratio}}]
   (let [avail-gb   (when mem-available-kb (/ mem-available-kb 1048576.0))
         low-mem?   (boolean (and avail-gb (< avail-gb floor-gb)))
         swap-full? (boolean (and swap-free-kb swap-total-kb (pos? swap-total-kb)
                                  (< (/ (double swap-free-kb) swap-total-kb) min-swap-free-ratio)))]
     (when (or low-mem? swap-full?)
       {:ok? false
        :refused? true
        :reason :hot/memory-low
        :mem-available-gb (when avail-gb (/ (Math/round (* 10 avail-gb)) 10.0))
        :floor-gb floor-gb
        :swap-free-kb swap-free-kb
        :swap-total-kb swap-total-kb
        :message (str "refused: a forced reload or reload-all needs memory the host does not have ("
                      (str/join ", "
                                (cond-> []
                                  low-mem?   (conj (format "MemAvailable %.1f GB < %d GB floor" avail-gb floor-gb))
                                  swap-full? (conj (format "SwapFree %d of %d kB < %.0f%%"
                                                           swap-free-kb swap-total-kb
                                                           (* 100.0 min-swap-free-ratio)))))
                      "). A plain reload of one changed addon is still allowed. "
                      "Operator override: force_memory=true.")}))))

(defn heavy?
  "Pure. Whether a reload call is the heavy kind the gate guards: reload-all,
   or a reload with force."
  [command params]
  (or (= command :reload-all)
      (contains? #{true "true"} (:force params))))

(defn refusal
  "The refusal for this call, or nil. `read-mem` defaults to `read-meminfo`."
  ([command params] (refusal command params read-meminfo))
  ([command params read-mem]
   (when (and (heavy? command params)
              (not (contains? #{true "true"} (:force_memory params))))
     (some-> (read-mem) verdict))))

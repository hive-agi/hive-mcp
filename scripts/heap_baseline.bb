#!/usr/bin/env bb
;; Usage: bb scripts/heap_baseline.bb [PID] [--fast] [--label TEXT]
;; Appends one EDN line to ~/.local/share/hive-mcp/heap-baseline.edn and prints it.
;; Without --fast it runs GC.class_histogram, a stop-the-world full GC.
(require '[babashka.process :refer [shell]]
         '[babashka.fs :as fs]
         '[clojure.string :as str])

(def args *command-line-args*)
(def fast? (some #{"--fast"} args))
(def label (second (drop-while #(not= "--label" %) args)))

(defn sh-out [& cmd]
  (let [{:keys [out exit]} (apply shell {:out :string :err :string :continue true} cmd)]
    (when (zero? exit) out)))

(defn tokens [line]
  (vec (remove str/blank? (.split ^String (-> line (str/replace "," " ") (str/replace "\t" " ")) " "))))

(defn lines-starting [out prefix]
  (filter #(str/starts-with? (str/triml %) prefix) (str/split-lines out)))

(defn strip [s prefix suffix]
  (some-> s
          (cond-> (str/starts-with? s prefix) (subs (count prefix)))
          (as-> t (if (str/ends-with? t suffix) (subs t 0 (- (count t) (count suffix))) t))
          parse-long))

(def hive-mcp-dir (str (fs/canonicalize (fs/path (fs/home) "PP" "hive" "hive-mcp"))))

(defn hive-mcp-pid []
  (->> (str/split-lines (or (sh-out "pgrep" "-x" "java") ""))
       (remove str/blank?)
       (filter #(= hive-mcp-dir (str (fs/read-link (str "/proc/" % "/cwd")))))
       (filter #(str/includes? (or (sh-out "cat" (str "/proc/" % "/cmdline")) "") "nrepl"))
       last))

(def pid (or (first (remove #(or (str/starts-with? % "-") (= label %)) args))
             (hive-mcp-pid)
             (do (binding [*out* *err*] (println "hive-mcp JVM not found")) (System/exit 1))))

(defn kb->mb [kb] (some-> kb (quot 1024)))

(defn status-kb [field]
  (some-> (sh-out "cat" (str "/proc/" pid "/status"))
          (lines-starting (str field ":"))
          first tokens second parse-long))

(defn heap-info []
  (when-let [line (some-> (sh-out "jcmd" pid "GC.heap_info") (lines-starting "garbage-first heap") first)]
    (let [t (tokens line)
          after #(get t (inc (.indexOf t %)))]
      {:heap-committed-mb (kb->mb (strip (after "committed") "" "K"))
       :heap-used-mb (kb->mb (strip (after "used") "" "K"))})))

(defn jstat-gc []
  (when-let [out (sh-out "jstat" "-gc" pid)]
    (let [[header row] (map tokens (str/split-lines out))
          m (zipmap header row)
          num #(some-> (m %) parse-double long)]
      {:metaspace-committed-mb (kb->mb (num "MC"))
       :metaspace-used-mb (kb->mb (num "MU"))
       :young-gcs (num "YGC")
       :full-gcs (num "FGC")
       :gc-seconds (some-> (m "GCT") parse-double)})))

(defn histogram-row [line]
  (let [[rank inst bytes cls] (tokens line)]
    (when (and rank (str/ends-with? rank ":") cls)
      {:rank (strip rank "" ":")
       :instances (parse-long inst)
       :mb (quot (parse-long bytes) (* 1024 1024))
       :class cls})))

(defn histogram []
  (when-let [out (sh-out "jcmd" pid "GC.class_histogram")]
    (let [rows (keep histogram-row (str/split-lines out))]
      {:dynamic-class-loaders (some #(when (= "clojure.lang.DynamicClassLoader" (:class %)) (:instances %)) rows)
       :top-classes (vec (take 30 rows))})))

(defn native-memory []
  (when-let [line (some-> (sh-out "jcmd" pid "VM.native_memory" "summary" "scale=MB") (lines-starting "Total:") first)]
    (let [[_ reserved committed] (tokens line)]
      {:nmt-reserved-mb (strip reserved "reserved=" "MB")
       :nmt-committed-mb (strip committed "committed=" "MB")})))

(def sample
  (merge {:at (str (java.time.Instant/now))
          :pid (parse-long pid)
          :label label
          :rss-mb (kb->mb (status-kb "VmRSS"))
          :swap-mb (kb->mb (status-kb "VmSwap"))
          :threads (status-kb "Threads")}
         (heap-info)
         (jstat-gc)
         (native-memory)
         (when-not fast? (histogram))))

(def out-file (fs/path (fs/home) ".local" "share" "hive-mcp" "heap-baseline.edn"))
(fs/create-dirs (fs/parent out-file))
(spit (str out-file) (str (pr-str sample) "\n") :append true)
(prn (dissoc sample :top-classes))

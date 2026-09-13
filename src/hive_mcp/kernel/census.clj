(ns hive-mcp.kernel.census
  "Namespace census for the kernel gate.

   Reads the `ns` form of every .clj/.cljc file under a source root with the
   reader (never grep: docstrings carry require examples), classifies each
   namespace against the kernel allowlist in resources/hive-mcp/kernel.edn,
   and computes the require edges that leave the kernel.

   Contract: every function is pure over the data it is given; the only IO is
   `ns-form` (reads one file) and `load-allowlist` (reads the EDN resource)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Reading

(def ^:private source-file-re #"\.cljc?$")

(defn source-files
  "Every .clj / .cljc file under ROOT (a path string or File), sorted by path."
  [root]
  (->> (file-seq (io/file root))
       (filter #(.isFile ^java.io.File %))
       (filter #(re-find source-file-re (.getName ^java.io.File %)))
       (sort-by #(.getPath ^java.io.File %))))

(defn ns-form
  "The first top-level `(ns ...)` form in FILE, read with :read-cond :allow
   and *read-eval* off; nil when the file has none or cannot be read."
  [file]
  (try
    (with-open [r (java.io.PushbackReader. (io/reader file))]
      (binding [*read-eval* false]
        (loop []
          (let [x (read {:read-cond :allow :eof ::eof} r)]
            (cond (= x ::eof) nil
                  (and (seq? x) (= 'ns (first x))) x
                  :else (recur))))))
    (catch Exception _ nil)))

(defn- libspec-namespaces
  "Namespace symbols named by one :require libspec. Handles a bare symbol,
   a [lib :as a] vector, and a prefix list [prefix lib1 [lib2 :as b]]."
  [spec]
  (cond
    (symbol? spec) [spec]
    (and (vector? spec) (symbol? (first spec)))
    (let [[head & more] spec]
      (if (or (empty? more) (keyword? (first more)))
        [head]
        (mapv (fn [sub]
                (symbol (str head "." (if (vector? sub) (first sub) sub))))
              (remove keyword? more))))
    :else []))

(defn required-namespaces
  "Every namespace symbol NS-FORM requires through :require clauses."
  [ns-form]
  (->> (rest ns-form)
       (filter seq?)
       (filter #(= :require (first %)))
       (mapcat rest)
       (mapcat libspec-namespaces)
       (filter symbol?)
       distinct
       vec))

(defn census
  "Vector of {:ns sym :file path :requires [sym ...]} for every file under
   ROOT that carries an ns form. Example:
   (census \"src/hive_mcp\")"
  [root]
  (->> (source-files root)
       (keep (fn [f]
               (when-let [form (ns-form f)]
                 (when (symbol? (second form))
                   {:ns (second form)
                    :file (.getPath ^java.io.File f)
                    :requires (required-namespaces form)}))))
       vec))

;; ---------------------------------------------------------------------------
;; Allowlist

(def allowlist-resource "hive-mcp/kernel.edn")

(defn load-allowlist
  "The kernel allowlist read from the classpath resource
   `hive-mcp/kernel.edn`."
  []
  (if-let [r (io/resource allowlist-resource)]
    (edn/read-string (slurp r))
    (throw (ex-info "kernel allowlist resource missing" {:resource allowlist-resource}))))

(def ^:private root-prefix "hive-mcp.")

(defn relative-name
  "NS-NAME without the leading `hive-mcp.` root; nil for a foreign namespace."
  [ns-name]
  (let [s (str ns-name)]
    (when (str/starts-with? s root-prefix)
      (subs s (count root-prefix)))))

(defn- prefix-matches?
  "True when REL equals PREFIX or sits below it as a dotted child."
  [prefix rel]
  (or (= prefix rel)
      (str/starts-with? rel (str prefix "."))))

(defn- longest-prefix
  "The longest key of PREFIX-MAP that matches REL, or nil."
  [prefix-map rel]
  (->> (keys prefix-map)
       (filter #(prefix-matches? % rel))
       (sort-by count >)
       first))

(defn segment
  "Top-level segment of NS-NAME under hive-mcp (\"tools\" for
   hive-mcp.tools.core); \"ROOT\" for hive-mcp itself; nil for a foreign ns."
  [ns-name]
  (when-let [rel (relative-name ns-name)]
    (first (str/split rel #"\."))))

(defn classify
  "Classification of NS-NAME under ALLOWLIST:
   {:class :kernel}                     kernel namespace
   {:class :extract :target :hive-x}    leaves the kernel for addon :hive-x
   {:class :unknown}                    hive-mcp namespace nobody claimed
   {:class :foreign}                    not a hive-mcp namespace
   An exact :kernel/tools entry wins; otherwise the longest matching prefix
   across :kernel/segments and :extraction decides."
  [allowlist ns-name]
  (if-let [rel (relative-name ns-name)]
    (let [tools (set (map str (:kernel/tools allowlist)))
          kernel-prefixes (into {} (map (fn [s] [(str s) :kernel])) (:kernel/segments allowlist))
          extraction (into {} (map (fn [[k v]] [(str k) v])) (:extraction allowlist))
          k-hit (longest-prefix kernel-prefixes rel)
          e-hit (longest-prefix extraction rel)]
      (cond
        (contains? tools (str ns-name)) {:class :kernel}
        (and k-hit e-hit) (if (>= (count k-hit) (count e-hit))
                            {:class :kernel}
                            {:class :extract :target (get extraction e-hit)})
        k-hit {:class :kernel}
        e-hit {:class :extract :target (get extraction e-hit)}
        :else {:class :unknown}))
    {:class :foreign}))

(defn classified
  "CENSUS rows each merged with their `classify` result under :class (and
   :target when extracting). Example:
   (classified (load-allowlist) (census \"src/hive_mcp\"))"
  [allowlist rows]
  (mapv #(merge % (classify allowlist (:ns %))) rows))

(defn by-class
  "Map from class keyword to the vector of ns symbols in that class."
  [classified-rows]
  (reduce (fn [m {:keys [class ns]}] (update m class (fnil conj []) ns))
          {}
          classified-rows))

;; ---------------------------------------------------------------------------
;; Edges

(defn kernel-edges
  "Set of {:from kernel-ns :to required-ns :target t} for every require that
   leaves the kernel: a hive-mcp namespace that is not :kernel (:target is
   the addon keyword for :extract, :unknown otherwise), or a foreign
   namespace under a :kernel/forbidden-requires prefix (:target :forbidden)
   unless the requiring namespace sits under one of that prefix's :owners.
   Example:
   (kernel-edges (load-allowlist) (census \"src/hive_mcp\"))"
  [allowlist rows]
  (let [forbidden (into {} (map (fn [[p spec]] [(str p) (mapv str (:owners spec))]))
                        (:kernel/forbidden-requires allowlist))
        forbidden-hit (fn [from req]
                        (when-let [p (longest-prefix forbidden (str req))]
                          (let [rel (relative-name from)]
                            (when-not (some #(prefix-matches? % rel) (get forbidden p))
                              :forbidden))))]
    (into #{}
          (for [{:keys [ns requires]} rows
                :when (= :kernel (:class (classify allowlist ns)))
                req requires
                :let [c (classify allowlist req)
                      target (case (:class c)
                               :extract (:target c)
                               :unknown :unknown
                               :foreign (forbidden-hit ns req)
                               nil)]
                :when target]
            {:from ns :to req :target target}))))

(defn edge-key
  "The {:from :to} pair that identifies EDGE-OR-WAIVER."
  [m]
  {:from (symbol (str (:from m))) :to (symbol (str (:to m)))})

(defn uncovered-edges
  "Edges in EDGES with no waiver of the same {:from :to} in WAIVERS."
  [waivers edges]
  (let [covered (set (map edge-key waivers))]
    (remove #(covered (edge-key %)) edges)))

(defn stale-waivers
  "Waivers in WAIVERS whose {:from :to} matches no edge in EDGES."
  [waivers edges]
  (let [live (set (map edge-key edges))]
    (remove #(live (edge-key %)) waivers)))

(defn segment-matrix
  "Map from source segment to {target-segment require-count} over hive-mcp
   requires between different segments of ROWS."
  [rows]
  (reduce (fn [acc {:keys [ns requires]}]
            (let [fs (segment ns)]
              (reduce (fn [a req]
                        (let [ts (segment req)]
                          (if (and ts (not= fs ts))
                            (update-in a [fs ts] (fnil inc 0))
                            a)))
                      acc
                      requires)))
          {}
          rows))

(defn class-matrix
  "Map from source segment to {target-addon require-count} for every edge
   in `kernel-edges`, grouped by the kernel segment the edge leaves from."
  [edges]
  (reduce (fn [acc {:keys [from target]}]
            (update-in acc [(segment from) target] (fnil inc 0)))
          {}
          edges))

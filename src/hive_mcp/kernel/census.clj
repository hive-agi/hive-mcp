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
            [clojure.string :as str]
            [malli.core :as m]))

;; ---------------------------------------------------------------------------
;; Value schemas
;;
;; The contracts below transcribe the docstrings. Each generator is drawn from
;; namespaces and allowlist shapes the kernel gate really meets, so a property
;; reaches the :kernel, :extract, :unknown and :foreign arms instead of only
;; random symbols that classify as :foreign.

(def NsSym
  "A namespace symbol."
  [:symbol {:gen/elements '[hive-mcp hive-mcp.server.core hive-mcp.server.init hive-mcp.agent
                            hive-mcp.agent.ling hive-mcp.agent.cost.model hive-mcp.hivemind.state
                            hive-mcp.hivemind.fresh hive-mcp.tools.core hive-mcp.tools.agent.status
                            hive-mcp.swarm.datascript.lings hive-mcp.mystery.thing
                            hive-mcp.graph.schema clojure.string datascript.core
                            datascript.core.impl]}])

(def NsName
  "A namespace named by symbol or string; every fn reads it through `str`."
  [:or NsSym [:string {:gen/elements ["hive-mcp.kernel.census" "hive-mcp.agent.x" "malli.core"
                                      "hive-mcp." "hive-mcp.hivemind"]}]])

(def SourceRoot
  "A source root: a path string or a java.io.File. A root that does not exist
   yields no files."
  [:or {:gen/elements ["src/hive_mcp/kernel" "src/hive_mcp/hooks" "no/such/root"]}
   :string
   [:fn {:error/message "a java.io.File"} #(instance? java.io.File %)]])

(def SourceFile
  "A java.io.File naming a .clj or .cljc source."
  [:fn {:error/message "a java.io.File"
        :gen/schema [:enum "src/hive_mcp/kernel/census.clj" "src/hive_mcp/hooks/core.clj"]
        :gen/fmap io/file}
   #(instance? java.io.File %)])

(def NsForm
  "A read `(ns ...)` form: a seq whose head is the symbol ns."
  [:and {:gen/schema [:enum
                      '(ns a.b (:require [x.y :as y] z.w [p q [r :as s]] [t.u :refer [v]])
                           (:import [java.io File]))
                      '(ns hive-mcp.agent.ling "doc"
                           (:require [hive-mcp.server.core :as sc] [hive-mcp.hivemind.state]))
                      '(ns hive-mcp.kernel.census)
                      '(ns hive-mcp.tools.core
                           (:require hive-mcp.agent hive-mcp.mystery.thing [datascript.core :as d]))
                      '(ns hive-mcp.swarm.datascript.lings
                           (:require [datascript.core :as d] [clojure.string :as str]))]}
   seq? [:fn #(= 'ns (first %))]])

(def CensusRow
  "One row of `census`."
  [:map [:ns :symbol] [:file :string] [:requires [:vector :symbol]]])

(def RowIn
  "What the row-reading fns use of a census row: :ns, and :requires when edges
   are computed."
  [:map [:ns NsName] [:requires {:optional true} [:sequential NsName]]])

(def Allowlist
  "resources/hive-mcp/kernel.edn as read. Prefixes may be strings or symbols;
   every fn reads them through `str`."
  [:map
   [:kernel/segments {:optional true}
    [:sequential {:gen/elements [["server" "tools.catchup" "kernel"] ["server" "swarm.datascript"] []]}
     [:or :string :symbol]]]
   [:kernel/tools {:optional true}
    [:sequential {:gen/elements [['hive-mcp.tools.core] [] ['hive-mcp.agent.ling]]} [:or :symbol :string]]]
   [:kernel/forbidden-requires {:optional true}
    [:map-of {:gen/elements [{"datascript.core" {:owners ["swarm.datascript"]}} {}]}
     [:or :string :symbol] [:map [:owners [:sequential [:or :string :symbol]]]]]]
   [:extraction {:optional true}
    [:map-of {:gen/elements [{"agent" :hive-agent "hivemind" :hive-agent "graph" :hive-memory
                              "agent.cost" :hive-cost}
                             {"tools" :hive-tools} {}]}
     [:or :string :symbol] :keyword]]
   [:extraction/listed {:optional true}
    [:map-of {:gen/elements [{:hive-agent '[hive-mcp.agent hive-mcp.agent.ling hive-mcp.hivemind.state
                                            hive-mcp.hivemind.gone]}
                             {}]}
     :keyword [:sequential [:or :symbol :string]]]]
   [:waiver-baseline {:optional true} :int]
   [:waivers {:optional true} [:sequential [:map [:from NsName] [:to NsName]]]]])

(def Classification
  "What `classify` answers."
  [:multi {:dispatch :class}
   [:extract [:map {:closed true} [:class [:= :extract]] [:target :keyword]]]
   [::m/default [:map {:closed true} [:class [:enum :kernel :unknown :foreign]]]]])

(def ClassKw
  "The four classes."
  [:enum :kernel :extract :unknown :foreign])

(def EdgeRef
  "An edge or a waiver as `edge-key` reads it."
  [:map [:from NsName] [:to NsName]])

(def Edge
  "A require that leaves the kernel."
  [:map [:from NsName] [:to NsName] [:target :keyword]])

(def EdgeRefs
  "Edges or waivers: a set (what `kernel-edges` answers) or a sequence."
  [:or [:sequential EdgeRef] [:set EdgeRef]])

(def OwnedNs
  "A listed or unlisted extraction row."
  [:map {:closed true} [:ns NsName] [:target :keyword]])


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

(m/=> source-files [:=> [:cat SourceRoot] [:sequential SourceFile]])

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

(m/=> ns-form [:=> [:cat [:or SourceFile :string]] [:maybe NsForm]])

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

(m/=> required-namespaces [:=> [:cat [:maybe NsForm]] [:vector :symbol]])

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

(m/=> census [:=> [:cat SourceRoot] [:vector CensusRow]])

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

(m/=> load-allowlist [:=> [:cat] Allowlist])

(def ^:private root-prefix "hive-mcp.")

(defn relative-name
  "NS-NAME without the leading `hive-mcp.` root; nil for a foreign namespace."
  [ns-name]
  (let [s (str ns-name)]
    (when (str/starts-with? s root-prefix)
      (subs s (count root-prefix)))))

(m/=> relative-name [:=> [:cat NsName] [:maybe :string]])

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

(m/=> segment [:=> [:cat NsName] [:maybe :string]])

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

(m/=> classify [:=> [:cat Allowlist NsName] Classification])

(defn classified
  "CENSUS rows each merged with their `classify` result under :class (and
   :target when extracting). Example:
   (classified (load-allowlist) (census \"src/hive_mcp\"))"
  [allowlist rows]
  (mapv #(merge % (classify allowlist (:ns %))) rows))

(m/=> classified
      [:=> [:cat Allowlist [:sequential RowIn]]
       [:vector [:map [:ns NsName] [:class ClassKw] [:target {:optional true} :keyword]]]])

(defn by-class
  "Map from class keyword to the vector of ns symbols in that class."
  [classified-rows]
  (reduce (fn [m {:keys [class ns]}] (update m class (fnil conj []) ns))
          {}
          classified-rows))

(m/=> by-class
      [:=> [:cat [:sequential [:map [:class ClassKw] [:ns NsName]]]]
       [:map-of ClassKw [:vector NsName]]])

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

(m/=> kernel-edges [:=> [:cat Allowlist [:sequential RowIn]] [:set Edge]])

(defn edge-key
  "The {:from :to} pair that identifies EDGE-OR-WAIVER."
  [m]
  {:from (symbol (str (:from m))) :to (symbol (str (:to m)))})

(m/=> edge-key [:=> [:cat EdgeRef] [:map {:closed true} [:from :symbol] [:to :symbol]]])

(defn uncovered-edges
  "Edges in EDGES with no waiver of the same {:from :to} in WAIVERS."
  [waivers edges]
  (let [covered (set (map edge-key waivers))]
    (remove #(covered (edge-key %)) edges)))

(m/=> uncovered-edges [:=> [:cat EdgeRefs EdgeRefs] [:sequential EdgeRef]])

(defn stale-waivers
  "Waivers in WAIVERS whose {:from :to} matches no edge in EDGES."
  [waivers edges]
  (let [live (set (map edge-key edges))]
    (remove #(live (edge-key %)) waivers)))

(m/=> stale-waivers [:=> [:cat EdgeRefs EdgeRefs] [:sequential EdgeRef]])

;; ---------------------------------------------------------------------------
;; Frozen extractions
;;
;; An :extraction prefix only says where a namespace is going. For an owner
;; named under :extraction/listed the namespaces that exist today are listed
;; one by one, and a new namespace under that owner's prefixes is refused: new
;; behaviour for that owner is written in the addon, not in core.

(defn listed-extractions
  "Map from owner keyword to the set of namespace symbols ALLOWLIST lists
   under :extraction/listed."
  [allowlist]
  (into {} (map (fn [[owner nss]] [owner (set (map #(symbol (str %)) nss))]))
        (:extraction/listed allowlist)))

(m/=> listed-extractions [:=> [:cat Allowlist] [:map-of :keyword [:set :symbol]]])

(defn unlisted-extractions
  "Rows of ROWS that classify as :extract for an owner under
   :extraction/listed but are not in that owner's list, as {:ns :target}.
   Example:
   (unlisted-extractions (load-allowlist) (census \"src/hive_mcp\"))"
  [allowlist rows]
  (let [listed (listed-extractions allowlist)]
    (vec (for [{:keys [ns]} rows
               :let [{:keys [class target]} (classify allowlist ns)]
               :when (and (= :extract class)
                          (contains? listed target)
                          (not (contains? (get listed target) ns)))]
           {:ns ns :target target}))))

(m/=> unlisted-extractions [:=> [:cat Allowlist [:sequential RowIn]] [:vector OwnedNs]])

(defn stale-listings
  "Entries of :extraction/listed naming a namespace ROWS no longer has, or
   one that no longer classifies as :extract for that owner, as {:ns :target}."
  [allowlist rows]
  (let [present (set (map :ns rows))]
    (vec (for [[owner nss] (listed-extractions allowlist)
               ns (sort nss)
               :when (or (not (contains? present ns))
                         (not= {:class :extract :target owner} (classify allowlist ns)))]
           {:ns ns :target owner}))))

(m/=> stale-listings [:=> [:cat Allowlist [:sequential RowIn]] [:vector OwnedNs]])

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

(m/=> segment-matrix
      [:=> [:cat [:sequential RowIn]] [:map-of [:maybe :string] [:map-of :string pos-int?]]])

(defn class-matrix
  "Map from source segment to {target-addon require-count} for every edge
   in `kernel-edges`, grouped by the kernel segment the edge leaves from."
  [edges]
  (reduce (fn [acc {:keys [from target]}]
            (update-in acc [(segment from) target] (fnil inc 0)))
          {}
          edges))

(m/=> class-matrix
      [:=> [:cat [:or [:sequential Edge] [:set Edge]]] [:map-of [:maybe :string] [:map-of :keyword pos-int?]]])

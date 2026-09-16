;; Census for [CAPTURE-BY-VAR-FLEET] (kanban 20260821211613-0c0e618f), ported
;; from hive-carto's test/hive_carto/runtime/var_seam_test.clj.
;;
;; A dev probe and not a test, deliberately: hive-mcp's number is not zero yet,
;; and a gate that starts red is a gate the suite learns to ignore. This
;; MEASURES, so the conversion can be scoped by what is actually frozen rather
;; than by the 2026-08-22 figure, which was taken in a warm image of a
;; different tree. It becomes test/hive_mcp/runtime/var_seam_test.clj the day
;; both counts reach zero.
;;
;; Cold, per precondition 7 of the card: a warm image missed 43 namespaces
;; holding 50 sites in hive-carto. Run it as its own JVM.
;;
;;   clojure -Sdeps "$(bin/hive-mcp --print-deps)" -J-Xmx3g -M:dev -m var-seam-census
(ns var-seam-census
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; =============================================================================
;; Collect — every hive-mcp namespace on the source path
;; =============================================================================

(def ^:private src-root "src")

(defn- path->ns
  [^java.io.File root ^java.io.File f]
  (let [rel (-> (.toPath root) (.relativize (.toPath f)) str)]
    (when (re-find #"\.cljc?$" rel)
      (-> rel
          (str/replace #"\.cljc?$" "")
          (str/replace "/" ".")
          (str/replace "_" "-")
          symbol))))

(defn source-namespaces []
  (let [root (io/file src-root)]
    (when (.isDirectory root)
      (->> (file-seq root)
           (filter #(.isFile ^java.io.File %))
           (keep #(path->ns root %))
           (filter #(str/starts-with? (str %) "hive-mcp"))
           sort))))

(defn load-all!
  "-> {:loaded n :failures [[ns message] ...]}

   Every failure is reported rather than thrown: a namespace that cannot load
   is a hole in the census, and a hole must be visible. Gates that quantify
   over the image are blind to what never loaded (20260813235938-4a45db9c)."
  []
  (reduce (fn [acc n]
            (try (require n) (update acc :loaded inc)
                 (catch Throwable e
                   (update acc :failures conj
                           [(str n) (str (.getSimpleName (class e)) ": " (ex-message e))]))))
          {:loaded 0 :failures []}
          (source-namespaces)))

(defn- mcp-vars
  "Vars of the PRODUCTION surface only — namespaces that exist under src/.

   The test tree legitimately aliases production fns to reach them, and those
   aliases are not a production reload hazard. Including them would also make
   the verdict depend on whether the suite happened to load the test tree."
  []
  (let [src (set (source-namespaces))]
    (for [n (->> (all-ns) (map ns-name) (filter src))
          [_ v] (ns-interns n)
          :when (and (var? v) (try (bound? v) (catch Throwable _ false)))]
      v)))

(defn- fn-roots
  "fn value -> the hive-mcp var holding it as its root, keyed by IDENTITY.
   An IdentityHashMap, not a map keyed by identityHashCode: that hash is not
   injective over thousands of fn objects, and a collision would attribute a
   handler to a var that never held it."
  ^java.util.IdentityHashMap [vars]
  (let [m (java.util.IdentityHashMap.)]
    (doseq [v vars :let [x @v] :when (fn? x)]
      (.put m x v))
    m))

(defn- identity-classes
  "vars -> the groups of vars whose roots are the SAME object. Buckets by
   identity hash, then splits each bucket with `identical?`: two distinct fns
   can share an identityHashCode, and grouping by the hash alone reported two
   unrelated defns as one frozen alias in hive-carto on 2026-09-11."
  [vars]
  (->> vars
       (group-by (fn [v] (System/identityHashCode @v)))
       vals
       (mapcat (fn [bucket]
                 (reduce (fn [classes v]
                           (if-let [i (first (keep-indexed
                                              (fn [i c] (when (identical? @(first c) @v) i))
                                              classes))]
                             (update classes i conj v)
                             (conj classes [v])))
                         []
                         bucket)))))

;; =============================================================================
;; The two shapes
;; =============================================================================

(defn frozen-aliases
  "Groups of vars sharing ONE fn object: at most one is the definition and the
   rest are frozen copies of it."
  [vars]
  (->> (identity-classes vars)
       (filter #(> (count %) 1))
       (filter #(fn? @(first %)))
       (map (fn [g] (mapv #(symbol (str (symbol %))) g)))))

(defn frozen-dispatch-entries
  "[map-qn key handler-qn] for every map ENTRY that holds a fn value whose root
   lives in a hive-mcp var — the entry a reload of that var cannot reach.

   Nested one level, because the consolidated tools carry subdomain trees:
   `{:kg {:edge f}}` freezes `f` exactly as a flat map would."
  [vars]
  (let [roots (fn-roots vars)
        entry (fn [nm k value]
                (when-let [root (and (fn? value) (.get roots value))]
                  [nm k (symbol (str (symbol root)))]))]
    (for [v vars
          :let [x  @v
                nm (symbol (str (symbol v)))]
          :when (map? x)
          [k value] x
          hit   (if (map? value)
                  (keep (fn [[k2 v2]] (entry nm (str k " " k2) v2)) value)
                  [(entry nm k value)])
          :when hit]
      hit)))

;; =============================================================================
;; Report
;; =============================================================================

(defn- by-map
  "map-qn -> [captured total], ranked by captured — the shape the card's
   `worst offenders` table is written in, so the two are comparable."
  [vars frozen]
  (let [totals (into {} (for [v vars
                              :let [x @v]
                              :when (map? x)]
                          [(symbol (str (symbol v)))
                           (reduce + (for [[_ value] x]
                                       (if (map? value) (count value) 1)))]))]
    (->> (group-by first frozen)
         (map (fn [[nm hits]] [nm (count hits) (get totals nm 0)]))
         (sort-by second >))))

(defn -main [& _]
  (let [{:keys [loaded failures]} (load-all!)
        vars     (vec (mcp-vars))
        aliases  (frozen-aliases vars)
        frozen   (vec (frozen-dispatch-entries vars))]
    (println)
    (println "=== var-seam census, hive-mcp ===")
    (println "namespaces loaded :" loaded "of" (count (source-namespaces)))
    (println "load failures     :" (count failures))
    (doseq [f (take 20 failures)] (println "   " (pr-str f)))
    (println "vars inspected    :" (count vars))
    (println)
    (println "frozen alias-def groups   :" (count aliases))
    (println "frozen dispatch entries   :" (count frozen))
    (println "  across maps             :" (count (distinct (map first frozen))))
    (println)
    (println "--- worst offenders (captured / entries) ---")
    (doseq [[nm captured total] (take 30 (by-map vars frozen))]
      (println (format "  %-64s %3d/%-3d" (str nm) captured total)))
    (println)
    (println "--- frozen alias groups (first 30) ---")
    (doseq [g (take 30 aliases)] (println "  " (str/join " = " g)))
    (println)
    (flush)))

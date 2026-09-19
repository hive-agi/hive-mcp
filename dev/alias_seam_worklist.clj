;; Worklist generator for the [CAPTURE-BY-VAR] alias-def sweep
;; (kanban 20260916144420-19614d8c).
;;
;; `var-seam-census` COUNTS frozen alias groups. It does not say which member of
;; a group is the DEFINITION and which are the frozen copies, and that is the
;; one thing standing between the number and a safe sweep. Deciding it by hand,
;; four hundred times, is where a sweep of this size goes wrong: `(def x a/f)`
;; and `(def x b/f)` look identical in a diff and only one of them is the alias.
;;
;; The fn object already knows. A named fn's CLASS is minted in the namespace
;; that defined it, so the value held by
;;   hive-mcp.tools.memory/handle-mcp-memory-update-tags
;; carries the class `hive_mcp.tools.memory.crud.retrieve$handle_update_tags`,
;; which demunges to the true qn. Nothing is inferred from naming or from
;; require order.
;;
;; Two deliberate refusals, because the card says not to convert what it does
;; not understand:
;;   - a value that is not a NAMED top-level fn (anonymous fn, multimethod,
;;     reified object, a map, a schema, a constant) is skipped. Rewriting
;;     `(def S other/S)` to `#'other/S` would hand a var to something that
;;     wants a schema.
;;   - a fn defined OUTSIDE src/ (clojure.core, a library) is skipped: the
;;     alias is not a hive-mcp reload hazard and `#'` would pin a foreign var.
;;
;; Everything skipped is COUNTED and reported by reason, so the output is a
;; census of its own rather than a filtered list that quietly loses members
;; (20260813235938-4a45db9c).
;;
;;   clojure -Sdeps "$(cat local.deps.edn)" -M:dev -m alias-seam-worklist

(ns alias-seam-worklist
  (:require [clojure.string :as str]
            [var-seam-census :as census]))

(defn- demunge-part [^String s]
  (clojure.lang.Compiler/demunge s))

(defn- defining-qn
  "The qn a named fn was DEFINED at, read off its class name, or nil when the
   class is not a named top-level fn.

   `hive_mcp.a.b$some_fn` -> \"hive-mcp.a.b/some-fn\". The `__1234` suffix a
   closure-carrying fn picks up is stripped; `$fn__`, `$eval` and inner classes
   are refused outright rather than guessed at."
  [f]
  (let [n (.getName (class f))]
    (when (and (str/includes? n "$")
               (not (re-find #"\$fn__" n))
               (not (re-find #"\$eval" n)))
      (let [[ns-part fn-part] (str/split n #"\$" 2)]
        (when (and ns-part fn-part (not (str/includes? fn-part "$")))
          (let [fn-part (str/replace fn-part #"__\d+$" "")]
            (when (seq fn-part)
              (str (demunge-part ns-part) "/" (demunge-part fn-part)))))))))

(defn rows
  "-> {:convert [row ...] :skipped {reason count}}

   A row is {:file :line :alias-ns :alias-name :target} for one `(def alias
   target)` that currently freezes a fn defined elsewhere in src/."
  []
  (let [src-nses (set (map str (census/source-namespaces)))
        skipped  (atom {})
        skip!    (fn [why] (swap! skipped update why (fnil inc 0)) nil)
        convert
        (for [v ((resolve 'var-seam-census/mcp-vars))
              :let [x (try @v (catch Throwable _ ::unbound))]
              :let [row
                    (cond
                      (= x ::unbound)     (skip! :unbound)
                      (not (fn? x))       (skip! :not-a-fn)
                      :else
                      (if-let [target (defining-qn x)]
                        (let [own (str (symbol v))
                              tns (namespace (symbol target))]
                          (cond
                            (= own target)              (skip! :is-the-definition)
                            (not (contains? src-nses tns)) (skip! :defined-outside-src)
                            (nil? (:file (meta v)))     (skip! :no-file-meta)
                            :else
                            {:file       (:file (meta v))
                             :line       (:line (meta v))
                             :alias-ns   (str (ns-name (:ns (meta v))))
                             :alias-name (str (:name (meta v)))
                             :target     target}))
                        (skip! :not-a-named-fn)))]
              :when row]
          row)]
    {:convert (vec (sort-by (juxt :file :line) convert))
     :skipped @skipped}))

(defn -main [& _]
  (let [{:keys [loaded failures]} (census/load-all!)
        {:keys [convert skipped]} (rows)
        by-file (group-by :file convert)]
    (println)
    (println "=== alias-seam worklist, hive-mcp ===")
    (println "namespaces loaded :" loaded "of" (count (census/source-namespaces)))
    (println "load failures     :" (count failures))
    (println "convertible aliases:" (count convert) "across" (count by-file) "files")
    (println)
    (println "--- skipped, by reason (these are NOT convertible) ---")
    (doseq [[why n] (sort-by val > skipped)]
      (println (format "  %-24s %5d" (name why) n)))
    (println)
    (println "--- by file, most first ---")
    (doseq [[file rs] (->> by-file (sort-by (comp count val) >) (take 40))]
      (println (format "  %-60s %3d" file (count rs))))
    (println)
    (println "--- every edit, as (line) alias -> target ---")
    (doseq [[file rs] (sort-by key by-file)]
      (println)
      (println file)
      (doseq [{:keys [line alias-name target]} rs]
        (println (format "  %5d  (def %s #'%s)" line alias-name target))))
    (println)
    (flush)))

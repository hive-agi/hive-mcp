(ns hive-mcp.hot.keep
  "State that must survive a namespace reload, expressed as clj-reload KEEPS.

   clj-reload unloads a namespace (remove-ns) before loading it again, so a
   `defonce` root is recreated like any other def. Its `^:clj-reload/keep`
   metadata stashes a var's value across the pass, but only for forms that
   carry the metadata in source. This namespace derives the same keep entries
   for every `defonce` form and for every `def` whose live root is a resource
   (a reference type, a delay, an executor, a channel, a handle, a thread) and
   writes them into clj-reload's state before the pass, so the source needs
   no annotation.

   Loading a namespace on top of itself (clj-reload's :no-unload) is not an
   alternative: its `:as` aliases and `:refer`s still point at the namespace
   objects of its dependencies from BEFORE those were reloaded, and the ns
   form then throws 'alias already exists' / 'already refers to'. Measured
   2026-09-17 on the live coordinator (hive-mcp.tools.catchup.hierarchy).
   A keep unloads the namespace normally and carries only its state."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; clj-reload internals, by name — hive-hot pins clj-reload 1.0.0
;; =============================================================================

(defn- clj-var
  "A clj-reload var by name, private or not, loading clj-reload first: a cold
   image has not required it, and find-var cannot look inside a namespace that
   is not there yet."
  [sym]
  (require (symbol (namespace sym)))
  (or (find-var sym)
      (throw (ex-info (str "clj-reload internal not found: " sym) {:var sym}))))

(defn- state-atom [] @(clj-var 'clj-reload.core/*state))

(defn- config-var [] (clj-var 'clj-reload.core/*config*))

;; =============================================================================
;; Collect
;; =============================================================================

(defn live-resource?
  "Does the var's ROOT VALUE hold something a reload must not recreate: a
   reference type, a delay, an executor, a channel, a closeable handle, a
   thread. Read off the raw root, never deref'd. A Var is not one: `(def x
   #'other/x)` is a seam that must follow the CURRENT var, and keeping it would
   pin the one from before its namespace was recreated."
  [^clojure.lang.Var v]
  (and (.hasRoot v)
       (let [x (.getRawRoot v)]
         (boolean
          (and (not (var? x))
               (or (instance? clojure.lang.IRef x)
                   (instance? clojure.lang.Volatile x)
                   (instance? clojure.lang.Delay x)
                   (instance? java.util.concurrent.ExecutorService x)
                   (instance? java.util.concurrent.ConcurrentMap x)
                   (instance? java.io.Closeable x)
                   (instance? Thread x)
                   (some-> x class .getName (str/includes? "core.async"))))))))

(defn holds-state?
  "Does `ns-sym` intern a var, public or private, whose root is a live resource?"
  [ns-sym]
  (boolean (some live-resource? (vals (ns-interns ns-sym)))))

(defn top-level-defs
  "[[tag sym] ...] for every top-level `(<tag> <sym> ...)` form in `content`,
   read the way clj-reload reads a file."
  [content]
  (let [rdr       ((clj-var 'clj-reload.util/string-reader) content)
        read-form (clj-var 'clj-reload.util/read-form)]
    (loop [acc []]
      (let [form (read-form rdr)]
        (if (= :clj-reload.util/eof form)
          acc
          (recur (if (and (seq? form) (symbol? (first form)) (symbol? (second form)))
                   (conj acc [(first form) (with-meta (second form) nil)])
                   acc)))))))

;; =============================================================================
;; Promote
;; =============================================================================

(defn keep-plan
  "clj-reload keep entries for one namespace's forms: every `defonce`, and
   every `def` whose live var `resource?` answers true for. `var-of` maps a
   symbol to the namespace's own var, or nil."
  [defs var-of resource?]
  (into {}
        (keep (fn [[tag sym]]
                (cond
                  (= 'defonce tag) [sym {:tag 'defonce}]
                  (and (= 'def tag) (some-> (var-of sym) resource?)) [sym {:tag 'def}])))
        defs))

(defn stuck-namespaces
  "Namespaces clj-reload still has pending to load that are loaded and not
   pending unload: a pass that stopped early, or a :no-unload namespace.
   Loaded on top of themselves they keep dead links, so they unload first."
  [{:keys [to-load to-unload]} loaded]
  (let [unloading (set to-unload)]
    (into [] (filter #(and (contains? loaded %) (not (unloading %)))) to-load)))

(defn dead-links
  "{holder-ns [target-ns ...]}: for every loaded namespace under `prefix`, the
   namespaces it still reaches through a namespace OBJECT that is no longer
   the live one, because a pass recreated it or removed it: through an `:as`
   alias, a `:refer`, or a def whose root is a Var of that namespace (a var
   seam). Such a holder throws on its own ns form when loaded on top of
   itself, and keeps calling code the image no longer tracks."
  [prefix]
  (let [live?   (fn [^clojure.lang.Namespace n] (identical? n (find-ns (ns-name n))))
        dead-of (fn [^clojure.lang.Var v] (let [t (.ns v)] (when-not (live? t) (ns-name t))))]
    (into (sorted-map)
          (keep (fn [^clojure.lang.Namespace n]
                  (when (str/starts-with? (str (ns-name n)) prefix)
                    (let [targets (-> #{}
                                      (into (keep (fn [[_ t]] (when-not (live? t) (ns-name t))))
                                            (ns-aliases n))
                                      (into (keep (fn [[_ v]] (when (var? v) (dead-of v))))
                                            (ns-refers n))
                                      (into (keep (fn [[_ ^clojure.lang.Var v]]
                                                    (when (.hasRoot v)
                                                      (let [x (.getRawRoot v)]
                                                        (when (var? x) (dead-of x))))))
                                            (ns-interns n)))]
                      (when (seq targets) [(ns-name n) (vec (sort targets))])))))
          (all-ns))))

(defn repair-plan
  "What a pass must add to heal `links` (from `dead-links`): every holder
   unloads and loads again; a target that is gone from the image loads again,
   provided clj-reload tracks its file. {:unload [ns ...] :load [ns ...]}"
  [links tracked? present?]
  (let [holders (vec (keys links))
        gone    (into [] (comp (mapcat val) (distinct) (remove present?) (filter tracked?)) links)]
    {:unload holders
     :load   (vec (distinct (concat holders gone)))}))

(defn dependents-closure
  "`nses` plus every present namespace that depends on one of them,
   transitively, per clj-reload's parsed require graph in `state`, minus
   `pinned`. A reloaded namespace is a new object; a dependent left alone
   would hold dead links to it and want the next pass, so the pass takes the
   dependents now, the way clj-reload does for a changed file."
  [state nses present? pinned]
  (let [deps    ((clj-var 'clj-reload.parse/dependees) (:namespaces state))
        closure ((clj-var 'clj-reload.parse/transitive-closure) deps (vec nses))]
    (into [] (comp (distinct) (filter present?) (remove (set pinned))) (concat nses closure))))

;; =============================================================================
;; Boundary
;; =============================================================================

(defn- own-var-of
  [ns-obj]
  (fn [sym]
    (when ns-obj
      (let [v (ns-resolve ns-obj sym)]
        (when (and (var? v) (identical? ns-obj (.ns ^clojure.lang.Var v))) v)))))

(defn- declared-keeps
  "Keep entries `f` declares in source with ^:clj-reload/keep, read by
   clj-reload's own parser; nil when the file does not parse."
  [ns-sym f]
  (let [parsed ((clj-var 'clj-reload.parse/read-file) (io/file f))]
    (when (map? parsed) (get-in parsed [ns-sym :keep]))))

(defn keeps-for
  "Keep entries for a LOADED namespace: what its files declare in source plus
   what its live vars derive. Empty for a namespace that is not loaded: there
   is no state to carry."
  [state ns-sym]
  (let [var-of (own-var-of (find-ns ns-sym))]
    (reduce (fn [acc f]
              (merge acc
                     (declared-keeps ns-sym f)
                     (keep-plan (top-level-defs (slurp f)) var-of live-resource?)))
            {}
            (get-in state [:namespaces ns-sym :ns-files]))))

(defn inject-keeps!
  "SET the keep entries of every loaded, tracked namespace among `nses` in
   clj-reload's state to exactly `keeps-for`. Set, never merged: clj-reload
   carries :keep entries across every later scan, so an entry derived under
   an earlier rule would otherwise outlive the rule and pin a value the
   current rule says to recreate. Returns {ns [sym ...]} for the namespaces
   that keep anything."
  [nses]
  (let [state (state-atom)
        plans (into {}
                    (comp (filter find-ns)
                          (filter #(contains? (:namespaces @state) %))
                          (map (fn [ns] [ns (keeps-for @state ns)])))
                    nses)]
    (swap! state (fn [s] (reduce-kv (fn [s ns p] (assoc-in s [:namespaces ns :keep] p)) s plans)))
    (into {} (keep (fn [[ns p]] (when (seq p) [ns (vec (sort (keys p)))]))) plans)))

(defn release-no-unload!
  "Drop clj-reload's :no-unload set: a kept namespace unloads normally."
  []
  (let [v (config-var)]
    (when (bound? v)
      (alter-var-root v assoc :no-unload #{}))
    nil))

(defn force-unload!
  "Make `nses` unload at the start of the next pass."
  [nses]
  (when (seq nses)
    (swap! (state-atom) update :to-unload
           (fn [pending] (into (vec pending) (remove (set pending)) nses))))
  (vec nses))

(defn force-load!
  "Make `nses` load in the next pass; clj-reload sorts them with the rest."
  [nses]
  (when (seq nses)
    (swap! (state-atom) update :to-load
           (fn [pending] (into (vec pending) (remove (set pending)) nses))))
  (vec nses))

(defn prepare-pass!
  "Before a pass that may unload `nses`: drop :no-unload, queue the repairs the
   image needs (every pending-but-loaded namespace and every holder of a dead
   link unloads first, with their dependents; every referenced namespace that
   is gone loads again), and keep the state of every loaded namespace the
   pass will unload. A holder the interlock pins cannot be repaired and is
   named under :unrepairable.
   Returns {:forced [ns ...] :orphans [ns ...] :dead-links {holder [target ...]}
            :unrepairable [ns ...] :kept {ns [sym ...]}}."
  [prefix nses]
  (release-no-unload!)
  (let [state    @(state-atom)
        loaded   @@(clj-var 'clojure.core/*loaded-libs*)
        present? (fn [ns] (boolean (or (contains? loaded ns) (find-ns ns))))
        pinned   (set (:no-reload (when (bound? (config-var)) @(config-var))))
        links    (dead-links prefix)
        {:keys [unload load]} (repair-plan links (set (keys (:namespaces state))) find-ns)
        seeds    (distinct (concat (stuck-namespaces state loaded) unload))
        forced   (dependents-closure state seeds present? pinned)
        gone     (vec (remove (set unload) load))
        kept     (inject-keeps! (distinct (concat nses forced)))]
    (force-unload! forced)
    (force-load! (distinct (concat forced gone)))
    {:forced       (vec forced)
     :orphans      gone
     :dead-links   links
     :unrepairable (vec (filter pinned (keys links)))
     :kept         kept}))

(defn repair-preview
  "What `prepare-pass!` would queue, without touching anything."
  [prefix]
  (let [state    @(state-atom)
        loaded   @@(clj-var 'clojure.core/*loaded-libs*)
        present? (fn [ns] (boolean (or (contains? loaded ns) (find-ns ns))))
        pinned   (set (:no-reload (when (bound? (config-var)) @(config-var))))
        links    (dead-links prefix)
        {:keys [unload load]} (repair-plan links (set (keys (:namespaces state))) find-ns)
        stuck    (stuck-namespaces state loaded)]
    {:stuck        stuck
     :dead-links   links
     :forced       (dependents-closure state (distinct (concat stuck unload)) present? pinned)
     :orphans      (vec (remove (set unload) load))
     :unrepairable (vec (filter pinned (keys links)))}))

(defn pending?
  "Does clj-reload hold work for a pass that no file change would trigger?"
  []
  (let [s @(state-atom)]
    (boolean (or (seq (:to-load s)) (seq (:to-unload s))))))

(defn run-pending!
  "Drive one clj-reload pass over what is already queued: no baseline, no
   scope, the order is clj-reload's own. Answers the shape hive-hot's
   reloaders answer, so a caller treats both alike."
  []
  (let [start (System/currentTimeMillis)
        res   ((clj-var 'clj-reload.core/reload) {:throw false})]
    (-> (select-keys res [:loaded :unloaded :failed])
        (assoc :success (nil? (:failed res))
               :ms (- (System/currentTimeMillis) start)
               :error (some-> (:exception res) ex-message)
               :pending-pass? true))))

(ns hive-mcp.tools.catchup.hcr-scope-trifecta-test
  "Catchup is HCR scoped: an entry reaches a project's catchup only when it is
   visible from that project's hierarchy (scope tag authoritative, else
   :project-id), whatever branch fetched it.

   Stubbed world (hive-mcp.tools.catchup.hcr-fixture): the fixture hierarchy
   installed through the project-scope / project-tree seams and the stub
   memory store installed through the memory port. Three trifectas:

     hcr-visible-rule  the gate predicate, case by case
     hcr-gate-law      the gate over the fixture entries; gate laws over
                       generated entries and views
     catchup-view      every catchup reader (bundle buckets, query-axioms,
                       query-scoped-entries, query-expiring-entries) per
                       caller; HCR invariants over generated hierarchies

   Mutants are explicit: dropping the scope:global clause, falling back to
   :project-id when a scope tag exists, and skipping the gate on one bundle
   branch must each fail a golden."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.test.stub.hierarchy :as hier]
            [hive-mcp.tools.catchup.axiom-cache :as axc]
            [hive-mcp.tools.catchup.bundle :as bundle]
            [hive-mcp.tools.catchup.hcr-fixture :as fx]
            [hive-mcp.tools.catchup.scope-filter :as sf]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Oracle + mutants
;; =============================================================================

(def ^:private project-prefix "scope:project:")

(defn- oracle-visible?
  "Independent statement of the HCR rule for ENTRY seen from PID under PARENTS."
  [parents pid entry]
  (let [v        (hier/visible-ids parents pid)
        tags     (set (map str (:tags entry)))
        projects (keep #(when (str/starts-with? % project-prefix) (subs % (count project-prefix))) tags)]
    (if (or (contains? tags "scope:global") (seq projects))
      (boolean (or (contains? tags "scope:global") (some v projects)))
      (contains? v (:project-id entry)))))

(def ^:private real-visible? sf/hcr-visible?)
(def ^:private real-gate-branches sf/gate-branches)

(defn- scope-tag? [t] (or (= "scope:global" t) (str/starts-with? t project-prefix)))

(defn- visible-without-global
  "Mutant: the `scope:global` clause dropped."
  [entry scope-tags visible-ids]
  (let [tags (map str (:tags entry))]
    (if (some scope-tag? tags)
      (boolean (some (set scope-tags) tags))
      (contains? (set visible-ids) (:project-id entry)))))

(defn- visible-by-project-id-too
  "Mutant: tag authority inverted, :project-id admits even a foreign-tagged entry."
  [entry scope-tags visible-ids]
  (or (real-visible? entry scope-tags visible-ids)
      (contains? (set visible-ids) (:project-id entry))))

(defn- gate-skipping
  "Mutant of `sf/gate-branches` that leaves branch K ungated."
  [k]
  (fn [view always-gated branches]
    (assoc (real-gate-branches view always-gated branches) k (vec (get branches k)))))

(defn- branch-emptied
  "Mutant of `sf/gate-branches` that drops every row of branch K."
  [k]
  (fn [view always-gated branches]
    (real-gate-branches view always-gated (assoc branches k []))))

;; =============================================================================
;; 1. The gate predicate
;; =============================================================================

(def ^:private a-ids (hier/visible-ids hier/fixture "a"))
(def ^:private a-tags (hier/scope-tags-of a-ids))

(defn- seen-from-a [entry] [entry a-tags a-ids])

(defn visible-probe
  "[entry scope-tags visible-ids] -> {:in args :out (hcr-visible? ...)}."
  [args]
  {:in args :out (apply sf/hcr-visible? args)})

(defn- probe-of [f] (fn [args] {:in args :out (apply f args)}))

(def ^:private gen-pid (gen/elements ["a" "a1" "ws" "b" "far" "global"]))

(def ^:private gen-tag
  (gen/elements ["scope:global" "scope:project:a" "scope:project:a1" "scope:project:ws"
                 "scope:project:b" "scope:project:far" "kanban" "catchup-priority"]))

(def ^:private gen-entry
  (gen/hash-map :tags (gen/vector gen-tag 0 3) :project-id gen-pid))

(def ^:private gen-ids
  (gen/fmap #(conj (set %) "global")
            (gen/vector (gen/elements ["a" "a1" "ws" "b" "far"]) 0 4)))

(def ^:private gen-visible-args
  (gen/let [e gen-entry ids gen-ids] [e (hier/scope-tags-of ids) ids]))

(defn- visible-law?
  "scope:global admits; any other scope tag decides alone; no scope tag ->
   :project-id decides."
  [{[entry scope-tags ids] :in out :out}]
  (let [tags (set (map str (:tags entry)))]
    (= out (cond (contains? tags "scope:global")   true
                 (some scope-tag? tags)            (boolean (some (set scope-tags) tags))
                 :else                             (contains? ids (:project-id entry))))))

(deftrifecta hcr-visible-rule
  hive-mcp.tools.catchup.hcr-scope-trifecta-test/visible-probe
  {:golden-path "test/golden/catchup/hcr-visible.edn"
   :cases {:global-tag-foreign-author          (seen-from-a {:tags ["scope:global"] :project-id "far"})
           :foreign-project-tag                (seen-from-a {:tags ["scope:project:far"] :project-id "far"})
           :sibling-tag-seen-from-a            (seen-from-a {:tags ["scope:project:b"] :project-id "b"})
           :misfiled-tag-wins-over-visible-pid (seen-from-a {:tags ["scope:project:b"] :project-id "a"})
           :foreign-tag-wins-over-global-pid   (seen-from-a {:tags ["scope:project:far"] :project-id "global"})
           :ancestor-tag                       (seen-from-a {:tags ["scope:project:ws"] :project-id "ws"})
           :descendant-tag-foreign-author      (seen-from-a {:tags ["scope:project:a1"] :project-id "far"})
           :untagged-visible-pid               (seen-from-a {:tags [] :project-id "a1"})
           :untagged-global-pid                (seen-from-a {:tags ["note"] :project-id "global"})
           :untagged-foreign-pid               (seen-from-a {:tags [] :project-id "far"})
           :mixed-global-and-foreign           (seen-from-a {:tags ["scope:global" "scope:project:far"] :project-id "far"})
           :no-tags-key-foreign-pid            (seen-from-a {:project-id "b"})}
   :xf :out
   :gen gen-visible-args
   :pred visible-law?
   :num-tests 300
   :mutations [["drop-scope-global-clause" (probe-of visible-without-global)]
               ["project-id-overrides-scope-tag" (probe-of visible-by-project-id-too)]]})

;; =============================================================================
;; 2. The gate over a row set
;; =============================================================================

(defn- view-of
  "HCR view of PID under the fixture hierarchy, from the oracle."
  [pid]
  (if (sf/in-project? pid)
    (let [v (hier/visible-ids hier/fixture pid)]
      (sf/hcr-view pid (disj v "global") (hier/scope-tags-of v)))
    (sf/hcr-view pid nil #{"scope:global"})))

(defn gate-probe
  "[view entries] -> {:in [view entries] :out (hcr-gate view entries)}."
  [[view entries :as in]]
  {:in in :out (sf/hcr-gate view entries)})

(defn- sorted-ids [rows] (vec (sort (map :id rows))))

(def ^:private gen-gate-args
  (gen/let [entries (gen/fmap #(vec (map-indexed (fn [i e] (assoc e :id (str "g" i))) %))
                              (gen/vector gen-entry 0 12))
            ids gen-ids]
    [(sf/hcr-view "a" (disj ids "global") (hier/scope-tags-of ids)) entries]))

(defn- global-tagged? [e] (some #{"scope:global"} (:tags e)))

(defn- gate-laws?
  "Subset of the input, idempotent, keeps every scope:global row, and keeps
   exactly the rows `hcr-visible?` admits."
  [{[view entries] :in out :out}]
  (let [ids (set (map :id out))]
    (and (every? (set entries) out)
         (= out (sf/hcr-gate view out))
         (every? #(contains? ids (:id %)) (filter global-tagged? entries))
         (= ids (set (map :id (filter #(real-visible? % (:scope-tags view) (:visible-ids view))
                                      entries)))))))

(deftrifecta hcr-gate-law
  hive-mcp.tools.catchup.hcr-scope-trifecta-test/gate-probe
  {:golden-path "test/golden/catchup/hcr-gate.edn"
   :cases {:from-a      [(view-of "a") (fx/memory-entries)]
           :from-far    [(view-of "far") (fx/memory-entries)]
           :from-global [(view-of "global") (fx/memory-entries)]}
   :xf #(sorted-ids (:out %))
   :gen gen-gate-args
   :pred gate-laws?
   :num-tests 200
   :mutations [["gate-admits-everything" (fn [[_ es :as in]] {:in in :out (vec es)})]
               ["gate-drops-scope-global" (fn [[v es :as in]]
                                            {:in in :out (vec (remove global-tagged? (sf/hcr-gate v es)))})]]})

;; =============================================================================
;; 3. Every catchup reader, in the stubbed world
;; =============================================================================

(defn- bucket-ids [bundle]
  (into (sorted-map) (keep (fn [[k v]] (when (seq v) [k (sorted-ids v)]))) bundle))

(defn- read-catchup
  "Every catchup reader for PID, as sorted ids. Runs inside a world."
  [pid]
  (fx/reset-caches!)
  {:bundle                      (bucket-ids (bundle/query-catchup-bundle pid {}))
   :query-axioms                (sorted-ids (axc/query-axioms pid))
   :scoped-decisions            (sorted-ids (bundle/query-scoped-entries "decision" nil pid 20))
   :scoped-priority-conventions (sorted-ids (bundle/query-scoped-entries
                                             "convention" ["catchup-priority"] pid 20))
   :expiring                    (sorted-ids (bundle/query-expiring-entries pid 20))})

(defn- catchup-view*
  [[parents entries pid :as in]]
  {:in in :view (fx/with-world parents entries #(read-catchup pid))})

(defn catchup-view
  "[parents entries pid] -> {:in .. :view (every catchup reader's ids)}."
  [in]
  (catchup-view* in))

(defn- under
  "Probe mutant: `catchup-view` with var V's root bound to MUTANT."
  [v mutant]
  (fn [in]
    (let [original @v]
      (try
        (alter-var-root v (constantly mutant))
        (catchup-view* in)
        (finally (alter-var-root v (constantly original)))))))

(defn- fixture-case [pid] [hier/fixture (fx/memory-entries) pid])

(def ^:private gen-world
  "[parents entries caller]: a forest of 2-5 `pbh-*` projects, up to 25
   entries scoped to them, to \"global\" or to the unrelated `pbh-far`, about
   a quarter of them expiring within days; the caller is a project or
   \"global\"."
  (gen/let [n       (gen/choose 2 5)
            picks   (gen/vector (gen/choose -1 4) n)
            specs   (gen/vector (gen/tuple (gen/elements ["axiom" "axiom-candidate" "principle"
                                                          "decision" "convention" "note"])
                                           (gen/choose -1 5)
                                           (gen/vector (gen/elements [:global :p0 :p1 :p2 :p3 :p4 :p5
                                                                      "catchup-priority" "session-summary"
                                                                      "wrap-generated" "axiom"])
                                                       0 3)
                                           (gen/frequency [[3 (gen/return false)] [1 (gen/return true)]]))
                                0 25)
            caller  (gen/choose -1 4)]
    (let [pid     #(str "pbh-" %)
          parents (into {} (map-indexed (fn [i p] [(pid i) (when (< -1 p i) (pid p))]) picks))
          project #(if (< -1 % n) (pid %) "pbh-far")
          tag     #(cond (= :global %) "scope:global"
                         (keyword? %)   (str project-prefix (project (parse-long (subs (name %) 1))))
                         :else          %)
          now     (java.time.Instant/now)
          entries (vec (map-indexed
                        (fn [i [type p tags expiring?]]
                          (cond-> {:id (str "e" i) :type type
                                   :project-id (if (neg? p) "global" (project p))
                                   :tags (vec (distinct (map tag tags)))
                                   :content (str "e" i)
                                   :created (str (.minusSeconds now (* 60 (inc i))))}
                            expiring? (assoc :expires (str (.plusSeconds now (* 3 86400))))))
                        specs))]
      [parents entries (if (neg? caller) "global" (pid (mod caller n)))])))

(defn- catchup-laws?
  "From a project: every id any reader returns is HCR-visible from it, and
   every scope:global catchup-priority convention reaches the bundle. From
   any caller: every scope:global axiom and principle reaches the bundle."
  [{[parents entries pid] :in view :view}]
  (let [by-id     (into {} (map (juxt :id identity)) entries)
        seen      (concat (mapcat val (:bundle view)) (mapcat val (dissoc view :bundle)))
        bundled   (fn [& ks] (set (mapcat #(get-in view [:bundle %]) ks)))
        global    (fn [pred] (set (keep #(when (and (global-tagged? %) (pred %)) (:id %)) entries)))
        typed     (fn [type] #(= type (:type %)))
        prio-conv #(and (= "convention" (:type %)) (some #{"catchup-priority"} (:tags %)))
        project?  (sf/in-project? pid)]
    (and (or (not project?) (every? #(oracle-visible? parents pid (by-id %)) seen))
         (or (not project?) (every? (bundled :priority-conventions) (global prio-conv)))
         (every? (bundled :axioms) (global (typed "axiom")))
         (every? (set (:query-axioms view)) (global (typed "axiom")))
         (every? (bundled :principles :priority-principles) (global (typed "principle"))))))

(deftrifecta catchup-view-is-hcr-scoped
  hive-mcp.tools.catchup.hcr-scope-trifecta-test/catchup-view
  {:golden-path "test/golden/catchup/hcr-catchup-view.edn"
   :cases {:from-a      (fixture-case "a")
           :from-b      (fixture-case "b")
           :from-ws     (fixture-case "ws")
           :from-far    (fixture-case "far")
           :from-global (fixture-case "global")}
   :xf :view
   :gen gen-world
   :pred catchup-laws?
   :num-tests 50
   :mutations [["drop-scope-global-clause"    (under #'sf/hcr-visible? visible-without-global)]
               ["project-id-overrides-tag"    (under #'sf/hcr-visible? visible-by-project-id-too)]
               ["ungated-principles-priority" (under #'sf/gate-branches (gate-skipping :principles-priority))]
               ["ungated-axioms-branch"       (under #'sf/gate-branches (gate-skipping :axioms-global))]
               ["ungated-global-pierce"       (under #'sf/gate-branches (gate-skipping :global))]
               ["no-scope-global-tag-pierce"  (under #'sf/gate-branches (branch-emptied :global-tagged))]
               ["ungated-axiom-cache"         (under #'axc/axiom-view (fn [_] (sf/hcr-view nil nil)))]]})

(deftest the-fixture-hierarchy-is-installed-through-the-seams
  (hier/with-hierarchy hier/fixture
    #(is (hier/consistent? hier/fixture)
         "project-scope and project-tree answer the fixture's oracle")))

(deftest pierced-rows-need-a-visible-scope
  (testing "a project_id=global row tagged for a foreign project stays out"
    (let [view (view-of "a")]
      (is (= [] (sf/hcr-gate view (sf/scope-pierce-entries
                                   [{:id "x" :type "axiom" :project-id "global"
                                     :tags ["scope:project:far"]}]
                                   "a")))))))

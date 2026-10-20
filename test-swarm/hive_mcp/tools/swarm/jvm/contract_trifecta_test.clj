(ns hive-mcp.tools.swarm.jvm.contract-trifecta-test
  "Trifectas for the `hive-mcp.tools.swarm.jvm.*` compat shims, driven by
   their own contracts. The shims delegate to hive-agent, so these run in the
   swarm suite (test-swarm/), where hive-agent is on the classpath. The
   process-table readers are boundaries over `ps` and /proc: they are checked
   against their output schemas on the live host instead of a golden."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.swarm.jvm.classifier :as classifier]
            [hive-mcp.tools.swarm.jvm.memory :as memory]
            [hive-mcp.tools.swarm.jvm.orphan :as orphan]
            [hive-mcp.tools.swarm.jvm.parser :as parser]
            [hive-mcp.tools.swarm.jvm.summary :as summary]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ---------------------------------------------------------------------------
;;; parser
;;; ---------------------------------------------------------------------------

(def ^:private Etime
  "Elapsed times in every ps shape, plus values that do not parse."
  [:or
   [:fn {:gen/fmap (fn [[mm ss]] (format "%02d:%02d" mm ss))
         :gen/schema [:tuple [:int {:min 0 :max 59}] [:int {:min 0 :max 59}]]}
    string?]
   [:fn {:gen/fmap (fn [[hh mm ss]] (format "%02d:%02d:%02d" hh mm ss))
         :gen/schema [:tuple [:int {:min 0 :max 23}] [:int {:min 0 :max 59}] [:int {:min 0 :max 59}]]}
    string?]
   [:fn {:gen/fmap (fn [[dd hh mm ss]] (format "%d-%02d:%02d:%02d" dd hh mm ss))
         :gen/schema [:tuple [:int {:min 0 :max 30}] [:int {:min 0 :max 23}]
                      [:int {:min 0 :max 59}] [:int {:min 0 :max 59}]]}
    string?]
   [:enum "" "invalid" "abc:def" "1:2:3:4:5" nil]])

(defn- etime-minutes
  "Oracle: [[DD-]HH:]MM:SS to minutes, 0 for anything else."
  [s]
  (let [nums (when (string? s)
               (try (mapv #(Integer/parseInt %) (str/split s #"[-:]"))
                    (catch Exception _ nil)))]
    (case (count nums)
      2 (nums 0)
      3 (+ (* 60 (nums 0)) (nums 1))
      4 (+ (* 1440 (nums 0)) (* 60 (nums 1)) (nums 2))
      0)))

(hst/deftrifecta-from-schema parse-etime-contract
  hive-mcp.tools.swarm.jvm.parser/parse-etime-to-minutes
  {:in          Etime
   :out         [:int {:min 0}]
   :rel         (fn [in out] (= out (etime-minutes in)))
   :num-tests   200
   :golden-path "test/golden/tools/swarm/jvm/parse-etime.edn"})

;;; ---------------------------------------------------------------------------
;;; classifier (pure part)
;;; ---------------------------------------------------------------------------

(def ^:private Cmd
  [:enum "java -cp shadow-cljs.jar" "java -jar shadow.cljs.devtools" "java -m hive-mcp.server"
   "clojure -M hive_mcp" "java clojure-mcp" "clj-mcp" "java -m nrepl.cmdline"
   "java leiningen.core.main" "java -jar app.jar" "" "hive-mcp nrepl"])

(hst/deftrifecta-from-schema classify-type-contract
  hive-mcp.tools.swarm.jvm.classifier/classify-type
  {:in          Cmd
   :out         classifier/JvmType
   ;; first pattern in declaration order wins; nothing matched is :other
   :rel         (fn [in out]
                  (let [has? (fn [& subs] (some #(str/includes? in %) subs))]
                    (= out (cond (has? "shadow-cljs" "shadow.cljs") :shadow-cljs
                                 (has? "hive-mcp" "hive_mcp") :hive-mcp
                                 (has? "clojure-mcp" "clj-mcp") :clojure-mcp
                                 (has? "nrepl") :nrepl
                                 (has? "leiningen") :leiningen
                                 :else :other))))
   :n-cases     11
   :golden-path "test/golden/tools/swarm/jvm/classify-type.edn"})

;;; ---------------------------------------------------------------------------
;;; memory (pure part)
;;; ---------------------------------------------------------------------------

(def ^:private ThresholdArgs
  "Readings either side of the 90% warning line and of the threshold, and
   free MB either side of the floor, so every status arm is reached even at
   the small generator sizes the golden cases are drawn at."
  [:cat
   [:map [:percent-used [:enum 10 70 75 79 80 85 99]] [:available-mb [:enum 100 2047 2048 6000]]]
   [:enum 80 90]
   [:enum 512 2048]])

(defn- high? [mem thr min-mb]
  (or (>= (:percent-used mem) thr) (< (:available-mb mem) min-mb)))

(hst/deftrifecta-from-schema memory-high-contract
  hive-mcp.tools.swarm.jvm.memory/memory-high?
  {:in          ThresholdArgs
   :out         :boolean
   :rel         (fn [[mem thr min-mb] out] (= out (high? mem thr min-mb)))
   :num-tests   200
   :golden-path "test/golden/tools/swarm/jvm/memory-high.edn"})

(hst/deftrifecta-from-schema memory-status-contract
  hive-mcp.tools.swarm.jvm.memory/memory-status
  {:in          ThresholdArgs
   :out         memory/MemoryStatus
   :rel         (fn [[mem thr min-mb] out]
                  (= out (cond (< (:percent-used mem) (* thr 0.9)) :healthy
                               (high? mem thr min-mb) :critical
                               :else :warning)))
   :num-tests   200
   :golden-path "test/golden/tools/swarm/jvm/memory-status.edn"})

(hst/deftrifecta-from-schema spawn-recommendation-contract
  hive-mcp.tools.swarm.jvm.memory/spawn-recommendation
  {:in          [:cat [:maybe :boolean] [:maybe :boolean] [:maybe :boolean]]
   :out         memory/Recommendation
   :rel         (fn [[can auto dry] out]
                  (= out (cond can "Safe to spawn new processes"
                               (not auto) "Memory high - consider enabling auto_cleanup"
                               dry "Memory high - set cleanup_dry_run=false to actually kill orphans"
                               :else "Capacity reached - wait for running tasks to complete")))
   :n-cases     12
   :golden-path "test/golden/tools/swarm/jvm/spawn-recommendation.edn"})

;;; ---------------------------------------------------------------------------
;;; orphan
;;; ---------------------------------------------------------------------------

(def ^:private TypeKw [:enum :nrepl :shadow-cljs :leiningen :other])

(hst/deftrifecta-from-schema protected-type-contract
  hive-mcp.tools.swarm.jvm.orphan/protected-type?
  {:in          [:cat [:map [:type TypeKw]] [:set [:enum "nrepl" "shadow-cljs" "leiningen"]]]
   :out         :boolean
   :rel         (fn [[proc types] out] (= out (contains? types (name (:type proc)))))
   :golden-path "test/golden/tools/swarm/jvm/protected-type.edn"})

(hst/deftrifecta-from-schema age-orphan-contract
  hive-mcp.tools.swarm.jvm.orphan/age-orphan?
  {:in          [:cat [:map [:etime Etime]] [:int {:min 0 :max 3000}]]
   :out         :boolean
   :rel         (fn [[proc min-age] out] (= out (>= (etime-minutes (:etime proc)) min-age)))
   :num-tests   200
   :golden-path "test/golden/tools/swarm/jvm/age-orphan.edn"})

(def ^:private Parents
  {"100" {:ppid "50" :comm "claude"}
   "50"  {:ppid "1" :comm "bash"}
   "1"   {:ppid "0" :comm "init"}})

(hst/deftrifecta-from-schema enrich-with-parent-info-contract
  hive-mcp.tools.swarm.jvm.orphan/enrich-with-parent-info
  {:in          [:cat [:map [:pid [:enum "7" "8"]] [:ppid [:enum "100" "50" "1" "9999"]]]
                 [:enum Parents {}]]
   :out         orphan/EnrichedProcess
   :rel         (fn [[proc parents] out]
                  (let [parent (get parents (:ppid proc))]
                    (and (= (:pid proc) (:pid out))
                         (= (boolean parent) (:parent-alive out))
                         (= (:comm parent) (:parent-comm out))
                         (= (= "claude" (:comm parent)) (:parent-is-claude out))
                         (= (or (nil? parent) (= "1" (:ppid proc))) (:truly-orphaned out)))))
   :golden-path "test/golden/tools/swarm/jvm/enrich-with-parent-info.edn"})

(hst/deftrifecta-from-schema truly-orphaned-contract
  hive-mcp.tools.swarm.jvm.orphan/truly-orphaned?
  {:in          [:map [:truly-orphaned {:optional true} [:maybe :boolean]]]
   :out         [:maybe :boolean]
   :rel         (fn [in out] (= out (:truly-orphaned in)))
   :golden-path "test/golden/tools/swarm/jvm/truly-orphaned.edn"})

(def ^:private Detectable
  [:map
   [:type TypeKw]
   [:truly-orphaned :boolean]
   [:parent-is-claude :boolean]
   [:parent-comm [:enum "bash" "claude" nil]]
   [:ppid [:enum "100" "1"]]
   [:etime [:enum "05:00" "45:00" "02:00:00"]]])

(def ^:private Opts
  "Options as pairs, always naming every key so the oracle reads them."
  [:cat
   [:= :protected-types] [:set [:enum "shadow-cljs" "leiningen"]]
   [:= :true-orphans-only] :boolean
   [:= :min-age-minutes] [:enum 0 30 60]])

(defn- verdict [proc types true-only min-age]
  (cond (contains? types (name (:type proc))) false
        true-only (:truly-orphaned proc)
        :else (and (:truly-orphaned proc) (>= (etime-minutes (:etime proc)) min-age))))

(defn- reason [proc types]
  (cond (contains? types (name (:type proc))) "protected-type"
        (:truly-orphaned proc) (str "truly-orphaned (parent: " (or (:parent-comm proc) "dead") ")")
        (:parent-is-claude proc) (str "managed-by-claude (ppid: " (:ppid proc) ")")
        :else (str "has-parent: " (:parent-comm proc))))

(hst/deftrifecta-from-schema identify-orphan-contract
  hive-mcp.tools.swarm.jvm.orphan/identify-orphan
  {:in          (into [:cat Detectable] (rest Opts))
   :out         orphan/IdentifiedProcess
   :rel         (fn [[proc _ types _ true-only _ min-age] out]
                  (and (= (verdict proc types true-only min-age) (:orphan out))
                       (= (etime-minutes (:etime proc)) (:age-minutes out))
                       (= (reason proc types) (:reason out))
                       (= (:type proc) (:type out))))
   :num-tests   200
   :golden-path "test/golden/tools/swarm/jvm/identify-orphan.edn"})

(hst/deftrifecta-from-schema identify-orphans-contract
  hive-mcp.tools.swarm.jvm.orphan/identify-orphans
  {:in          (into [:cat [:vector {:max 4} Detectable]] (rest Opts))
   :out         [:sequential orphan/IdentifiedProcess]
   :rel         (fn [[procs _ types _ true-only _ min-age] out]
                  (= (map #(verdict % types true-only min-age) procs) (map :orphan out)))
   :golden-path "test/golden/tools/swarm/jvm/identify-orphans.edn"})

(deftest orphan-detector-arms-test
  (testing "the built detector applies the same verdict as identify-orphan"
    (let [procs [{:type :nrepl :truly-orphaned true :etime "05:00"}
                 {:type :nrepl :truly-orphaned true :etime "45:00"}
                 {:type :nrepl :truly-orphaned false :etime "45:00"}
                 {:type :shadow-cljs :truly-orphaned true :etime "45:00"}]]
      (is (= [true true false false]
             (map (orphan/orphan-detector :protected-types #{"shadow-cljs"}) procs)))
      (is (= [false true false false]
             (map (orphan/orphan-detector {:protected-types #{"shadow-cljs"}
                                           :true-orphans-only false :min-age-minutes 30})
                  procs)))
      (is (= [true true false true] (map (orphan/orphan-detector) procs)) "defaults")))
  (testing "a never-enriched process has a nil verdict, which the contract allows"
    (is (nil? ((orphan/orphan-detector) {:type :nrepl})))))

;;; ---------------------------------------------------------------------------
;;; summary
;;; ---------------------------------------------------------------------------

(def ^:private SummaryProc
  [:map
   [:pid [:enum "1" "2" "3"]]
   [:type TypeKw]
   [:swarm-spawned :boolean]
   [:swarm-slave-id [:enum "s1" nil]]
   [:orphan :boolean]
   [:truly-orphaned :boolean]
   [:parent-is-claude :boolean]])

(hst/deftrifecta-from-schema build-cleanup-summary-contract
  hive-mcp.tools.swarm.jvm.summary/build-cleanup-summary
  {:in          [:cat [:vector {:max 4} :map] [:vector {:max 4} SummaryProc] [:vector {:max 4} SummaryProc]
                 [:vector {:max 3} [:map [:pid [:enum "1" "2"]]]] [:maybe [:vector [:enum "1" "2"]]]
                 [:map [:dry-run :boolean] [:swarm-only :boolean] [:min-age [:enum 0 30]]
                  [:true-orphans-only :boolean]]]
   :out         summary/CleanupSummary
   :rel         (fn [[all classified all-cl orphans killed {:keys [dry-run true-orphans-only min-age]}] out]
                  (and (= (count all) (:total-jvm-processes out))
                       (= (count orphans) (:orphans-found out))
                       (= (map :pid orphans) (:orphan-pids out))
                       (= (if dry-run [] (or killed [])) (:killed out))
                       (= min-age (:min-age-threshold out))
                       (= (if true-orphans-only "true-orphans" "age-based")
                          (get-in out [:orphan-detection :mode]))
                       (= (count (filter :swarm-spawned classified))
                          (get-in out [:swarm :total-swarm-spawned]))
                       (= (frequencies (map (comp name :type) classified)) (:by-type out))
                       (= (count all-cl) (count (:details out)))))
   :golden-path "test/golden/tools/swarm/jvm/build-cleanup-summary.edn"})

(def ^:private Mem [:map [:percent-used [:int {:min 0 :max 100}]] [:available-mb [:int {:min 0 :max 4096}]]])

(hst/deftrifecta-from-schema build-resource-guard-summary-contract
  hive-mcp.tools.swarm.jvm.summary/build-resource-guard-summary
  {:in          [:cat :boolean Mem Mem [:enum 80] [:enum 2048] :boolean :boolean
                 [:map [:auto-clean :boolean] [:cleanup-dry :boolean]]
                 [:maybe [:map [:orphans-found [:int {:min 0 :max 3}]] [:killed [:vector {:max 3} [:enum "1" "2"]]]]]]
   :out         summary/GuardSummary
   :rel         (fn [[can init final _ _ ih fh {:keys [auto-clean cleanup-dry]} cd] out]
                  (and (= can (:can-spawn out))
                       (= init (get-in out [:memory :initial]))
                       (= final (get-in out [:memory :final]))
                       (= (cond (not ih) :healthy (not fh) :recovered-after-cleanup :else :capacity-reached)
                          (:status out))
                       (= (when cd {:ran true :dry-run cleanup-dry :orphans-found (:orphans-found cd)
                                    :killed (count (:killed cd))})
                          (:cleanup out))
                       (= (memory/spawn-recommendation can auto-clean cleanup-dry) (:recommendation out))))
   :golden-path "test/golden/tools/swarm/jvm/build-resource-guard-summary.edn"})

;;; ---------------------------------------------------------------------------
;;; Boundaries: the live process table and /proc
;;; ---------------------------------------------------------------------------

(defn- out-schema [ns-sym fn-sym]
  (-> (m/function-schemas) (get-in [ns-sym fn-sym :schema]) m/schema m/children second))

(deftest process-table-readers-answer-their-schemas-test
  (let [procs   (classifier/find-jvm-processes)
        parents (classifier/get-all-process-parents)
        disc    (classifier/discover-and-classify)]
    (is (m/validate (out-schema 'hive-mcp.tools.swarm.jvm.classifier 'find-jvm-processes) procs))
    (is (seq procs) "this test JVM is itself a java process")
    (is (m/validate (out-schema 'hive-mcp.tools.swarm.jvm.classifier 'get-all-process-parents) parents))
    (is (m/validate (out-schema 'hive-mcp.tools.swarm.jvm.classifier 'discover-and-classify) disc))
    (testing "classify-jvm-process assocs type and swarm flag over the row"
      (let [p (first procs) c (classifier/classify-jvm-process p)]
        (is (= (:pid p) (:pid c)))
        (is (= (classifier/classify-type (:cmd p)) (:type c)))
        (is (boolean? (:swarm-spawned c)))))
    (testing "a pid with no /proc entry has no swarm info"
      (is (nil? (classifier/get-process-swarm-info "999999999")))
      (is (nil? (classifier/get-process-swarm-info nil))))))

(deftest memory-usage-answers-its-schema-test
  (let [mi (memory/get-memory-usage)]
    (is (m/validate (out-schema 'hive-mcp.tools.swarm.jvm.memory 'get-memory-usage) mi))
    (testing "on a Linux host the reading arm, not the error arm"
      (is (m/validate memory/MemInfo mi))
      (is (not (m/validate memory/MemError mi)))
      ;; each MB figure is its own kB value quot 1024, so the sum can be one short
      (is (<= 0 (- (:total-mb mi) (+ (:used-mb mi) (:available-mb mi))) 1)
          "used is total minus available")
      (is (<= 0 (:percent-used mi) 100)))))

(deftest parser-multimethod-aliases-test
  (is (= {:pid "1" :cpu "2" :mem "3" :etime "4" :cmd "java x"}
         (parser/parse-process-line "1 2 3 4 java x")))
  (is (= {:pid "1" :ppid "9" :cpu "2" :mem "3" :etime "4" :cmd "java x"}
         (parser/parse-process-line-extended "1 9 2 3 4 java x"))))

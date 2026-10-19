(ns hive-mcp.crystal.contract-facets-test
  "Schema-derived facets for the contracted crystal namespaces other than
   core (see core-contract-test): fanout, synthesis, recall, pipeline and
   persist. Pure fns get trifectas drawn from their contract schemas; the
   store boundary (persist-wraps!) runs against the stub IMemoryStore
   installed through the registry port, never with-redefs."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.crystal.fanout :as fan]
            [hive-mcp.crystal.harvest.by-scope :as bs]
            [hive-mcp.crystal.persist :as persist]
            [hive-mcp.crystal.pipeline :as pl]
            [hive-mcp.crystal.recall :as recall]
            [hive-mcp.crystal.synthesis :as syn]
            [hive-mcp.test.stub.memory-store :as stub]
            [hive-schemas.test :as hst]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Pid [:enum "hive" "funeraria" "sisf-crm"])

;; -----------------------------------------------------------------------------
;; fanout
;; -----------------------------------------------------------------------------

(hst/deftrifecta-from-schema with-scope-tag-contract
  hive-mcp.crystal.fanout/with-scope-tag
  {:in  [:cat [:map [:tags [:vector [:enum "session-summary" "scope:project:hive" "scope:multi-project"]]]]
         [:or [:= :umbrella] Pid]]
   :out [:map [:tags [:vector :string]]]
   :rel (fn [[entry pid] out]
          (let [tag (fan/scope-tag-for pid)]
            (and (some #{tag} (:tags out))
                 (if (some #{tag} (:tags entry))
                   (= entry out)
                   (= (:tags out) (into [tag] (:tags entry)))))))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/crystal/with-scope-tag-contract.edn"})

(hst/deftrifecta-from-schema slice->harvested-contract
  hive-mcp.crystal.fanout/slice->harvested
  {:in  [:cat Pid bs/ScopeSlice [:map [:session [:enum "s1"]]]]
   :out fan/SliceHarvested
   :rel (fn [[pid slice _] out]
          (and (= pid (:project-id out))
               (= (count (:git-commits slice)) (get-in out [:summary :commit-count]))
               (= (count (:kg-edges-created slice)) (get-in out [:kg-edges-created :count]))
               (= (count (:progress-notes slice)) (count (:progress-notes out)))))
   :num-tests 100
   :golden-path "test/golden/hive-mcp/crystal/slice-harvested-contract.edn"})

(deftest synthesize-wraps-arms
  (testing "an empty HarvestByScope emits nothing"
    (is (= [] (fan/synthesize-wraps (bs/empty-by-scope)))))
  (testing "one entry per non-empty scope plus an umbrella, each scope-tagged"
    (let [hbs (-> (bs/empty-by-scope)
                  (bs/assoc-scope "hive" (assoc bs/empty-scope-slice :git-commits ["a1 fix"]))
                  (bs/assoc-scope "funeraria" bs/empty-scope-slice)
                  (bs/merge-umbrella {:cross-pid-edges [{:e 1}]}))
          out (fan/synthesize-wraps hbs)]
      (is (= ["hive" :umbrella] (mapv :pid out)))
      (is (= ["scope:project:hive" "scope:multi-project"]
             (mapv (comp first :tags :entry) out))))))

;; -----------------------------------------------------------------------------
;; synthesis
;; -----------------------------------------------------------------------------

(def ^:private TaggedEntry
  [:map [:tags [:vector [:enum "scope:project:hive" "scope:project:funeraria" "note" "scope:global"]]]])

(def ^:private HarvestedIn
  [:map
   [:progress-notes [:vector TaggedEntry]]
   [:completed-tasks [:vector TaggedEntry]]
   [:memory-ids-created [:vector TaggedEntry]]
   [:memory-ids-accessed [:vector [:or :string TaggedEntry]]]])

(hst/deftrifecta-from-schema extract-project-scopes-contract
  hive-mcp.crystal.synthesis/extract-project-scopes
  {:in  HarvestedIn
   :out [:set :string]
   ;; An entry contributes only its FIRST scope:project:* tag (entry-project-id
   ;; uses `some`), so the oracle keeps the first match per entry.
   :rel (fn [h out]
          (= out (set (for [k [:progress-notes :completed-tasks :memory-ids-created :memory-ids-accessed]
                            e (get h k) :when (map? e)
                            :let [t (first (filter #(.startsWith ^String % "scope:project:") (:tags e)))]
                            :when t]
                        (subs t 14)))))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/crystal/extract-project-scopes-contract.edn"})

(hst/deftrifecta-from-schema group-harvested-by-scope-contract
  hive-mcp.crystal.synthesis/group-harvested-by-scope
  {:in  [:cat HarvestedIn [:enum "fallback"]]
   :out [:map-of :string :map]
   :rel (fn [[h fb] out]
          (let [n (fn [m] (reduce + (map #(count (filter map? (get m %)))
                                         [:progress-notes :completed-tasks :memory-ids-created :memory-ids-accessed])))]
            ;; every map datum lands in exactly one bucket
            (= (n h) (reduce + 0 (map n (vals out))))))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/crystal/group-harvested-by-scope-contract.edn"})

(hst/deftrifecta-from-schema minimal-wrap-summary-contract
  hive-mcp.crystal.synthesis/minimal-wrap-summary
  {:in  [:cat [:enum "2026-10-16" "s-x"]
         [:map [:summary [:map [:progress-count [:int {:min 0 :max 9}]]
                          [:commit-count [:int {:min 0 :max 9}]]]]]]
   :out [:map [:content :string] [:duration [:= :short]]]
   :rel (fn [[sid h] out]
          (and (.contains ^String (:content out) (str "## Session Summary: " sid))
               (.contains ^String (:content out) (str "- Commits: " (get-in h [:summary :commit-count])))
               (.contains ^String (:content out) (str "- Progress notes: " (get-in h [:summary :progress-count])))))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/crystal/minimal-wrap-summary-contract.edn"})

(deftest format-temporal-block-arms
  (is (= "\n\n### Temporal Metadata\n- Session start: unknown\n- Session end: unknown\n- Duration: 0 minutes"
         (syn/format-temporal-block nil nil)))
  (is (.endsWith ^String (syn/format-temporal-block {:duration-minutes 5}
                                                    {:memory-ids-created [1 2] :memory-ids-accessed [3]})
                 "- Memory entries created: 2\n- Memory entries accessed: 1")))

;; -----------------------------------------------------------------------------
;; recall
;; -----------------------------------------------------------------------------

(hst/deftrifecta-from-schema partition-by-project-contract
  hive-mcp.crystal.recall/partition-by-project
  {:in  [:cat [:vector recall/CreatedEntryS] [:enum "proj-a" "proj-b"]]
   :out [:tuple [:vector recall/CreatedEntryS] [:vector recall/CreatedEntryS]]
   :rel (fn [[entries pid] [matched retained]]
          (and (= (count entries) (+ (count matched) (count retained)))
               (every? #(or (= :entry/unscoped (:adt/variant %)) (= pid (:project-id %))) matched)
               (every? #(and (= :entry/scoped (:adt/variant %)) (not= pid (:project-id %))) retained)))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/crystal/partition-by-project-contract.edn"})

(deftest created-entry-arms-are-disjoint
  (let [scoped   (recall/created-entry :entry/scoped {:id "a" :timestamp "t" :project-id "p"})
        unscoped (recall/created-entry :entry/unscoped {:id "b" :timestamp "t"})
        [_ _ scoped-arm unscoped-arm] (m/form recall/CreatedEntryS)]
    (is (m/validate (last scoped-arm) scoped))
    (is (not (m/validate (last unscoped-arm) scoped)))
    (is (m/validate (last unscoped-arm) unscoped))
    (is (not (m/validate (last scoped-arm) unscoped)))))

(deftest crosses-boundary-arms
  (is (nil? (recall/crosses-project-boundary? "a" ["agent:x"])) "no scope tag: nil")
  (is (false? (recall/crosses-project-boundary? "a" ["scope:project:a"])))
  (is (true? (recall/crosses-project-boundary? "a" ["scope:project:b"])))
  (is (nil? (recall/crosses-session-boundary? "s1" [])))
  (is (true? (recall/crosses-session-boundary? "s1" ["session:s2"]))))

;; -----------------------------------------------------------------------------
;; pipeline
;; -----------------------------------------------------------------------------

(deftest pipeline-outcome-arms
  (let [ok  (pl/harvest-one (pl/make-source :git (fn [_] {:commits ["a"]})) {})
        err (pl/harvest-one (pl/make-source :git (fn [_] (throw (Exception. "boom")))) {})
        [_ _ ok-arm _ err-arm] (m/form pl/Outcome)]
    (is (m/validate (last ok-arm) ok))
    (is (not (m/validate (last err-arm) ok)))
    (is (m/validate (last err-arm) err))
    (is (not (m/validate (last ok-arm) err))))
  (testing "merge-outcomes: err on empty, ok otherwise"
    (is (= :pipeline/no-sources (:error (pl/merge-outcomes []))))
    (is (= {:git {:commits ["a"]}}
           (:ok (pl/merge-outcomes [(pl/harvest-one (pl/make-source :git (fn [_] {:commits ["a"]})) {})]))))))

;; -----------------------------------------------------------------------------
;; persist (store boundary through the stub port)
;; -----------------------------------------------------------------------------

(deftest pid->project-id-arms
  (is (= "multi-project" (persist/pid->project-id :umbrella)))
  (is (= "hive" (persist/pid->project-id "hive")))
  (is (= "" (persist/pid->project-id nil))))

(deftest persist-wraps-through-stub-store
  (stub/with-stub-store
    (fn []
      (let [r (persist/persist-wraps! [{:pid "hive" :entry {:type :note :content "c" :tags ["scope:project:hive"]}}
                                       {:pid :umbrella :entry {:type :note :content "u" :tags []}}])]
        (is (m/validate persist/PersistReport r))
        (is (= 2 (:total r)))
        (is (= ["hive" "multi-project"] (mapv :project-id (:results r))))))))

(deftest persist-wraps-without-store
  (stub/with-no-store
    (fn []
      (let [r (persist/persist-wraps! [{:pid "hive" :entry {}}])]
        (is (= {:total 1 :persisted 0 :failed 1 :results [] :error "IMemoryStore not registered"} r))))))

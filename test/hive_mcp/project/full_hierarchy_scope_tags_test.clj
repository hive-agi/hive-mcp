(ns hive-mcp.project.full-hierarchy-scope-tags-test
  "Trifecta for `hive-mcp.project.scope/full-hierarchy-scope-tags`: self +
   ancestors + descendants as `scope:project:*` tags, `#{\"scope:global\"}` for
   nil \"global\" \"\" scopes. Run over the fixture hierarchy
   `hive-mcp.test.stub.hierarchy` installs through the hierarchy's own seams
   (registered configs + project-tree cache); nothing is redefined.

     golden    : tag set per project of the fixture
     property  : equals the pure oracle for any generated parent map
     mutation  : dropping the ancestors, the descendants or global is caught"
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.project.scope :as scope]
            [hive-mcp.test.stub.hierarchy :as hier]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- expected [parents pid]
  (conj (hier/scope-tags-of (hier/visible-ids parents pid)) "scope:global"))

(deftest-golden full-hierarchy-scope-tags-golden
  "test/golden/hive-mcp/project/full-hierarchy-scope-tags.edn"
  (hier/with-hierarchy hier/fixture
    (fn []
      (into (sorted-map)
            (for [pid [nil "" "global" "ws" "a" "a1" "b" "far"]]
              [(str (pr-str pid)) (vec (sort (scope/full-hierarchy-scope-tags pid)))])))))

(deftest every-fixture-project-matches-the-oracle
  (hier/with-hierarchy hier/fixture
    (fn []
      (doseq [pid (keys hier/fixture)]
        (testing pid
          (is (= (expected hier/fixture pid) (scope/full-hierarchy-scope-tags pid))))))))

(deftest nil-and-global-scopes-see-only-global
  (doseq [s [nil "" "global"]]
    (is (= #{"scope:global"} (scope/full-hierarchy-scope-tags s)))))

(def ^:private gen-parents
  "A forest over p0..p5: each project's parent is an earlier project or nil,
   so the map is acyclic."
  (gen/bind (gen/choose 1 6)
            (fn [n]
              (gen/fmap (fn [picks]
                          (into {} (map-indexed
                                    (fn [i pick]
                                      [(str "p" i) (when (and (pos? i) (<= pick (dec i))) (str "p" pick))])
                                    picks)))
                        (gen/vector (gen/choose 0 6) n)))))

(defspec matches-the-oracle-for-any-forest 40
  (prop/for-all [parents gen-parents]
    (hier/with-hierarchy parents
      (fn []
        (every? (fn [pid] (= (expected parents pid) (scope/full-hierarchy-scope-tags pid)))
                (keys parents))))))

(defspec always-contains-the-scope-itself 40
  (prop/for-all [parents gen-parents]
    (hier/with-hierarchy parents
      (fn []
        (every? (fn [pid] (contains? (scope/full-hierarchy-scope-tags pid) (hier/scope-tag pid)))
                (keys parents))))))

(hmut/deftest-mutations full-hierarchy-scope-tags-mutations
  hive-mcp.project.scope/full-hierarchy-scope-tags
  [["visible only (no descendants)" (fn [s] (scope/visible-scope-tags s))]
   ["descendants only"              (fn [s] (or (scope/descendant-scope-tags s) #{"scope:global"}))]
   ["no global for nil"             (fn [s] (set/difference (scope/visible-scope-tags s) #{"scope:global"}))]
   ["self only"                     (fn [s] #{(scope/scope->tag s)})]]
  (fn []
    (hier/with-hierarchy hier/fixture
      (fn []
        (is (= (expected hier/fixture "a") (scope/full-hierarchy-scope-tags "a")))
        (is (= (expected hier/fixture "ws") (scope/full-hierarchy-scope-tags "ws")))
        (is (= #{"scope:global"} (scope/full-hierarchy-scope-tags nil)))))))

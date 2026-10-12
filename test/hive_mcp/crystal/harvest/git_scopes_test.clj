(ns hive-mcp.crystal.harvest.git-scopes-test
  "Trifecta tests for the HCR-aware commit harvest (kanban
   20260516114613-10aeb0be): an umbrella wrap must count the commits of
   descendant projects that live in their own git repositories."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.crystal.harvest.git-scopes :as gs]
            [hive-test.trifecta :refer [deftrifecta]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- ent [id git-root]
  {:project/id id :project/path (str "/p/" id) :project/git-root git-root})

;; =============================================================================
;; descendant-repos
;; =============================================================================

(defn- repos-of
  "One-argument wrapper: {:root r :entities es} -> descendant-repos."
  [{:keys [root entities]}]
  (gs/descendant-repos root entities))

(defn- repos-ok?
  "Every kept repo differs from the root, and no repository appears twice."
  [{:keys [root entities]} out]
  (and (every? #(not= root (:dir %)) out)
       (= (count out) (count (distinct (map :dir out))))
       (every? (fn [{:keys [pid dir]}]
                 (some #(and (= pid (:project/id %)) (= dir (:project/git-root %))) entities))
               out)))

(def ^:private gen-repos-args
  (gen/let [root (gen/elements ["/u" "/a" nil])
            ents (gen/vector (gen/let [id (gen/elements ["a" "b" "c" "d"])
                                       gr (gen/elements ["/u" "/a" "/b" nil])]
                               (ent id gr))
                             0 6)]
    {:root root :entities ents}))

(deftrifecta descendant-repos-selection
  repos-of
  {:golden-path "test/golden/hive-mcp/crystal/harvest/git-scopes-repos.edn"
   :cases       {:no-descendants   {:root "/u" :entities []}
                 :child-own-repo   {:root "/u" :entities [(ent "sealed" "/u/sealed")]}
                 :child-same-repo  {:root "/u" :entities [(ent "inner" "/u")]}
                 :no-git-root      {:root "/u" :entities [(ent "plain" nil)]}
                 :shared-repo-once {:root "/u" :entities [(ent "x" "/r") (ent "y" "/r")]}
                 :root-not-in-git  {:root nil :entities [(ent "z" "/z")]}}
   :gen         gen-repos-args
   :pred        (fn [out] (vector? out))
   :mutations   [["keeps-umbrella-repo" (fn [{:keys [entities]}]
                                          (vec (keep (fn [e] (when (:project/git-root e)
                                                               {:pid (:project/id e) :dir (:project/git-root e)}))
                                                     entities)))]
                 ["drops-everything"    (fn [_] [])]]})

(deftest descendant-repos-invariants
  (testing "kept repos never repeat the umbrella's repository nor each other"
    (doseq [args (gen/sample gen-repos-args 200)]
      (is (repos-ok? args (repos-of args))))))

;; =============================================================================
;; merge-commit-sets
;; =============================================================================

(defn- merged [{:keys [root scoped]}] (gs/merge-commit-sets root scoped))

(def ^:private gen-merge-args
  (gen/let [rc (gen/vector (gen/elements ["r1 a" "r2 b"]) 0 3)
            sc (gen/vector (gen/one-of [(gen/let [pid (gen/elements ["p" "q"])
                                                  cs (gen/vector (gen/elements ["c1 x" "c2 y"]) 0 3)]
                                          {:pid pid :commits cs})
                                        (gen/return {:pid "e" :error :timeout})])
                           0 4)]
    {:root {:commits rc :count (count rc) :directory "/u"} :scoped sc}))

(defn- count-conserved?
  "Count equals the umbrella's commits plus every descendant commit."
  [{:keys [root scoped]} out]
  (= (:count out)
     (count (:commits out))
     (+ (count (:commits root)) (reduce + (map (comp count :commits) scoped)))))

(deftrifecta merge-commit-sets-shape
  merged
  {:golden-path "test/golden/hive-mcp/crystal/harvest/git-scopes-merge.edn"
   :cases       {:no-scoped   {:root {:commits ["r1 a"] :count 1} :scoped []}
                 :umbrella-0  {:root {:commits [] :count 0}
                               :scoped [{:pid "sealed-workload" :commits ["69fd99d SW-M5.4"]}]}
                 :mixed       {:root {:commits ["r1 a"] :count 1}
                               :scoped [{:pid "p" :commits ["c1 x" "c2 y"]}
                                        {:pid "q" :commits []}
                                        {:pid "e" :error :timeout}]}}
   :gen         gen-merge-args
   :pred        (fn [out] (= (:count out) (count (:commits out))))
   :mutations   [["ignores-descendants" (fn [{:keys [root]}] root)]
                 ["stale-count"         (fn [{:keys [root scoped]}]
                                          (assoc root :commits (into (vec (:commits root))
                                                                     (mapcat :commits scoped))))]]})

(deftest merge-conserves-commits
  (testing "no commit is lost or invented by the merge"
    (doseq [args (gen/sample gen-merge-args 200)]
      (is (count-conserved? args (merged args)))))
  (testing "a descendant commit keeps its source scope"
    (is (= ["[sealed-workload] 69fd99d SW-M5.4"]
           (:commits (merged {:root {:commits [] :count 0}
                              :scoped [{:pid "sealed-workload" :commits ["69fd99d SW-M5.4"]}]}))))))

;; =============================================================================
;; harvest-descendant-commits (git reader injected)
;; =============================================================================

(deftest harvest-runs-the-injected-reader-per-repo
  (let [reader (fn [dir]
                 (case dir
                   "/a" {:commits ["a1 one"]}
                   "/b" {:error {:type :git-failed}}
                   "/c" (throw (ex-info "boom" {}))
                   "/d" (do (Thread/sleep 2000) {:commits ["late"]})))
        out (gs/harvest-descendant-commits reader
                                           [{:pid "a" :dir "/a"} {:pid "b" :dir "/b"}
                                            {:pid "c" :dir "/c"} {:pid "d" :dir "/d"}]
                                           200)]
    (is (= {:pid "a" :commits ["a1 one"]} (nth out 0)))
    (is (= "b" (:pid (nth out 1))))
    (is (some? (:error (nth out 1))))
    (is (string? (:error (nth out 2))))
    (is (= {:pid "d" :error :timeout} (nth out 3)))))

(deftest harvest-timeout-bounds-the-whole-fan-out
  (testing "N hung repositories cost one timeout, not N"
    (let [reader (fn [_] (Thread/sleep 2000) {:commits ["late"]})
          repos  (mapv (fn [i] {:pid (str "p" i) :dir (str "/" i)}) (range 5))
          t0     (System/currentTimeMillis)
          out    (gs/harvest-descendant-commits reader repos 200)
          ms     (- (System/currentTimeMillis) t0)]
      (is (every? #(= :timeout (:error %)) out))
      (is (< ms 900) (str "fan-out took " ms " ms")))))

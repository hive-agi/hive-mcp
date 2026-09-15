(ns hive-mcp.session.identity-test
  "Tests for the session identity + HCR ownership algebra.

   The case that matters is `two-coordinators-do-not-see-each-other`: it is the
   one today's date-based session id fails, and the reason this namespace
   exists."
  (:require [clojure.test :refer [deftest testing is]]
            [hive-mcp.session.identity :as sid]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Fixtures — two coordinators in one project, each with its own lings
;; =============================================================================

(def world
  {:slaves
   {"coord-a"  {:slave/id "coord-a" :slave/depth 0 :slave/session-id "s-a"
                :slave/project-id "hive"}
    "ling-a1"  {:slave/id "ling-a1" :slave/depth 1 :slave/parent-id "coord-a"
                :slave/session-id "s-a1" :slave/project-id "hive"}
    "ling-a2"  {:slave/id "ling-a2" :slave/depth 2 :slave/parent-id "ling-a1"
                :slave/session-id "s-a2" :slave/project-id "hive"}
    "coord-b"  {:slave/id "coord-b" :slave/depth 0 :slave/session-id "s-b"
                :slave/project-id "hive"}
    "ling-b1"  {:slave/id "ling-b1" :slave/depth 1 :slave/parent-id "coord-b"
                :slave/session-id "s-b1" :slave/project-id "hive"}}
   :coordinators
   {"coord-a" {:coordinator/id "coord-a" :coordinator/session-id "s-a"
               :coordinator/project "hive" :coordinator/status :active}
    "coord-b" {:coordinator/id "coord-b" :coordinator/session-id "s-b"
               :coordinator/project "hive" :coordinator/status :active}}})

(def parent-of
  "Session-id -> parent session-id for the fixture hierarchy."
  {"s-a1" "s-a" "s-a2" "s-a1" "s-b1" "s-b"})

(def ref-a  (sid/resolve-ref world {:agent-id "coord-a" :project-id "hive"}))
(def ref-a1 (sid/resolve-ref world {:agent-id "ling-a1" :project-id "hive"}))
(def ref-b  (sid/resolve-ref world {:agent-id "coord-b" :project-id "hive"}))

;; =============================================================================
;; Resolution
;; =============================================================================

(deftest resolve-coordinator-test
  (testing "a depth-0 slave resolves to a :coordinator with no parent"
    (is (= :coordinator (:session/kind ref-a)))
    (is (= "s-a" (:session/id ref-a)))
    (is (nil? (:session/parent-id ref-a)))
    (is (= 0 (:session/depth ref-a)))
    (is (sid/valid? ref-a))))

(deftest resolve-ling-test
  (testing "a ling carries its own session and its ROOT coordinator's as parent"
    (is (= :ling (:session/kind ref-a1)))
    (is (= "s-a1" (:session/id ref-a1)))
    (is (= "s-a" (:session/parent-id ref-a1))))
  (testing "a nested ling still points at the depth-0 root, not its direct parent"
    (let [r (sid/resolve-ref world {:agent-id "ling-a2" :project-id "hive"})]
      (is (= "s-a" (:session/parent-id r)))
      (is (= 2 (:session/depth r))))))

(deftest resolve-adhoc-test
  (testing "no slave row means :adhoc, keeping the id the caller supplied"
    (let [r (sid/resolve-ref world {:agent-id "editor-7"
                                    :project-id "hive"
                                    :session-id "s-adhoc"})]
      (is (= :adhoc (:session/kind r)))
      (is (= "s-adhoc" (:session/id r)))
      (is (sid/valid? r))))
  (testing "an adhoc session in a project with a live coordinator takes it as parent,
            chosen deterministically so two wraps cannot both claim it"
    (is (= "s-a" (:session/parent-id
                  (sid/resolve-ref world {:agent-id "editor-7"
                                          :project-id "hive"
                                          :session-id "s-adhoc"})))))
  (testing "a project with no coordinator leaves the adhoc session parentless"
    (is (nil? (:session/parent-id
               (sid/resolve-ref world {:agent-id "editor-7"
                                       :project-id "elsewhere"
                                       :session-id "s-adhoc"})))))
  (testing "a terminated coordinator is not a parent"
    (let [w (assoc-in world [:coordinators "coord-a" :coordinator/status] :terminated)
          w (update w :coordinators dissoc "coord-b")]
      (is (nil? (:session/parent-id
                 (sid/resolve-ref w {:agent-id "editor-7"
                                     :project-id "hive"
                                     :session-id "s-adhoc"})))))))

(deftest no-invented-ids-test
  (testing "a caller with no slave row and no session id is INVALID, never given
            a fresh id -- inventing one would make it look like a new owner"
    (let [r (sid/resolve-ref world {:project-id "hive"})]
      (is (nil? (:session/id r)))
      (is (not (sid/valid? r))))))

(deftest cycle-safe-test
  (testing "a parent cycle in the slave table terminates instead of hanging"
    (let [w (-> world
                (assoc-in [:slaves "ling-a1" :slave/parent-id] "ling-a2")
                (assoc-in [:slaves "ling-a2" :slave/parent-id] "ling-a1"))]
      (is (nil? (sid/root-slave w "ling-a1")))
      (is (= :ling (:session/kind (sid/resolve-ref w {:agent-id "ling-a1"}))))))
  (testing "a session-id cycle bounds the ancestor walk"
    (let [p {"x" "y" "y" "x"}]
      (is (>= sid/max-depth (count (sid/ancestor-session-ids p "x")))))))

;; =============================================================================
;; Ownership
;; =============================================================================

(deftest coordinator-owns-its-subtree-test
  (testing "a coordinator owns itself and every descendant, at any depth"
    (is (sid/owns? parent-of ref-a "s-a"))
    (is (sid/owns? parent-of ref-a "s-a1"))
    (is (sid/owns? parent-of ref-a "s-a2"))))

(deftest two-coordinators-do-not-see-each-other-test
  (testing "neither coordinator owns the other's subtree -- the concurrency case
            the date-based session id got wrong"
    (is (not (sid/owns? parent-of ref-a "s-b")))
    (is (not (sid/owns? parent-of ref-a "s-b1")))
    (is (not (sid/owns? parent-of ref-b "s-a")))
    (is (not (sid/owns? parent-of ref-b "s-a1")))))

(deftest ling-owns-only-itself-test
  (testing "a ling owns its own rows"
    (is (sid/owns? parent-of ref-a1 "s-a1")))
  (testing "a ling owns NEITHER its siblings NOR its parent"
    (is (not (sid/owns? parent-of ref-a1 "s-a2")))
    (is (not (sid/owns? parent-of ref-a1 "s-a")))
    (is (not (sid/owns? parent-of ref-a1 "s-b1")))))

(deftest untagged-rows-are-never-owned-test
  (testing "a row with no session id belongs to nobody: rows predating session
            tagging must not be cleared by whoever wraps first"
    (is (not (sid/owns? parent-of ref-a nil)))))

(deftest descendant-is-not-reflexive-test
  (is (not (sid/descendant-of? parent-of "s-a" "s-a")))
  (is (sid/descendant-of? parent-of "s-a1" "s-a")))

;; =============================================================================
;; Adoption
;; =============================================================================

(deftest adoption-test
  (let [loose {:session-id "s-adhoc" :kind :adhoc :project-id "hive" :parent-id nil}]
    (testing "a coordinator adopts a loose adhoc session in its own project"
      (is (sid/adoptable? ref-a loose)))
    (testing "an adhoc session already pointing at THIS coordinator is adoptable"
      (is (sid/adoptable? ref-a (assoc loose :parent-id "s-a"))))
    (testing "one already claimed by another coordinator is not"
      (is (not (sid/adoptable? ref-a (assoc loose :parent-id "s-b")))))
    (testing "a different project is not"
      (is (not (sid/adoptable? ref-a (assoc loose :project-id "elsewhere")))))
    (testing "only coordinators adopt -- a ling never does"
      (is (not (sid/adoptable? ref-a1 loose))))
    (testing "a ling session is not adoptable, it belongs to its own coordinator"
      (is (not (sid/adoptable? ref-a (assoc loose :kind :ling)))))))

;; =============================================================================
;; Partition / harvest
;; =============================================================================

(def opts
  {:parent-of      parent-of
   :row->session   :session
   :row->candidate (fn [row]
                     (when (= :adhoc (:kind row))
                       {:session-id (:session row)
                        :kind       :adhoc
                        :project-id (:project row)
                        :parent-id  (:parent row)}))})

(deftest partition-rows-test
  (let [rows [{:session "s-a"}                                   ;; own
              {:session "s-a1"}                                  ;; descendant
              {:session "s-b1"}                                  ;; another coordinator's
              {:session nil}                                     ;; untagged
              {:session "s-adhoc" :kind :adhoc :project "hive"}]  ;; adoptable
        {:keys [own adopted foreign]} (sid/partition-rows opts ref-a rows)]
    (is (= ["s-a" "s-a1"] (mapv :session own)))
    (is (= ["s-adhoc"] (mapv :session adopted)))
    (is (= #{"s-b1" nil} (set (map :session foreign))))))

(deftest harvestable-joins-own-and-adopted-only-test
  (let [rows [{:session "s-a"} {:session "s-b1"} {:session nil}
              {:session "s-adhoc" :kind :adhoc :project "hive"}]]
    (is (= #{"s-a" "s-adhoc"} (set (map :session (sid/harvestable opts ref-a rows)))))))

(deftest adhoc-is-adopted-by-exactly-one-coordinator-test
  (testing "the same loose adhoc row cannot be harvested by both coordinators"
    (let [rows [{:session "s-adhoc" :kind :adhoc :project "hive" :parent "s-a"}]
          a    (sid/harvestable opts ref-a rows)
          b    (sid/harvestable opts ref-b rows)]
      (is (= 1 (count a)))
      (is (= 0 (count b))))))

(ns hive-mcp.system.sweepers.orphan-edge-test
  "OrphanEdgeSweep prunes auto-kg edges whose source memory entry is gone, and
   only those. Collaborators are injected ports, so no store is touched."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.system.sweepers.orphan-edge :as sweeper]
            [hive-spi.lifecycle.ports :as ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private gone  "20260101000000-aaaaaaaa")
(def ^:private alive "20260101000001-bbbbbbbb")
(def ^:private flaky "20260101000002-cccccccc")

(defn- edge [id from source-type]
  {:kg-edge/id id :kg-edge/from from :kg-edge/to "x"
   :kg-edge/relation :derived-from :kg-edge/source-type source-type})

(def ^:private all-edges
  [(edge "e1" gone :automated)
   (edge "e2" gone :co-access)
   (edge "e3" alive :automated)
   (edge "e4" gone :manual)
   (edge "e5" flaky :automated)
   (edge "e6" "not-a-memory-id" :automated)])

(defn- state [id]
  (cond (= id gone) :absent
        (= id flaky) :unknown
        :else :present))

(defn- scan-all [offset limit]
  (let [es (vec (take limit (drop offset all-edges)))]
    {:edges es :taken (count es)}))

(defn- ports-with [removed & {:as extra}]
  (merge {:scan-fn scan-all
          :entry-state-fn state
          :remove-fn (fn [id] (swap! removed conj id) true)}
         extra))

(deftest memory-id-shape
  (is (sweeper/memory-id? gone))
  (is (not (sweeper/memory-id? "not-a-memory-id")))
  (is (not (sweeper/memory-id? nil)))
  (is (not (sweeper/memory-id? "20260101000000-zzzzzzzz"))))

(deftest only-absent-auto-edges-are-orphans
  (testing "present, unknown, manual and non-memory sources are kept"
    (is (= ["e1" "e2"]
           (mapv :kg-edge/id (sweeper/find-orphans state all-edges))))))

(deftest dry-run-retracts-nothing
  (let [removed (atom [])
        r (ports/sweep! (sweeper/->sweep (ports-with removed)) {})]
    (is (empty? @removed))
    (is (= 2 (:would-prune r)))
    (is (zero? (:swept r)))
    (is (true? (:dry-run? r)))))

(deftest retract-removes-orphans-up-to-cap
  (let [removed (atom [])
        r (ports/sweep! (sweeper/->sweep (ports-with removed :retract? true :prune-cap 1)) {})]
    (is (= ["e1"] @removed))
    (is (= 1 (:swept r)))
    (is (false? (:dry-run? r)))))

(deftest a-failing-removal-is-reported-and-does-not-stop-the-sweep
  (let [removed (atom [])
        remove! (fn [id] (if (= id "e1") (throw (ex-info "boom" {})) (do (swap! removed conj id) true)))
        r (ports/sweep! (sweeper/->sweep (ports-with removed :retract? true :remove-fn remove!)) {})]
    (is (= ["e2"] @removed))
    (is (= 1 (:swept r)))
    (is (= 1 (count (:errors r))))))

(deftest cursor-rotates-through-windows
  (let [removed (atom [])
        sweep (sweeper/->sweep (ports-with removed :retract? true :scan-window 3))]
    (ports/sweep! sweep {})
    (is (= ["e1" "e2"] @removed))
    (is (= 3 @(:cursor sweep)))
    (ports/sweep! sweep {})
    (is (= 0 @(:cursor sweep)) "a short window wraps to the start")))

(deftest backfill-reports-count-before-retracting
  (let [removed (atom [])
        dry (sweeper/backfill (ports-with removed :scan-window 2))]
    (is (empty? @removed))
    (is (= 2 (:orphans dry)))
    (is (= 6 (:scanned dry)))
    (is (true? (:dry-run? dry)))
    (let [wet (sweeper/backfill (ports-with removed :scan-window 2 :retract? true))]
      (is (= 2 (:swept wet)))
      (is (= ["e1" "e2"] @removed)))))

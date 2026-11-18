(ns hive-mcp.swarm.lineage-depth-readers-test
  "RRS-S5A: every reader of :slave/depth treats a ling at ANY lineage depth
   as a ling. Spawn records the real depth (coordinator 0, child 1,
   grandchild 2), so a reader that filtered `(= 1 depth)` lost every
   grandchild from listings, cleanup, the terminal sweep and forge.

   Each test seeds the real swarm DataScript store (isolated per test) with a
   depth-0 coordinator, a depth-1 ling and a depth-2 ling, and asserts the
   reader sees both lings and never the coordinator. No with-redefs: readers
   with an injectable port take it, the rest read the isolated store."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.ling.status :as ling-status]
            [hive-mcp.swarm.datascript.lings :as ds-lings]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.swarm.lifecycle.terminal-sweep :as terminal-sweep]
            [hive-mcp.swarm.sync :as sync]
            [hive-mcp.tools.agent.lifecycle :as agent-lifecycle]
            [hive-mcp.tools.consolidated.workflow.forge-ops :as forge-ops]
            [hive-mcp.tools.consolidated.workflow.spawn :as wf-spawn]
            [hive-mcp.tools.olympus :as olympus]
            [clojure.data.json :as json]
            [hive-test.isolation :as iso]
            [hive-mcp.isolation-methods]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (iso/with-isolations :swarm-ds))

(def ^:private lings #{"ling-d1" "ling-d2"})

(defn- seed!
  "A coordinator (0), its child (1) and a grandchild (2)."
  ([] (seed! {}))
  ([extra]
   (ds-lings/add-slave! "coordinator:t1" (merge {:status :idle :depth 0} extra))
   (ds-lings/add-slave! "ling-d1" (merge {:status :idle :depth 1 :parent "coordinator:t1"} extra))
   (ds-lings/add-slave! "ling-d2" (merge {:status :idle :depth 2 :parent "ling-d1"} extra))))

(deftest list-lings-sees-a-grandchild
  (seed!)
  (is (= lings (set (map :id (ling-status/list-lings))))))

(deftest olympus-active-lings-sees-a-grandchild
  (seed!)
  (is (= lings (set (map :slave/id (#'olympus/get-active-lings))))))

(deftest terminal-sweep-probes-a-grandchild
  (let [row (fn [id depth] {:slave/id id :slave/depth depth :ling/spawn-mode :vterm})
        terminal? (partial #'terminal-sweep/terminal-slave? #{:vterm})]
    (is (true? (boolean (terminal? (row "ling-d1" 1)))))
    (is (true? (boolean (terminal? (row "ling-d2" 2)))))
    (is (false? (boolean (terminal? (row "coordinator:t1" 0)))))))

(deftest cleanup-removes-an-orphaned-grandchild
  (testing "with no Emacs membership every ling row is an orphan, at any depth; the coordinator stays"
    (seed!)
    (let [res (json/read-str (:text (agent-lifecycle/handle-cleanup {})) :key-fn keyword)]
      (is (= lings (set (:removed-ids res))))
      (is (some? (queries/get-slave "coordinator:t1"))))))

(deftest forge-smite-reaps-a-grandchild
  (let [rows [{:slave/id "coordinator:t1" :slave/depth 0 :slave/status :completed}
              {:slave/id "ling-d1" :slave/depth 1 :slave/status :completed}
              {:slave/id "ling-d2" :slave/depth 2 :slave/status :completed}]
        killed (atom #{})
        res (forge-ops/smite! {} {:agents-fn (constantly rows)
                                  :kill-fn (fn [a] (swap! killed conj (:slave/id a))
                                             {:id (:slave/id a) :killed true})})]
    (is (some? res))
    (is (= lings @killed))))

(deftest forge-status-counts-a-grandchild
  (seed!)
  (let [r (forge-ops/forge-status* {} (atom {}))]
    (is (= 2 (get-in r [:ok :lings :total])))
    (is (= lings (set (get-in r [:ok :lings :ids]))))))

(deftest forge-slot-count-includes-a-grandchild
  (let [rows [{:slave/id "coordinator:t1" :slave/depth 0 :slave/status :idle}
              {:slave/id "ling-d1" :slave/depth 1 :slave/status :idle :ling/spawn-mode :headless}
              {:slave/id "ling-d2" :slave/depth 2 :slave/status :idle :ling/spawn-mode :headless}]
        {:keys [active-vterm active-headless]} (#'wf-spawn/count-project-lings nil (constantly rows))]
    (is (= 2 (+ active-vterm active-headless)))))

(deftest snapshot-restore-derives-a-missing-depth-from-the-parent
  (testing "a restored row without :depth sits one below its restored parent"
    (ds-lings/add-slave! "ling-d1" {:status :idle :depth 1})
    (#'sync/register-slave-from-status! {:slave-id "ling-r2" :status "idle" :parent-id "ling-d1"})
    (is (= 2 (:slave/depth (queries/get-slave "ling-r2")))))
  (testing "with no parent known it is a ling at depth 1"
    (#'sync/register-slave-from-status! {:slave-id "ling-r1" :status "idle"})
    (is (= 1 (:slave/depth (queries/get-slave "ling-r1"))))))

(ns hive-mcp.memory.temporal-prune-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.memory.temporal :as temporal]))

(def ^:private day-ms (* 1000 60 60 24))

(defn- days-ago [now n] (java.util.Date. (long (- now (* n day-ms)))))

(defn- rows-for
  "One [eid entry-id ts op] row per op, all `age-days` old, eids from `start`."
  [now start entry-id age-days ops]
  (map-indexed (fn [i op] [(+ start i) entry-id (days-ago now age-days) op]) ops))

(def ^:private bounds {:max-per-entry 20 :batch-cap 500})

(deftest kanban-ops-survive-age-out
  (let [now     (System/currentTimeMillis)
        cutoff  (days-ago now 30)
        kanban  (rows-for now 1 "task-a" 90 [:kanban-move :kanban-done :kanban-delete])
        other   (rows-for now 100 "task-a" 90 [:decay :migrate :feedback :expire])
        victims (set (temporal/prune-victims (concat kanban other) cutoff bounds))]
    (testing "aged non-kanban ops are pruned"
      (is (= (set (map first other)) victims)))
    (testing "aged kanban ops are retained"
      (is (empty? (filter victims (map first kanban)))))))

(deftest kanban-ops-survive-per-entry-overflow
  (let [now     (System/currentTimeMillis)
        cutoff  (days-ago now 30)
        kanban  (rows-for now 1 "task-b" 1 (repeat 50 :kanban-move))
        decays  (map-indexed (fn [i _] [(+ 1000 i) "task-b" (java.util.Date. (long (- now i))) :decay])
                             (range 30))
        victims (set (temporal/prune-victims (concat kanban decays) cutoff
                                             {:max-per-entry 5 :batch-cap 500}))]
    (testing "fifty fresh kanban rows on one entry: none pruned"
      (is (empty? (filter victims (map first kanban)))))
    (testing "kanban rows do not consume the per-entry budget of other ops"
      (is (= 25 (count victims))))
    (testing "the five NEWEST decays survive"
      (is (empty? (filter victims (range 1000 1005)))))))

(deftest batch-cap-still-bounds-the-sweep
  (let [now    (System/currentTimeMillis)
        cutoff (days-ago now 30)
        rows   (rows-for now 1 "e" 90 (repeat 40 :decay))]
    (is (= 10 (count (temporal/prune-victims rows cutoff {:max-per-entry 20 :batch-cap 10}))))))

(deftest retained-ops-are-the-kanban-lifecycle
  (is (= #{:kanban-move :kanban-done :kanban-delete :kanban-retag :kanban-edit}
         temporal/retained-ops))
  (testing "every retained op is one record-mutation! accepts"
    (is (empty? (remove @#'temporal/valid-ops temporal/retained-ops)))))

(ns hive-mcp.tools.catchup.block-cache-kanban-test
  "A kanban write through the kanban facade drops the cached kanban catchup
   block synchronously, well inside its fresh window. Stub memory store
   (installed through the port under both :default and :kanban, so the test
   holds under every kanban-store mode), pinned clock, and a recording
   write-events listener. No backend."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.events.write-events :as write-events]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.test.stub.memory-store :as stub]
            [hive-mcp.tools.catchup.block-cache :as bc]
            [hive-mcp.tools.kanban.catchup-block :as kanban-block]
            [hive-mcp.vectordb.kanban-facade :as kanban-facade]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private store (atom nil))

(defn- stub-store-fixture
  "One stub store under :default AND :kanban; restore the prior registry."
  [f]
  (stub/with-stub-store
    (fn []
      (let [s (mem-proto/get-store)]
        (mem-proto/register-store! :kanban s)
        (reset! store s)
        (f)))))

(use-fixtures :each stub-store-fixture (fn [t] (bc/reset-cache!) (t) (bc/reset-cache!)))

(defn- recording-listener
  "Register a write-events listener that records every write. Returns the
   atom; unregister with ::recorder."
  []
  (let [seen (atom [])]
    (write-events/register-listener! ::recorder #(swap! seen conj %))
    seen))

(defn- counting-kanban-block
  "The production kanban block (its id and cache policy) with a counting fn,
   so the test proves the policy the block ships with."
  [computes]
  (assoc kanban-block/block :block/fn (fn [{:keys [project-id]}]
                                        {:project project-id :n (swap! computes inc)})))

(defn- seed-task! []
  (first (stub/seed! @store [{:id "task-1" :type "note"
                              :content "{\"title\":\"t\",\"status\":\"todo\"}"
                              :tags ["kanban" "todo" "scope:project:p"]}])))

(deftest kanban-update-drops-cached-block-before-ttl-test
  (let [clock    (atom 1000000)
        computes (atom 0)
        block    (counting-kanban-block computes)
        ctx      {:project-id "p" :directory "/d" :caller-id "a"}
        seen     (recording-listener)
        id       (seed-task!)]
    (try
      (with-redefs [bc/now-ms (fn [] @clock)]
        (is (= 1 (:n (bc/run-block block ctx))) "cold: computed once")
        (swap! clock + 1000)
        (is (= 1 (:n (bc/run-block block ctx))) "1 s later: a fresh hit")
        (kanban-facade/update-entry! id {:tags ["kanban" "doing" "scope:project:p"]})
        (testing "the facade announced the write with the kanban tag"
          (is (some #(and (= :updated (:op %)) (= id (:id %))
                          (some #{"kanban"} (:tags %)))
                    @seen)))
        (swap! clock + 1000)
        (is (= 2 (:n (bc/run-block block ctx)))
            "2 s in (TTL is 30 s): the kanban update dropped the block"))
      (finally (write-events/unregister-listener! ::recorder)))))

(deftest kanban-delete-drops-cached-block-before-ttl-test
  (let [clock    (atom 1000000)
        computes (atom 0)
        block    (counting-kanban-block computes)
        ctx      {:project-id "p"}
        id       (seed-task!)]
    (with-redefs [bc/now-ms (fn [] @clock)]
      (bc/run-block block ctx)
      (kanban-facade/delete-entry! id)
      (swap! clock + 1000)
      (bc/run-block block ctx)
      (is (= 2 @computes) "a delete carries no tags; the facade still tags it kanban"))))

(deftest kanban-add-announces-test
  (let [seen (recording-listener)]
    (try
      (let [id (kanban-facade/add-entry! {:type "note" :content "{}"
                                          :tags ["todo"] :project-id "p"})]
        (is (string? id))
        (is (some #(and (= :added (:op %)) (= id (:id %))
                        (= #{"kanban" "todo"} (set (:tags %))))
                  @seen)))
      (finally (write-events/unregister-listener! ::recorder)))))

(ns hive-mcp.events.effects.kanban-move-done-vessel-test
  "Tests for the :kanban-move-done effect driving the :kanban/move-to-done op
   through the :vessel :dispatch capability (stub over the port, no
   with-redefs).

   handle-kanban-move-done is private: invoked via its var.

   Contract: one op per task id, in order, each {:op :kanban/move-to-done
   :task-id id} plus :directory when one is given, with a 10s timeout. A
   failure, a timeout, a throwing vessel or no vessel at all is logged and
   never stops the remaining tasks or escapes the effect."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-spi.editor.services :as svc]
            [hive-mcp.events.effects.infrastructure :as infra]
            [hive-mcp.test.stub.swarm-host :as sh]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private moved
  {:success true :result "t" :timed-out false})

(defn- move-done! [data]
  (#'infra/handle-kanban-move-done data))

(deftest dispatches-one-op-per-task-with-directory
  (testing "each task id becomes its own op, in order, scoped to the directory"
    (sh/with-swarm-host [host (sh/answering {:kanban/move-to-done moved})]
      (move-done! {:task-ids ["t-1" "t-2"] :directory "/tmp/hive"})
      (is (= [[{:op :kanban/move-to-done :task-id "t-1" :directory "/tmp/hive"} 10000]
              [{:op :kanban/move-to-done :task-id "t-2" :directory "/tmp/hive"} 10000]]
             (sh/calls-of host :kanban/move-to-done))
          "exact op maps and timeout"))))

(deftest no-directory-means-no-directory-key
  (sh/with-swarm-host [host (sh/answering {:kanban/move-to-done moved})]
    (move-done! {:task-ids ["t-1"]})
    (is (= [[{:op :kanban/move-to-done :task-id "t-1"} 10000]]
           (sh/calls-of host :kanban/move-to-done))
        "op map is exactly {:op :kanban/move-to-done :task-id id}")))

(deftest no-task-ids-dispatches-nothing
  (sh/with-swarm-host [host (sh/answering {:kanban/move-to-done moved})]
    (is (nil? (move-done! {:task-ids []})))
    (is (nil? (move-done! {})))
    (is (= [] (sh/calls host)))))

(deftest a-failing-task-does-not-stop-the-rest
  (testing "failure envelope, timeout and a throwing vessel are each contained per task"
    (let [answer (fn [{:keys [task-id]}]
                   (case task-id
                     "fails"   {:success false :error "no such task" :timed-out false}
                     "slow"    {:success false :timed-out true}
                     "throws"  (throw (ex-info "vessel exploded" {}))
                     moved))]
      (sh/with-swarm-host [host (sh/answering {:kanban/move-to-done answer})]
        (is (nil? (move-done! {:task-ids ["fails" "slow" "throws" "ok"]})))
        (is (= ["fails" "slow" "throws" "ok"]
               (mapv (comp :task-id first) (sh/calls-of host :kanban/move-to-done)))
            "every task was attempted, in order")))))

(deftest no-vessel-registered-is-contained
  (testing "with no :vessel capability the unavailable envelope is logged, nothing throws"
    (let [prior (get (svc/registered) sh/registry-key)]
      (svc/unregister-services! sh/registry-key)
      (try
        (is (nil? (move-done! {:task-ids ["t-1" "t-2"] :directory "/tmp/hive"})))
        (finally
          (svc/unregister-services! sh/registry-key)
          (when (seq prior)
            (svc/register-services! sh/registry-key prior)))))))

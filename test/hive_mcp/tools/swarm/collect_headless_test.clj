(ns hive-mcp.tools.swarm.collect-headless-test
  "Collect of a task Emacs does not own: a dispatched id waits for the
   journal, an id nobody dispatched fails at once, on both poll paths."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [hive-mcp.tools.swarm.collect :as collect]
            [hive-mcp.tools.swarm.channel :as channel]
            [hive-mcp.test.stub.swarm-host :as sh]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- emacs-answering
  "Swarm host: the swarm addon is loaded, and every collect answers
   COLLECT-JSON after running ON-COLLECT."
  [collect-json on-collect]
  (sh/answering {:swarm/collect (fn [_op] (on-collect) {:success true :result collect-json})}))

(defn- with-host
  "Run F with RESPOND published as the :swarm-host capabilities."
  [respond f]
  (sh/with-swarm-host [_host respond] (f)))

(defn- body [resp] (json/read-str (:text resp) :key-fn keyword))

(deftest a-dispatched-task-emacs-does-not-own-is-collected-from-the-journal
  (testing "Emacs says Task not found, but the JVM dispatched the id: keep polling"
    (let [task-id   (str "headless-" (random-uuid))
          not-found "{\"status\":\"error\",\"error\":\"Task not found\"}"
          polls     (atom 0)
          _         (channel/record-dispatched-task! task-id)
          resp      (with-host
                      (emacs-answering not-found
                                       #(when (= 2 (swap! polls inc))
                                          (channel/record-task-result!
                                           task-id {:status "completed" :result "done" :slave-id "w"})))
                      #(collect/handle-swarm-collect {:task_id task-id :timeout_ms 10000}))]
      (is (not (:isError resp)))
      (is (= "completed" (:status (body resp))))
      (is (= "done" (:result (body resp))))
      (is (<= 2 @polls) "the result landed after Emacs had already said Task not found"))))

(deftest an-id-nobody-dispatched-fails-fast
  (testing "unknown to the journal, to Emacs and to the dispatched-task registry"
    (let [task-id (str "bogus-" (random-uuid))
          t0      (System/currentTimeMillis)
          resp    (with-host
                    (emacs-answering "{\"status\":\"error\",\"error\":\"Task not found\"}" (fn []))
                    #(collect/handle-swarm-collect {:task_id task-id :timeout_ms 10000}))
          elapsed (- (System/currentTimeMillis) t0)]
      (is (:isError resp))
      (is (= "error" (:status (body resp))))
      (is (= (str "Unknown task id: " task-id) (:error (body resp))))
      (is (< elapsed 3000) "answers at once instead of waiting out timeout_ms"))))

(deftest a-real-task-error-stays-terminal
  (testing "an Emacs error other than Task not found is final, dispatched or not"
    (doseq [dispatched? [false true]]
      (let [task-id (str "vterm-" (random-uuid))
            _       (when dispatched? (channel/record-dispatched-task! task-id))
            resp    (with-host
                      (emacs-answering "{\"status\":\"error\",\"error\":\"Task crashed\"}" (fn []))
                      #(collect/handle-swarm-collect {:task_id task-id :timeout_ms 5000}))]
        (is (= "error" (:status (body resp))))
        (is (= "Task crashed" (:error (body resp))))))))

(def ^:private emacs-absent
  "Swarm host: registered, but the swarm addon is not loaded, so collect
   takes the JVM poll path. Any collect call answers an unscripted failure."
  (sh/answering {:swarm/available? sh/addon-unloaded}))

(deftest jvm-poll-fails-fast-for-an-id-nobody-dispatched
  (let [task-id (str "bogus-" (random-uuid))
        t0      (System/currentTimeMillis)
        resp    (with-host
                  emacs-absent
                  #(collect/handle-swarm-collect {:task_id task-id :timeout_ms 10000}))
        elapsed (- (System/currentTimeMillis) t0)]
    (is (:isError resp))
    (is (= "error" (:status (body resp))))
    (is (= (str "Unknown task id: " task-id) (:error (body resp))))
    (is (< elapsed 3000) "answers at once instead of waiting out timeout_ms")))

(deftest jvm-poll-keeps-polling-a-dispatched-pending-id
  (let [task-id (str "headless-" (random-uuid))
        _       (channel/record-dispatched-task! task-id)
        lands   (future (Thread/sleep 400)
                        (channel/record-task-result!
                         task-id {:status "completed" :result "late" :slave-id "w"}))
        resp    (with-host
                  emacs-absent
                  #(collect/handle-swarm-collect {:task_id task-id :timeout_ms 10000}))]
    @lands
    (is (not (:isError resp)))
    (is (= "completed" (:status (body resp))))
    (is (= "late" (:result (body resp))))
    (is (= "journal-jvm-poll" (:via (body resp))))))

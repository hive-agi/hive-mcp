(ns hive-mcp.tools.swarm.status-test
  "Tests for swarm status handlers - broadcast fix verification.

   BUG FIX: hivemind_broadcast/swarm_broadcast was silently succeeding
   even when no slaves were available. Now returns error with clear message.

   Kanban: 20260130114548"
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.swarm.status :as status]
            [clojure.data.json :as json]
            [hive-mcp.test.stub.swarm-host :as sh]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; handle-swarm-broadcast Tests
;; =============================================================================

(defn- broadcast-host
  "A swarm host whose addon reports present and whose broadcast answers
   ANSWER (a full {:success :result :timed-out} envelope)."
  [answer]
  (sh/answering {:swarm/broadcast answer}))

(deftest handle-swarm-broadcast-no-targets-returns-error
  (testing "broadcast returns error when no slaves available (bug fix)"
    (sh/with-swarm-host
      [host (broadcast-host
             ;; the host returns an empty list: no slaves
             {:success true :result "[]" :timed-out false})]
      (let [result (status/handle-swarm-broadcast {:prompt "test prompt"})
            parsed (json/read-str (:text result) :key-fn keyword)]
        ;; Should be an error, not success
        (is (:isError result)
            "Should return isError when no targets")
        (is (= "no-targets" (:error parsed))
            "Error type should be 'no-targets'")
        (is (zero? (:delivered-count parsed))
            "Delivered count should be 0")
        (is (string? (:message parsed))
            "Should have helpful error message")
        (is (= [[{:op :swarm/broadcast, :prompt "test prompt"} 5000]] (sh/calls-of host :swarm/broadcast))
            "Prompt should reach the swarm host")))))

(deftest handle-swarm-broadcast-success-returns-count
  (testing "broadcast returns delivery count on success"
    (sh/with-swarm-host
      [host (broadcast-host
             ;; the host returns a list of task-ids
             {:success true
              :result "[\"task-1\", \"task-2\", \"task-3\"]"
              :timed-out false})]
      (let [result (status/handle-swarm-broadcast {:prompt "test prompt"})
            parsed (json/read-str (:text result) :key-fn keyword)]
        ;; Should NOT be an error
        (is (not (:isError result))
            "Should not be an error when slaves received broadcast")
        (is (= 3 (:delivered-count parsed))
            "Should report correct delivery count")
        (is (= ["task-1" "task-2" "task-3"] (:task-ids parsed))
            "Should include task IDs")
        (is (string? (:message parsed))
            "Should have success message")
        (is (= [:swarm/available? :swarm/broadcast] (sh/ops host))
            "Availability probe then broadcast, both through the swarm host")))))

(deftest handle-swarm-broadcast-timeout
  (testing "broadcast returns timeout error when the host times out"
    (sh/with-swarm-host
      [_host (broadcast-host
              {:success false :result nil :timed-out true})]
      (let [result (status/handle-swarm-broadcast {:prompt "test"})
            parsed (json/read-str (:text result) :key-fn keyword)]
        (is (:isError result)
            "Should return error on timeout")
        (is (= "timeout" (:status parsed))
            "Status should be timeout")))))

(deftest handle-swarm-broadcast-addon-not-loaded
  (testing "broadcast returns error when the host reports the swarm addon absent"
    (sh/with-swarm-host
      [host (sh/answering {:swarm/available? sh/addon-unloaded})]
      (let [result (status/handle-swarm-broadcast {:prompt "test"})]
        (is (:isError result)
            "Should return error when addon not loaded")
        (is (re-find #"unavailable" (:text result))
            "Should say the swarm host is unavailable")
        (is (empty? (sh/calls-of host :swarm/broadcast))
            "Nothing is broadcast when the addon is absent"))))
  (testing "broadcast returns error when no swarm host is registered at all"
    (let [prior (sh/install! (sh/->host (sh/answering {})))]
      (try
        (sh/restore! nil)
        (let [result (status/handle-swarm-broadcast {:prompt "test"})]
          (is (:isError result)
              "Should return error when hive-emacs is not mounted")
          (is (re-find #"unavailable" (:text result))
              "Should say the swarm host is unavailable"))
        (finally (sh/restore! prior))))))

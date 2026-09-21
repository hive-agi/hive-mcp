;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.tools.swarm.dispatch-disc-port-test
  "Swarm dispatch learns about stale files through hive-spi's IDiscStaleness
   port and names no knowledge-graph namespace.

   The collaborator is a recording port installed in the real slot. The
   fixture puts back exactly what it found: the adapter that was installed,
   or an empty slot when none was."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-spi.swarm.ports.memory-scope :as scope-port]
            [hive-mcp.tools.swarm.dispatch :as dispatch]))

(def ^:private asked (atom []))

(defn- recording-port
  "A port whose disc half records each question and answers with fixed data.
   The slot admits only an IProjectScope, so that half is present and inert."
  []
  (reify
    scope-port/IProjectScope
    (project-id-for-path [_ _path] "global")
    (infer-scope-from-path [_ _path] "global")

    scope-port/IDiscStaleness
    (staleness-warnings [_ paths]
      (swap! asked conj [:staleness-warnings paths])
      [{:message "a.clj changed on disk"}])
    (format-staleness-warnings [_ warnings]
      (swap! asked conj [:format-staleness-warnings warnings])
      "STALE: a.clj")
    (kg-first-context [_ paths]
      (swap! asked conj [:kg-first-context paths])
      {:kg-known [] :needs-read [] :stale ["a.clj"] :summary {}})))

(defn- with-recording-port [f]
  (let [found (when (scope-port/memory-scope-set?) (scope-port/get-memory-scope))]
    (reset! asked [])
    (scope-port/set-memory-scope! (recording-port))
    (try
      (f)
      (finally
        (if found
          (scope-port/set-memory-scope! found)
          (scope-port/clear-memory-scope!))))))

(use-fixtures :each with-recording-port)

(deftest stale-files-put-the-hosts-warning-before-the-prompt
  (is (= "STALE: a.clj\ndo the task"
         (dispatch/inject-staleness-context "do the task" {:stale ["a.clj"]})))
  (is (= [[:staleness-warnings ["a.clj"]]
          [:format-staleness-warnings [{:message "a.clj changed on disk"}]]]
         @asked)))

(deftest nothing-stale-leaves-the-prompt-alone-and-the-port-unasked
  (is (= "do the task" (dispatch/inject-staleness-context "do the task" {:stale []})))
  (is (= [] @asked)))

(deftest the-disc-context-comes-from-the-port
  (is (= {:kg-known [] :needs-read [] :stale ["a.clj"] :summary {}}
         (#'dispatch/get-disc-context "do the task" ["a.clj"])))
  (is (= [[:kg-first-context ["a.clj"]]] @asked)))

(deftest with-no-host-installed-the-prompt-passes-through
  (scope-port/clear-memory-scope!)
  (testing "the noop knows of no warnings, so stale paths inject nothing"
    (is (= "do the task"
           (dispatch/inject-staleness-context "do the task" {:stale ["a.clj"]})))))

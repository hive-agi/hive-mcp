(ns hive-mcp.server.routes.async-tasks.facet-test
  "Facets for hive-mcp.server.routes.async-tasks (cov-facets f07). The task
   registry is driven through its ITaskRunner port with a faithful inline
   runner (runs f on the calling thread, answers a view of a finished handle),
   installed via the namespace's own `runner` seam, so the golden needs no
   threads and no sleeps. :elapsed-ms is wall-clock and is dropped from the
   snapshot."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.server.routes.async-tasks :as at]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord InlineRunner []
  at/ITaskRunner
  ;; a pool future captures a throw from its body; so does this runner
  (-run [_ f] (try (f) (catch Throwable _ nil)) ::handle)
  (-cancel [_ _] false)
  (-view [_ h] {:present? (some? h) :done? true :cancelled? false}))

(defn- row [t] (some-> t (dissoc :elapsed-ms)))

(defn- with-inline-runner [f]
  (at/reset-registry!)
  (reset! at/runner (->InlineRunner))
  (try (f)
       (finally (reset! at/runner nil) (at/reset-registry!))))

(deftest-golden async-tasks-registry-golden
  "test/golden/hive-mcp/f07-async-tasks-registry.edn"
  (with-inline-runner
    (fn []
      (at/submit! {:task-id "t-ok" :tool "memory" :caller-id "c1" :f (fn [] :ok)})
      (at/submit! {:task-id "t-boom" :tool "kanban" :caller-id "c2"
                   :f (fn [] (throw (ex-info "boom" {})))})
      (at/submit! {:task-id "t-short" :tool "git" :caller-id "c3" :lane :short
                   :timeout-ms 5000 :f (fn [] nil)})
      (let [before {:ok      (row (at/get-task "t-ok"))
                    :boom    (row (at/get-task "t-boom"))
                    :short   (row (at/get-task "t-short"))
                    :missing (at/get-task "t-none")
                    :listed  (sort (map :task-id (at/list-tasks)))
                    :running (mapv :task-id (at/running-tasks))
                    :cancel-done    (at/cancel! "t-ok")
                    :cancel-unknown (at/cancel! "t-none")
                    :runner-is-injected (instance? InlineRunner (at/current-runner))}]
        (assoc before
               :forget (at/forget!)
               :after-forget (mapv :task-id (at/list-tasks))
               :reset (at/reset-registry!))))))

(deftest runner-for-each-lane-satisfies-the-port
  (is (satisfies? at/ITaskRunner (at/runner-for :batch)))
  (is (satisfies? at/ITaskRunner (at/runner-for :short))))

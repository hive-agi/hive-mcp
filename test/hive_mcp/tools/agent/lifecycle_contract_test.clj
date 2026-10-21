(ns hive-mcp.tools.agent.lifecycle-contract-test
  "Contract facets for the lifecycle handlers: a trifecta over
   latest-task-id (the newest :task/started-at wins), a trifecta over the
   2-arity handle-collect with the task-lookup port injected (no ids is an
   error, an agent with no task is an error naming it), and a golden over
   the validation envelopes of interrupt / broadcast / claims / cleanup."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.agent.lifecycle :as lc]
            [hive-mcp.tools.result-bridge :as rb]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private TaskRow
  "A task row with a distinct id per start time, so the newest is unique."
  [:map {:gen/fmap (fn [{t :t}] {:task/id (str "t" t) :task/started-at (java.util.Date. (long t))})}
   [:t [:int {:min 0 :max 1000}]]])

(deftrifecta-from-schema latest-task-id-picks-the-newest
  #'lc/latest-task-id
  {:in [:cat [:vector {:max 5} TaskRow]]
   :out [:maybe :string]
   :rel (fn [[tasks] out]
          (if (empty? tasks)
            (nil? out)
            (let [newest (apply max (map #(.getTime ^java.util.Date (:task/started-at %)) tasks))]
              (= newest (.getTime ^java.util.Date
                         (:task/started-at (first (filter #(= out (:task/id %)) tasks))))))))
   :classify (fn [[tasks] _] (if (empty? tasks) :none :some))
   :classify-domain #{:none :some}
   :classify-floor 10
   :golden-path "test/golden/hive-mcp/tools/agent/lifecycle-latest-task.edn"
   :num-tests 150})

(defn- no-tasks
  "Task-lookup port of an empty registry."
  [_agent-id]
  [])

;; No :golden-path: the input holds the task-lookup port fn, which has no
;; EDN form; lifecycle-envelopes-golden pins the same envelopes literally.
(deftrifecta-from-schema collect-without-a-task-is-an-error
  #'lc/handle-collect
  {:in [:cat [:fn {:gen/elements [no-tasks]} ifn?]
        [:map [:agent_id {:optional true} [:enum "" "ling-1" "ling-2"]]]]
   :out rb/McpResponse
   :rel (fn [[_ {a :agent_id}] out]
          (and (true? (:isError out))
               (if (str/blank? a)
                 (= "task_id or agent_id is required" (:text out))
                 (str/includes? (:text out) (str "No dispatched task recorded for agent " a)))))
   :classify (fn [[_ {a :agent_id}] _] (if (str/blank? a) :no-ids :no-task))
   :classify-domain #{:no-ids :no-task}
   :classify-floor 10
   :num-tests 80})

(golden/deftest-golden lifecycle-envelopes-golden
  "test/golden/hive-mcp/tools/agent/lifecycle-envelopes.edn"
  {:interrupt-blank (lc/handle-interrupt {:agent_id " "})
   :broadcast-empty (lc/handle-broadcast {:prompt ""})
   :claims          (lc/handle-claims {})
   :cleanup         (lc/handle-cleanup {})
   :collect-none    (lc/handle-collect no-tasks {})
   :collect-ghost   (lc/handle-collect no-tasks {:agent_id "ghost"})})

(deftest one-arity-collect-uses-the-registry-port
  (is (true? (:isError (lc/handle-collect {})))))

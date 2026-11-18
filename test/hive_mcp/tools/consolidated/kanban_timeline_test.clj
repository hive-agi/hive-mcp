(ns hive-mcp.tools.consolidated.kanban-timeline-test
  "`kanban timeline` and `kanban evolution` forward onto the hive-knowledge
   :tl/timeline and :tl/evolution extensions and project their answer to
   compact rows.

   Both are driven through the real registry: the extension is registered with
   the actual live/near-live fn shape, so a change in what the addon's facade
   returns is caught here. Nothing touches a store or a board."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.consolidated.kanban :as kanban-tool]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private wired (atom {}))

(defn- wire!
  "Install `impl` under the :tl/* keys the handlers read, restoring after."
  [impl]
  (reset! wired impl)
  (doseq [[k f] impl] (ext/register! k f)))

(use-fixtures :each
  (fn [f]
    (reset! wired {})
    (f)
    (doseq [k (keys @wired)] (ext/deregister! k))))

(defn- body [resp] (json/read-str (:text resp) :key-fn keyword))

(defn- call
  [params]
  (kanban-tool/handle-kanban (assoc params :command (:command params))))

;; =============================================================================
;; timeline
;; =============================================================================

(def ^:private timeline
  {:task    {:id "20260812163734-6b2ba521" :title "The card" :scope "hive" :status :done}
   :events  [{:task-id "20260812163734-6b2ba521" :at 1786000000000 :kind :created :to :todo :source :kanban}
             {:task-id "20260812163734-6b2ba521" :at 1786000060000 :kind :started :from :todo :to :inprogress :source :mutation-log}
             {:task-id "20260812163734-6b2ba521" :at 1786000120000 :kind :done :from :inprogress :to :done :source :mutation-log}]
   :derived {:lead-time-ms 120000 :cycle-time-ms 60000 :reopened? true :touches 2}})

(deftest timeline-forwards-the-task-id-and-compacts-the-answer
  (let [seen (atom nil)]
    (wire! {:tl/timeline (fn [p] (reset! seen p) timeline)})
    (let [result (body (call {:command "timeline" :task_id "20260812163734-6b2ba521"}))]
      (is (= {:task_id "20260812163734-6b2ba521"} @seen))
      (testing "identity and metrics survive"
        (is (= "20260812163734-6b2ba521" (:id result)))
        (is (= "The card" (:title result)))
        (is (= "done" (:status result)))
        (is (= 120000 (:lead_time_ms result)))
        (is (true? (:reopened result))))
      (testing "events are ordered, ISO-stamped and carry their source"
        (is (= [["created" nil "todo" "kanban"]
                ["started" "todo" "inprogress" "mutation-log"]
                ["done" "inprogress" "done" "mutation-log"]]
               (mapv (juxt :kind :from :to :source) (:events result))))
        (is (= "2026-08-06T07:06:40Z" (-> result :events first :at))))
      (testing "the scope is left to the caller, the task id is not repeated per event"
        (is (not (contains? result :scope)))
        (is (not-any? :task-id (:events result)))))))

(deftest timeline-forwards-the-dedupe-window
  (let [seen (atom nil)]
    (wire! {:tl/timeline (fn [p] (reset! seen p) timeline)})
    (call {:command "timeline" :task_id "t1" :dedupe_window_ms 0})
    (is (= {:task_id "t1" :dedupe-window-ms 0} @seen))
    (call {:command "timeline" :task_id "t1"})
    (is (= {:task_id "t1"} @seen))))

(deftest timeline-refusals-become-errors
  (wire! {:tl/timeline (fn [_] {:error :tl/not-found :message "No board entry, mutation or archive row for task nope"})})
  (let [resp (call {:command "timeline" :task_id "nope"})]
    (is (:isError resp))
    (is (re-find #"tl/not-found" (:text resp)))
    (is (re-find #"nope" (:text resp)))))

(deftest timeline-survives-a-throwing-extension
  (wire! {:tl/timeline (fn [_] (throw (ex-info "boom" {})))})
  (let [resp (call {:command "timeline" :task_id "t1"})]
    (is (:isError resp))
    (is (re-find #"boom" (:text resp)))))

;; =============================================================================
;; evolution
;; =============================================================================

(def ^:private evolution
  {:scope  "hive"
   :from   "2026-08"
   :to     "2026-10"
   :tasks  2
   :months [{:month "2026-08" :created 1 :started 1 :done 0}
            {:month "2026-09" :created 0 :started 0 :done 0}
            {:month "2026-10" :created 1 :started 0 :done 1}]
   :totals {:created 2 :started 1 :done 1}})

(deftest evolution-forwards-the-scope-and-window
  (let [seen (atom nil)]
    (wire! {:tl/evolution (fn [p] (reset! seen p) evolution)})
    (let [result (body (call {:command "evolution" :project_id "hive"
                              :from "2026-08" :to "2026-10" :zone "America/Fortaleza"}))]
      (is (= {:project_id "hive" :from "2026-08" :to "2026-10"
              :window nil :zone "America/Fortaleza"}
             @seen))
      (testing "a named window is forwarded instead of months"
        (call {:command "evolution" :project_id "hive" :window "6m"})
        (is (= {:project_id "hive" :from nil :to nil :window "6m" :zone nil} @seen)))
      (testing "the empty month is dropped, the totals are kept"
        (is (= ["2026-08" "2026-10"] (mapv :month (:months result))))
        (is (= {:created 2 :started 1 :done 1} (:totals result)))
        (is (= 2 (:tasks result)))
        (is (= "hive" (:scope result)))))))

(deftest evolution-keeps-a-scope-set-sorted
  (wire! {:tl/evolution (fn [_] (assoc evolution :scope #{"sisf" "hive"}))})
  (is (= ["hive" "sisf"] (:scope (body (call {:command "evolution" :project_id "hive"
                                             :from "2026-08" :to "2026-10"}))))))

(deftest evolution-refusals-become-errors
  (wire! {:tl/evolution (fn [_] {:error :tl/bad-window :message "evolution requires a month window"})})
  (let [resp (call {:command "evolution" :project_id "hive"})]
    (is (:isError resp))
    (is (re-find #"month window" (:text resp)))))

;; =============================================================================
;; extension absent
;; =============================================================================

(deftest both-commands-error-without-the-extension
  (doseq [params [{:command "timeline" :task_id "t1"}
                  {:command "evolution" :project_id "hive" :from "2026-08" :to "2026-10"}]]
    (let [resp (call params)]
      (is (:isError resp) (pr-str params))
      (is (re-find #"hive-knowledge" (:text resp)))
      (is (re-find #"not mounted" (:text resp))))))

;; =============================================================================
;; advertised surface
;; =============================================================================

(deftest the-commands-are-advertised
  (let [schema (:inputSchema kanban-tool/tool-def)
        props  (:properties schema)]
    (is (contains? (set (get-in schema [:properties "command" :enum]))
                   "timeline"))
    (is (contains? (set (get-in schema [:properties "command" :enum]))
                   "evolution"))
    (testing "every param carries a description naming its command"
      (doseq [p ["task_id" "dedupe_window_ms" "project_id" "from" "to" "window" "zone"]]
        (is (string? (get-in props [p :description])) p)))
    (testing "the description states the addon dependency"
      (is (re-find #"hive-knowledge" (:description kanban-tool/tool-def))))))

(deftest help-lists-both
  (let [text (:text (call {:command "help"}))]
    (is (re-find #"timeline" text))
    (is (re-find #"evolution" text))))

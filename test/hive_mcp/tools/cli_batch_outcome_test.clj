(ns hive-mcp.tools.cli-batch-outcome-test
  "The batch summary counts outcomes: an op whose JSON body says
   success false is failed, even without an :isError flag."
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [hive-mcp.tools.cli :as cli]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- text [m] {:type "text" :text (json/write-str m)})

(def ^:private handlers
  {:ok       (fn [_] (text {:success true :id "a"}))
   :deferred (fn [_] (text {:success false :deferred true :reason "heap-pressure"}))
   :listing  (fn [_] (text [{:success false} {:success true}]))
   :plain    (fn [_] {:type "text" :text "done"})
   :flagged  (fn [_] {:type "text" :text "boom" :isError true})})

(defn- run [cmds]
  (-> ((cli/make-batch-handler handlers) {:operations (mapv #(hash-map :command %) cmds)})
      :text
      (json/read-str :key-fn keyword)))

(deftest a-body-reporting-failure-is-counted-failed
  (let [{:keys [results summary]} (run ["ok" "deferred" "flagged"])]
    (is (= {:total 3 :success 1 :failed 2} summary))
    (is (= "heap-pressure" (:error (second results))))))

(deftest bodies-that-are-not-a-failed-map-stay-successful
  (let [{:keys [summary]} (run ["ok" "listing" "plain"])]
    (is (= {:total 3 :success 3 :failed 0} summary))))

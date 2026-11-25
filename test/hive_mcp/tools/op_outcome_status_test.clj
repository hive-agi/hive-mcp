(ns hive-mcp.tools.op-outcome-status-test
  "RUN-LEDGER-7: a handler that answers {:status :invalid ...} (or :error)
   failed, whether the answer is a map, a JSON text envelope or an EDN text
   envelope (the memory friction reply is `pr-str`ed EDN). Both batch paths
   must count such an op as failed, per op and in the summary."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [hive-mcp.tools.cli :as cli]
            [hive-mcp.batch :as batch]
            [hive-mcp.tools.op-outcome :as oo]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private friction-invalid-reply
  "The exact shape memory command=friction answers for a bad origin."
  {:type "text"
   :text (pr-str {:status :invalid
                  :errors {"origin" ["should be either :explicit, :retro or :auto"]}
                  :required ["target_tool (alias tool)" "kind" "observed (alias content)"]})})

(deftest status-invalid-is-failed
  (testing "a map whose :status is :invalid"
    (let [o (oo/op-outcome {:status :invalid :errors {"kind" ["required"]}})]
      (is (:failed? o))
      (is (= :status-failure (:kind o)))
      (is (re-find #"kind" (:message o)) "the message names the invalid field")))
  (testing "a map whose :status is the string \"error\""
    (is (= :status-failure (:kind (oo/op-outcome {:status "error" :error "boom"})))))
  (testing "an EDN text envelope with :status :invalid (memory friction)"
    (let [o (oo/op-outcome friction-invalid-reply)]
      (is (:failed? o))
      (is (= :status-failure (:kind o)))
      (is (re-find #"origin" (:message o)))))
  (testing "a JSON text envelope with status invalid"
    (is (= :status-failure
           (:kind (oo/op-outcome {:type "text"
                                  :text (json/write-str {:status "invalid" :errors ["bad"]})})))))
  (testing "an EDN text envelope with :success false is a body failure"
    (is (= :body-failure
           (:kind (oo/op-outcome {:type "text" :text (pr-str {:success false :error "nope"})}))))))

(deftest ok-statuses-are-not-failed
  (doseq [r [{:status :ok} {:status :resolved} {:status "recorded"}
             {:type "text" :text (pr-str {:status :recorded :id "x"})}
             {:type "text" :text "{:not edn"}
             {:type "text" :text "{#bad 1}"}]]
    (is (= {:failed? false} (oo/op-outcome r)) (pr-str r))))

(deftest legacy-batch-counts-status-invalid-as-failed
  (let [out (-> ((cli/make-batch-handler {:friction (constantly friction-invalid-reply)})
                 {:operations [{:command "friction"} {:command "friction"}]})
                :text
                (json/read-str :key-fn keyword))]
    (is (every? false? (map :success (:results out))))
    (is (= {:total 2 :success 0 :failed 2} (:summary out)))))

(deftest batchable-path-counts-status-invalid-as-failed
  (let [handler (constantly friction-invalid-reply)
        out     (batch/run-operations
                 [{:id "f1" :tool "memory" :command "friction"}
                  {:id "f2" :tool "memory" :command "friction"}]
                 {:resolve-handler (constantly handler)})]
    (is (false? (:success out)))
    (is (= 2 (get-in out [:summary :failed])))
    (is (= 0 (get-in out [:summary :success])))
    (is (= [] (:committed out)))
    (is (= ["f1" "f2"] (:failed out)))))

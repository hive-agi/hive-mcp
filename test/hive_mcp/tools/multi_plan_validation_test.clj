(ns hive-mcp.tools.multi-plan-validation-test
  "A batch fails its PLAN, not its run: every op's params are checked before
   any op executes, and an executed run states which ops committed."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.tools.multi :as multi]
            [hive-mcp.test.stub.isolated-registry :as isolated]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each isolated/with-fresh-registries)

(defn- recording-resolver
  "A resolve-tool-handler whose \"stub\" tool records every call in CALLS and
   whose \"boom\" tool throws. Every other tool is unknown."
  [calls]
  (fn [tool]
    (case tool
      "stub" (fn [params] (swap! calls conj params) {:type "text" :text "{\"ok\":true}"})
      "boom" (fn [_] (throw (ex-info "store down" {})))
      nil)))

(defmacro ^:private with-stub-tools [calls & body]
  `(with-redefs [multi/resolve-tool-handler (recording-resolver ~calls)
                 hive-mcp.tools.multi/op-input-schema (constantly nil)]
     ~@body))

(deftest mixed-batch-with-one-invalid-op-runs-zero-ops
  (let [calls (atom [])]
    (with-stub-tools calls
      (let [r (multi/run-multi [{:id "a" :tool "stub" :command "x"}
                                {:id "b" :tool "kanban" :command "list" :bogus_param 1}
                                {:id "c" :tool "stub" :command "y" :depends_on ["a"]}])]
        (testing "nothing executed"
          (is (empty? @calls)))
        (testing "the plan is rejected with a per-op error naming op, index and param"
          (is (false? (:success r)))
          (is (true? (:plan-rejected? r)))
          (is (= 1 (count (:errors r))))
          (is (str/includes? (first (:errors r)) "'b' (index 1)"))
          (is (str/includes? (first (:errors r)) "bogus_param"))
          (is (= {:total 3 :success 0 :failed 0 :waves 0} (:summary r))))))))

(deftest every-invalid-op-is-reported
  (let [calls (atom [])]
    (with-stub-tools calls
      (let [r (multi/run-multi [{:id "a" :tool "kanban" :command "list" :x 1}
                                {:id "b" :tool "stub" :command "x"}
                                {:id "c" :tool "memory" :command "search" :y 2}])]
        (is (empty? @calls))
        (is (= 2 (count (:errors r))))
        (is (str/includes? (first (:errors r)) "(index 0)"))
        (is (str/includes? (second (:errors r)) "(index 2)"))))))

(deftest undecodable-typed-param-fails-the-plan
  (let [calls (atom [])]
    (with-redefs [multi/resolve-tool-handler (recording-resolver calls)
                  hive-mcp.tools.multi/op-input-schema
                  (constantly {:properties {"limit" {:type "integer"}}})]
      (let [r (multi/run-multi [{:id "a" :tool "stub" :command "x"}
                                {:id "b" :tool "stub" :command "y" :limit "many"}])]
        (is (empty? @calls))
        (is (true? (:plan-rejected? r)))
        (is (str/includes? (first (:errors r)) "limit"))))))

(deftest dry-run-rejects-the-same-plan
  (let [calls (atom [])]
    (with-stub-tools calls
      (let [r (multi/run-multi [{:id "a" :tool "stub" :command "x"}
                                {:id "b" :tool "kanban" :command "list" :bogus_param 1}]
                               :dry-run true)]
        (is (empty? @calls))
        (is (false? (:success r)))
        (is (str/includes? (first (:errors r)) "bogus_param"))))))

(deftest valid-batch-is-unchanged
  (let [calls (atom [])]
    (with-stub-tools calls
      (let [r (multi/run-multi [{:id "a" :tool "stub" :command "x" :anything 1}
                                {:id "b" :tool "stub" :command "y" :depends_on ["a"]}])]
        (is (true? (:success r)))
        (is (= 2 (count @calls)))
        (is (= {:total 2 :success 2 :failed 0 :waves 2} (:summary r)))
        (is (= ["a" "b"] (:committed r)))
        (is (= [] (:failed r)))
        (is (false? (:partial? r)))
        (is (nil? (:plan-rejected? r)))))))

(deftest runtime-failure-reports-what-committed
  (let [calls (atom [])]
    (with-stub-tools calls
      (let [r (multi/run-multi [{:id "a" :tool "stub" :command "x"}
                                {:id "b" :tool "boom" :command "x"}
                                {:id "c" :tool "stub" :command "y" :depends_on ["b"]}])
            out (json/read-str (:text (multi/format-results r)) :key-fn keyword)]
        (is (false? (:success r)))
        (is (nil? (:plan-rejected? r)) "a runtime failure is not a plan rejection")
        (is (= ["a"] (:committed r)))
        (is (= #{"b" "c"} (set (:failed r))))
        (is (true? (:partial? r)))
        (testing "the MCP response states it too"
          (is (= ["a"] (:committed out)))
          (is (true? (:partial out))))))))

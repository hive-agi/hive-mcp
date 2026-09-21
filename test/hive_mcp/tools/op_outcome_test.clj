(ns hive-mcp.tools.op-outcome-test
  "One classifier, two call sites. Each row is classified directly, then
   run through the legacy make-batch-handler path and the Batchable
   enrich-op-result path; all three must agree on the verdict and on the
   error message."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [hive-mcp.tools.cli :as cli]
            [hive-mcp.batch :as batch]
            [hive-mcp.tools.op-outcome :as op-outcome]
            [hive-mcp.extensions.registry :as ext]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- text
  "Wrap a value as the standard text tool envelope."
  [m] {:type "text" :text (json/write-str m)})

(defn- with-text-envelope-parser
  "Register a :bx/b that parses the {:type \"text\" :text json} envelope, as
   the prod batch addon does at boot. Restores any prior :bx/b."
  [f]
  (let [prior (ext/get-extension :bx/b)]
    (ext/register! :bx/b
                   (fn [result]
                     (if (and (map? result)
                              (= "text" (:type result))
                              (string? (:text result)))
                       (try (json/read-str (:text result) :key-fn keyword)
                            (catch Exception _ result))
                       result)))
    (try (f)
         (finally
           (if prior (ext/register! :bx/b prior) (ext/deregister! :bx/b))))))

(use-fixtures :once with-text-envelope-parser)

(def ^:private rows
  "[name shape kind message]; kind nil means not failed. `kind` is the
   classifier's verdict on the raw shape."
  [["isError envelope, message from :text"
    {:type "text" :text "boom" :isError true}          :is-error      "boom"]
   ["isError envelope with :error"
    {:isError true :error "bad input" :text "x"}       :is-error      "bad input"]
   ["direct :success false, message from :reason"
    {:success false :reason "heap-pressure"}           :success-false "heap-pressure"]
   ["direct :success false, message from :errors head"
    {:success false :errors ["backend timeout" "b"]}   :success-false "backend timeout"]
   ["direct :success false, message from :message"
    {:success false :message "quota"}                  :success-false "quota"]
   ["direct :success false, no reason"
    {:success false}                                   :success-false "tool reported failure (:success false)"]
   ["JSON body :success false, message from :reason"
    (text {:success false :deferred true :reason "heap-pressure"})
    :body-failure "heap-pressure"]
   ["JSON body :success false, message from :errors head"
    (text {:success false :errors ["backend timeout"]}) :body-failure "backend timeout"]
   ["JSON body :success false, message from :error"
    (text {:success false :error "nope"})              :body-failure  "nope"]
   [":ok false"
    {:ok false}                                        :ok-false      "tool reported failure (:isError/:ok false)"]
   ["bare :error"
    {:error "went wrong"}                              :error-key     "went wrong"]
   [":error beside :success true: explicit success wins"
    {:success true :error "partial"}                   nil           nil]
   ["plain text"
    {:type "text" :text "done"}                        nil nil]
   ["isError false"
    {:type "text" :text "fine" :isError false}         nil nil]
   ["listing JSON array"
    (text [{:success false} {:success true}])          nil nil]
   ["success body"
    (text {:success true :id "a"})                     nil nil]
   ["nil :error"
    {:error nil :value 1}                              nil nil]
   ["nil result"
    nil                                                nil nil]
   ["not a map"
    "weird"                                            nil nil]])

(defn- via-cli
  "The legacy make-batch-handler row for `shape`."
  [shape]
  (-> ((cli/make-batch-handler {:op (constantly shape)}) {:operations [{:command "op"}]})
      :text
      (json/read-str :key-fn keyword)
      (get-in [:results 0])))

(defn- via-batch
  "The Batchable enrich-op-result row for `shape`."
  [shape]
  (batch/enrich-op-result {:tool "test" :result shape :success true}))

(deftest classifier-table
  (doseq [[name shape kind message] rows]
    (testing name
      (is (= (if kind
               {:failed? true :kind kind :message message}
               {:failed? false})
             (op-outcome/op-outcome shape))))))

(deftest both-batch-paths-agree-with-the-classifier
  (doseq [[name shape kind message] rows]
    (testing name
      (let [cli-row   (via-cli shape)
            batch-row (via-batch shape)]
        (is (= (nil? kind) (:success cli-row)) "legacy path verdict")
        (is (= (nil? kind) (:success batch-row)) "Batchable path verdict")
        (is (= message (:error cli-row)) "legacy path message")
        (is (= message (:error batch-row)) "Batchable path message")))))

(deftest nil-id-rule-is-armed-only-by-the-creation-key
  (is (= {:failed? true :kind :null-id
          :message "creation tool returned nil id, degraded backend?"}
         (op-outcome/op-outcome {:id nil :success nil} {:null-id-key :id})))
  (is (= {:failed? false} (op-outcome/op-outcome {:id nil :success nil})))
  (is (= {:failed? false} (op-outcome/op-outcome {:id "x"} {:null-id-key :id})))
  (testing "the Batchable path arms it for a creation tool"
    (let [row (batch/enrich-op-result {:tool "memory" :success true
                                       :result (text {:id nil :success nil})})]
      (is (false? (:success row)))
      (is (re-find #"nil id" (:error row))))))

(deftest a-thrown-op-keeps-its-error
  (let [row (batch/enrich-op-result {:tool "test" :success false :error "threw"
                                     :result {:success false :reason "later"}})]
    (is (false? (:success row)))
    (is (= "threw" (:error row)))))

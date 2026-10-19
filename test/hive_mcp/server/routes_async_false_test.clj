(ns hive-mcp.server.routes-async-false-test
  "async:false runs a default-async write command inline, whether the caller
   sends the boolean or the string, and every advertised surface declares
   `async` so the boolean survives transit."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.channel.async-result :as async-buf]
            [hive-mcp.channel.piggyback :as piggyback]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.tools.async-params :as async-params]
            [hive-mcp.tools.registry :as registry]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- write-tool
  "A tool queueing COMMANDS by default, through the full middleware chain."
  [commands]
  (routes/make-tool {:name "memory-like" :description "d"
                     :inputSchema {:type "object"
                                   :properties {"command" {:type "string" :description "c"}}}
                     :default-async-commands commands
                     :handler (fn [args] (pr-str {:inline (:command args)}))}))

(defn- call [tool args]
  (with-redefs [piggyback/get-messages       (fn [& _] nil)
                async-buf/record-submission! (fn [& _] true)
                async-buf/enqueue-result!    (fn [& _] nil)]
    (-> ((:handler tool) (assoc args "_caller_id" "routes-async-false-test"))
        :content first :text)))

(deftest async-false-runs-a-write-inline
  (let [tool (write-tool #{:add :edit :review (keyword "ingest batch")})]
    (doseq [cmd  ["add" "edit" "review" "ingest batch"]
            flag [false "false" "FALSE" " false "]]
      (let [out (call tool {"command" cmd "async" flag})]
        (is (re-find (re-pattern (str ":inline \"" cmd "\"")) out)
            (str cmd " async " (pr-str flag) " must run inline"))
        (is (not (re-find #":queued" out)))))))

(deftest default-and-true-still-queue
  (let [tool (write-tool #{:add})]
    (is (re-find #":queued true" (call tool {"command" "add"})))
    (is (re-find #":queued true" (call tool {"command" "add" "async" true})))
    (is (re-find #":queued true" (call tool {"command" "search" "async" "true"})))
    (is (re-find #":inline \"search\"" (call tool {"command" "search"})))))

(deftest async-flag-reads-strings-as-booleans
  (is (nil? (async-params/async-flag nil)))
  (is (false? (async-params/async-flag false)))
  (is (false? (async-params/async-flag "false")))
  (is (true? (async-params/async-flag "true")))
  (is (true? (async-params/async-flag true))))

(deftest every-registry-surface-declares-async
  (testing "external loaders (bb-mcp) read these surfaces and never run make-tool;
            an undeclared boolean false is stripped in transit"
    (doseq [[surface tools] {"consolidated" (registry/get-consolidated-tools)
                             "advertised"   (registry/get-advertised-tools)
                             "compact"      (registry/get-advertised-tools {:compact-schema? true})}
            tool tools
            :let [prop (get-in tool [:inputSchema :properties "async"])]]
      (is (= "boolean" (:type prop)) (str surface " " (:name tool)))))
  (testing "a default-async tool gets the opt-out wording, and its own declaration wins"
    (let [bare {:name "t" :inputSchema {:type "object" :properties {}}
                :default-async-commands #{:add}}
          own  {:type "boolean" :description "own"}]
      (is (re-find #"Set false"
                   (get-in (async-params/with-async-params bare)
                           [:inputSchema :properties "async" :description])))
      (is (= own (get-in (async-params/with-async-params
                          (assoc-in bare [:inputSchema :properties "async"] own))
                         [:inputSchema :properties "async"]))))))

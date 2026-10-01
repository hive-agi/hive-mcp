(ns hive-mcp.server.routes-async-opt-in-test
  "A tool that runs synchronously by default still queues when the caller
   passes async:true, so a long call does not hold the agent loop."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.channel.async-result :as async-buf]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.channel.piggyback :as piggyback]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- tool [name handler]
  (routes/make-tool {:name name :description "d"
                     :inputSchema {:type "object" :properties {"x" {:type "string" :description "x"}}}
                     :handler handler}))

(deftest a-sync-by-default-tool-queues-when-asked
  (testing "async:true on a tool with no :default-async-commands returns the
            ack at once instead of running the handler on the caller's thread"
    (let [release  (promise)
          enqueued (promise)]
      ;; The hivemind piggyback read is stubbed: loading `routes` registers
      ;; the hivemind message source, which needs the swarm addon on the
      ;; classpath, and the :test alias does not carry it.
      (with-redefs [piggyback/get-messages       (fn [& _] nil)
                    async-buf/record-submission! (fn [& _] true)
                    async-buf/enqueue-result!    (fn [_ r] (deliver enqueued r))]
        ;; If the call is NOT queued the handler holds this thread on
        ;; `release`; the deref bound frees it so the elapsed check fails
        ;; instead of the suite hanging.
        (let [t       (tool "slow" (fn [_] (deref release 5000 :late) "done"))
              t0      (System/currentTimeMillis)
              resp    ((:handler t) {"async" true "_caller_id" "routes-async-test"})
              elapsed (- (System/currentTimeMillis) t0)]
          (deliver release true)
          (is (< elapsed 2000) "the call must not wait for the handler")
          (is (re-find #":queued true" (-> resp :content first :text)))
          (is (= :completed (:status (deref enqueued 5000 nil)))
              "the queued work still runs and reports its result"))))))

(deftest without-async-the-call-runs-in-band
  (with-redefs [piggyback/get-messages (fn [& _] nil)]
    (let [resp ((:handler (tool "fast" (fn [_] "in-band")))
                {"_caller_id" "routes-async-test"})]
      (is (re-find #"in-band" (-> resp :content first :text))))))

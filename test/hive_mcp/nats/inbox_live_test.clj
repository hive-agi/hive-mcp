(ns hive-mcp.nats.inbox-live-test
  "hive-mcp.nats.inbox against a REAL throwaway nats-server -js container on a
   random loopback port, started and removed here. Never the operator's
   broker. Tagged :integration; skipped (with a message) without docker."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.shell :as sh]
            [clojure.data.json :as json]
            [hive-mcp.nats.inbox :as inbox]
            [hive-mcp.tools.consolidated.inbox :as tool])
  (:import [io.nats.client Nats]
           [java.net ServerSocket]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- free-port []
  (with-open [s (ServerSocket. 0 0 (java.net.InetAddress/getLoopbackAddress))]
    (.getLocalPort s)))

(defn- docker? []
  (try (zero? (:exit (sh/sh "docker" "info" "--format" "{{.ID}}"))) (catch Exception _ false)))

(defn- connect-when-up [url ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (or (try (Nats/connect ^String url) (catch Exception _ nil))
          (when (< (System/currentTimeMillis) deadline)
            (Thread/sleep 100) (recur))))))

(defn with-broker [f]
  (if-not (docker?)
    (println "SKIP: docker unavailable")
    (let [port (free-port) cname (str "hive-test-inbox-" port)
          url  (str "nats://127.0.0.1:" port)]
      (try
        (sh/sh "docker" "run" "-d" "--name" cname "-p" (str "127.0.0.1:" port ":4222")
               "nats:2.10-alpine" "-js")
        (with-open [conn (connect-when-up url 15000)]
          (f conn))
        (finally (sh/sh "docker" "rm" "-f" cname))))))

(defn- bodies [r] (mapv :body (:messages r)))

(deftest ^:integration arm-send-read-status
  (with-broker
    (fn [conn]
      (testing "a message sent before the first arm waits in the stream"
        (is (= 1 (:seq (inbox/send! conn {:to "bob" :from "alice" :body "early"}))))
        (let [a (inbox/arm! conn "bob")]
          (is (true? (:created? a)))
          (is (= 1 (:pending a))))
        (is (= ["early"] (bodies (inbox/read! conn "bob")))))
      (testing "re-arm keeps the cursor: kill the reader, send 3, re-arm, exactly 3 in order"
        (doseq [i [1 2 3]] (inbox/send! conn {:to "bob" :body i}))
        (is (false? (:created? (inbox/arm! conn "bob"))))
        (is (= 3 (:pending (inbox/status conn "bob"))))
        (let [r (inbox/read! conn "bob" {:n 10})]
          (is (= [1 2 3] (bodies r)))
          (is (apply < (map :seq (:messages r))))
          (is (zero? (:pending r))))
        (is (= [] (bodies (inbox/read! conn "bob"))) "acked messages are not replayed"))
      (testing "inboxes are isolated"
        (inbox/arm! conn "carol")
        (inbox/send! conn {:to "carol" :body "for carol"})
        (is (= [] (bodies (inbox/read! conn "bob"))))
        (is (= ["for carol"] (bodies (inbox/read! conn "carol")))))
      (testing "a resend with the same msg id is stored once"
        (let [a (inbox/send! conn {:id "dup-1" :to "bob" :body "x"})
              b (inbox/send! conn {:id "dup-1" :to "bob" :body "x"})]
          (is (false? (:duplicate? a)))
          (is (true? (:duplicate? b)))
          (is (= ["x"] (bodies (inbox/read! conn "bob"))))))
      (testing "read on an unarmed inbox refuses"
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not armed"
                              (inbox/read! conn "nobody"))))
      (testing "status lists every armed inbox"
        (is (= ["bob" "carol"] (mapv :inbox (:inboxes (inbox/status conn nil))))))
      (testing "wait_ms long-polls: a send from another thread arrives inside the wait"
        (let [t0 (System/currentTimeMillis)
              _  (future (Thread/sleep 300) (inbox/send! conn {:to "bob" :body "late"}))
              r  (inbox/read! conn "bob" {:wait-ms 5000})]
          (is (= ["late"] (bodies r)))
          (is (< (- (System/currentTimeMillis) t0) 2000)))))))

(deftest ^:integration verbs-answer-json-through-the-live-connection
  (with-broker
    (fn [conn]
      (with-redefs [hive-mcp.tools.consolidated.inbox/connection (constantly conn)]
        (let [j #(json/read-str (:text %) :key-fn keyword)]
          (is (true? (:armed? (j (tool/handle-arm {:inbox "dave"})))))
          (is (= 1 (:seq (j (tool/handle-send {:to "dave" :inbox "erin" :body {:hello 1}})))))
          (let [m (first (:messages (j (tool/handle-read {:inbox "dave" :n "5"}))))]
            (is (= {:hello 1} (:body m)))
            (is (= "erin" (:from m))))
          (is (= 0 (:pending (j (tool/handle-status {:inbox "dave"}))))))))))

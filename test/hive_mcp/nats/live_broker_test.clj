(ns hive-mcp.nats.live-broker-test
  "Against a REAL throwaway nats-server -js container on a random loopback
   port, started and removed by this namespace. Never the operator's broker.
   Tagged :integration; skipped (with a message) when docker is unavailable."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [hive-mcp.nats.client :as client]
            [hive-mcp.nats.streams :as streams]
            [hive-mcp.protocols.event-backbone :as eb]
            [hive-mcp.nats.backbone :as nats-bb])
  (:import [io.nats.client Nats]
           [java.net ServerSocket]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private image "nats:2.10-alpine")

(defn- free-port []
  (with-open [s (ServerSocket. 0 0 (java.net.InetAddress/getLoopbackAddress))]
    (.getLocalPort s)))

(defn- docker? []
  (try (zero? (:exit (sh/sh "docker" "info" "--format" "{{.ID}}"))) (catch Exception _ false)))

(defn- broker-up! [cname port]
  (let [r (sh/sh "docker" "run" "-d" "--name" cname
                 "-p" (str "127.0.0.1:" port ":4222") image "-js")]
    (when-not (zero? (:exit r)) (throw (ex-info "docker run failed" r)))))

(defn- broker-rm! [cname] (sh/sh "docker" "rm" "-f" cname))

(defn- wait-until [pred ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (cond (pred) true
            (> (System/currentTimeMillis) deadline) false
            :else (do (Thread/sleep 50) (recur))))))

(defn- reset-client! []
  (client/stop!)
  (client/remove-on-connect-hook! ::streams)
  (eb/clear-backbone!))

(use-fixtures :each (fn [t] (reset-client!) (t) (reset-client!)))

(defn- stream-names [url]
  (with-open [c (Nats/connect ^String url)]
    (set (.getStreamNames (.jetStreamManagement c)))))

(deftest ^:integration a-broker-down-at-boot-is-joined-when-it-comes-up
  (if-not (docker?)
    (println "SKIP: docker unavailable")
    (let [port  (free-port)
          url   (str "nats://127.0.0.1:" port)
          cname (str "hive-test-nats-" port)]
      (try
        (eb/set-backbone! (nats-bb/create-backbone))
        (client/add-on-connect-hook! ::streams #(streams/ensure-streams! % streams/default-streams))
        (testing "start! with no broker returns promptly and does not throw"
          (let [t0 (System/currentTimeMillis)]
            (client/start! {:url url :connection-timeout 500 :reconnect-wait 200})
            (is (< (- (System/currentTimeMillis) t0) 3000))))
        (testing "status says the backbone is installed but not connected"
          (let [s (eb/backbone-status)]
            (is (= :nats (:backbone-id s)))
            (is (false? (:connected? s)))
            (is (false? (:working? s)))
            (is (re-find #"NOT connected" (:summary s))))
          (is (false? (:connected? (client/status)))))
        (broker-up! cname port)
        (testing "the boot-time client joins the broker without another start!"
          (is (wait-until #(client/connected?) 15000))
          (is (true? (:working? (eb/backbone-status)))))
        (testing "on-connect hooks ensured HIVE_EVENTS and HIVE_INBOX"
          (is (wait-until #(= #{"HIVE_EVENTS" "HIVE_INBOX"} (stream-names url)) 5000)))
        (testing "a broker restart is survived and streams re-ensured"
          (sh/sh "docker" "restart" "-t" "1" cname)
          (is (wait-until #(client/connected?) 15000))
          (is (= #{"HIVE_EVENTS" "HIVE_INBOX"} (stream-names url))))
        (finally (broker-rm! cname))))))

(deftest ^:integration ensure-streams-applies-the-declared-limits
  (if-not (docker?)
    (println "SKIP: docker unavailable")
    (let [port (free-port) url (str "nats://127.0.0.1:" port) cname (str "hive-test-nats-" port)]
      (try
        (broker-up! cname port)
        (is (wait-until #(try (with-open [_ (Nats/connect ^String url)] true) (catch Exception _ false)) 10000))
        (with-open [c (Nats/connect ^String url)]
          (is (= [{:stream "HIVE_EVENTS" :action :created} {:stream "HIVE_INBOX" :action :created}]
                 (streams/ensure-streams! c streams/default-streams)))
          (is (= [:updated :updated] (mapv :action (streams/ensure-streams! c streams/default-streams)))
              "idempotent: a second ensure updates, never fails")
          (let [jsm (.jetStreamManagement c)
                ev  (.getConfiguration (.getStreamInfo jsm "HIVE_EVENTS"))
                in  (.getConfiguration (.getStreamInfo jsm "HIVE_INBOX"))]
            (is (= ["hive.v1.inbox.>"] (vec (.getSubjects in))))
            (is (not-any? #(str/starts-with? % "hive.v1.inbox") (.getSubjects ev)))
            (is (not-any? #(str/starts-with? % "hive.v1.wave") (.getSubjects ev)))
            (is (= "File" (str/capitalize (str (.getStorageType ev)))))
            (is (= (* 72 3600) (.toSeconds (.getMaxAge ev))))
            (is (= (* 2 1024 1024 1024) (.getMaxBytes ev)))
            (is (= "Old" (str/capitalize (str (.getDiscardPolicy ev)))))
            (is (= 120 (.toSeconds (.getDuplicateWindow ev))))
            (is (= (* 7 24 3600) (.toSeconds (.getMaxAge in))))))
        (finally (broker-rm! cname))))))

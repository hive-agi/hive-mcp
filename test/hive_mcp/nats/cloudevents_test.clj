(ns hive-mcp.nats.cloudevents-test
  "STD-04 E1: every hive.v1.* publish carries the CloudEvents binary-mode
   headers of hive-events (ce-*), beside the trace headers on the same
   message, with the body unchanged. Checked on a recording jnats Connection."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.context.request :as ctx]
            [hive-mcp.extensions.soft :as soft]
            [hive-mcp.nats.backbone :as nats-bb]
            [hive-mcp.nats.bridge :as bridge]
            [hive-mcp.nats.client :as nc]
            [hive-mcp.nats.inbox :as inbox]
            [hive-mcp.protocols.event-backbone :as eb]
            [hive-ttracking.tracecontext :as tc]
            [hive.events.cloudevents :as ce]
            [malli.core :as m])
  (:import [io.nats.client Connection Connection$Status JetStream
            JetStreamManagement PublishOptions]
           [io.nats.client.api PublishAck ServerInfo]
           [io.nats.client.impl Headers NatsMessage]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def caller-tp "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")

(defn- header-map [^Headers h]
  (when h (into {} (for [k (.keySet h)] [k (.getFirst h k)]))))

(defn- recording-connection
  "A CONNECTED jnats Connection recording every core publish as
   {:subject :headers :body} into SENT; server info says HEADERS?."
  ^Connection [sent headers?]
  (reify Connection
    (getStatus [_] Connection$Status/CONNECTED)
    (getServerInfo [_] (ServerInfo. (str "INFO {\"headers\":" (boolean headers?) "}")))
    (^void publish [_ ^String subject ^bytes body]
      (swap! sent conj {:subject subject :headers nil :body (String. body "UTF-8")}))
    (^void publish [_ ^String subject ^Headers h ^bytes body]
      (swap! sent conj {:subject subject :headers (header-map h)
                        :body (String. body "UTF-8")}))
    (close [_])))

(def ^:dynamic *sent* nil)

(defn- install! [conn]
  (reset! @#'nc/connection conn))

(use-fixtures :each
  (fn [t]
    (let [conn-atom @#'nc/connection
          before    @conn-atom
          sent      (atom [])]
      (reset! conn-atom (recording-connection sent true))
      (try (binding [*sent* sent] (t))
           (finally (reset! conn-atom before))))))

(defn- ce-keys [headers]
  (into {} (filter (fn [[k _]] (.startsWith ^String k "ce-"))) headers))

(defn- assert-ce [{:keys [headers body]} type id]
  (let [attrs (:ok (ce/<-headers headers))]
    (is (ce/rfc3339? (:time attrs)) "ce-time is RFC 3339")
    (is (= {:specversion "1.0" :type type :id id
            :source (str "/hive/" (bridge/source-node-id))
            :hiveschemaversion 1 :datacontenttype "application/json"}
           (dissoc attrs :time)))
    (is (m/validate ce/Attributes attrs) "valid against the hive-events schema")
    (is (= "application/json" (get headers "content-type")) "mirrored to content-type")
    (is (map? (json/read-str body :key-fn keyword)) "a header-blind consumer decodes the body")))

(deftest ce-type-maps-the-subject-kind
  (is (= "hive.shout" (nc/ce-type "hive.v1.shout.hive.ling-1")))
  (is (= "hive.event.agent-spawn" (nc/ce-type "hive.v1.event.agent-spawn")))
  (is (= "hive.event.a.b" (nc/ce-type "hive.v1.event.a.b")))
  (is (= "hive.inbox" (nc/ce-type "hive.v1.inbox.coordinator-1")))
  (is (= "hive.ci" (nc/ce-type "hive.v1.ci.repo.abc123")))
  (doseq [s ["hive.v1.event" "hive.v1.mystery.x" "hive.conv.tell.a" "other" nil]]
    (is (nil? (nc/ce-type s)) (pr-str s))))

(deftest shout-event-inbox-ci-carry-the-required-attributes
  (doseq [[subject type] [["hive.v1.shout.hive.ling-1" "hive.shout"]
                          ["hive.v1.event.agent-spawn" "hive.event.agent-spawn"]
                          ["hive.v1.inbox.coordinator-1" "hive.inbox"]
                          ["hive.v1.ci.repo.abc123" "hive.ci"]]]
    (reset! *sent* [])
    (testing subject
      (nc/publish! subject {:event-id (str "evt-" type) :ok true})
      (is (= 1 (count @*sent*)))
      (let [msg (first @*sent*)]
        (is (= subject (:subject msg)))
        (assert-ce msg type (str "evt-" type))
        (is (= {:event-id (str "evt-" type) :ok true}
               (json/read-str (:body msg) :key-fn keyword))
            "the body is the payload, unchanged")))))

(deftest a-payload-without-event-id-gets-a-fresh-ce-id
  (nc/publish! "hive.v1.event.x" {:ok true})
  (nc/publish! "hive.v1.event.x" {:ok true})
  (let [[a b] (map #(get-in % [:headers "ce-id"]) @*sent*)]
    (is (and (seq a) (seq b) (not= a b)))))

(deftest bridge-publishers-stamp-through-the-backbone
  (let [before (when (eb/backbone-set?) (eb/get-backbone))]
    (try
      (eb/set-backbone! (nats-bb/create-backbone))
      (bridge/publish-shout! {:agent-id "ling-1" :project-id "hive" :message "hi"})
      (bridge/publish-event! {:type :agent-spawn :agent-id "ling-1"})
      (let [[shout event] @*sent*]
        (assert-ce shout "hive.shout" (get-in shout [:headers "ce-id"]))
        (assert-ce event "hive.event.agent-spawn" (get-in event [:headers "ce-id"]))
        (is (= (bridge/source-node-id)
               (get (json/read-str (:body shout)) "source-node"))
            ":nats/source-node stays in the body"))
      (finally (if before (eb/set-backbone! before) (eb/clear-backbone!))))))

(deftest trace-and-ce-headers-share-one-message
  (ctx/with-request-context {:trace (tc/parse caller-tp)}
    (nc/publish! "hive.v1.shout.hive.ling-1" {:event-id "e-1"}))
  (is (= 1 (count @*sent*)))
  (let [{:keys [headers] :as msg} (first @*sent*)]
    (is (= "4bf92f3577b34da6a3ce929d0e0e4736" (:trace-id (tc/<-headers headers))))
    (assert-ce msg "hive.shout" "e-1")))

(deftest a-subject-outside-hive-v1-gets-no-ce-headers
  (nc/publish! "hive.conv.tell.a" {:ok true})
  (nc/publish! "other.subject" {:ok true})
  (is (every? #(empty? (ce-keys (:headers %))) @*sent*))
  (is (every? #(nil? (:headers %)) @*sent*) "nothing to send: a plain publish"))

(deftest an-unknown-hive-v1-kind-publishes-plain
  (nc/publish! "hive.v1.mystery.x" {:ok true})
  (is (= [{:subject "hive.v1.mystery.x" :headers nil :body "{\"ok\":true}"}] @*sent*)))

(deftest a-server-without-headers-gets-one-plain-publish
  (let [sent (atom [])]
    (install! (recording-connection sent false))
    (ctx/with-request-context {:trace (tc/parse caller-tp)}
      (nc/publish! "hive.v1.event.x" {:event-id "e-2" :ok true}))
    (is (= [{:subject "hive.v1.event.x" :headers nil
             :body "{\"event-id\":\"e-2\",\"ok\":true}"}] @sent))))

(deftest a-header-refusal-falls-back-to-one-plain-publish
  (let [sent (atom [])]
    (install! (reify Connection
                (getStatus [_] Connection$Status/CONNECTED)
                (^void publish [_ ^String subject ^bytes body]
                  (swap! sent conj {:subject subject :body (String. body "UTF-8")}))
                (^void publish [_ ^String _s ^Headers _h ^bytes _b]
                  (throw (IllegalArgumentException. "Headers are not supported by the server")))
                (close [_])))
    (nc/publish! "hive.v1.event.x" {:ok true})
    (is (= [{:subject "hive.v1.event.x" :body "{\"ok\":true}"}] @sent))))

(deftest without-the-codec-publish-is-unchanged
  (binding [soft/*resolve* (fn [_] nil)]
    (nc/publish! "hive.v1.shout.hive.ling-1" {:event-id "e-3" :ok true})
    (is (nil? (nc/ce-headers "hive.v1.shout.hive.ling-1" "e-3" 0))))
  (is (= [{:subject "hive.v1.shout.hive.ling-1" :headers nil
           :body "{\"event-id\":\"e-3\",\"ok\":true}"}] @*sent*)))

(deftest a-codec-err-degrades-to-no-ce-headers
  (binding [soft/*resolve* (fn [sym]
                             (if (= 'hive.events.cloudevents/envelope sym)
                               (fn [_] {:error :cloudevents/invalid-attributes})
                               (requiring-resolve sym)))]
    (nc/publish! "hive.v1.event.x" {:ok true}))
  (is (= [{:subject "hive.v1.event.x" :headers nil :body "{\"ok\":true}"}] @*sent*)))

;; ── Inbox: the JetStream publish site ───────────────────────────────────────

(defn- publish-ack ^PublishAck []
  (PublishAck. (-> (NatsMessage/builder) (.subject "ack")
                   (.data (.getBytes "{\"stream\":\"HIVE_INBOX\",\"seq\":7}" "UTF-8"))
                   (.build))))

(defn- jetstream-connection
  "A Connection whose JetStream records every publish into SENT."
  ^Connection [sent headers?]
  (let [js (reify JetStream
             (^PublishAck publish [_ ^String subject ^bytes body ^PublishOptions _o]
               (swap! sent conj {:subject subject :headers nil :body (String. body "UTF-8")})
               (publish-ack))
             (^PublishAck publish [_ ^String subject ^Headers h ^bytes body ^PublishOptions _o]
               (swap! sent conj {:subject subject :headers (header-map h)
                                 :body (String. body "UTF-8")})
               (publish-ack)))
        jsm (reify JetStreamManagement
              (getStreamInfo [_ _name] nil))]
    (reify Connection
      (getStatus [_] Connection$Status/CONNECTED)
      (getServerInfo [_] (ServerInfo. (str "INFO {\"headers\":" (boolean headers?) "}")))
      (^JetStream jetStream [_] js)
      (^JetStreamManagement jetStreamManagement [_] jsm)
      (close [_]))))

(deftest inbox-send-carries-the-ce-headers
  (let [sent (atom [])
        receipt (inbox/send! (jetstream-connection sent true)
                             {:id "m-1" :to "coordinator-1" :body {:x 1} :ts 5})]
    (is (= 7 (:seq receipt)))
    (is (= 1 (count @sent)))
    (let [msg (first @sent)]
      (is (= "hive.v1.inbox.coordinator-1" (:subject msg)))
      (assert-ce msg "hive.inbox" "m-1")
      (is (= {:id "m-1" :to "coordinator-1" :body {:x 1} :ts 5}
             (json/read-str (:body msg) :key-fn keyword))))))

(deftest inbox-send-to-a-server-without-headers-is-plain
  (let [sent (atom [])]
    (inbox/send! (jetstream-connection sent false) {:id "m-2" :to "coordinator-1" :body 1 :ts 5})
    (is (= [nil] (map :headers @sent)))))

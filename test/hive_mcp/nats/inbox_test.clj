(ns hive-mcp.nats.inbox-test
  "Unit tests for the pure half of hive-mcp.nats.inbox and the `inbox` verbs'
   refusals. The JetStream half runs in hive-mcp.nats.inbox-live-test."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-mcp.nats.inbox :as inbox]
            [hive-mcp.nats.sense-adapters :as sense-adapters]
            [hive-mcp.nats.sense-republish :as sense-republish]
            [hive-mcp.tools.consolidated.inbox :as tool]
            [hive-mcp.tools.consolidated.swarm :as swarm]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest inbox-ids-are-one-subject-token
  (is (inbox/valid-inbox-id? "coordinator-s36605"))
  (is (inbox/valid-inbox-id? "A_b-9"))
  (doseq [bad [nil "" "a.b" "a b" "a>" "*" "x/y" (apply str (repeat 65 "a")) 42]]
    (is (not (inbox/valid-inbox-id? bad)) (pr-str bad))))

(deftest naming-is-stable
  (is (= "hive.v1.inbox.alice" (inbox/subject "alice")))
  (is (= "inbox-alice" (inbox/durable "alice")))
  (is (= "HIVE_INBOX" inbox/stream-name)))

(deftest envelope-keeps-a-given-id-and-mints-one-otherwise
  (let [e (inbox/envelope {:to "bob" :body {:x 1} :from "alice" :ts 7})]
    (is (= {:to "bob" :body {:x 1} :from "alice" :ts 7} (dissoc e :id)))
    (is (string? (:id e))))
  (is (= "m-1" (:id (inbox/envelope {:id "m-1" :to "bob" :body "hi"}))))
  (is (not (contains? (inbox/envelope {:to "bob" :body "hi"}) :from))))

(deftest decode-round-trips-an-envelope-and-keeps-foreign-payloads
  (let [env (inbox/envelope {:id "m-2" :to "bob" :from "alice" :body {:k "v"} :ts 9})
        d   (inbox/decode (.getBytes ^String (json/write-str env) "UTF-8") 5)]
    (is (= {:seq 5 :id "m-2" :from "alice" :body {:k "v"} :ts 9} d)))
  (is (= {:seq 3 :body "plain text"} (inbox/decode (.getBytes "plain text" "UTF-8") 3))))

(deftest resolve-inbox-id-precedence
  (is (= "a" (tool/resolve-inbox-id {:inbox "a" :id "b"} "c")))
  (is (= "b" (tool/resolve-inbox-id {:inbox " " :id "b"} "c")))
  (is (= "c" (tool/resolve-inbox-id {} "c")))
  (is (nil? (tool/resolve-inbox-id {} nil))))

(defn- err-text [resp] (when (:isError resp) (:text resp)))

(deftest verbs-refuse-plainly
  (with-redefs [hive-mcp.tools.consolidated.inbox/connection (constantly nil)]
    (testing "no bus"
      (is (re-find #"no connected event bus" (err-text (tool/handle-arm {:inbox "x"}))))
      (is (re-find #"no connected event bus" (err-text (tool/handle-status {})))))
    (testing "missing params are named"
      (is (re-find #"`to`" (err-text (tool/handle-send {:body "x"}))))
      (is (re-find #"`body`" (err-text (tool/handle-send {:to "x"})))))))

(deftest swarm-routes-the-inbox-subdomain
  (is (= #{:arm :send :read :status} (set (keys (:inbox swarm/canonical-handlers))))))

;; -- TC-S3: canonical inbox id encoding --------------------------------------

(defn- thrown-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest encode-inbox-id-canonicalises-the-coordinator-lane
  (is (= "coordinator-134462" (inbox/encode-inbox-id "coordinator:134462")))
  (is (= "coordinator:134462"
         (inbox/decode-inbox-id (inbox/encode-inbox-id "coordinator:134462")))
      "round trip")
  (is (= "coordinator-s36605" (inbox/encode-inbox-id "coordinator-s36605")))
  (is (= "ling-7" (inbox/decode-inbox-id "ling-7")) "a plain dash stays a dash"))

(deftest encode-inbox-id-is-idempotent-on-valid-ids
  (let [alphabet (vec "abcXYZ019_-")
        ids      (for [n (range 1 40)
                       s (range 3)]
                   (apply str (map #(nth alphabet (mod (+ (* % 7) (* n 3) s) (count alphabet)))
                                   (range n))))]
    (doseq [id ids]
      (is (inbox/valid-inbox-id? id))
      (is (= id (inbox/encode-inbox-id id)) id)
      (is (= (inbox/encode-inbox-id id)
             (inbox/encode-inbox-id (inbox/encode-inbox-id id)))))))

(deftest encode-inbox-id-refuses-garbage-naming-both-forms
  (doseq [bad [nil "" "a.b" "a b" "a>" "*" "x/y" "coordinator:a.b" 42
               (apply str (repeat 65 "a"))]]
    (is (= :inbox/invalid-id (:error (thrown-data #(inbox/encode-inbox-id bad))))
        (pr-str bad)))
  (let [d (thrown-data #(inbox/encode-inbox-id "coordinator:a.b"))]
    (is (= "coordinator:a.b" (:inbox d)))
    (is (= "coordinator-a.b" (:encoded d)))))

(deftest validity-stays-strict
  (is (not (inbox/valid-inbox-id? "coordinator:134462")) "no leniency in the grammar"))

(deftest safe-encode-is-total
  (is (= "coordinator-1" (inbox/safe-encode-inbox-id "coordinator:1")))
  (is (nil? (inbox/safe-encode-inbox-id "a.b")))
  (is (nil? (inbox/safe-encode-inbox-id nil))))

(deftest tool-refuses-garbage-and-encodes-coordinator-ids
  (with-redefs [hive-mcp.tools.consolidated.inbox/connection (constantly ::conn)]
    (let [seen (atom nil)]
      (with-redefs [inbox/read! (fn [_ id _] (reset! seen id) {:inbox id :messages [] :pending 0})]
        (tool/handle-read {:inbox "coordinator:134462"})
        (is (= "coordinator-134462" @seen))))
    (let [r (tool/handle-read {:inbox "a.b"})]
      (is (:isError r))
      (is (str/includes? (:text r) "inbox/invalid-id")))))

(deftest auto-arm-only-arms-coordinator-sessions
  (is (= {:armed? false :reason :not-a-coordinator-session} (tool/auto-arm! "ling-7")))
  (is (= {:armed? false :reason :not-a-coordinator-session} (tool/auto-arm! nil)))
  (with-redefs [hive-mcp.tools.consolidated.inbox/connection (constantly nil)]
    (is (= {:armed? false :reason :no-bus :inbox "coordinator-134462"}
           (tool/auto-arm! "coordinator:134462"))))
  (let [armed (atom nil)]
    (with-redefs [hive-mcp.tools.consolidated.inbox/connection (constantly ::conn)
                  inbox/arm! (fn [_ id] (reset! armed id) {:inbox id :armed? true})
                  sense-adapters/start! (fn [& _] nil)]
      (is (= {:armed? true :inbox "coordinator-134462"} (tool/auto-arm! "coordinator:134462")))
      (is (= "coordinator-134462" @armed))
      (is (= "coordinator-134462"
             (get (sense-republish/bindings) "coordinator:134462")))
      (sense-republish/unbind-inbox! "coordinator:134462"))))

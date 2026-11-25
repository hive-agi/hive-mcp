(ns hive-mcp.await.resumers.session-test
  "The :session and :journal owner kinds (card RUN-LEDGER-1).

   They pass the same resumer conformance cases as :hive-agent-run. Through
   the registry a :session awaiter is ARMED at create (an app coordinator
   never parks), a manual fire sends one inbox message and journals it, and
   a bus that is down is a FAILED resume that stays :waking, never a silent
   success. The doubles are reached through the namespace's own ports
   (*inbox*, *journal*, *now*), bound with `binding`; the real adapters are
   tested as far as the test tier allows (the EDN-lines journal writes, the
   NATS inbox refuses without a bus)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.await.ports :as ports]
            [hive-mcp.await.registry :as registry]
            [hive-mcp.await.resumer-conformance :as conformance]
            [hive-mcp.await.resumers.session :as session]
            [hive-mcp.await.sources.manual :as manual]
            [hive-mcp.tools.consolidated.await :as await-tool]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- recording-inbox
  "An IWakeInbox double over `sent` (an atom). It answers the shape
   hive-mcp.nats.inbox/send! answers, {:id :to :subject :seq :duplicate?},
   and like the stream it stores a repeated message id once."
  [sent]
  (reify session/IWakeInbox
    (send-wake! [_ msg]
      (let [dup? (boolean (some #(= (:id msg) (:id %)) @sent))]
        (when-not dup? (swap! sent conj msg))
        {:id (:id msg) :to (:to msg) :subject (str "hive.v1.inbox." (:to msg))
         :seq (count @sent) :duplicate? dup?}))))

(defn- failing-inbox []
  (reify session/IWakeInbox
    (send-wake! [_ msg]
      (throw (ex-info "await: no connected event bus; the wake is re-driven"
                      {:error :await/no-bus :inbox (:to msg)})))))

(defn- recording-journal [entries]
  (reify session/IWakeJournal
    (append-wake! [_ e] (swap! entries conj e) e)))

(defn- temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "await-session" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- wait-until [ms pred]
  (let [end (+ (System/currentTimeMillis) ms)]
    (loop []
      (let [v (pred)]
        (if (or v (> (System/currentTimeMillis) end)) v (do (Thread/sleep 10) (recur)))))))

(def session-owner {:kind :session :inbox "coordinator-abc123" :label "N3 wait"})

;; =============================================================================
;; Port conformance
;; =============================================================================

(deftest session-resumer-conforms
  (let [sent (atom [])]
    (binding [session/*inbox*   (recording-inbox sent)
              session/*journal* (recording-journal (atom []))]
      (conformance/run-cases!
       session-owner
       (fn [] (let [[old _] (reset-vals! sent [])]
                (mapv (fn [m] {:prompt (get-in m [:body :prompt])}) old)))))))

(deftest journal-resumer-conforms
  (let [entries (atom [])]
    (binding [session/*journal* (recording-journal entries)]
      (conformance/run-cases!
       {:kind :journal :label "ledger"}
       (fn [] (let [[old _] (reset-vals! entries [])]
                (mapv (fn [e] {:prompt (pr-str (:payload e))}) old)))))))

(deftest both-kinds-arm-on-create-and-hive-agent-run-does-not
  (is (true? (ports/arm-on-create? {:kind :session})))
  (is (true? (ports/arm-on-create? {:kind :journal})))
  (is (false? (ports/arm-on-create? {:kind :hive-agent-run})))
  (is (false? (ports/arm-on-create? {:kind :no-such-kind}))))

;; =============================================================================
;; Pure calculations
;; =============================================================================

(deftest wake-message-addresses-the-inbox-with-a-stable-id
  (let [req {:awaiter {:id "aw-1" :attempt 2}
             :payload {:reason :event :event {:type :manual :data {:k 1}}}
             :prompt "continue N3"}
        msg (session/wake-message session-owner req)]
    (is (= "coordinator-abc123" (:to msg)))
    (is (= "aw-1-2" (:id msg)) "one id per attempt: a resend is deduped by the stream")
    (is (= {:type "await/wake" :awaiter-id "aw-1" :reason "event" :label "N3 wait"
            :prompt "continue N3"}
           (dissoc (:body msg) :payload)))
    (is (= (:payload req) (edn/read-string (get-in msg [:body :payload])))
        "the payload round-trips as EDN")))

(deftest journal-entry-names-the-wake
  (is (= {:awaiter-id "aw-1" :kind :journal :attempt 1 :at 99 :reason :timeout
          :payload {:reason :timeout} :label "ledger"}
         (session/journal-entry {:kind :journal :label "ledger"}
                                {:awaiter {:id "aw-1" :attempt 1} :payload {:reason :timeout}}
                                99 nil))))

;; =============================================================================
;; Through the registry
;; =============================================================================

(deftest session-awaiter-is-armed-at-create-and-a-fire-reaches-the-inbox
  (let [sent    (atom [])
        entries (atom [])
        reg     (registry/make-registry (str (io/file (temp-dir) "registry.edn")))]
    (registry/init! reg)
    (try
      (binding [session/*inbox*   (recording-inbox sent)
                session/*journal* (recording-journal entries)
                session/*now*     (constantly 4242)]
        (let [a (registry/create! reg {:id "s1" :owner session-owner :prompt "check N3"
                                       :subscription {:type :manual :filter {:to "coordinator-abc123"}}})]
          (is (= :armed (:status a)) "an app coordinator never parks: create arms it")))
      (testing "a non-matching broadcast does not wake it"
        (manual/fire! {:to "someone-else"})
        (is (= :armed (:status (registry/lookup reg "s1")))))
      (testing "the matching fire sends ONE inbox message and journals it"
        (manual/fire! {:to "coordinator-abc123" :verdict "pass"})
        (is (wait-until 2000 #(some? (registry/resume-result reg "s1"))))
        (is (= :woken (:status (registry/lookup reg "s1"))))
        (is (= 1 (count @sent)))
        (is (= "s1-1" (:id (first @sent))))
        (is (str/starts-with? (get-in (first @sent) [:body :prompt]) "check N3"))
        (is (= [{:awaiter-id "s1" :kind :session :attempt 1 :at 4242 :reason :event
                 :label "N3 wait" :inbox "coordinator-abc123"}]
               (mapv #(dissoc % :payload :delivered) @entries)))
        (is (true? (:ok? (registry/resume-result reg "s1")))))
      (finally (registry/shutdown! reg)))))

(deftest journal-awaiter-times-out-into-the-journal
  (let [entries (atom [])
        reg     (registry/make-registry (str (io/file (temp-dir) "registry.edn")))]
    (registry/init! reg)
    (try
      (binding [session/*journal* (recording-journal entries)]
        (registry/create! reg {:id "j1" :owner {:kind :journal} :timeout-ms 20
                               :subscription {:type :manual}}))
      (is (wait-until 2000 #(some? (registry/resume-result reg "j1"))))
      (is (= :woken (:status (registry/lookup reg "j1"))))
      (is (= [:timeout] (mapv :reason @entries)))
      (finally (registry/shutdown! reg)))))

(deftest bus-down-is-a-failed-resume-that-stays-waking
  (let [entries (atom [])
        reg     (registry/make-registry (str (io/file (temp-dir) "registry.edn"))
                                        {:max-attempts 1})]
    (registry/init! reg)
    (try
      (binding [session/*inbox*   (failing-inbox)
                session/*journal* (recording-journal entries)]
        (registry/create! reg {:id "b1" :owner session-owner
                               :subscription {:type :manual}}))
      (manual/fire! {} "b1")
      (is (wait-until 2000 #(some? (registry/resume-result reg "b1"))))
      (let [res (registry/resume-result reg "b1")]
        (is (false? (:ok? res)))
        (is (= :await/no-bus (get-in res [:data :error]))))
      (is (= :waking (:status (registry/lookup reg "b1"))) "kept for a re-drive, not lost")
      (is (empty? @entries) "nothing is journaled as delivered when the send failed")
      (finally (registry/shutdown! reg)))))

(deftest session-owner-without-an-inbox-is-refused-before-recording
  (let [reg (registry/make-registry (str (io/file (temp-dir) "registry.edn")))]
    (registry/init! reg)
    (try
      (let [e (try (registry/create! reg {:id "x1" :owner {:kind :session}
                                          :subscription {:type :manual}})
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (= :await/invalid-owner (:error (ex-data e))))
        (is (nil? (registry/lookup reg "x1"))))
      (finally (registry/shutdown! reg)))))

;; =============================================================================
;; Real adapters
;; =============================================================================

(deftest ednl-journal-appends-one-readable-line-per-wake
  (let [path (str (io/file (temp-dir) "sub" "wakes.ednl"))
        j    (session/->EdnlJournal path)]
    (session/append-wake! j {:awaiter-id "a" :kind :journal :attempt 1 :at 1})
    (session/append-wake! j {:awaiter-id "b" :kind :session :attempt 1 :at 2})
    (is (= ["a" "b"] (mapv (comp :awaiter-id edn/read-string)
                           (str/split-lines (slurp path)))))))

(deftest nats-inbox-refuses-without-a-bus
  (testing "the test tier runs with no connected bus; the adapter throws, so the wake is re-driven"
    (let [e (try (session/send-wake! (session/->NatsInbox) {:to "x" :id "i" :body {}})
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :await/no-bus (:error (ex-data e)))))))

;; =============================================================================
;; The verb
;; =============================================================================

(deftest create-request-fills-a-session-owner-from-the-caller
  (testing "no inbox given: the calling coordinator session's own inbox"
    (is (= {:kind :session :inbox "coordinator-85f1584a" :session "85f1584a"}
           (get-in (await-tool/create-request {:owner_kind "session"
                                               :_caller_id "coordinator:85f1584a"
                                               :_env_inbox nil})
                   [:ok :owner]))))
  (testing "an explicit inbox wins and is canonicalised"
    (is (= "coordinator-777"
           (get-in (await-tool/create-request {:owner_kind "session" :inbox "coordinator:777"
                                               :_caller_id "coordinator:85f1584a"})
                   [:ok :owner :inbox]))))
  (testing "HIVE_INBOX_ID is the fallback for a caller that names no session"
    (is (= "funeraria-coord"
           (get-in (await-tool/create-request {:owner_kind "session" :_caller_id "coordinator"
                                               :_env_inbox "funeraria-coord"})
                   [:ok :owner :inbox]))))
  (testing "a journal owner keeps its label and gains nothing"
    (is (= {:kind :journal :label "ledger"}
           (get-in (await-tool/create-request {:owner_kind "journal" :label "ledger"
                                               :_caller_id "coordinator:85f1584a"})
                   [:ok :owner])))))

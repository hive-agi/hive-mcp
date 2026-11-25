(ns hive-mcp.await.resumers.session-test
  "The :session and :journal owner kinds (card RUN-LEDGER-1) and the
   registry's wake journal, after the PR #760 review.

   The doubles implement the ports (ports/IWakeInbox, ports/IWakeJournal)
   and are injected the way production injects them: the inbox through
   session/*inbox* (bound with `binding`), the journal through the
   registry's :journal option. No with-redefs. The real adapters are tested
   as far as the test tier allows."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-mcp.await.adapters.ednl-journal :as ednl]
            [hive-mcp.await.adapters.nats-inbox :as nats-inbox]
            [hive-mcp.await.ports :as ports]
            [hive-mcp.await.registry :as registry]
            [hive-mcp.await.resumer-conformance :as conformance]
            [hive-mcp.await.resumers.session :as session]
            [hive-mcp.await.sources.manual :as manual]
            [hive-mcp.tools.consolidated.await :as await-tool]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Doubles, each proven against the port it stands in for
;; =============================================================================

(defn- recording-inbox
  "An IWakeInbox over `sent` (an atom of every send ATTEMPT). Like the NATS
   adapter it accepts only valid inbox ids, and like the stream it stores a
   repeated message id once (:duplicate? true)."
  ([sent] (recording-inbox sent (fn [_] nil)))
  ([sent fail-fn]
   (reify ports/IWakeInbox
     (valid-address? [_ a] (boolean (and (string? a) (re-matches #"[A-Za-z0-9_-]{1,64}" a))))
     (send-wake! [_ msg]
       (fail-fn msg)
       (let [dup? (boolean (some #(= (:id msg) (:id %)) @sent))]
         (swap! sent conj msg)
         {:id (:id msg) :to (:to msg) :seq (count @sent) :duplicate? dup?})))))

(defn- recording-journal
  ([entries] (recording-journal entries (fn [_] nil)))
  ([entries fail-fn]
   (reify ports/IWakeJournal
     (append-wake! [_ e] (fail-fn e) (swap! entries conj e) e))))

(deftest the-inbox-double-agrees-with-the-nats-adapter-on-addresses
  (let [real (nats-inbox/inbox) dbl (recording-inbox (atom []))]
    (doseq [a ["coordinator-abc123" "A_b-9" "has space" "a.b" "" (apply str (repeat 65 "a"))]]
      (is (= (ports/valid-address? real a) (ports/valid-address? dbl a)) (pr-str a)))))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "await-session" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- registry-with [journal & {:as opts}]
  (let [reg (registry/make-registry (str (io/file (temp-dir) "registry.edn"))
                                    (merge {:journal journal} opts))]
    (registry/init! reg)
    reg))

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
    (binding [session/*inbox* (recording-inbox sent)]
      (conformance/run-cases!
       session-owner
       (fn [] (let [[old _] (reset-vals! sent [])]
                (mapv (fn [m] {:prompt (get-in m [:body :prompt])}) old)))))))

(deftest journal-kind-is-registered-and-resumes-nobody
  (is (some #{:journal} (ports/known-resumers)))
  (is (= {:journaled true} ((ports/open-resumer {:kind :journal :label "l"}) {:awaiter {:id "j"}}))))

(deftest both-kinds-arm-on-create-and-hive-agent-run-does-not
  (is (true? (ports/arm-on-create? {:kind :session})))
  (is (true? (ports/arm-on-create? {:kind :journal})))
  (is (false? (ports/arm-on-create? {:kind :hive-agent-run})))
  (is (false? (ports/arm-on-create? {:kind :no-such-kind}))))

;; =============================================================================
;; Pure calculations
;; =============================================================================

(deftest wake-message-id-is-stable-per-awaiter
  (let [msg #(session/wake-message session-owner
                                   {:awaiter {:id "aw-1" :attempt %}
                                    :payload {:reason :event :event {:type :manual :data {:k 1}}}
                                    :prompt "continue N3"})]
    (is (= "aw-1-wake" (:id (msg 1)) (:id (msg 2)) (:id (msg 7)))
        "every attempt publishes the same id (review B1)")
    (is (= {:type "await/wake" :awaiter-id "aw-1" :reason "event" :label "N3 wait"
            :prompt "continue N3"}
           (dissoc (:body (msg 1)) :payload)))
    (is (= {:reason :event :event {:type :manual :data {:k 1}}}
           (edn/read-string (get-in (msg 1) [:body :payload]))))))

;; =============================================================================
;; Through the registry
;; =============================================================================

(deftest session-awaiter-is-armed-at-create-and-a-fire-reaches-the-inbox
  (let [sent (atom []) entries (atom [])
        reg  (registry-with (recording-journal entries))]
    (try
      (binding [session/*inbox* (recording-inbox sent)]
        (is (= :armed (:status (registry/create! reg {:id "s1" :owner session-owner :prompt "check N3"
                                                      :subscription {:type :manual :filter {:to "coordinator-abc123"}}})))
            "inserted :armed: an app coordinator never parks"))
      (manual/fire! {:to "someone-else"})
      (is (= :armed (:status (registry/lookup reg "s1"))))
      (manual/fire! {:to "coordinator-abc123" :verdict "pass"})
      (is (wait-until 2000 #(some? (registry/resume-result reg "s1"))))
      (is (= :woken (:status (registry/lookup reg "s1"))))
      (is (= ["s1-wake"] (mapv :id @sent)))
      (is (str/starts-with? (get-in (first @sent) [:body :prompt]) "check N3"))
      (is (= [{:awaiter-id "s1" :owner-kind :session :attempt 1 :ok? true :reason :event
               :label "N3 wait" :inbox "coordinator-abc123"}]
             (mapv #(select-keys % [:awaiter-id :owner-kind :attempt :ok? :reason :label :inbox]) @entries)))
      (finally (registry/shutdown! reg)))))

(deftest b1-a-retried-wake-publishes-one-message-id
  (testing "send fails on attempt 1, succeeds on attempt 2: two sends, ONE distinct id"
    (let [sent (atom []) calls (atom 0) entries (atom [])
          reg  (registry-with (recording-journal entries) :max-attempts 2)]
      (try
        (binding [session/*inbox* (recording-inbox
                                   sent (fn [_] (when (= 1 (swap! calls inc))
                                                  (throw (ex-info "bus down" {:error :await/no-bus})))))]
          (registry/create! reg {:id "r1" :owner session-owner :subscription {:type :manual}}))
        (manual/fire! {} "r1")
        (is (wait-until 3000 #(= :woken (:status (registry/lookup reg "r1")))))
        (is (= #{"r1-wake"} (set (map :id @sent))) "the re-drive reused the id")
        (is (= [[1 false] [2 true]] (mapv (juxt :attempt :ok?) @entries))
            "the journal sees every attempt, the failed one too")
        (finally (registry/shutdown! reg))))))

(deftest b1-a-journal-failure-after-a-good-send-neither-fails-nor-resends
  (let [sent (atom [])
        reg  (registry-with (recording-journal (atom []) (fn [_] (throw (java.io.IOException. "disk full"))))
                            :max-attempts 2)]
    (try
      (binding [session/*inbox* (recording-inbox sent)]
        (registry/create! reg {:id "jf1" :owner session-owner :subscription {:type :manual}}))
      (manual/fire! {} "jf1")
      (is (wait-until 2000 #(some? (registry/resume-result reg "jf1"))))
      (Thread/sleep 400)
      (is (true? (:ok? (registry/resume-result reg "jf1"))) "the resume stands")
      (is (= :woken (:status (registry/lookup reg "jf1"))))
      (is (= 1 (count @sent)) "nothing was re-sent")
      (is (= #{"jf1-wake"} (set (map :id @sent))))
      (finally (registry/shutdown! reg)))))

(deftest journal-awaiter-times-out-into-the-journal
  (let [entries (atom [])
        reg     (registry-with (recording-journal entries))]
    (try
      (registry/create! reg {:id "j1" :owner {:kind :journal :label "ledger"} :timeout-ms 20
                             :subscription {:type :manual}})
      (is (wait-until 2000 #(some? (registry/resume-result reg "j1"))))
      (is (= :woken (:status (registry/lookup reg "j1"))))
      (is (= [[:journal :timeout "ledger"]] (mapv (juxt :owner-kind :reason :label) @entries)))
      (finally (registry/shutdown! reg)))))

(defmethod ports/open-resumer :journal-test-stub
  ;; A parking kind that is neither :session nor :journal.
  [_owner]
  (fn [_req] {:resumed :ok}))

(deftest the-wake-journal-records-every-owner-kind
  (let [entries (atom [])
        reg     (registry-with (recording-journal entries))]
    (try
      (registry/create! reg {:id "k1" :owner {:kind :journal-test-stub} :subscription {:type :manual}})
      (registry/arm! reg "k1" nil)
      (manual/fire! {} "k1")
      (is (wait-until 2000 #(some? (registry/resume-result reg "k1"))))
      (is (= [:journal-test-stub] (mapv :owner-kind @entries)))
      (finally (registry/shutdown! reg)))))

(deftest b2-an-undeliverable-inbox-is-refused-before-recording
  (let [reg (registry-with (recording-journal (atom [])))]
    (try
      (binding [session/*inbox* (recording-inbox (atom []))]
        (doseq [[id owner] [["x1" {:kind :session}]
                            ["x2" {:kind :session :inbox "has a space"}]
                            ["x3" {:kind :session :inbox (apply str (repeat 65 "a"))}]]]
          (let [e (try (registry/create! reg {:id id :owner owner :subscription {:type :manual}})
                       (catch clojure.lang.ExceptionInfo e e))]
            (is (= :await/invalid-owner (:error (ex-data e))) id)
            (is (nil? (registry/lookup reg id)) (str id " left no row")))))
      (finally (registry/shutdown! reg)))))

;; =============================================================================
;; The durable journal adapter and its reader contract (B4)
;; =============================================================================

(deftest ednl-journal-appends-fsynced-lines-and-the-reader-skips-a-torn-tail
  (let [path (str (io/file (temp-dir) "sub" "wakes.ednl"))
        j    (ednl/journal path)]
    (ports/append-wake! j {:awaiter-id "a" :attempt 1 :ok? false})
    (ports/append-wake! j {:awaiter-id "a" :attempt 2 :ok? true})
    (ports/append-wake! j {:awaiter-id "a" :attempt 2 :ok? true})
    (spit path "{:awaiter-id \"b\" :attem" :append true)
    (let [{:keys [entries skipped]} (ednl/read-journal path)]
      (is (= [["a" 1] ["a" 2]] (mapv (juxt :awaiter-id :attempt) entries))
          "a repeated [awaiter-id attempt] is deduped")
      (is (= 1 skipped) "the torn last line is skipped, not fatal"))
    (is (= {:entries [] :skipped 0} (ednl/read-journal (str path ".none"))))))

(deftest nats-inbox-refuses-without-a-bus
  (let [e (try (ports/send-wake! (nats-inbox/inbox) {:to "x" :id "i" :body {}})
               (catch clojure.lang.ExceptionInfo e e))]
    (is (= :await/no-bus (:error (ex-data e))))))

;; =============================================================================
;; The verb
;; =============================================================================

(deftest create-request-completes-a-session-owner
  (testing "no inbox: the calling coordinator session's own inbox"
    (is (= {:kind :session :inbox "coordinator-85f1584a" :session "85f1584a"}
           (get-in (await-tool/create-request {:owner_kind "session" :_caller_id "coordinator:85f1584a"}
                                              "server-inbox")
                   [:ok :owner]))))
  (testing "an explicit inbox wins and is canonicalised"
    (is (= "coordinator-777"
           (get-in (await-tool/create-request {:owner_kind "session" :inbox "coordinator:777"
                                               :_caller_id "coordinator:85f1584a"})
                   [:ok :owner :inbox]))))
  (testing "B2: an explicit inbox that cannot be encoded is refused"
    (let [res (await-tool/create-request {:owner_kind "session" :inbox "has a space"
                                          :_caller_id "coordinator:85f1584a"})]
      (is (r/err? res))
      (is (re-find #"not a valid inbox id" (:message res)))))
  (testing "item 4: a bare coordinator caller is refused, not routed to HIVE_INBOX_ID"
    (let [res (await-tool/create-request {:owner_kind "session" :_caller_id "coordinator"}
                                         "server-inbox")]
      (is (r/err? res))
      (is (re-find #"no\s+session token" (:message res)))))
  (testing "a caller outside the coordinator lane falls back to HIVE_INBOX_ID"
    (is (= "server-inbox"
           (get-in (await-tool/create-request {:owner_kind "session" :_caller_id "script-7"}
                                              "server-inbox")
                   [:ok :owner :inbox]))))
  (testing "item 5: _env_inbox on the wire is ignored"
    (is (r/err? (await-tool/create-request {:owner_kind "session" :_caller_id "coordinator"
                                            :_env_inbox "smuggled"}))))
  (testing "a journal owner keeps its label and gains nothing"
    (is (= {:kind :journal :label "ledger"}
           (get-in (await-tool/create-request {:owner_kind "journal" :label "ledger"
                                               :_caller_id "coordinator:85f1584a"})
                   [:ok :owner])))))

(deftest the-tool-refuses-a-bare-coordinator-session-await
  (let [resp (await-tool/handle-create {:owner_kind "session" :_caller_id "coordinator"})]
    (is (:isError resp))
    (is (re-find #"await create: .*session token" (:text resp)))))

(deftest the-schema-documents-the-session-owner-params
  (let [props (get-in await-tool/tool-def [:inputSchema :properties])]
    (doseq [k ["inbox" "label" "session"]]
      (is (string? (get-in props [k :description])) k))))

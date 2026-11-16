(ns hive-mcp.nats.sense-republish-test
  "hive-mcp.nats.sense-republish through its ports, with in-memory adapters
   (no with-redefs). The JetStream round trip is in
   hive-mcp.nats.sense-republish-live-test."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [hive-mcp.nats.inbox :as inbox]
            [hive-mcp.nats.sense-republish :as sr]
            [hive-mcp.nats.sense-adapters :as adapters]
            [hive-mcp.tools.consolidated.inbox :as tool]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; In-memory adapters
;; =============================================================================

(defrecord MemFeed [listeners* present?]
  sr/ISenseFeed
  (feed-id [_] :mem)
  (subscribe! [_ k f] (if present? (do (swap! listeners* assoc k f) true) false))
  (unsubscribe! [_ k] (swap! listeners* dissoc k) nil))

(defn- mem-feed ([] (mem-feed true)) ([present?] (->MemFeed (atom {}) present?)))

(defn- emit! [feed sense] (doseq [[_ f] @(:listeners* feed)] (f sense)))

(defrecord MemPublisher [up?* sent* fail?*]
  sr/IInboxPublisher
  (publisher-available? [_] @up?*)
  (publish-message! [_ msg]
    (when @fail?* (throw (ex-info "broker said no" {})))
    (let [dup? (some #(= (:id msg) (:id %)) @sent*)]
      (when-not dup? (swap! sent* conj msg))
      {:duplicate? (boolean dup?)})))

(defn- mem-publisher [] (->MemPublisher (atom true) (atom []) (atom false)))

(defn- sense
  ([cls] (sense cls "s-1"))
  ([cls id]
   {:sense/id id :sense/class cls :sense/agent "ling-a" :sense/project "hive"
    :sense/parent "coordinator:abc" :sense/text "done" :sense/at 42}))

(defn- clean [f]
  (sr/uninstall!)
  (doseq [k (keys (sr/bindings))] (sr/unbind-inbox! k))
  (try (f) (finally
             (sr/uninstall!)
             (doseq [k (keys (sr/bindings))] (sr/unbind-inbox! k)))))

(use-fixtures :each clean)

;; =============================================================================
;; Pure
;; =============================================================================

(deftest exactly-the-terminal-and-attention-classes-republish
  (doseq [c [:sense/completed :sense/ask :sense/blocked :sense/error
             :sense/context-death :sense/truncated :sense/never-started]]
    (is (sr/republishable? (sense c)) (str c)))
  (doseq [c [:sense/progress :sense/rate-limited :sense/commit]]
    (is (not (sr/republishable? (sense c))) (str c)))
  (is (not (sr/republishable? nil))))

(deftest a-coordinator-lane-binds-by-its-session
  (is (= "coordinator:abc" (sr/binding-key "coordinator:abc")))
  (is (= "coordinator:abc" (sr/binding-key "coordinator:abc-hive")))
  (is (= "ling-7" (sr/binding-key "ling-7"))))

(deftest inbox-for-reads-bindings-then-ling-names
  (let [b {"coordinator:abc" "pedro"}]
    (is (= "pedro" (sr/inbox-for b "coordinator:abc")))
    (is (= "pedro" (sr/inbox-for b "coordinator:abc-hive")) "project suffix")
    (is (nil? (sr/inbox-for b "coordinator:zzz")) "an unarmed coordinator has no inbox")
    (is (nil? (sr/inbox-for b "coordinator")) "a lane without a session has none")
    (is (= "ling-parent" (sr/inbox-for b "ling-parent")) "a ling parent reads its own name")
    (is (nil? (sr/inbox-for b "has.dot")) "not a subject token")
    (is (nil? (sr/inbox-for b nil)))
    (is (nil? (sr/inbox-for b " ")))))

(deftest the-message-is-the-wire-sense-under-a-stable-id
  (let [s   (assoc (sense :sense/error "e-9") :sense/detail {:termination :max-turns :turns 3})
        msg (sr/->message s "pedro")]
    (is (= "sense-e-9" (:id msg) (sr/msg-id s)))
    (is (= "pedro" (:to msg)))
    (is (= "error" (get-in msg [:body "class"])))
    (is (= {"termination" "max-turns" "turns" 3} (get-in msg [:body "detail"])))
    (is (= "sense" (get-in msg [:meta :kind])))
    (is (m/validate sr/Message msg))
    (testing "it survives JSON"
      (is (= "error" (get-in (json/read-str (json/write-str msg)) ["body" "class"]))))))

(deftest remember-is-bounded
  (let [seen (reduce #(sr/remember %1 %2 3) nil ["a" "b" "c" "d"])]
    (is (= ["b" "c" "d"] (:order seen)))
    (is (= #{"b" "c" "d"} (:ids seen))))
  (is (= #{"x"} (:ids (sr/remember nil "x")))))

;; =============================================================================
;; Republisher
;; =============================================================================

(defn- rp [pub] (sr/republisher (mem-feed) pub {:resolve-inbox #(sr/inbox-for {"coordinator:abc" "pedro"} %)}))

(deftest a-terminal-sense-reaches-the-parent-inbox-once
  (let [pub (mem-publisher) r (rp pub)]
    (is (= {:outcome :sent :to "pedro" :id "sense-s-1"} (sr/republish! r (sense :sense/completed))))
    (is (= :duplicate (:outcome (sr/republish! r (sense :sense/completed)))) "remembered locally")
    (is (= 1 (count @(:sent* pub))))
    (testing "a fresh republisher (restart) is deduped by the broker's msg id"
      (is (= :duplicate (:outcome (sr/republish! (rp pub) (sense :sense/completed))))))
    (is (= 1 (count @(:sent* pub))))))

(deftest skips-are-named
  (let [pub (mem-publisher) r (rp pub)]
    (is (= {:outcome :skipped :reason :class} (sr/republish! r (sense :sense/progress))))
    (is (= {:outcome :skipped :reason :no-inbox}
           (sr/republish! r (assoc (sense :sense/ask) :sense/parent "coordinator:nobody"))))
    (is (= {:outcome :skipped :reason :no-inbox}
           (sr/republish! r (dissoc (sense :sense/ask) :sense/parent))))
    (is (empty? @(:sent* pub)))))

(deftest no-bus-degrades-quietly-and-recovers
  (let [pub (mem-publisher) r (rp pub)]
    (reset! (:up?* pub) false)
    (is (= :degraded (:outcome (sr/republish! r (sense :sense/blocked "b1")))))
    (is (= :degraded (:outcome (sr/republish! r (sense :sense/blocked "b2")))))
    (is (true? (get-in @(:state* r) [:stats :degraded?])))
    (is (= 2 (get-in @(:state* r) [:stats :degraded])))
    (testing "a publish that throws is :failed, never thrown"
      (reset! (:up?* pub) true)
      (reset! (:fail?* pub) true)
      (is (= :failed (:outcome (sr/republish! r (sense :sense/blocked "b3")))))
      (is (= "broker said no" (get-in @(:state* r) [:stats :last-error]))))
    (testing "back up: sends again and clears degraded?"
      (reset! (:fail?* pub) false)
      (is (= :sent (:outcome (sr/republish! r (sense :sense/blocked "b4")))))
      (is (false? (get-in @(:state* r) [:stats :degraded?]))))))

(deftest a-rejected-hand-off-is-counted-not-thrown
  (let [r (sr/republisher (mem-feed) (mem-publisher) {:execute (fn [_] false)})]
    ((sr/listener r) (sense :sense/completed))
    (is (= 1 (get-in @(:state* r) [:stats :dropped])))))

(deftest install-subscribes-the-feed-and-binding-routes-it
  (let [feed (mem-feed) pub (mem-publisher)
        r    (sr/republisher feed pub)]
    (is (= {:installed? true :feed :mem} (sr/install! r)))
    (is (sr/installed?))
    (testing "unbound parent: nothing sent"
      (emit! feed (sense :sense/ask "a1"))
      (is (empty? @(:sent* pub))))
    (testing "after the coordinator arms, its lings' senses arrive"
      (is (= "coordinator:abc" (sr/bind-inbox! "coordinator:abc-hive" "pedro")))
      (emit! feed (sense :sense/ask "a2"))
      (is (= ["sense-a2"] (mapv :id @(:sent* pub))))
      (is (= "pedro" (:to (first @(:sent* pub))))))
    (let [st (sr/status)]
      (is (m/validate sr/Status st))
      (is (= {"coordinator:abc" "pedro"} (:bindings st)))
      (is (= 1 (get-in st [:stats :sent]))))
    (sr/uninstall!)
    (is (empty? @(:listeners* feed)))
    (is (not (sr/installed?)))
    (is (false? (:installed? (sr/status))))))

(deftest an-absent-feed-installs-nothing
  (is (= {:installed? false :feed :mem} (sr/install! (sr/republisher (mem-feed false) (mem-publisher)))))
  (is (not (sr/installed?))))

(deftest bind-refuses-a-bad-inbox-id
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"invalid inbox id"
                        (sr/bind-inbox! "coordinator:abc" "a.b"))))

;; =============================================================================
;; Adapters
;; =============================================================================

(deftest sixth-sense-feed-adapts-a-listen-port
  (let [ls (atom {})
        port {:listen! (fn [k f] (swap! ls assoc k f) k) :unlisten! (fn [k] (swap! ls dissoc k) nil)}
        feed (adapters/sixth-sense-feed (constantly port))]
    (is (= :sixth-sense (sr/feed-id feed)))
    (is (true? (sr/subscribe! feed :k identity)))
    (is (contains? @ls :k))
    (sr/unsubscribe! feed :k)
    (is (empty? @ls)))
  (let [absent (adapters/sixth-sense-feed (constantly nil))]
    (is (false? (sr/subscribe! absent :k identity)))
    (is (nil? (sr/unsubscribe! absent :k)))))

(deftest nats-publisher-without-a-connection-is-unavailable
  (let [p (adapters/nats-inbox-publisher (constantly nil))]
    (is (false? (sr/publisher-available? p)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no connected event bus"
                          (sr/publish-message! p (sr/->message (sense :sense/error) "pedro"))))))

(deftest bounded-executor-runs-in-order-and-refuses-when-full
  (let [ex   (adapters/bounded-executor 1)
        gate (promise) out (atom [])]
    (is (true? (ex #(deref gate 5000 nil))))
    ;; the first thunk occupies the thread; one more fits the queue
    (Thread/sleep 50)
    (is (true? (ex #(swap! out conj 1))))
    (is (false? (ex #(swap! out conj 2))) "queue full: refused, not blocked")
    (deliver gate :go)
    (let [deadline (+ (System/currentTimeMillis) 2000)]
      (while (and (empty? @out) (< (System/currentTimeMillis) deadline)) (Thread/sleep 10)))
    (is (= [1] @out))))

(deftest host-sense-port-is-a-port-or-nil
  (let [p (adapters/host-sense-port)]
    (is (or (nil? p) (and (ifn? (:listen! p)) (ifn? (:unlisten! p)))))))

(deftest start-is-idempotent-and-never-throws
  (let [a (adapters/start!)
        b (adapters/start!)]
    (is (= :sixth-sense (:feed a) (:feed b)))
    (is (= (:installed? a) (:installed? b) (sr/installed?))
        "installed exactly when hive-agent's sixth-sense is in this JVM")))

(deftest inbox-ids-are-what-the-binding-checks
  (is (inbox/valid-inbox-id? "pedro"))
  (is (= "hive.v1.inbox.pedro" (inbox/subject (sr/inbox-for {"coordinator:abc" "pedro"} "coordinator:abc")))))

(deftest status-verb-without-a-bus-still-refuses-plainly
  (is (:isError (tool/handle-status {:inbox "nobody-armed-here"}))
      "no hive NATS connection in the test JVM"))

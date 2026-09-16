(ns hive-mcp.channel.a2a-conversation-test
  "The two things directed addressing needs that a pure test cannot reach.

   A2A THREADING. a2a-routing-test proves one directed message reaches one
   reader. It says nothing about the SECOND turn, and a peer-to-peer channel
   that cannot hold a conversation is a channel for announcements. So this
   drives three real turns through `shout-with-verdict!` and reads them back
   through `piggyback/get-messages` — the delivery path a ling actually uses —
   and asserts the contextId is minted once and reused, each turn lands in the
   addressed peer's inbox alone, and the coordinator sees ONE summary row
   naming the id rather than the transcript.

   THE VOLUME GATE, WIRED. broadcast-policy could always refuse on volume, and
   in production never did: nothing counted, so `:recent-broadcasts` was never
   passed and the branch was unreachable. A test on `decide` alone passes
   whether or not anything calls it that way, which is exactly how the gap
   survived. These tests go through the shout pipeline, so they fail if the
   counter is ever unhooked again.

   Neither test touches a live system: shouts land in the in-process ring, the
   backbone is unconnected in a cold JVM, and every id is fresh per run."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.channel.broadcast-ledger :as bledger]
            [hive-mcp.channel.broadcast-policy :as bp]
            [hive-mcp.channel.piggyback :as pb]
            [hive-mcp.hivemind.messaging :as msg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- fresh [prefix] (str prefix "-" (random-uuid)))

(defn- ours
  "Rows authored by one of `ids`. A reader with a brand-new id has a cursor at
   zero, so its first read also drains whatever global shouts the ring happens
   to hold; this keeps the assertions about THIS exchange."
  [ids rows]
  (filterv #(contains? (set ids) (:a %)) rows))

;; =============================================================================
;; Threading — a conversation, not an announcement
;; =============================================================================

(deftest a-directed-exchange-threads-across-turns-test
  (let [project (fresh "conv-project")
        a (fresh "ling-a")
        b (fresh "ling-b")
        bystander (fresh "ling-c")
        coordinator (fresh "coordinator:conv")
        say (fn [from to ctx text]
              ;; :deliberate? is what the hivemind tool sets on a shout the
              ;; AGENT chose to make. Without it the digest folds a burst of
              ;; :progress rows from one agent into one, and turns 1 and 3
              ;; would be indistinguishable from a single noisy one.
              (msg/shout-with-verdict!
               from :progress
               (cond-> {:to to :message text :project-id project :deliberate? true}
                 ctx (assoc :context-id ctx))))
        ;; Turns are separated so their timestamps differ. Ordering is the
        ;; claim under test, and three shouts inside one millisecond would
        ;; make the ordered assertion pass without meaning anything.
        turn-1 (say a b nil "is :order/total a string on purpose?")
        _ (Thread/sleep 2)
        turn-2 (say b a (:context-id turn-1) "no, that is a bug, fixing it")
        _ (Thread/sleep 2)
        turn-3 (say a b (:context-id turn-1) "thanks, I will wait on your commit")
        ctx (:context-id turn-1)]

    (testing "the first directed turn opens a conversation and names it"
      (is (= :direct (:routing turn-1)))
      (is (string? ctx))
      (is (seq ctx) "a directed message must come back with an id to continue under"))

    (testing "a reply under that id stays in the same conversation"
      (is (= ctx (:context-id turn-2)) "the reply must not mint a second id")
      (is (= ctx (:context-id turn-3)))
      (is (= :direct (:routing turn-2)))
      (is (= :direct (:routing turn-3))))

    (testing "each turn lands in the addressed peer's inbox and nobody else's"
      (let [b-rows (ours [a b] (pb/get-messages b :project-id project))
            a-rows (ours [a b] (pb/get-messages a :project-id project))
            c-rows (ours [a b] (pb/get-messages bystander :project-id project))]
        (is (= 2 (count b-rows)) "B is addressed by turns 1 and 3")
        (is (every? #(= a (:a %)) b-rows))
        (is (= 1 (count a-rows)) "A is addressed by turn 2 alone")
        (is (= b (:a (first a-rows))))
        (is (empty? c-rows) "a peer nobody addressed pays nothing")))

    (testing "every row carries the id its reader needs to answer in-thread"
      (let [rows (ours [a b] (pb/get-messages (fresh "ling-b-again") :project-id project))]
        ;; A fresh reader is addressed by nothing here; the claim is about the
        ;; rows the addressed peers already saw, re-read from the conversation.
        (is (empty? rows)))
      (let [conversation (pb/fetch-conversation ctx)]
        (is (= 3 (count conversation)))
        (is (= ["is :order/total a string on purpose?"
                "no, that is a bug, fixing it"
                "thanks, I will wait on your commit"]
               (mapv :m conversation))
            "oldest first, so the exchange reads as it happened")
        (is (= [a b a] (mapv :a conversation)))
        (is (= [b a b] (mapv :to conversation))
            "a redeemed conversation says who each turn was for")))

    (testing "the coordinator sees that they talked, not what they said"
      (let [rows (pb/get-messages coordinator :project-id project)
            peer-rows (filterv #(= "peer-traffic" (:e %)) rows)]
        (is (= 1 (count peer-rows))
            "three turns of one conversation cost the coordinator one row")
        (let [row (first peer-rows)]
          (is (= 3 (:n row)) "the row says how many turns passed")
          (is (= ctx (:ctx row)) "and names the id the transcript is behind")
          (is (not (.contains ^String (str (:m row)) ":order/total"))
              "the coordinator must not be paying for the peers' sentences"))
        (is (empty? (ours [a b] rows))
            "no directed turn enters the coordinator's context verbatim")))))

(deftest an-unknown-recipient-is-reported-not-rerouted-test
  (let [project (fresh "conv-project")
        sender (fresh "ling-a")
        verdict (msg/shout-with-verdict!
                 sender :progress
                 {:to (fresh "ling-nobody") :message "hello?"
                  :project-id project :deliberate? true})]
    (is (= :direct (:routing verdict))
        "the roster is silent about coordinator lanes and foreign peers, so a
         miss is not evidence enough to change the route")
    (is (some? (:to-unresolved verdict))
        "but the sender is told, in the same result it is already reading")))

;; =============================================================================
;; The volume gate, reached through the pipeline that had unhooked it
;; =============================================================================

(deftest broadcast-budget-bites-in-the-shout-pipeline-test
  (let [project (fresh "budget-project")
        sender (fresh "ling-loud")
        shout (fn [reason i]
                (msg/shout-with-verdict!
                 sender :progress
                 {:message (str "finding " i) :broadcast? true
                  :broadcast-reason reason :project-id project :deliberate? true}))
        budget bp/default-budget
        verdicts (mapv #(shout :shared-discovery %) (range (+ budget 2)))]

    (testing "an admissible reason buys a budget, not a standing permission"
      (is (= budget (count (filterv #(= :broadcast (:routing %)) verdicts)))
          "exactly the budget is admitted")
      (is (every? #(= :budget-exhausted (:broadcast-refused %))
                  (drop budget verdicts))
          "everything past it is refused on volume alone"))

    (testing "a refused broadcast is downgraded, never dropped"
      (is (every? :delivered verdicts))
      (is (every? #(= :spawner (:routing %)) (drop budget verdicts))
          "it goes by its ordinary route, so the information survives"))

    (testing "a halt is exempt: the budget must not silence a stop-work order"
      (let [halt (shout :halt 99)]
        (is (= :broadcast (:routing halt)))
        (is (nil? (:broadcast-refused halt)))))

    (testing "the budget is per project, so one swarm cannot spend another's"
      (let [other (msg/shout-with-verdict!
                   sender :progress
                   {:message "fresh audience" :broadcast? true
                    :broadcast-reason :shared-discovery
                    :project-id (fresh "budget-project") :deliberate? true})]
        (is (= :broadcast (:routing other)))))))

(deftest a-coordinator-lane-is-not-metered-test
  (let [project (fresh "budget-project")
        coordinator (fresh "coordinator:driver")
        shout (fn [i]
                (msg/shout-with-verdict!
                 coordinator :progress
                 {:message (str "directive " i) :broadcast? true
                  :broadcast-reason :coordinator-directive
                  :project-id project :deliberate? true}))
        verdicts (mapv shout (range (* 2 bp/default-budget)))]
    ;; A coordinator addressing its own swarm is above the readers rather than
    ;; beside them, and its directives are how a wave is driven. Metering it
    ;; would let a busy wave silence its own scheduler.
    (is (every? #(= :broadcast (:routing %)) verdicts))
    (is (every? #(nil? (:broadcast-refused %)) verdicts))))

;; =============================================================================
;; The ledger itself — pure, so these need no pipeline and no clock
;; =============================================================================

(deftest ledger-ages-entries-out-of-the-window-test
  (let [window 1000
        ledger {"p" [100 500 900]}]
    (is (= 3 (bledger/spent ledger "p" 1000 window)))
    (is (= 2 (bledger/spent ledger "p" 1400 window))
        "the entry at 100 has aged out at now=1400")
    (is (= 0 (bledger/spent ledger "p" 2000 window)))
    (testing "the boundary is strict: an entry exactly a window old is gone"
      (is (= 0 (bledger/spent {"p" [100]} "p" 1100 window))))))

(deftest ledger-prunes-itself-empty-test
  (let [window 1000]
    (testing "a project that stops broadcasting stops being tracked"
      (is (= {} (bledger/prune {"p" [100] "q" [200]} 5000 window))
          "no eviction policy is needed, because idle projects empty themselves"))
    (testing "spending prunes on the way through"
      ;; now=5000 with a 1000ms window keeps only stamps after 4000, so the
      ;; entry at 1 is genuinely outside it and 4900 is genuinely inside.
      (let [after (bledger/spend {"stale" [1] "p" [4900]} "p" 5000 window)]
        (is (nil? (get after "stale")) "an idle project is swept by another's traffic")
        (is (= [4900 5000] (get after "p")))))))

(deftest ledger-keeps-projects-apart-test
  (let [window 1000
        ledger (-> {} (bledger/spend "a" 100 window) (bledger/spend "a" 200 window))]
    (is (= 2 (bledger/spent ledger "a" 300 window)))
    (is (= 0 (bledger/spent ledger "b" 300 window))
        "two swarms running at once are two audiences")
    (testing "an unnamed project is metered, not exempted"
      (let [l (bledger/spend {} nil 100 window)]
        (is (= 1 (bledger/spent l nil 200 window)))
        (is (= 1 (bledger/spent l "global" 200 window))
            "blank and \"global\" name the same audience")))))

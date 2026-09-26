;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.swarm.claim.negotiate-test
  "Ling claims through the span registry: all or nothing, no steal, one yield
   request per holder, and the release that answers it.

   Messages are read the way a ling reads them: every hivemind message whose
   :to addresses the reader (audience/addressed-to?), whoever sent it. The
   swarm conn and the agent registry are reached through the seams their
   namespaces expose (`with-test-conn`, `bclear!`), never by redefining vars."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.channel.audience :as aud]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.effects :as effects]
            [hive-mcp.events.handlers :as handlers]
            [hive-mcp.hivemind.core :as hivemind]
            [hive-mcp.swarm.claim.negotiate :as negotiate]
            [hive-mcp.swarm.claim.span :as span]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.datascript.connection :as conn]
            [hive-mcp.swarm.datascript.lings :as lings]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-dsl.bounded-atom :refer [bget bkeys bclear!]]))

;; =============================================================================
;; Fixture
;; =============================================================================

(defn- fixture
  [f]
  (conn/with-test-conn
   (conn/create-conn)
   (fn []
     (bclear! hivemind/agent-registry)
     (negotiate/reset-yield-ledger!)
     (ev/reset-all!)
     (effects/reset-registration!)
     (handlers/reset-registration!)
     (ev/init!)
     (effects/register-effects!)
     (handlers/register-handlers!)
     (try
       (f)
       (finally
         (negotiate/reset-yield-ledger!)
         (bclear! hivemind/agent-registry)
         (ev/reset-all!))))))

(use-fixtures :each fixture)

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- messages-to
  "Every hivemind message addressed to `reader` with `event-type`."
  [reader event-type]
  (->> (bkeys hivemind/agent-registry)
       (mapcat (fn [sender]
                 (map #(assoc % :agent-id sender)
                      (:messages (bget hivemind/agent-registry sender)))))
       (filter #(= event-type (:event-type %)))
       (filter #(aud/addressed-to? reader %))
       vec))

(defn- await-message-to
  [reader event-type pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (or (first (filter pred (messages-to reader event-type)))
          (when (< (System/currentTimeMillis) deadline)
            (Thread/sleep 25)
            (recur))))))

(defn- wait-queue
  "Keys `ling-id` is parked on."
  [ling-id]
  (set (map first
            (ds/q-db (ds/current-db)
                     '[:find ?file :in $ ?ling
                       :where [?w :wait-queue/ling-id ?ling]
                              [?w :wait-queue/file ?file]]
                     ling-id))))

(defn- holder-of [file]
  (:slave-id (queries/get-claims-for-file file)))

(defn- two-lings! []
  (ds/add-slave! "ling-a" {:name "a" :status :working})
  (ds/add-slave! "ling-b" {:name "b" :status :working})
  (ds/add-task! "task-a" "ling-a" {:status :dispatched :files ["src/held.clj"]})
  (ds/add-task! "task-b" "ling-b" {:status :dispatched
                                   :files ["src/free.clj" "src/held.clj"]}))

;; =============================================================================
;; All or nothing
;; =============================================================================

(deftest one-held-file-claims-nothing-and-parks-on-it
  (two-lings!)
  (is (:acquired? (negotiate/acquire-for-ling! "ling-a" ["src/held.clj"]
                                               {:task-id "task-a"})))
  (let [r (negotiate/acquire-for-ling! "ling-b" ["src/free.clj" "src/held.clj"]
                                       {:task-id "task-b"})]
    (testing "refused as a whole"
      (is (false? (:acquired? r)))
      (is (= 0 (:spans-claimed r)))
      (is (= ["ling-a"] (mapv :held-by (:conflicts r)))))
    (testing "the free file was NOT claimed"
      (is (nil? (holder-of "src/free.clj"))))
    (testing "the holder keeps its file"
      (is (= "ling-a" (holder-of "src/held.clj"))))
    (testing "the requester is parked on the held file only"
      (is (= ["src/held.clj"] (:parked r)))
      (is (= #{"src/held.clj"} (wait-queue "ling-b"))))))

(deftest acquire-claims-every-file-with-its-task
  (two-lings!)
  (let [r (negotiate/acquire-for-ling! "ling-b" ["src/free.clj" "src/held.clj"]
                                       {:task-id "task-b"})]
    (is (:acquired? r))
    (is (= 2 (:spans-claimed r)))
    (is (= "ling-b" (holder-of "src/free.clj") (holder-of "src/held.clj")))
    (is (= "task-b" (:task-id (queries/get-claims-for-file "src/free.clj"))))
    (is (empty? (messages-to "ling-a" :claim/yield-request)))))

(deftest ling-claim-files-routes-through-acquire
  (two-lings!)
  (negotiate/acquire-for-ling! "ling-a" ["src/held.clj"] {:task-id "task-a"})
  (let [r (proto/claim-files! (ling/->ling "ling-b" {})
                              ["src/free.clj" "src/held.clj"] "task-b")]
    (is (false? (:acquired? r)) "the refusal is returned, not swallowed")
    (is (nil? (holder-of "src/free.clj")) "no partial claim")
    (is (= #{"src/held.clj"} (wait-queue "ling-b")))
    (is (= 1 (count (messages-to "ling-a" :claim/yield-request))))))

;; =============================================================================
;; No steal
;; =============================================================================

(deftest claim-file-refuses-a-live-foreign-holder
  (two-lings!)
  (lings/claim-file! "src/held.clj" "ling-a" {:task-id "task-a"})
  (let [r (lings/claim-file! "src/held.clj" "ling-b" {:task-id "task-b"})]
    (is (= {:claimed? false :refused :held-by-live-slave
            :file "src/held.clj" :held-by "ling-a"}
           r))
    (is (= "ling-a" (holder-of "src/held.clj")) "the claim was not stolen")))

(deftest claim-file-still-upserts-over-own-and-dead-holders
  (two-lings!)
  (testing "re-claiming one's own key"
    (lings/claim-file! "src/own.clj" "ling-a")
    (is (not (false? (:claimed? (lings/claim-file! "src/own.clj" "ling-a")))))
    (is (= "ling-a" (holder-of "src/own.clj"))))
  (testing "a dead holder does not fence the file off"
    (lings/claim-file! "src/orphan.clj" "ling-a")
    (lings/update-slave! "ling-a" {:slave/status :terminated})
    (lings/claim-file! "src/orphan.clj" "ling-b")
    (is (= "ling-b" (holder-of "src/orphan.clj")))))

;; =============================================================================
;; Yield request
;; =============================================================================

(deftest refusal-sends-one-yield-request-to-the-holder
  (two-lings!)
  (negotiate/acquire-for-ling! "ling-a" ["src/held.clj"] {:task-id "task-a"})
  (let [r    (negotiate/acquire-for-ling! "ling-b" ["src/held.clj"]
                                          {:task-id "task-b"})
        msgs (messages-to "ling-a" :claim/yield-request)
        msg  (first msgs)]
    (is (= [{:holder "ling-a" :file "src/held.clj"}] (:yield-requested r)))
    (is (= 1 (count msgs)))
    (is (= "ling-b" (:agent-id msg)) "sent by the refused ling")
    (is (= {:file "src/held.clj" :requested-by "ling-b" :task-id "task-b"}
           (select-keys (:data msg) [:file :requested-by :task-id])))
    (is (empty? (messages-to "ling-b" :claim/yield-request))
        "the requester is not addressed by its own request")))

(deftest retries-do-not-repeat-the-yield-request
  (two-lings!)
  (negotiate/acquire-for-ling! "ling-a" ["src/held.clj"] {:task-id "task-a"})
  (dotimes [_ 3]
    (negotiate/acquire-for-ling! "ling-b" ["src/held.clj"] {:task-id "task-b"}))
  (is (= 1 (count (messages-to "ling-a" :claim/yield-request))))
  (testing "a request that outlived the ttl may be sent again"
    (negotiate/acquire-for-ling! "ling-b" ["src/held.clj"]
                                 {:task-id "task-b"
                                  :now-ms  (+ (System/currentTimeMillis)
                                              negotiate/yield-ttl-ms 1)})
    (is (= 2 (count (messages-to "ling-a" :claim/yield-request))))))

(deftest yield-request-goes-through-the-notify-port
  (two-lings!)
  (negotiate/acquire-for-ling! "ling-a" ["src/held.clj"] {:task-id "task-a"})
  (let [sent    (atom [])
        record! (fn [& args] (swap! sent conj (vec args)))]
    (negotiate/acquire-for-ling! "ling-b" ["src/held.clj"]
                                 {:task-id "task-b" :notify! record!})
    (is (= [["ling-b" "ling-a" :claim/yield-request]]
           (mapv #(subvec % 0 3) @sent)))))

;; =============================================================================
;; Cross-worktree, forward compatible
;; =============================================================================

(deftest without-worktree-identity-the-behaviour-is-unchanged
  (testing "a hive-agent without default-context yields no ctx"
    (when-not (resolve 'hive-agent.swarm.claim.span/default-context)
      (is (nil? (span/context-for "/tmp")))))
  (testing "assess without a ctx is today's conflicts and no warnings"
    (let [held [(assoc (span/file-span "src/x.clj") :claim/slave "ling-a")]]
      (is (= [] (:warnings (span/assess nil held "src/x.clj" "ling-b" nil))))
      (is (= 1 (count (:conflicts (span/assess nil held "src/x.clj" "ling-b" nil))))))))

;; =============================================================================
;; End to end
;; =============================================================================

(deftest holder-completes-and-the-parked-ling-acquires
  (two-lings!)
  (hivemind/register-agent! "ling-a" {:name "a"})
  (hivemind/register-agent! "ling-b" {:name "b"})
  (is (:acquired? (negotiate/acquire-for-ling! "ling-a" ["src/held.clj"]
                                               {:task-id "task-a"})))
  (let [refused (negotiate/acquire-for-ling! "ling-b" ["src/held.clj"]
                                             {:task-id "task-b"})]
    (is (false? (:acquired? refused)))
    (is (= #{"src/held.clj"} (wait-queue "ling-b")) "B is parked")
    (is (= 1 (count (messages-to "ling-a" :claim/yield-request)))
        "A was asked to yield"))
  (lings/complete-task! "task-a")
  (let [woken (await-message-to "ling-b" :file-available
                                #(= "src/held.clj" (get-in % [:data :file]))
                                5000)]
    ;; Not asserted: the sender. release-claim! (hive-datascript) dispatches
    ;; :claim/file-released without :released-by, so a task-completion
    ;; release is sent as "coordinator".
    (is (some? woken) "B receives :file-available"))
  (let [deadline (+ (System/currentTimeMillis) 5000)]
    (while (and (seq (wait-queue "ling-b"))
                (< (System/currentTimeMillis) deadline))
      (Thread/sleep 25)))
  (is (empty? (wait-queue "ling-b")) "B is off the wait queue once woken")
  (let [retry (negotiate/acquire-for-ling! "ling-b" ["src/held.clj"]
                                           {:task-id "task-b"})]
    (is (:acquired? retry) "B's retry acquires")
    (is (= "ling-b" (holder-of "src/held.clj")))))

(deftest one-release-wakes-every-parked-requester
  (ds/add-slave! "ling-a" {:name "a" :status :working})
  (doseq [id ["ling-b" "ling-c" "ling-d"]]
    (ds/add-slave! id {:name id :status :working}))
  (ds/add-task! "task-a" "ling-a" {:status :dispatched :files ["src/hot.clj"]})
  (negotiate/acquire-for-ling! "ling-a" ["src/hot.clj"] {:task-id "task-a"})
  (doseq [id ["ling-b" "ling-c" "ling-d"]]
    (negotiate/acquire-for-ling! id ["src/hot.clj"] {:task-id (str "task-" id)}))
  (is (= 3 (count (messages-to "ling-a" :claim/yield-request)))
      "one request per requester")
  (lings/complete-task! "task-a")
  (doseq [id ["ling-b" "ling-c" "ling-d"]]
    (is (some? (await-message-to id :file-available
                                 #(= "src/hot.clj" (get-in % [:data :file]))
                                 5000))
        (str id " is woken")))
  (let [winners (filterv #(:acquired? (negotiate/acquire-for-ling!
                                        % ["src/hot.clj"] {:task-id (str "task-" %)}))
                         ["ling-b" "ling-c" "ling-d"])]
    (is (= 1 (count winners)) "exactly one retry wins the file")
    (is (= (first winners) (holder-of "src/hot.clj")))))

(ns hive-mcp.hivemind.messaging-test
  "Integration: cap on :message / :task strings carried in shouts.

   Why: a single oversized shout fans out across (per-agent ring × backbone
   subscribers × N consumers). Observed: a kanban-list dump (~50 entries
   serialized as JSON) emitted as an agent error message bloated every
   downstream context window. Bound at canonical ingestion in shout!.

   Unit-level contract for `cap-message` is covered in cap_test.clj;
   property-level invariants in cap_property_test.clj. This file spies on
   the per-agent ring bucket to verify the bound is enforced at the shout!
   boundary, not just in the pure helper."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.hivemind.messaging :as msg]
            [hive-mcp.hivemind.state :as state]
            [hive-dsl.bounded-atom :refer [bget]]
            [hive-mcp.channel.payload-ref :as pref]
            [hive-mcp.channel.context-store :as ctx-store]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private ellipsis "…")

(defn- read-back-message
  "Pull the most recent :message string the shout! pipeline persisted to the
   per-agent ring."
  [agent-id]
  (let [entry (bget state/agent-registry agent-id)
        last-msg (last (:messages entry))]
    (:message last-msg)))

(defn- big-string [n]
  (apply str (repeat n "x")))

(deftest cap-oversized-message
  (testing "Oversized :message is bounded AND its tail stays recoverable"
    ;; The contract CHANGED 2026-09-15. An over-cap :message used to be
    ;; truncated with an ellipsis, which bounded the cost and DESTROYED the
    ;; tail: nothing anywhere held what was cut. It is now elided to a
    ;; context-store id, which costs the same handful of characters on the
    ;; wire and loses nothing.
    ;;
    ;; The bound is asserted as before. The recovery is asserted too, because
    ;; the bound alone passes for BOTH behaviours, and the whole point of the
    ;; change is the half the bound cannot see.
    (let [agent-id (str "test-shout-cap-" (random-uuid))
          cap msg/default-shout-message-cap
          payload (big-string (* 3 cap))
          _ (msg/shout! agent-id :progress {:message payload})
          stored (read-back-message agent-id)]
      (is (some? stored))
      (is (<= (count stored) cap)
          (str "stored len=" (count stored) ", expected <= " cap))
      (is (.contains ^String stored ellipsis)
          "an elided payload must show that it was cut")
      (let [ref (pref/parse-ref stored)]
        (is (some? ref)
            "an elided payload must carry the id its tail can be fetched by")
        (is (= payload (:data (ctx-store/context-get ref)))
            "and that id must return the ORIGINAL payload, not the visible head")))))

(deftest directed-shout-reports-an-unknown-recipient-test
  ;; Directed addressing has exactly one way to lose a message outright: a `to`
  ;; nobody answers to matches no reader, so it reaches NOBODY, where the
  ;; spawner route would at least have reached the coordinator.
  ;;
  ;; The routing is deliberately NOT changed on this evidence, because the
  ;; roster is authoritative about slaves and silent about coordinator lanes
  ;; and peers registered elsewhere; downgrading on it would break valid
  ;; delivery to catch a typo. So the VERDICT carries the warning, in the same
  ;; tool result the sender reads.
  (testing "an unregistered recipient is reported, and delivery is unchanged"
    (let [v (msg/shout-with-verdict! (str "test-unknown-to-" (random-uuid))
                                     :progress
                                     {:message "peer question" :to "ling-does-not-exist"})]
      (is (true? (:delivered v)))
      (is (= :direct (:routing v)) "the address the sender gave is still honoured")
      (is (= "ling-does-not-exist" (:to-unresolved v))
          "and the sender is told the recipient was not recognised")))
  (testing "a coordinator lane is a legitimate recipient, not an unknown one"
    (let [v (msg/shout-with-verdict! (str "test-coord-to-" (random-uuid))
                                     :progress
                                     {:message "for the coordinator" :to "coordinator:7"})]
      (is (nil? (:to-unresolved v))
          "warning on a coordinator lane would cry wolf on every valid reply")))
  (testing "an undirected shout is never accused of a bad recipient"
    (let [v (msg/shout-with-verdict! (str "test-no-to-" (random-uuid))
                                     :progress {:message "my own status"})]
      (is (= :spawner (:routing v)))
      (is (nil? (:to-unresolved v))))))

(deftest short-message-untouched
  (testing "Sub-cap :message passes through verbatim"
    (let [agent-id (str "test-shout-short-" (random-uuid))
          payload "small payload"
          _ (msg/shout! agent-id :progress {:message payload})
          stored (read-back-message agent-id)]
      (is (= payload stored)))))

(deftest cap-oversized-task
  (testing "Oversized :task is also capped"
    (let [agent-id (str "test-shout-task-cap-" (random-uuid))
          cap msg/default-shout-message-cap
          _ (msg/shout! agent-id :started {:task (big-string (* 2 cap))})
          entry (bget state/agent-registry agent-id)
          last-msg (last (:messages entry))
          stored-task (:task last-msg)]
      (is (<= (count stored-task) cap))
      (is (.endsWith ^String stored-task ellipsis)))))

(deftest non-string-message-bounded
  (testing "Accidental coll-shaped :message is pr-str'd then capped"
    (let [agent-id (str "test-shout-coll-" (random-uuid))
          cap msg/default-shout-message-cap
          payload (vec (repeat 500 {:title "x" :status "todo" :id "y"}))
          _ (msg/shout! agent-id :error {:message payload})
          stored (read-back-message agent-id)]
      (is (some? stored))
      (is (<= (count stored) cap)))))

(deftest nil-message-stays-absent
  (testing "Missing :message key does not synthesize a stored message"
    (let [agent-id (str "test-shout-nil-" (random-uuid))
          _ (msg/shout! agent-id :progress {:task "task only"})
          entry (bget state/agent-registry agent-id)
          last-msg (last (:messages entry))]
      (is (nil? (:message last-msg)))
      (is (= "task only" (:task last-msg))))))

;; ---------------------------------------------------------------------------
;; Empty-shout suppression (kanban-hygiene-2026-04-27 regression)
;;
;; During a bulk-close pass (69 review→done in ~8 min) 11 hivemind shouts
;; were emitted with empty content `[] ()` — zero-payload broadcasts. Root
;; cause: side-effect chains in batch ops occasionally invoke shout! with
;; nil/blank :task + :message + no residual :data.
;;
;; Fix: shout! short-circuits when `empty-shout?` is true, returns false,
;; logs at debug level, and persists nothing to the per-agent ring.
;; ---------------------------------------------------------------------------

(deftest empty-shout-predicate
  (testing "empty-shout? identifies zero-information payloads"
    (is (msg/empty-shout? nil)
        "nil data is empty")
    (is (msg/empty-shout? {})
        "empty map is empty")
    (is (msg/empty-shout? {:task ""})
        "blank :task only is empty")
    (is (msg/empty-shout? {:task "" :message ""})
        "blank :task + :message is empty")
    (is (msg/empty-shout? {:task [] :message ()})
        "empty-vec :task + empty-list :message is empty")
    (is (msg/empty-shout? {:task nil :message nil :directory nil})
        "nil values across reserved keys is empty")
    (is (msg/empty-shout? {:task "" :message "" :extra nil})
        "blank reserved + nil residual is empty"))
  (testing "empty-shout? recognizes any non-blank signal"
    (is (not (msg/empty-shout? {:task "real"}))
        "non-blank :task is not empty")
    (is (not (msg/empty-shout? {:message "hi"}))
        "non-blank :message is not empty")
    (is (not (msg/empty-shout? {:percent 50}))
        "residual :data with value is not empty")
    (is (not (msg/empty-shout? "non-map non-blank"))
        "non-map opaque payload is not empty")))

(deftest bulk-op-empty-shout-suppressed
  (testing "shout! with empty payload returns false and persists nothing"
    (let [agent-id (str "test-empty-shout-" (random-uuid))]
      (is (false? (msg/shout! agent-id :progress nil))
          "nil data -> false")
      (is (false? (msg/shout! agent-id :progress {}))
          "empty map -> false")
      (is (false? (msg/shout! agent-id :progress {:task "" :message ""}))
          "blank task+message -> false")
      (is (false? (msg/shout! agent-id :completed {:task [] :message ()}))
          "the literal `[] ()` payload shape from the kanban-hygiene incident -> false")
      (is (nil? (bget state/agent-registry agent-id))
          "no per-agent ring entry created for any of the suppressed shouts"))))

(deftest non-empty-shout-still-persists
  (testing "shout! with any signal is still persisted"
    (let [agent-id (str "test-real-shout-" (random-uuid))]
      (is (true? (msg/shout! agent-id :progress {:task "do something"})))
      (is (= ["do something"]
             (mapv :task (:messages (bget state/agent-registry agent-id))))
          "real-task shout reaches the per-agent ring"))))

(deftest bulk-close-suppression-cap
  (testing "Bulk emission of empty shouts leaves agent ring empty"
    (let [agent-id (str "test-bulk-empty-" (random-uuid))]
      (dotimes [_ 11]
        (msg/shout! agent-id :completed {:task "" :message "" :data nil}))
      (is (nil? (bget state/agent-registry agent-id))
          "11 empty shouts (matching the observed 2026-04-27 incident count) -> no ring entries"))))

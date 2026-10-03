(ns hive-mcp.agent.attribution-test
  "Who a write is attributed to: the verified id, else the transport-stamped
   caller id, else the legacy value. A model-supplied agent_id never moves
   attribution once a caller id is stamped."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.context.request :as ctx]
            [hive-mcp.memory.temporal :as temporal]
            [hive-mcp.tools.kg.commands :as kg-commands]
            [hive-mcp.tools.memory.crud.write :as write]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private build-entry-tags #'write/build-entry-tags)
(def ^:private op->edge-spec #'kg-commands/op->edge-spec)
(def ^:private mutation-attribution #'temporal/mutation-attribution)

(defn- coordinator-ctx [caller]
  {:agent-id "coordinator" :caller-id caller
   :identity {:caller-id caller :claimant :coordinator :verified? false}})

(defn- verified-ling-ctx [id]
  {:agent-id id :caller-id id
   :identity {:caller-id id :claimant :spawned :verified? true}})

(deftest attribution-resolution
  (testing "the verified id wins"
    (is (= {:id "ling-a" :verified? true :source :verified}
           (ctx/attribution {:verified-id "ling-a" :caller-id "ling-b" :legacy-id "x"}))))
  (testing "else the transport-stamped caller id, unverified"
    (is (= {:id "coordinator:42" :verified? false :source :caller}
           (ctx/attribution {:caller-id "coordinator:42" :legacy-id "forged"}))))
  (testing "a model-supplied agent_id does not change attribution when a caller id is stamped"
    (is (= (ctx/attribution {:caller-id "coordinator:42" :legacy-id "coordinator"})
           (ctx/attribution {:caller-id "coordinator:42" :legacy-id "someone-else"}))))
  (testing "legacy path: no caller id, the legacy value as given"
    (is (= {:id "coordinator" :verified? false :source :legacy}
           (ctx/attribution {:legacy-id "coordinator"})))
    (is (= {:id nil :verified? false :source :legacy}
           (ctx/attribution {:verified-id " " :caller-id "" :legacy-id nil})))))

(deftest attribution-tags-and-created-by
  (testing "unverified caller: session tag only"
    (is (= ["agent-session:coordinator:42"]
           (ctx/attribution-tags (ctx/attribution {:caller-id "coordinator:42"})))))
  (testing "verified: session tag and the verified marker"
    (is (= ["agent-session:ling-a" ctx/verified-tag]
           (ctx/attribution-tags (ctx/attribution {:verified-id "ling-a"})))))
  (testing "legacy: no tags added"
    (is (= [] (ctx/attribution-tags (ctx/attribution {:legacy-id "coordinator"})))))
  (testing "created-by: attributed id, else the legacy value unchanged"
    (is (= "agent:coordinator:42"
           (ctx/attribution-created-by (ctx/attribution {:caller-id "coordinator:42"}) "agent:x")))
    (is (= "system:batch"
           (ctx/attribution-created-by (ctx/attribution {:legacy-id "c"}) "system:batch")))))

(deftest current-attribution-reads-the-request
  (testing "outside a request it is the legacy id"
    (is (= :legacy (:source (ctx/current-attribution "coordinator")))))
  (testing "an unverified identity never counts as verified"
    (ctx/with-request-context (coordinator-ctx "coordinator:7")
      (is (= {:id "coordinator:7" :verified? false :source :caller}
             (ctx/current-attribution "forged")))))
  (testing "a verified spawn credential"
    (ctx/with-request-context (verified-ling-ctx "ling-a")
      (is (= {:id "ling-a" :verified? true :source :verified}
             (ctx/current-attribution "forged"))))))

(deftest memory-add-tags
  (let [tags (fn [agent-id]
               (build-entry-tags [] agent-id (ctx/current-attribution agent-id) {} nil))]
    (testing "coordinator session: the role tag stays and the session tag is added"
      (ctx/with-request-context (coordinator-ctx "coordinator:7")
        (let [t (set (tags "coordinator"))]
          (is (contains? t "agent:coordinator"))
          (is (contains? t "agent-session:coordinator:7"))
          (is (not (contains? t ctx/verified-tag))))))
    (testing "a model-supplied agent_id does not change the session tag"
      (ctx/with-request-context (coordinator-ctx "coordinator:7")
        (let [t (set (tags "impostor"))]
          (is (contains? t "agent-session:coordinator:7"))
          (is (not-any? #(= "agent-session:impostor" %) t)))))
    (testing "verified ling: agent:<slave-id> stays and the verified marker is added"
      (ctx/with-request-context (verified-ling-ctx "ling-a")
        (let [t (set (tags "ling-a"))]
          (is (contains? t "agent:ling-a"))
          (is (contains? t "agent-session:ling-a"))
          (is (contains? t ctx/verified-tag)))))
    (testing "legacy: no caller id, tags exactly as before"
      (is (= ["agent:coordinator" "scope:project:"] (tags "coordinator"))))))

(deftest kg-edge-created-by
  (testing "a stamped caller id overrides the created_by argument"
    (ctx/with-request-context (coordinator-ctx "coordinator:7")
      (is (= "agent:coordinator:7"
             (:created-by (op->edge-spec {:from "a" :to "b" :relation "refines"
                                          :created_by "agent:forged"}))))))
  (testing "legacy: created_by as given, absent when not given"
    (is (= "agent:x" (:created-by (op->edge-spec {:from "a" :to "b" :relation "refines"
                                                  :created_by "agent:x"}))))
    (is (not (contains? (op->edge-spec {:from "a" :to "b" :relation "refines"})
                        :created-by)))))

(deftest temporal-mutation-attribution
  (testing "a stamped caller id overrides the :agent-id option"
    (ctx/with-request-context (coordinator-ctx "coordinator:7")
      (is (= {:id "coordinator:7" :verified? false} (mutation-attribution "forged")))))
  (testing "verified"
    (ctx/with-request-context (verified-ling-ctx "ling-a")
      (is (= {:id "ling-a" :verified? true} (mutation-attribution nil)))))
  (testing "legacy: the :agent-id option as given"
    (is (= {:id "ling-x" :verified? false} (mutation-attribution "ling-x")))))

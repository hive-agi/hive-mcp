(ns hive-mcp.channel.a2a-trifecta-test
  "Trifectas for the A2A envelope vocabulary (`hive-mcp.channel.a2a`), driven
   by its contracts. The relations restate the docstrings: the closed state
   sets, the event mapping and the shout <-> Message round trip."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.channel.a2a :as a2a]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private StateLike
  (into [:enum nil "TASK_STATE_BOGUS" :completed] (sort a2a/task-states)))

(hst/deftrifecta-from-schema terminal-state-contract
  hive-mcp.channel.a2a/terminal-state?
  {:in        StateLike
   :out       :boolean
   :rel       (fn [s out]
                (= out (contains? #{"TASK_STATE_COMPLETED" "TASK_STATE_FAILED"
                                    "TASK_STATE_CANCELED" "TASK_STATE_REJECTED"} s)))
   :n-cases   14
   :golden-path "test/golden/channel/a2a-terminal-state.edn"})

(hst/deftrifecta-from-schema interrupted-state-contract
  hive-mcp.channel.a2a/interrupted-state?
  {:in        StateLike
   :out       :boolean
   :rel       (fn [s out]
                (= out (contains? #{"TASK_STATE_INPUT_REQUIRED" "TASK_STATE_AUTH_REQUIRED"} s)))
   :n-cases   14})

(hst/deftrifecta-from-schema event-task-state-contract
  hive-mcp.channel.a2a/event->task-state
  {:in        [:enum :started "progress" :completed "error" :failed "aborted" :cancelled
               "ask" :shout "weird" nil 42]
   :out       a2a/TaskState
   ;; an unknown event, nil included, is WORKING
   :rel       (fn [ev out]
                (let [k (when ev (if (keyword? ev) (name ev) (str ev)))]
                  (= out (get {"started" "TASK_STATE_SUBMITTED" "completed" "TASK_STATE_COMPLETED"
                               "error" "TASK_STATE_FAILED" "failed" "TASK_STATE_FAILED"
                               "aborted" "TASK_STATE_CANCELED" "cancelled" "TASK_STATE_CANCELED"
                               "ask" "TASK_STATE_INPUT_REQUIRED"}
                              k "TASK_STATE_WORKING"))))
   :n-cases   16
   :golden-path "test/golden/channel/a2a-event-task-state.edn"})

(hst/deftrifecta-from-schema text-part-contract
  hive-mcp.channel.a2a/text-part
  {:in        [:or :string :int :nil :keyword]
   :out       a2a/TextPart
   :rel       (fn [in out] (= (str in) (:text out)))})

(hst/deftrifecta-from-schema data-part-contract
  hive-mcp.channel.a2a/data-part
  {:in        [:map-of :keyword :int]
   :out       a2a/DataPart
   :rel       (fn [in out] (= in (:data out)))})

(def ^:private Part
  [:or a2a/TextPart a2a/DataPart [:map [:kind [:enum "file"]]]])

(hst/deftrifecta-from-schema part-text-contract
  hive-mcp.channel.a2a/part-text
  {:in        [:maybe Part]
   :out       [:maybe :string]
   :rel       (fn [in out] (= out (when (= "text" (:kind in)) (:text in))))})

(hst/deftrifecta-from-schema parts-text-contract
  hive-mcp.channel.a2a/parts-text
  {:in        [:vector {:max 4} Part]
   :out       :string
   ;; text parts joined by newline, every other part contributing nothing
   :rel       (fn [parts out]
                (= out (str/join "\n" (keep #(when (= "text" (:kind %)) (:text %)) parts))))
   :num-tests 200})

(hst/deftrifecta-from-schema new-message-id-contract
  hive-mcp.channel.a2a/new-message-id
  {:in        [:cat]
   :out       [:string {:min 40 :max 40}]
   :rel       (fn [_ out] (and (str/starts-with? out "msg-") (uuid? (parse-uuid (subs out 4)))))})

(hst/deftrifecta-from-schema new-context-id-contract
  hive-mcp.channel.a2a/new-context-id
  {:in        [:cat]
   :out       [:string {:min 43 :max 43}]
   :rel       (fn [_ out] (and (str/starts-with? out "a2actx-") (uuid? (parse-uuid (subs out 7)))))})

(def ^:private ShoutIn
  "Shouts that reach every optional field. :shout-id is always given so the
   golden is deterministic; a missing one mints a random messageId, checked in
   round-trip-test."
  [:map
   [:agent-id [:enum "ling-1" "coordinator:7"]]
   [:event-type [:enum :progress "completed" :error]]
   [:message {:optional true} [:enum "did a thing" "" "  "]]
   [:task {:optional true} [:enum "task-9" ""]]
   [:shout-id [:enum "msg-fixed" "msg-other"]]
   [:to {:optional true} [:enum "ling-2"]]
   [:context-id {:optional true} [:enum "a2actx-c"]]
   [:broadcast? {:optional true} [:enum true]]
   [:broadcast-reason {:optional true} [:enum :halt]]
   [:deliberate? {:optional true} [:enum true]]
   [:ref {:optional true} [:enum "ctx-1-0"]]
   [:timestamp {:optional true} [:enum 1700000000000]]
   [:project-id {:optional true} [:enum "hive"]]])

(defn- non-blank [s] (when (and (string? s) (not (str/blank? s))) s))

(hst/deftrifecta-from-schema hive-to-a2a-contract
  hive-mcp.channel.a2a/hive->a2a
  {:in        ShoutIn
   :out       a2a/Message
   ;; the body is the message, else the task; routing rides in metadata
   :rel       (fn [in out]
                (and (= (or (non-blank (:message in)) (non-blank (:task in)) "")
                         (get-in out [:parts 0 :text]))
                     (= [a2a/extension-uri] (:extensions out))
                     (= (:to in) (get-in out [:metadata a2a/meta-to]))
                     (= (boolean (:broadcast? in)) (contains? (:metadata out) a2a/meta-broadcast))
                     (= (non-blank (:task in)) (:taskId out))))
   :num-tests 200
   :golden-path "test/golden/channel/a2a-hive-to-a2a.edn"})

(hst/deftrifecta-from-schema a2a-to-hive-contract
  hive-mcp.channel.a2a/a2a->hive
  {:in        [:map
               [:messageId [:enum "m-1"]]
               [:taskId {:optional true} [:enum "t-1"]]
               [:contextId {:optional true} [:enum "c-1"]]
               [:parts [:vector {:max 3} Part]]
               [:metadata {:optional true}
                [:map-of [:enum "hive.swarm/from" "hive.swarm/to" "hive.swarm/event"
                          "hive.swarm/deliberate"] [:enum "x" true]]]]
   :out       a2a/ShoutBack
   :rel       (fn [in out]
                (and (= (:messageId in) (:shout-id out))
                     (= (get-in in [:metadata "hive.swarm/event"] "progress") (:event-type out))
                     (= (:taskId in) (:task out))))
   :num-tests 200
   :golden-path "test/golden/channel/a2a-a2a-to-hive.edn"})

(deftest round-trip-test
  (testing "every field that round-trips comes back"
    (let [shout {:agent-id "ling-1" :event-type "progress" :message "hello" :task "t"
                 :shout-id "msg-1" :to "ling-2" :context-id "a2actx-1" :broadcast? true
                 :broadcast-reason "halt" :deliberate? true :ref "ctx-1" :timestamp 5
                 :project-id "hive"}]
      (is (= shout (a2a/a2a->hive (a2a/hive->a2a shout))))))
  (testing "a shout with no id is given a fresh messageId"
    (is (m/validate [:string {:min 40 :max 40}] (:messageId (a2a/hive->a2a {:agent-id "a"})))))
  (testing "a foreign message with no hive metadata still yields a usable shout"
    (is (= {:agent-id nil :event-type "progress" :message "" :shout-id nil}
           (a2a/a2a->hive nil)))))

(deftest a2a-schemas-generate-test
  (doseq [s [a2a/TaskState a2a/TextPart a2a/DataPart a2a/Shout a2a/Message a2a/ForeignMessage
             a2a/ShoutBack]]
    (is (seq (mg/sample s {:size 3})) (pr-str (m/form s)))))

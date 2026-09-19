;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.agora.dialogue-push-test
  "Agora reaches the UI clients through hive-spi's IFrontendPush port and
   names no hive-mcp transport.

   The collaborator is a recording port installed in the real slot. The
   fixture puts back exactly what it found: the adapter that was installed,
   or an empty slot when none was."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-spi.swarm.ports.messaging :as push]
            [hive-mcp.agora.dialogue :as dialogue]))

(def ^:private pushed (atom []))

(defn- recording-port
  "A messaging port whose frontend push records into `pushed`. The slot
   admits only an IEventBus, so the bus half is present and inert."
  []
  (reify
    push/IEventBus
    (publish! [_ _event] nil)
    (subscribe! [_ _event-type] nil)
    (unsubscribe! [_ _event-type _ch] nil)

    push/IFrontendPush
    (broadcast! [_ _msg] nil)
    (emit! [_ event-type data]
      (swap! pushed conj [event-type data])
      nil)
    (frontend-status [_]
      {:channel-connected? false :ws-connected? false :ws-clients 0})))

(defn- with-recording-port [f]
  (let [found (when (push/messaging-set?) (push/get-messaging))]
    (reset! pushed [])
    (dialogue/reset-dialogues!)
    (push/set-messaging! (recording-port))
    (try
      (f)
      (finally
        (if found
          (push/set-messaging! found)
          (push/clear-messaging!))))))

(use-fixtures :each with-recording-port)

(defn- pushed-types [] (mapv first @pushed))

(deftest the-dialogue-lifecycle-is-pushed-through-the-port
  (let [id (dialogue/create-dialogue {:participants ["writer" "critic"]
                                      :topic "review"})]
    (testing "creation"
      (is (= [[:agora/created {:dialogue-id id
                               :participants ["writer" "critic"]
                               :topic "review"}]]
             @pushed)))
    (testing "a join and a leave each push one event carrying the dialogue id"
      (dialogue/join-dialogue id "mediator")
      (dialogue/leave-dialogue id "mediator")
      (is (= [:agora/created :agora/participant-joined :agora/participant-left]
             (pushed-types)))
      (is (every? #(= id (:dialogue-id (second %))) @pushed)))))

(deftest an-operation-that-changes-nothing-pushes-nothing
  (is (false? (dialogue/join-dialogue "no-such-dialogue" "someone")))
  (is (= [] @pushed)))

(deftest with-no-adapter-installed-the-dialogue-still-works
  (push/clear-messaging!)
  (let [id (dialogue/create-dialogue {:participants ["a" "b"] :topic "t"})]
    (is (string? id))
    (is (true? (dialogue/join-dialogue id "c")))
    (is (= [] @pushed))))

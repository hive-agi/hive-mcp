(ns hive-mcp.agora.participants-swarm-test
  "join-dialogue / leave-dialogue against the real agora store, through the
   schema shim's add-participant! / remove-participant! (hive-agent). No
   with-redefs: the store is the addon's own conn, reset per test."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agora.dialogue :as dialogue]
            [hive-mcp.agora.schema :as schema]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (schema/reset-conn!) (f)))

(defn- members [id] (set (:participants (schema/get-dialogue id))))

(deftest the-shim-delegates-membership-to-the-store
  (let [{:keys [id]} (schema/create-dialogue! {:participants ["a" "b"]})]
    (is (some? (:db-after (schema/add-participant! id "c"))))
    (is (= #{"a" "b" "c"} (members id)))
    (is (some? (:db-after (schema/remove-participant! id "a"))))
    (is (= #{"b" "c"} (members id)))
    (testing "an unknown dialogue answers nil"
      (is (nil? (schema/add-participant! "no-such-dialogue" "x")))
      (is (nil? (schema/remove-participant! "no-such-dialogue" "x"))))))

(deftest join-adds-the-participant-to-the-dialogue
  (let [id (dialogue/create-dialogue {:participants ["writer" "critic"] :topic "t"})]
    (is (true? (dialogue/join-dialogue id "mediator")))
    (is (= #{"writer" "critic" "mediator"} (members id)))
    (is (false? (dialogue/join-dialogue "no-such-dialogue" "mediator")))))

(deftest leave-removes-the-participant-and-aborts-below-two
  (let [id (dialogue/create-dialogue {:participants ["a" "b" "c"] :topic "t"})]
    (is (true? (dialogue/leave-dialogue id "c")))
    (is (= #{"a" "b"} (members id)))
    (is (= :active (:status (schema/get-dialogue id))) "two remain: still active")
    (is (true? (dialogue/leave-dialogue id "b")))
    (is (= #{"a"} (members id)))
    (is (= :aborted (:status (schema/get-dialogue id))) "one remains: aborted")
    (is (false? (dialogue/leave-dialogue "no-such-dialogue" "a")))))

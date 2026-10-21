(ns hive-mcp.hivemind.shout-resolution-test
  "Shout project labelling and the slave-status write gate.

   project: explicit > vessel > the slave row's :slave/project-id >
   directory > global. status: written only when it changes."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.hivemind.messaging :as msg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest project-priority-chain-test
  (is (= "explicit" (msg/resolve-shout-project-id "explicit" "vessel" "slave" nil)))
  (is (= "vessel" (msg/resolve-shout-project-id nil "vessel" "slave" nil)))
  (testing "a ling with no vessel context is labelled with its slave row's project"
    (is (= "slave" (msg/resolve-shout-project-id nil nil "slave" nil))))
  (is (= "global" (msg/resolve-shout-project-id nil nil nil nil))))

(deftest status-write-gate-test
  (is (nil? (msg/status-write-needed {:slave/status :working} :working))
      "unchanged status is not rewritten")
  (is (= :idle (msg/status-write-needed {:slave/status :working} :idle)))
  (is (= :working (msg/status-write-needed {} :working)))
  (is (nil? (msg/status-write-needed {:slave/status :working} nil))))

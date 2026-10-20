(ns hive-mcp.tools.swarm.channel-contract-test
  "Contract facets for the JVM event journal and dispatched-task registry:
   what is recorded reads back by (str task-id), and a clear forgets it."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.swarm.channel :as ch]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn journal-round-trip
  "Subject: record RESULT for TASK-ID, read it back by the string id."
  [[task-id result]]
  (ch/clear-event-journal!)
  (ch/record-task-result! task-id result)
  (let [read (ch/check-event-journal (str task-id))]
    (ch/clear-event-journal!)
    read))

(deftrifecta-from-schema recorded-results-read-back-by-string-id
  #'journal-round-trip
  {:in [:tuple [:or [:enum "t-1" "t-2"] [:int {:min 0 :max 9}] [:enum :k1]]
        [:map [:status [:enum "completed" "error"]] [:n [:int {:min 0 :max 5}]]]]
   :out :map
   :rel (fn [[_ result] out] (= result out))
   :num-tests 60})

(golden/deftest-golden channel-registry-golden
  "test/golden/hive-mcp/tools/swarm/channel-registry.edn"
  (do (ch/clear-dispatched-tasks!)
      (let [before (ch/dispatched-task? "golden-1")
            _      (ch/record-dispatched-task! "golden-1")
            after  (ch/dispatched-task? "golden-1")
            _      (ch/clear-dispatched-tasks!)
            gone   (ch/dispatched-task? "golden-1")]
        {:before before :after after :cleared gone
         :unknown-journal (ch/check-event-journal "never-recorded")})))

(deftest stopping-without-subscriptions-is-a-noop
  (is (nil? (ch/stop-channel-subscriptions!))))

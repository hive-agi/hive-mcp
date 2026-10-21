(ns hive-mcp.tools.swarm.collect-contract-test
  "Contract facets for swarm collect: the chat-point projection, and an
   immediate journal hit answered without any polling host."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.swarm.channel :as ch]
            [hive-mcp.tools.swarm.collect :as col]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema chat-point-projects-to-snake-case
  #'col/chat-point->response
  {:in [:cat col/ChatPoint]
   :out col/ChatPointResponse
   :rel (fn [[cp] out] (= out {:run_id (:run-id cp) :tip (:tip cp)
                               :resumable (boolean (:resumable? cp))}))
   :num-tests 100})

(defn collect-recorded
  "Subject: collect a task whose result is already in the journal."
  [status]
  (ch/record-task-result! "collect-contract-1" {:status status :result "done"})
  (let [out (col/handle-swarm-collect {:task_id "collect-contract-1" :timeout_ms 10})]
    (ch/clear-event-journal!)
    out))

(deftrifecta-from-schema a-journal-hit-is-answered-at-once
  #'collect-recorded
  {:in [:enum "completed" "error"]
   :out [:map [:type [:= "text"]] [:text :string]]
   :rel (fn [_ out] (= "collect-contract-1" (get (json/read-str (:text out)) "task_id")))
   :num-tests 20})

(golden/deftest-golden collect-chat-point-golden
  "test/golden/hive-mcp/tools/swarm/collect-chat-point.edn"
  {:full  (col/chat-point->response {:run-id "r" :tip 2 :resumable? true})
   :empty (col/chat-point->response {})})

(deftest an-unknown-task-is-an-error
  (is (true? (:isError (col/handle-swarm-collect {:task_id "nope-m29" :timeout_ms 10})))))

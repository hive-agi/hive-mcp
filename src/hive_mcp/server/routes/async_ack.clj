(ns hive-mcp.server.routes.async-ack
  "The acknowledgement an async:true tool call returns at once.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def result-via
  "Where the caller finds the result of a queued call."
  "---TOOLRESULT--- block on your next hive call; pass async:false to get the result inline")

(defn ack
  "Ack map for a queued call: {:queued true :task-id :tool :result-via}, plus
   :timeout-ms when given and :durable false when the submission journal
   could not be written."
  [task-id tool-name timeout-ms durable?]
  (cond-> {:queued true :task-id task-id :tool tool-name :result-via result-via}
    timeout-ms     (assoc :timeout-ms timeout-ms)
    (not durable?) (assoc :durable false)))

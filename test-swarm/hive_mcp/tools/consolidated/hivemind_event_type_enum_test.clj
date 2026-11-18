;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.tools.consolidated.hivemind-event-type-enum-test
  "With the swarm addon on the classpath the hivemind tool MUST advertise the
   event-type enum. The addon-free tree (test/.../multi_tool_integration_test)
   can only say 'the vocabulary or nothing', since it also runs where the
   vocabulary's owner is absent; this is the half that needs the owner."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.consolidated.hivemind :as hivemind]
            [hive-mcp.hivemind.event-registry :as er]))

(deftest hivemind-advertises-the-event-type-enum
  (testing "event_type carries :enum, and it is the swarm's vocabulary"
    (let [event-type (get-in hivemind/tool-def [:inputSchema :properties "event_type"])]
      (is (contains? event-type :enum)
          "the addon is on this classpath, so the enum must not be omitted")
      ;; The vocabulary's owner is the event registry; it grew the ling exit
      ;; events truncated and context-death (hive-agent 18b2ee8).
      (is (= (set (er/mcp-enum)) (set (:enum event-type)))
          "the advertised enum is the registry's MCP-visible vocabulary")
      (is (= #{"progress" "completed" "error" "blocked" "started"
               "truncated" "context-death"}
             (set (:enum event-type)))))))

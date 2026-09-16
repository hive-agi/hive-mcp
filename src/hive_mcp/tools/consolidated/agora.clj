(ns hive-mcp.tools.consolidated.agora
  "Consolidated Agora dialogue CLI tool."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.agora :as agora-handlers]))

(def canonical-handlers
  "The `agora` verbs, stored as VARS so a reload of the agora handler namespace
   reaches this table (20260817195749-0d407e9c)."
  {:dialogue       #'agora-handlers/handle-agora-create-dialogue
   :dispatch       #'agora-handlers/handle-agora-dispatch
   :consensus      #'agora-handlers/handle-agora-check-consensus
   :list           #'agora-handlers/handle-agora-list-dialogues
   :join           #'agora-handlers/handle-agora-join-dialogue
   :history        #'agora-handlers/handle-agora-get-history})

(def handlers
  canonical-handlers)

(def handle-agora
  (make-cli-handler #'handlers))

(def tool-def
  {:name "agora"
   :consolidated true
   :description "Agora ling dialogue system: dialogue (create), dispatch (send message), consensus (check Nash equilibrium), list (all dialogues), join (add participant), history (transcript). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["dialogue" "dispatch" "consensus" "list" "join" "history" "help"]
                                         :description "Agora operation to perform"}
                              "participants" {:type "array"
                                              :items {:type "string"}
                                              :description "Vector of ling slave-ids (min 2)"}
                              "topic" {:type "string"
                                       :description "Dialogue topic"}
                              "config" {:type "object"
                                        :description "Optional: {threshold, timeout-ms}"}
                              "dialogue_id" {:type "string"
                                             :description "Dialogue ID"}
                              "to" {:type "string"
                                    :description "Target ling slave-id"}
                              "message" {:type "string"
                                         :description "Message content"}
                              "signal" {:type "string"
                                        :enum ["propose" "counter" "approve" "no-change" "defer"]
                                        :description "Explicit signal"}
                              "from" {:type "string"
                                      :description "Sender slave-id"}
                              "timeout_ms" {:type "number"
                                            :description "Dispatch timeout"}
                              "files" {:type "array"
                                       :items {:type "string"}
                                       :description "Related files"}
                              "status" {:type "string"
                                        :enum ["active" "consensus" "timeout" "aborted"]
                                        :description "Filter by status"}
                              "slave_id" {:type "string"
                                          :description "Slave-id to join"}
                              "limit" {:type "integer"
                                       :description "Limit to last N turns"}}
                 :required ["command"]}
   :handler #'handle-agora})

(def tools [tool-def])

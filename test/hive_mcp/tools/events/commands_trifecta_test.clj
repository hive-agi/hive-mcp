(ns hive-mcp.tools.events.commands-trifecta-test
  "Trifectas over the `events` tool. Every subcommand reaches hive-agent
   lazily; in this JVM hive-agent is absent, so the refusal and
   missing-param arms are what is pinned: each must answer the EventsReply
   envelope with an :error naming the problem, never throw."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.tools.core :as tc]
            [hive-mcp.tools.events.commands :as cmd]
            [hive-mcp.tools.events.core :as core]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn missing-param-reply
  "Unary adapter: run subcommand K over empty params, so every one refuses on
   its first required param."
  [k]
  ((get {:enable cmd/handle-enable :tail cmd/handle-tail :dump cmd/handle-dump
         :stats cmd/handle-stats :register-handler cmd/handle-register-handler
         :unreg-fx cmd/handle-unreg-fx}
        k)
   {}))

(deftrifecta missing-params-are-refused
  hive-mcp.tools.events.commands-trifecta-test/missing-param-reply
  {:golden-path "test/golden/tools/events/missing-param-replies.edn"
   :cases       {:enable :enable :tail :tail :dump :dump :stats :stats
                 :register-handler :register-handler :unreg-fx :unreg-fx}
   :gen         (gen/elements [:enable :tail :dump :stats :register-handler :unreg-fx])
   :pred        #(and (m/validate cmd/EventsReply %) (false? (:ok %)) (not (str/blank? (:error %))))
   :num-tests   30
   :mutations   [["claims-success" (fn [k] {:ok true :command k :result nil})]
                 ["wrong-command" (fn [_] {:ok false :command :help :error "x"})]]})

(defn routed
  "Unary adapter: route COMMAND through the MCP entrypoint and read the reply."
  [command]
  (let [resp (core/handle {:command command})]
    {:isError (boolean (:isError resp))
     :body    (json/read-str (:text resp) :key-fn keyword)}))

(deftrifecta handle-routes-every-command
  hive-mcp.tools.events.commands-trifecta-test/routed
  {:golden-path "test/golden/tools/events/handle-routing.edn"
   :cases       {:help "help" :none nil :unknown "m27-nope" :tail "tail" :colon ":dump"}
   :xf          #(update % :body dissoc :result)
   :gen         (gen/elements ["help" "tail" "dump" "stats" "unreg-fx" "bogus" nil])
   :pred        #(= (:isError %) (false? (get-in % [:body :ok])))
   :num-tests   40
   :mutations   [["never-error" (fn [c] (assoc (routed c) :isError false))]
                 ["always-help" (fn [_] (routed "help"))]]})

(deftest help-answers-the-text
  (let [r (cmd/handle-help {})]
    (is (= :help (:command r)))
    (is (str/includes? (:result r) "register-handler"))))

(deftest every-mcp-answer-is-an-mcp-response
  (doseq [c ["help" "enable" "disable" "tail" "stats" "nope"]]
    (is (m/validate tc/McpResponse (core/handle {:command c})) c)))

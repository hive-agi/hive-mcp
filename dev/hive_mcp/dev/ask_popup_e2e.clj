(ns hive-mcp.dev.ask-popup-e2e
  "Human-in-the-loop end to end: a hivemind ask with context, asked through
   core's messaging/ask!, pops up natively via hive-agent and hive-notify on
   this display. Type an answer; it prints the asker's result and the shouts
   that carried it to the spawner and the cc'd agent.

   Needs hive-agent and hive-notify on the classpath and a display:
     DISPLAY=:0 clojure -Sdeps '{:deps {io.github.hive-agi/hive-agent {:local/root \"../hive-agent\"}
                                        io.github.hive-agi/hive-notify {:local/root \"../hive-notify\"}}
                               :aliases {:e2e {:extra-paths [\"dev\"]}}}' \\
       -M:test:e2e -m hive-mcp.dev.ask-popup-e2e"
  (:require [hive-mcp.hivemind.messaging :as messaging]
            [hive-mcp.swarm.datascript.queries :as queries]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn run
  "Ask once and return {:result .. :shouts [..]}. The swarm registry and the
   shout transport are stubbed; everything between them is real."
  []
  (let [shouts (atom [])]
    (with-redefs [messaging/shout! (fn [from event data]
                                     (swap! shouts conj {:from from :event event
                                                         :to (:to data) :answer (:answer data)
                                                         :message (:message data)})
                                     true)
                  queries/get-slave-by-name-or-id (fn [_] {:slave/parent "coordinator"})]
      {:result (select-keys (messaging/ask! "ling-e2e" "Which schema should the migration target?" nil
                                            :context "v1 has 3 consumers. v2 has none yet but is the plan."
                                            :cc ["ling-b"]
                                            :timeout-ms 180000)
                            [:answer :by :timeout])
       :shouts @shouts})))

(defn -main [& _]
  (let [{:keys [result shouts]} (run)]
    (println "E2E-RESULT" (pr-str result))
    (doseq [s shouts] (println "E2E-SHOUT" (pr-str s))))
  (shutdown-agents)
  (System/exit 0))

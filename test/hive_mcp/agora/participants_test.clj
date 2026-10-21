(ns hive-mcp.agora.participants-test
  "Dialogue membership belongs to the agora store's owner,
   hive-agent.swarm.agora.schema. hive-mcp reaches it through the schema shim
   (add-participant! / remove-participant!) and never opens the agora
   DataScript conn itself. Runs without hive-agent: the behaviour with the
   addon lives in test-swarm/hive_mcp/agora/participants_swarm_test.clj."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.agora.dialogue :as dialogue]
            [hive-mcp.agora.schema :as schema]
            [hive-mcp.swarm.delegate :as delegate]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- required-namespaces [file]
  (let [form (read-string (slurp (io/file file)))]
    (->> form
         (filter #(and (seq? %) (= :require (first %))))
         (mapcat rest)
         (map #(if (sequential? %) (first %) %))
         set)))

(deftest dialogue-does-not-reach-into-datascript
  (is (not (contains? (required-namespaces "src/hive_mcp/agora/dialogue.clj") 'datascript.core))
      "join/leave go through hive-mcp.agora.schema, not d/transact! on the agora conn"))

(deftest the-schema-shim-carries-the-membership-verbs
  (is (fn? schema/add-participant!))
  (is (fn? schema/remove-participant!)))

(deftest membership-fails-loudly-without-the-addon
  (if (delegate/available? "hive-agent.swarm.agora.schema")
    (is true "hive-agent is on the classpath; test-swarm exercises membership")
    (doseq [[label call] {"add-participant!"    #(schema/add-participant! "d" "ling-1")
                          "remove-participant!" #(schema/remove-participant! "d" "ling-1")
                          "join-dialogue"       #(dialogue/join-dialogue "d" "ling-1")
                          "leave-dialogue"      #(dialogue/leave-dialogue "d" "ling-1")}]
      (testing label
        (let [e (try (call) nil (catch clojure.lang.ExceptionInfo e e))]
          (is (true? (:swarm/addon-missing (ex-data e))))
          (is (= "hive-agent" (:addon (ex-data e)))))))))

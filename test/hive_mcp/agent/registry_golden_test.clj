(ns hive-mcp.agent.registry-golden-test
  "Goldens over the agent and tool registries of `hive-mcp.agent.registry`.
   The tool surface is injected through the `*advertised-tools*` port, and
   agents are stub records implementing `IAgent`, so no live tool surface,
   addon registry or swarm store is read."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.agent.protocol :refer [IAgent]]
            [hive-mcp.agent.registry :as reg]
            [hive-test.golden :refer [deftest-golden-fn]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- stub-surface []
  [{:name "read_file" :description "Read." :handler identity
    :inputSchema {:type "object" :properties {"path" {:type "string"}}}}
   {:name "memory" :description "Memory." :consolidated true :handler identity
    :inputSchema {:type "object" :properties {"command" {:type "string"}}}}])

(defn- reset-all [f]
  (binding [reg/*advertised-tools* (constantly [])] (reg/refresh!))
  (reg/clear-agents!)
  (try (f)
       (finally
         (binding [reg/*advertised-tools* (constantly [])] (reg/refresh!))
         (reg/clear-agents!))))

(use-fixtures :each reset-all)

(defrecord StubAgent [id kind status]
  IAgent
  (spawn! [this _] this)
  (dispatch! [_ _] nil)
  (status [_] {:status status})
  (kill! [_] {:killed? true})
  (agent-type [_] kind)
  (can-chain-tools? [_] false)
  (claims [_] [])
  (claim-files! [_ _ _] nil)
  (release-claims! [_] nil))

(deftest-golden-fn tool-registry-golden
  "test/golden/agent/registry-tools.edn"
  (fn []
    (binding [reg/*advertised-tools* stub-surface]
      (let [n (reg/refresh!)]
        (reg/register! [{:name "extra" :description "x" :handler identity}])
        {:refreshed  n
         :tools      (sort (reg/list-tools))
         :has-handler (fn? (:handler (reg/get-tool "read_file")))
         :missing    (reg/get-tool "nope")
         :schemas    (sort-by :name (reg/get-schemas ["read_file" "memory"]))
         :filtered   (mapv :name (reg/get-schemas ["extra"]))
         :reseat     (do (reg/refresh!) (sort (reg/list-tools)))}))))

(deftest-golden-fn agent-registry-golden
  "test/golden/agent/registry-agents.edn"
  (fn []
    (let [ids [(reg/register-agent! (->StubAgent "ling-1" :ling :idle))
               (reg/register-agent! (->StubAgent "drone-1" :drone :working))
               (reg/register-agent! (->StubAgent "ling-2" :ling :working))]]
      {:registered ids
       :all        (sort (map :id (reg/list-all-agents)))
       :lings      (sort (map :id (reg/list-lings)))
       :by-id      (into {} (reg/get-agent-by-id "drone-1"))
       :unregister (reg/unregister-agent! "ling-1")
       :after      (sort (map :id (reg/list-all-agents)))
       :missing    (reg/get-agent-by-id "ling-1")
       :cleared    (do (reg/clear-agents!) (reg/list-all-agents))})))

(deftest refresh-with-an-empty-surface-test
  (binding [reg/*advertised-tools* (constantly [])]
    (is (zero? (reg/refresh!)))
    (is (empty? (reg/list-tools)))))

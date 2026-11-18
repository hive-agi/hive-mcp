(ns hive-mcp.tools.agent.spawn-routed-report-test
  "The spawn response and its preflight report the opts actually USED.

   The :spawn.opts-overlay/<key> extension is a port: a router registers a
   contributor there and may replace :model and :provider. The handler must
   route BEFORE it preflights, logs and answers, so a `tier cheap` spawn
   reports the routed member rather than the requested one — and it must route
   exactly once, or the router's cursor advances twice per spawn.

   The router here is a plain fn registered under the port key, held to the
   contributor contract (f opts spawn-ctx -> opts'): no with-redefs of the
   subject. The terminal is the shipped recording stub addon, so the spawn runs
   its real pipeline and the test can read the opts the backend received."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.ling.start-preflight :as preflight]
            [hive-mcp.agent.ling.terminal-registry :as treg]
            [hive-mcp.agent.provider.collect :as provider-collect]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.isolation-methods]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.test.stub.terminal-addon :as term-stub]
            [hive-mcp.tools.agent.spawn :as spawn]
            [hive-spi.swarm.guards :as guards]
            [hive-test.isolation :as iso]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private router-key :spawn.opts-overlay/router)

(def ^:private routed
  "The member the stub router picks — deliberately not the host default."
  {:provider "venice" :model "deepseek-v4-flash"
   :pool :cheap :strategy :weighted :work-class :grunt})

(defn- stub-router
  "Contributor honouring the port contract: it reads the caller's own request
   from spawn-ctx :request (opts :model already holds the host's seed) and
   routes a cheap tier, leaving everything else alone."
  [opts spawn-ctx]
  (if (= "cheap" (:tier (:request spawn-ctx)))
    (assoc opts
           :model (:model routed)
           :provider (:provider routed)
           :router/choice routed)
    opts))

(def ^:private calls (atom 0))

(defn- counting-router
  "stub-router, counting invocations: one spawn must route once."
  [opts spawn-ctx]
  (swap! calls inc)
  (stub-router opts spawn-ctx))

(defn- host-fixture
  "Isolated swarm store, an open spawn guard, a host model default (the test
   config declares none), and a recording stub :claude terminal so neither the
   editor nor ~/.claude.json decides what runs."
  [f]
  (logic/reset-db!)
  (reset! calls 0)
  (let [stub (term-stub/register-terminal! :claude)]
    (try
      (with-redefs [guards/child-ling? (constantly false)
                    provider-collect/agent-type-defaults
                    (constantly {:provider :anthropic :model "claude-test-model"})]
        (binding [spawn/*editor-reachable?* (constantly true)
                  preflight/*folder-trust*
                  (reify preflight/IFolderTrust
                    (trusted-roots [_] #{"/tmp/project"})
                    (checkout-root [_ _] nil))]
          (f)))
      (finally
        (treg/deregister-terminal! :claude)
        (logic/reset-db!)))))

(defn- with-router [f]
  (ext/register! router-key counting-router)
  (try (f) (finally (ext/deregister! router-key))))

(use-fixtures :each (iso/with-isolations :swarm-ds) host-fixture)

(defn- spawn!*
  "Spawn through the handler and answer {:res .. :body .. :opts ..}, where
   :opts is what the backend was handed."
  [params stub]
  (let [res (spawn/handle-spawn
             (merge {:type "ling" :cwd "/tmp/project" :spawn_mode "claude"}
                    params))]
    (is (not (:isError res)) (str "spawn failed: " (:text res)))
    {:res res
     :body (when-not (:isError res) (json/read-str (:text res) :key-fn keyword))
     :opts (some-> (last (term-stub/calls-of stub :spawn!)) second)}))

(deftest a-cheap-spawn-reports-the-routed-model-not-the-requested-one
  (with-router
    (fn []
      (let [stub (treg/get-terminal-addon :claude)
            {:keys [body opts]} (spawn!* {:name "ling-r" :tier "cheap"
                                          :model "requested-model"
                                          :_caller_id "coordinator:s1"} stub)]
        (testing "the response names the member that runs"
          (is (= "deepseek-v4-flash" (:model body)))
          (is (= "venice" (:provider body)))
          (is (= "cheap" (:tier body))))
        (testing "the seed and the decision stay readable next to it"
          (is (true? (:routed? body)))
          (is (= "claude-test-model" (:requested-model body)))
          ;; the response is JSON, so the seed's keyword provider reads as a string
          (is (= "anthropic" (:requested-provider body)))
          (is (= {:pool "cheap" :strategy "weighted" :work-class "grunt"}
                 (:router-choice body))))
        (testing "the backend was handed the routed model too, not just the report"
          (is (= "deepseek-v4-flash" (:model opts)))
          (is (= "venice" (:provider opts))))))))

(deftest one-spawn-routes-exactly-once
  (with-router
    (fn []
      (spawn!* {:name "ling-once" :tier "cheap"} (treg/get-terminal-addon :claude))
      (is (= 1 @calls)
          "route in the handler; spawn! must not route the same spawn again"))))

(deftest without-a-router-nothing-is-claimed-to-be-routed
  (let [stub (treg/get-terminal-addon :claude)
        {:keys [body opts]} (spawn!* {:name "ling-plain"} stub)]
    (is (false? (:routed? body)))
    (is (nil? (:router-choice body)))
    (is (= "claude-test-model" (:model body)) "the seed stands")
    (is (= "claude-test-model" (:model opts)))))

(deftest an-explicit-frontier-model-survives-the-router
  (with-router
    (fn []
      (let [{:keys [body]} (spawn!* {:name "ling-f" :tier "frontier"
                                     :model "claude-opus-5-5"}
                                    (treg/get-terminal-addon :claude))]
        (is (false? (:routed? body)) "the stub only routes a cheap tier")
        (is (= "claude-opus-5-5" (:model body)))))))

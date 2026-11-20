(ns hive-mcp.tools.agent.spawn-handle-test
  "Characterization of handle-spawn through its existing ports, not mocks of its
   implementation. These cases pin the MCP response, the options crossing the
   spawn boundary, and the principal refusal and failure branches."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.ling.terminal-registry :as terminal-registry]
            [hive-mcp.system.heap-admission :as heap-admission]
            [hive-mcp.agent.ling.start-preflight :as preflight]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.source :as config-source]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.isolation-methods]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.test.stub.headless-backend :as headless-stub]
            [hive-mcp.test.stub.terminal-addon :as term-stub]
            [hive-mcp.tools.agent.spawn :as spawn]
            [hive-test.isolation :as iso]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private base-request
  {:type "ling"
   :name "qh3-ling"
   :cwd "/tmp/project"
   :spawn_mode "claude"
   :provider "anthropic"
   :model "claude-test-model"})

(defn- host-fixture [f]
  (logic/reset-db!)
  (binding [spawn/*editor-reachable?* (constantly true)
            preflight/*folder-trust*
            (reify preflight/IFolderTrust
              (trusted-roots [_] #{"/tmp/project"})
              (checkout-root [_ _] nil))]
    (try
      (term-stub/with-terminal f)
      (finally (logic/reset-db!)))))

(use-fixtures :each (iso/with-isolations :swarm-ds) host-fixture)

(defn- body [response]
  (json/read-str (:text response) :key-fn keyword))

(deftest successful-spawn-preserves-response-and-routed-options
  (let [events (atom [])
        router (fn [opts ctx]
                 (swap! events conj [:overlay (:request ctx)])
                 (assoc opts :model "routed-model" :provider "routed-provider"
                        :router/choice {:pool :cheap :strategy :weighted :work-class :grunt}))
        request (assoc base-request
                       :task "Preserve this brief"
                       :_caller_id "coordinator:qh3"
                       :presets ["coding" "testing"]
                       :project_id "qh3-characterization"
                       :max_budget_usd 2.5
                       :kg_compress false
                       :verbose false
                       :sliding_window_size 4
                       :token_budget 128
                       :llm_retries 2
                       :sandbox false)]
    (ext/register! :spawn.opts-overlay/qh3-characterization router)
    (try
      (let [response (spawn/handle-spawn request)
            actual (body response)
            terminal-call (last (term-stub/calls-of
                                 (terminal-registry/get-terminal-addon :claude) :spawn!))
            [_terminal-ctx terminal-opts] terminal-call
            [event overlay-request] (first @events)]
        (is (not (:isError response)) (:text response))
        (is (= {:success true
                :agent-id "qh3-ling"
                :type "ling"
                :parent "coordinator:qh3"
                :spawn-mode "claude"
                :provider "routed-provider"
                :model "routed-model"
                :tier nil
                :routed? true
                :requested-model "claude-test-model"
                :requested-provider "anthropic"
                :token-budget 128
                :task-attached true
                :cwd "/tmp/project"
                :presets ["coding" "testing"]
                :project-id "qh3-characterization"
                :router-choice {:pool "cheap" :strategy "weighted" :work-class "grunt"}}
               actual))
        (is (= :overlay event))
        (is (= request overlay-request))
        (is (= 1 (count @events)))
        (testing "the original request and routed choice cross the boundary in order"
          (is (= "Preserve this brief" (:task terminal-opts)))
          (is (= "coordinator:qh3" (:parent terminal-opts)))
          (is (= "coordinator:qh3" (:spawned-by terminal-opts)))
          (is (= "routed-model" (:model terminal-opts)))
          (is (= "routed-provider" (:provider terminal-opts)))
          (is (= 2.5 (:max-budget-usd terminal-opts)))
          (is (false? (contains? terminal-opts :spawn/request)))
          (is (false? (contains? terminal-opts :spawn/opts-routed)))))
      (finally
        (ext/deregister! :spawn.opts-overlay/qh3-characterization)))))

(deftest absent-brief-has-the-idle-warning
  (let [response (spawn/handle-spawn base-request)
        actual (body response)]
    (is (not (:isError response)) (:text response))
    (is (false? (:task-attached actual)))
    (is (str/includes? (:warning actual) "no task"))))

(deftest unsupported-resume-is-refused-before-the-backend
  (let [response (spawn/handle-spawn
                  (assoc base-request :resume {:run_id "run-qh3"}))]
    (is (:isError response))
    (is (str/includes? (:text response) "resume needs a chat-point capable headless backend"))
    (is (empty? (term-stub/calls-of
                 (terminal-registry/get-terminal-addon :claude) :spawn!)))))

(deftest resume-without-brief-runs-on-a-chat-point-backend
  (let [backend (headless-stub/->backend :hive-agent)]
    (headless-stub/with-backend :hive-agent backend
      (hive-mcp.agent.ling.headless-registry/register-headless!
       :hive-agent backend {:capabilities #{:chat-points}})
      (let [response (spawn/handle-spawn
                      (assoc base-request :spawn_mode "headless"
                             :resume {:run_id "run-qh3"}))
            actual (body response)
            [backend-ctx] (first (headless-stub/calls-of backend :spawn!))]
        (is (not (:isError response)) (:text response))
        (is (true? (:task-attached actual)))
        (is (not (contains? actual :warning)))
        (is (= "hive-agent" (:spawn-mode actual)))
        (is (= :hive-agent (:spawn-mode backend-ctx)))))))

(deftest default-headless-chain-uses-the-resolved-backend-mode
  (let [backend (headless-stub/->backend :hive-agent)]
    (headless-stub/with-backend :hive-agent backend
      (hive-mcp.agent.ling.headless-registry/register-headless!
       :hive-agent backend {:capabilities #{:chat-points}})
      (binding [config/*config-source*
                (config-source/static-source
                 {:ling {:default-spawn-mode :headless}
                  :headless {:default-backend :auto}
                  :transports {:spawn {:chain [:headless]}}})]
        (let [response (spawn/handle-spawn (dissoc base-request :spawn_mode))
              actual (body response)
              [backend-ctx] (first (headless-stub/calls-of backend :spawn!))]
          (is (not (:isError response)) (:text response))
          (is (= "hive-agent" (:spawn-mode actual)))
          (is (= :hive-agent (:spawn-mode backend-ctx))))))))

(deftest heap-pressure-refusal-is-the-complete-response
  (let [probes (atom 0)]
    (ext/register! :agent/heap-probe
                   (fn [_watermarks]
                     (swap! probes inc)
                     {:level :hard :basis :post-gc :heap-pct 94 :now-pct 97
                      :post-gc-pct 94 :pool "G1 Old Gen" :watermarks {:hard 0.92}}))
    (try
      (let [response (spawn/handle-spawn {:type "not-a-type"})
            actual (body response)]
        (is (not (:isError response)))
        (is (= 1 @probes))
        (is (= {:success false :refused true :reason "heap-pressure" :level "hard"
                :basis "post-gc" :heap_post_gc_pct 94 :heap_now_pct 97
                :hard_watermark_pct nil :retry_after_s heap-admission/retry-after-s
                :brief_kept false}
               (dissoc actual :message)))
        (is (str/includes? (:message actual) "brief was NOT kept")))
      (finally (ext/deregister! :agent/heap-probe)))))

(deftest invalid-requests-are-refused-without-reaching-the-backend
  (doseq [[request fragment]
          [[{:type "not-a-type"} "type must be one of:"]
           [(assoc base-request :spawn_mode "not-a-mode") "spawn_mode must be one of:"]
           [(assoc base-request :task "one" :prompt "two") "task and prompt disagree"]
           [(assoc base-request :sandbox "darkmatter") "sandbox must be true or false"]
           [(assoc base-request :token_budget 0) "token_budget must be a positive integer"]]]
    (let [response (spawn/handle-spawn request)]
      (is (:isError response) (pr-str request))
      (is (str/includes? (:text response) fragment))
      (is (empty? (term-stub/calls-of (terminal-registry/get-terminal-addon :claude) :spawn!))))))

(deftest backend-exception-is-translated-at-the-handler-boundary
  (let [stub (term-stub/register-terminal! :claude {:spawn! "backend exploded"})
        response (spawn/handle-spawn base-request)]
    (is (:isError response))
    (is (= "Failed to spawn ling: backend exploded" (:text response)))
    (is (= 1 (count (term-stub/calls-of stub :spawn!))))))

(deftest the-request-cwd-reaches-the-backend-and-the-registry
  (let [response (spawn/handle-spawn (assoc base-request :task "x"))
        [terminal-ctx] (last (term-stub/calls-of
                              (terminal-registry/get-terminal-addon :claude) :spawn!))]
    (is (not (:isError response)) (:text response))
    (is (= "/tmp/project" (:cwd terminal-ctx)))
    (is (= "/tmp/project" (:slave/cwd (queries/get-slave "qh3-ling"))))))

(deftest the-request-cwd-reaches-a-headless-backend
  (let [backend (headless-stub/->backend :hive-agent)]
    (headless-stub/with-backend :hive-agent backend
      (let [response (spawn/handle-spawn
                      (assoc base-request :spawn_mode "headless" :task "x"))
            [backend-ctx] (first (headless-stub/calls-of backend :spawn!))]
        (is (not (:isError response)) (:text response))
        (is (= "/tmp/project" (:cwd backend-ctx)))))))

(deftest an-untrusted-cwd-is-refused-before-the-backend
  (let [response (spawn/handle-spawn (assoc base-request :cwd "/tmp/untrusted"))]
    (is (:isError response))
    (is (str/includes? (str (:text response)) "has not trusted"))
    (is (empty? (term-stub/calls-of
                 (terminal-registry/get-terminal-addon :claude) :spawn!)))))

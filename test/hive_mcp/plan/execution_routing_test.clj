(ns hive-mcp.plan.execution-routing-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [hive-mcp.plan.schema :as schema]
            [hive-mcp.plan.tool :as plan]
            [hive-mcp.tools.memory-kanban :as kanban]
            [hive-mcp.tools.consolidated.workflow.spawn :as forge]
            [hive-mcp.tools.agent.spawn :as spawn]
            [hive-mcp.tools.consolidated.workflow.readiness :as ready]
            [hive-mcp.scheduler.dag-waves :as dag]
            [hive-mcp.scheduler.task-dispatch :as task-dispatch]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.agent.ling.lifecycle :as lifecycle]
            [hive-mcp.agent.ling.headless-registry :as headless]
            [hive-mcp.agent.ling.terminal-registry :as terminal]))

(use-fixtures :each
  (fn [f]
    (with-redefs [lifecycle/resolve-effective-mode :spawn-mode
                  headless/get-headless-backend (fn [mode] (when (= :headless mode) :test-api-backend))
                  headless/headless-metadata (constantly {:provides #{:openai :anthropic}})
                  terminal/get-terminal-addon (fn [mode] (when (= :claude mode) :test-claude))]
      (f))))

(def execution {:provider "openai" :model "test-model" :presets ["saa"]
                :persona {:priority-tags ["security"]}})

(deftest plan-metadata-survives-kanban-boundary
  (let [request (atom nil)
        step (schema/normalize-step {:id "s1" :title "Implement"
                                     :files ["src/a.clj"] :tags ["saa" "wave:0"]
                                     :execution execution})]
    (is (schema/valid-step? step))
    (with-redefs [kanban/handle-mem-kanban-create
                  (fn [params] (reset! request params) {:text "{\"id\":\"t1\"}"})]
      (is (= {:ok "t1"} (#'plan/create-kanban-task! step "/tmp/project" :wave 0))))
    (is (= ["saa" "wave:0"] (:tags @request)))
    (is (= {:plan-step-id "s1" :files ["src/a.clj"] :execution execution}
           (:context @request)))
    (let [task (json/read-str (json/write-str {:context (:context @request)})
                              :key-fn keyword)
          params (#'forge/make-spawn-params
                  {:task task :model "default-model" :default-presets ["ling"]
                   :agent-name "test-agent" :route :headless :spawn-mode-kw :headless})]
      (is (= (select-keys execution [:provider :model :presets])
             (select-keys params [:provider :model :presets]))))))

(defn- created-card
  "Run `create-kanban-task!` for STEP against a capturing kanban create and
   answer the card the board would hold: the request's tags and context."
  [step]
  (let [request (atom nil)]
    (with-redefs [kanban/handle-mem-kanban-create
                  (fn [params] (reset! request params) {:text "{\"id\":\"t1\"}"})]
      (is (= {:ok "t1"} (#'plan/create-kanban-task! step "/tmp/project" :wave 0))))
    (assoc (select-keys @request [:tags :context]) :id "t1")))

(deftest plan-step-tier-and-work-class-route-the-dag-spawn
  (testing "an :execution :tier / :work-class is valid and becomes tier: / work-class: tags"
    (let [step (schema/normalize-step {:id "s1" :title "Implement" :tags ["wave:0"]
                                       :execution {:tier :cheap :work-class :mechanical}})
          card (created-card step)]
      (is (schema/valid-step? step))
      (is (some #{"tier:cheap"} (:tags card)))
      (is (some #{"work-class:mechanical"} (:tags card)))
      (is (= {:tier :cheap :work-class :mechanical} (dag/task-routing card)))
      (testing "the tags alone route: a slim board row carries no :context"
        (is (= {:tier :cheap :work-class :mechanical}
               (dag/task-routing (dissoc card :context)))))
      (testing "the DAG spawn hands the router the same request"
        (let [task (merge {:task-id "t1" :title "Implement" :prompt "p"}
                          (dag/task-routing card))]
          (is (= {:tier :cheap :work-class :mechanical}
                 (:spawn/request (task-dispatch/create-ling-opts task {:cwd "/tmp"}))))))))
  (testing "string values are accepted and written as lower-case tags"
    (let [step (schema/normalize-step {:id "s2" :title "T"
                                       :execution {:tier "Cheap" :work-class "docs"}})
          card (created-card step)]
      (is (schema/valid-step? step))
      (is (some #{"tier:cheap"} (:tags card)))
      (is (some #{"work-class:docs"} (:tags card)))
      (is (= {:tier :cheap :work-class :docs} (dag/task-routing card)))))
  (testing "a step naming neither writes neither tag, and keeps its own tags"
    (let [card (created-card (schema/normalize-step {:id "s3" :title "T" :tags ["saa"]}))]
      (is (= ["saa" "wave:0"] (:tags card)))
      (is (= {} (dag/task-routing card))))))

(deftest persona-precedes-first-session-catchup
  (let [events (atom [])
        params (atom nil)]
    (with-redefs [ext/get-extension (fn [k]
                                     (case k
                                       :agent/register-persona-lens (fn [id lens] (swap! events conj [id lens]))
                                       :agent/unregister-persona-lens (fn [_] nil)
                                       nil))
                  spawn/handle-spawn (fn [p]
                                       (is (= [["test-agent" (:persona execution)]] @events))
                                       (reset! params p)
                                       {:text "{\"agent-id\":\"test-agent\"}"})
                  ready/wait-for-ling-ready (fn [& _] {:ready? true})]
      (#'forge/spawn-and-wait!
       {:agent-name "test-agent" :task {:context {:execution execution}}
        :route :headless :spawn-mode-kw :headless})
      (is (= "openai" (:provider @params)))))
  (with-redefs [ext/get-extension (constantly nil)
                spawn/handle-spawn (fn [_] (throw (Exception. "must not spawn")))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Persona catchup provider unavailable"
                         (#'forge/spawn-and-wait!
                          {:agent-name "test-agent"
                           :task {:context {:execution execution}}
                           :route :headless :spawn-mode-kw :headless})))))

(deftest explicit-provider-cannot-silently-launch-claude
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Provider is incompatible"
        (#'forge/make-spawn-params
          {:task {:context {:execution {:provider "openai"}}}
           :route :claude :spawn-mode-kw :claude})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no registered strategy"
        (#'forge/make-spawn-params
          {:task {:context {:execution {:spawn-mode "missing-backend"}}}
           :route :headless :spawn-mode-kw :headless})))
  (is (= "headless"
         (:spawn_mode (#'forge/make-spawn-params
                        {:task {:context {:execution {:provider "openai" :spawn-mode "headless"}}}
                         :route :claude :spawn-mode-kw :claude})))))

(deftest failed-spawn-removes-persona-registration
  (let [lenses (atom {})]
    (with-redefs [ext/get-extension
                  (fn [k] (case k
                            :agent/register-persona-lens #(swap! lenses assoc %1 %2)
                            :agent/unregister-persona-lens #(swap! lenses dissoc %)
                            nil))
                  spawn/handle-spawn (constantly {:isError true :text "failed"})]
      (is (thrown? clojure.lang.ExceptionInfo
            (#'forge/spawn-and-wait! {:agent-name "failed-test" :route :headless
                                     :spawn-mode-kw :headless
                                     :task {:context {:execution execution}}})))
      (is (empty? @lenses)))))

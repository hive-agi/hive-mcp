(ns hive-mcp.agent.session-agent-id-test
  "The id a request speaks as: a coordinator session answers as its own
   `coordinator:<session>`, never as the role every coordinator shares."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.tools.consolidated.session :as session]))

(deftest session-agent-id-resolution
  (testing "a specific agent id wins over the caller id"
    (is (= "swarm-ling-1" (ctx/session-agent-id "swarm-ling-1" "swarm-ling-1:4242")))
    (is (= "swarm-ling-1" (ctx/session-agent-id "swarm-ling-1" "coordinator:4242"))))
  (testing "the bare coordinator role resolves to the session's caller id"
    (is (= "coordinator:4242" (ctx/session-agent-id "coordinator" "coordinator:4242")))
    (is (= "coordinator:4242" (ctx/session-agent-id nil "coordinator:4242")))
    (is (= "coordinator:4242" (ctx/session-agent-id "  " "coordinator:4242"))))
  (testing "two sessions never share an id"
    (is (not= (ctx/session-agent-id "coordinator" "coordinator:1")
              (ctx/session-agent-id "coordinator" "coordinator:2"))))
  (testing "without a caller id the agent id is returned as given"
    (is (= "coordinator" (ctx/session-agent-id "coordinator" nil)))
    (is (= "coordinator" (ctx/session-agent-id "coordinator" "")))
    (is (nil? (ctx/session-agent-id nil nil)))
    (is (nil? (ctx/session-agent-id "" " ")))))

(deftest current-session-agent-id-reads-args-then-context
  (testing "args win over the bound request context"
    (ctx/with-request-context {:agent-id "coordinator" :caller-id "coordinator:7"}
      (is (= "coordinator:7" (ctx/current-session-agent-id)))
      (is (= "coordinator:9" (ctx/current-session-agent-id {:_caller_id "coordinator:9"})))
      (is (= "ling-a" (ctx/current-session-agent-id {:agent_id "ling-a"})))))
  (testing "outside a request there is nothing to answer with"
    (is (nil? (ctx/current-session-agent-id)))))

(defn- whoami [args]
  (-> (session/handle-whoami (assoc args :directory "/tmp/test"))
      :text
      (json/read-str :key-fn keyword)
      :agent-id))

(deftest whoami-answers-with-the-session-identity
  (testing "a coordinator session is named by its caller id"
    (is (= "coordinator:4242" (whoami {:agent_id "coordinator" :_caller_id "coordinator:4242"})))
    (is (= "coordinator:4242" (whoami {:_caller_id "coordinator:4242"}))))
  (testing "two coordinator sessions answer differently"
    (is (not= (whoami {:agent_id "coordinator" :_caller_id "coordinator:1"})
              (whoami {:agent_id "coordinator" :_caller_id "coordinator:2"}))))
  (testing "a ling keeps its own id"
    (is (= "swarm-ling-1" (whoami {:agent_id "swarm-ling-1" :_caller_id "swarm-ling-1:99"}))))
  (testing "a caller with no session stamp keeps the role"
    (is (= "coordinator" (whoami {:agent_id "coordinator"})))))

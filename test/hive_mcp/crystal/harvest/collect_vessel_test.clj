(ns hive-mcp.crystal.harvest.collect-vessel-test
  "Tests for the three legacy Emacs harvest fns driving hive-vessel ops
   through the :vessel :dispatch capability (stub, per commit ea7523a1's
   pattern; the fns are deprecated but kept for on-kanban-done fallback).

   Contract: {:success true :result \"<json>\"} => a successful (empty)
   harvest; {:success false :error \"x\"} => the fn's existing error shape
   (harvest-session-progress / harvest-git-commits carry {:error ...} and a
   :system/error telemetry dispatch; harvest-completed-tasks folds the
   emacs failure into its :error harvest-error map).

   No with-redefs on hive-mcp.emacs-ext — the emacs client is out of the
   path entirely; only the cheap ctx/scope/ds/crystal helpers are redefined
   to pin the inputs the fns derive from the environment."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.crystal.core :as crystal]
            [hive-mcp.crystal.harvest.collect :as collect]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.test.stub.swarm-host :as sh]))

(defn- notes-host
  "A vessel stub answering :crystal/session-notes with ENVELOPE, recording ops."
  [envelope]
  (sh/answering {:crystal/session-notes envelope}))

(defn- kanban-host
  "A vessel stub answering :crystal/kanban-notes with ENVELOPE, recording ops."
  [envelope]
  (sh/answering {:crystal/kanban-notes envelope}))

(defn- git-host
  "A vessel stub answering :crystal/git-commits with ENVELOPE, recording ops."
  [envelope]
  (sh/answering {:crystal/git-commits envelope}))

(deftest harvest-session-progress-dispatches-session-notes-op
  (testing "harvest-session-progress emits {:op :crystal/session-notes :project-id pid} at 12000ms"
    (with-redefs [ctx/current-directory (constantly "/tmp/hive")
                  scope/get-current-project-id (constantly "proj-1")
                  crystal/session-tag (constantly "sess-1")]
      (sh/with-swarm-host
        [host (notes-host {:success true
                           :result (json/write-str [{:note "n1"} {:note "n2"}])
                           :timed-out false})]
        (let [r (#'collect/harvest-session-progress {:directory "/tmp/hive"})]
          (is (= 2 (:count r)))
          (is (= "sess-1" (:session r)))
          (is (= "proj-1" (:project-id r)))
          (is (= [[{:op :crystal/session-notes :project-id "proj-1"} 12000]]
                 (sh/calls-of host :crystal/session-notes))
              "exact op map (project-id bound) and timeout"))))))

(deftest harvest-session-progress-no-project-id-op
  (testing "no directory => project-id nil => op carries no :project-id key"
    (with-redefs [ctx/current-directory (constantly nil)]
      (sh/with-swarm-host
        [host (notes-host {:success true :result "[]" :timed-out false})]
        (let [r (#'collect/harvest-session-progress nil)]
          (is (zero? (:count r)))
          (is (nil? (:project-id r)))
          (is (= [[{:op :crystal/session-notes} 12000]]
                 (sh/calls-of host :crystal/session-notes))
              "op map is exactly {:op :crystal/session-notes}"))))))

(deftest harvest-session-progress-failure-envelope
  (testing "{:success false :error \"x\"} => empty notes plus the harvest-error shape"
    (with-redefs [ctx/current-directory (constantly "/tmp/hive")
                  scope/get-current-project-id (constantly "proj-1")]
      (sh/with-swarm-host
        [host (notes-host {:success false :result nil :error "x" :timed-out false})]
        (let [r (#'collect/harvest-session-progress {:directory "/tmp/hive"})]
          (is (= 0 (:count r)))
          (is (= [] (:notes r)))
          (is (= {:type :harvest-failed
                  :fn "harvest-session-progress"
                  :msg "x"}
                 (:error r))
              "existing :harvest-failed error map"))))))

(deftest harvest-completed-tasks-dispatches-kanban-notes-op
  (testing "harvest-completed-tasks emits {:op :crystal/kanban-notes :project-id pid} at 15000ms"
    (with-redefs [ctx/current-directory (constantly "/tmp/hive")
                  scope/get-current-project-id (constantly "proj-1")
                  ds/get-completed-tasks-this-session (fn [& _] [])]
      (sh/with-swarm-host
        [host (kanban-host {:success true
                            :result (json/write-str [{:task-id "t1"}])
                            :timed-out false})]
        (let [r (#'collect/harvest-completed-tasks {:directory "/tmp/hive"})]
          (is (= 1 (:emacs-count r)))
          (is (= 1 (:count r)))
          (is (= "proj-1" (:project-id r)))
          (is (= [[{:op :crystal/kanban-notes :project-id "proj-1"} 15000]]
                 (sh/calls-of host :crystal/kanban-notes))
              "exact op map (project-id bound) and timeout"))))))

(deftest harvest-completed-tasks-no-project-id-op
  (testing "no directory => op carries no :project-id key"
    (with-redefs [ctx/current-directory (constantly nil)
                  ds/get-completed-tasks-this-session (fn [& _] [])]
      (sh/with-swarm-host
        [host (kanban-host {:success true :result "[]" :timed-out false})]
        (let [r (#'collect/harvest-completed-tasks nil)]
          (is (zero? (:count r)))
          (is (nil? (:project-id r)))
          (is (= [[{:op :crystal/kanban-notes} 15000]]
                 (sh/calls-of host :crystal/kanban-notes))
              "op map is exactly {:op :crystal/kanban-notes}"))))))

(deftest harvest-completed-tasks-failure-envelope
  (testing "{:success false :error \"x\"} => emacs failure folded into the error map"
    (with-redefs [ctx/current-directory (constantly "/tmp/hive")
                  scope/get-current-project-id (constantly "proj-1")
                  ds/get-completed-tasks-this-session (fn [& _] [])]
      (sh/with-swarm-host
        [host (kanban-host {:success false :result nil :error "x" :timed-out false})]
        (let [r (#'collect/harvest-completed-tasks {:directory "/tmp/hive"})]
          (is (= 0 (:emacs-count r)))
          (is (= {:type :harvest-failed
                  :fn "harvest-completed-tasks"
                  :msg "emacs: x"}
                 (:error r))
              "existing error shape: ds ok, emacs failed"))))))

(deftest harvest-git-commits-dispatches-git-commits-op
  (testing "harvest-git-commits emits {:op :crystal/git-commits :directory dir} at 10000ms"
    (with-redefs [ctx/current-directory (constantly "/tmp/hive")
                  crystal/get-session-start (constantly nil)]
      (sh/with-swarm-host
        [host (git-host {:success true
                         :result "abc123 fix thing\ndef456 other thing"
                         :timed-out false})]
        (let [r (#'collect/harvest-git-commits {:directory "/tmp/hive"})]
          (is (= 2 (:count r)))
          (is (= "/tmp/hive" (:directory r)))
          (is (= [[{:op :crystal/git-commits :since "midnight" :directory "/tmp/hive"} 10000]]
                 (sh/calls-of host :crystal/git-commits))
              "exact op map (directory bound, no session start => :since midnight)"))))))

(deftest harvest-git-commits-no-dir-op
  (testing "no directory => op carries no :directory key"
    (with-redefs [ctx/current-directory (constantly nil)
                  crystal/get-session-start (constantly nil)]
      (sh/with-swarm-host
        [host (git-host {:success true :result "" :timed-out false})]
        (let [r (#'collect/harvest-git-commits nil)]
          (is (zero? (:count r)))
          (is (= [[{:op :crystal/git-commits :since "midnight"} 10000]]
                 (sh/calls-of host :crystal/git-commits))
              "op map is exactly {:op :crystal/git-commits :since \"midnight\"}"))))))

(deftest harvest-git-commits-uses-recorded-session-start
  (testing "a recorded session start becomes :since <the instant's .toString>"
    (let [agent "vessel-test-agent"]
      (try
        (crystal/reset-session-start! agent)
        (let [start (crystal/record-session-start! agent)]
          (with-redefs [ctx/current-directory (constantly "/tmp/hive")]
            (sh/with-swarm-host
              [host (git-host {:success true :result "" :timed-out false})]
              (let [r (#'collect/harvest-git-commits {:directory "/tmp/hive" :agent-id agent})]
                (is (= 1 (count (sh/calls-of host :crystal/git-commits))))
                (is (= (.toString start)
                       (:since (ffirst (sh/calls-of host :crystal/git-commits))))
                    ":since equals the recorded session start instant's .toString")))))
        (finally
          (crystal/reset-session-start! agent))))))

(deftest harvest-git-commits-failure-envelope
  (testing "{:success false :error \"x\"} => empty commits plus the harvest-error shape"
    (with-redefs [ctx/current-directory (constantly "/tmp/hive")
                  crystal/get-session-start (constantly nil)]
      (sh/with-swarm-host
        [host (git-host {:success false :result nil :error "x" :timed-out false})]
        (let [r (#'collect/harvest-git-commits {:directory "/tmp/hive"})]
          (is (= 0 (:count r)))
          (is (= [] (:commits r)))
          (is (= {:type :harvest-failed
                  :fn "harvest-git-commits"
                  :msg "x"}
                 (:error r))
              "existing :harvest-failed error map"))))))

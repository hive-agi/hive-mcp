(ns hive-mcp.tools.consolidated.agent-description-test
  "The agent tool description states the contracts callers act on: what
   spawn_mode claude/vterm needs and what its success means, and what
   kill-batch's :killed and :aborting mean."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.consolidated.agent :as agent]))

(defn- spawn-mode-description []
  (get-in agent/tool-def [:inputSchema :properties "spawn_mode" :description]))

(deftest spawn-mode-states-the-emacs-contract
  (let [d (spawn-mode-description)]
    (testing "claude/vterm need a running Emacs daemon, checked up front"
      (is (str/includes? d "need a running Emacs daemon"))
      (is (str/includes? d "refuses up front")))
    (testing "success:true is not a started ling"
      (is (str/includes? d "success:true means Emacs was asked to open the session"))
      (is (str/includes? d "agent status")))))

(deftest kill-batch-states-what-killed-means
  (let [d (:description agent/tool-def)]
    (is (str/includes? d ":killed only once its run loop has exited"))
    (is (str/includes? d ":aborting"))))

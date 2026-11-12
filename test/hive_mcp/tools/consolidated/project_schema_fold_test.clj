(ns hive-mcp.tools.consolidated.project-schema-fold-test
  "The project root folds kanban, config, session, workflow and migrate-kanban
   HANDLERS, so it must fold their PARAMS too: the MCP layer forwards only the
   params the called tool declares, and a dropped one silently becomes the
   subdomain handler's default (kanban steal_claim, migrate-kanban dry_run)."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.composite :as composite]
            [hive-mcp.tools.consolidated.project :as project]))

(def subdomain-tools
  '[hive-mcp.tools.consolidated.kanban/tools
    hive-mcp.tools.consolidated.config/tools
    hive-mcp.tools.consolidated.session/tools
    hive-mcp.tools.consolidated.workflow/tools
    hive-mcp.tools.consolidated.migrate-kanban/tools])

(defn- root-props []
  (get-in project/tool-def [:inputSchema :properties]))

(deftest every-subdomain-param-survives-the-project-root
  (doseq [sym subdomain-tools
          :let [sub (composite/lazy-resolve-schema-props sym)]]
    (testing (str sym)
      (is (seq sub) "the subdomain advertises params")
      (is (= #{} (into (sorted-set) (remove (set (keys (root-props)))) (keys sub)))
          "no subdomain param is dropped at the project boundary"))))

(deftest destructive-gates-are-advertised
  (is (= "boolean" (get-in (root-props) ["dry_run" :type])))
  (is (= "boolean" (get-in (root-props) ["steal_claim" :type]))))

(deftest hand-written-root-descriptions-win
  (testing "the root's own command description is kept"
    (is (re-find #"Project operation" (get-in (root-props) ["command" :description]))))
  (testing "a root-described param keeps the root's description"
    (is (= "Maximum scan depth (default: 5)"
           (get-in (root-props) ["max_depth" :description])))))

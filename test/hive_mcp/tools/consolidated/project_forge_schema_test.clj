(ns hive-mcp.tools.consolidated.project-forge-schema-test
  "The project tool's inputSchema advertises the forge survey/strike params
   the handler already honours: plan_id (scoping) and max_slots (slot cap).
   Before, both worked by pass-through but no caller could discover them."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.tools.consolidated.project :as project]
            [hive-test.trifecta :refer [deftrifecta]]))

(defn advertised
  "The description the project tool schema gives `param`, or nil when the
   schema does not advertise it."
  [param]
  (get-in project/tool-def [:inputSchema :properties param :description]))

(defn- mentions-forge? [desc]
  (boolean (and (string? desc) (str/includes? desc "forge"))))

(deftest forge-params-are-advertised
  (is (= "integer"
         (get-in project/tool-def [:inputSchema :properties "max_slots" :type])))
  (is (mentions-forge? (advertised "max_slots")))
  (is (str/includes? (advertised "max_slots") "default 10"))
  (is (mentions-forge? (advertised "plan_id")))
  (is (str/includes? (advertised "plan_id") "plan-to-kanban")
      "the kanban plan-to-kanban meaning is kept"))

(deftrifecta forge-param-advertised
  #'hive-mcp.tools.consolidated.project-forge-schema-test/advertised
  {:golden-path "test/golden/project_forge_schema.edn"
   :cases       {:max-slots   "max_slots"
                 :plan-id     "plan_id"
                 :task-filter "task_filter"
                 :unknown     "no_such_param"}
   :xf          mentions-forge?
   :gen         (gen/elements ["max_slots" "plan_id" "task_filter" "presets"
                               "no_such_param" "command"])
   :pred        #(or (nil? %) (string? %))
   :num-tests   50
   :mutations   [["never-advertised" (fn [_] nil)]
                 ["kanban-only-plan-id"
                  (fn [p] (when (= p "plan_id") "[kanban plan-to-kanban] Memory plan entry ID"))]]})

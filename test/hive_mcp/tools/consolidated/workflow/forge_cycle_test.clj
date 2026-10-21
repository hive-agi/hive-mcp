(ns hive-mcp.tools.consolidated.workflow.forge-cycle-test
  "Facets for the Forge Belt resource builder.

   build-fsm-resources answers closures, so its trifecta carries no golden;
   the golden below pins the data part of the answer (directory and config)
   over the params arms. The strike fns call smite!/survey/spark! with no
   injectable seam at this layer; they are driven through forge-belt by the
   existing forge-readiness and forge-execution-propagation suites."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.consolidated.workflow.forge-cycle :as fc]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Params
  "Strike params as the forge tool hands them over, every key optional."
  [:maybe
   [:map
    [:directory {:optional true} [:maybe [:enum "/tmp/proj" "/home/x/hive"]]]
    [:max_slots {:optional true} [:maybe [:int {:min 1 :max 20}]]]
    [:presets {:optional true} [:maybe [:vector {:max 2} [:enum "ling" "tdd"]]]]
    [:spawn_mode {:optional true} [:maybe [:enum "headless" "vterm"]]]
    [:model {:optional true} [:maybe [:enum "sonnet" "kimi"]]]
    [:plan_id {:optional true} [:maybe [:enum "plan-1"]]]]])

(defn- expected-config [p]
  {:max-slots  (or (:max_slots p) 10)
   :presets    (or (:presets p) ["ling" "mcp-first" "saa"])
   :spawn-mode (some-> (:spawn_mode p) keyword)
   :model      (:model p)})

(deftrifecta-from-schema build-fsm-resources-fills-config-defaults
  hive-mcp.tools.consolidated.workflow.forge-cycle/build-fsm-resources
  {:in        Params
   :out       fc/FsmResources
   :rel       (fn [in out]
                (and (= (:directory in) (:directory out))
                     (= (expected-config in) (select-keys (:config out) [:max-slots :presets :spawn-mode :model]))
                     (= #{:kill-fn :spawn-fn :dispatch-fn :wait-ready-fn} (set (keys (:agent-ops out))))
                     (= #{:list-fn :update-fn} (set (keys (:kanban-ops out))))))
   :num-tests 200})

(defn- data-part [r] (select-keys r [:directory :config]))

(deftest-golden build-fsm-resources-data
  "test/golden/tools/consolidated/workflow/forge-cycle-resources.edn"
  {:nil-params (data-part (fc/build-fsm-resources nil))
   :empty      (data-part (fc/build-fsm-resources {}))
   :full       (data-part (fc/build-fsm-resources {:directory "/tmp/proj" :max_slots 3
                                                   :presets ["tdd"] :spawn_mode "headless"
                                                   :model "sonnet" :preset "p" :seeds ["s"]
                                                   :ctx_refs {:a "c"} :kg_node_ids ["n"]}))
   :two-arity  (data-part (fc/build-fsm-resources {:max_slots 1} {:spark {} :smite {}}))})

(deftest resource-closures-are-inert-where-the-belt-expects
  (let [r (fc/build-fsm-resources {:directory "/tmp/proj"})]
    (is (true? ((get-in r [:agent-ops :dispatch-fn]) "a" "t")))
    (is (true? ((get-in r [:agent-ops :wait-ready-fn]) "a")))
    (is (nil? ((get-in r [:kanban-ops :update-fn]) {})))
    (is (nil? ((:scope-fn r) nil)) "no directory, no scope")
    (is (instance? java.time.Instant ((:clock-fn r))))))

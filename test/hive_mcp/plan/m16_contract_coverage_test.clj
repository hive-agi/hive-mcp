(ns hive-mcp.plan.m16-contract-coverage-test
  "One contract-coverage gate per m16 directory: hive_mcp/plan,
   hive_mcp/plan/parser, hive_mcp/project, hive_mcp/presets,
   hive_mcp/nrepl, hive_mcp/nats."
  (:require [hive-schemas.coverage :refer [deftest-contract-coverage]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private parser-facade-exemptions
  "parser.clj is a pure Var facade: `(def x #'sub-ns/x)` captures no value, so
   an m/=> here would shadow the real contract on the aliased fn and keep
   redefinition of the target from reaching callers through this name."
  (into {}
        (map (fn [n]
               [(symbol "hive-mcp.plan.parser" n)
                "Var facade over the contracted fn in plan.parser.edn/markdown/util; the contract lives on the aliased var so instrumentation of the target reaches callers through this name."]))
        ["contains-edn-block?" "contains-edn-plan?" "parse-edn-plan"
         "parse-markdown-plan" "plan->task-specs" "validate-dependencies"]))

(deftest-contract-coverage plan-contracts-gate
  {:paths ["src/hive_mcp/plan/field_registry.clj"
           "src/hive_mcp/plan/fsm.clj"
           "src/hive_mcp/plan/gate.clj"
           "src/hive_mcp/plan/kg_degraded.clj"
           "src/hive_mcp/plan/parser.clj"
           "src/hive_mcp/plan/plans.clj"
           "src/hive_mcp/plan/schema.clj"
           "src/hive_mcp/plan/tool.clj"]
   ;; parser.clj's public surface is Var-alias `def`s (contains-edn-block?,
   ;; parse-edn-plan, parse-markdown-plan, plan->task-specs,
   ;; validate-dependencies, contains-edn-plan?): `def` forms are not in the
   ;; defn universe the scanner reads, so they need no exemption here; the
   ;; contracted fns they alias live in plan.parser.edn/markdown/util.
   :exempt {}})

(deftest-contract-coverage plan-parser-contracts-gate
  {:paths ["src/hive_mcp/plan/parser"]
   :exempt {}})

(deftest-contract-coverage project-contracts-gate
  {:paths ["src/hive_mcp/project"]
   :exempt {}})

(deftest-contract-coverage presets-contracts-gate
  {:paths ["src/hive_mcp/presets"]
   :exempt {}})

(deftest-contract-coverage nrepl-contracts-gate
  {:paths ["src/hive_mcp/nrepl"]
   :exempt {}})

(deftest-contract-coverage nats-contracts-gate
  {:paths ["src/hive_mcp/nats"]
   :exempt {}})

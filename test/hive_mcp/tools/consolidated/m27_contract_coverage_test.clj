(ns hive-mcp.tools.consolidated.m27-contract-coverage-test
  "Gate: every public fn in the tools/consolidated namespaces of coverage
   batch m27 carries an `m/=>` contract or a REASONED exemption, and no
   exemption is stale. The other files of src/hive_mcp/tools/consolidated
   (agent, agora, analysis, code, fs, git, kanban, kg, magit, migrate_kanban,
   migration, olympus, preset, and the frozen swarm) belong to other batches."
  (:require [hive-schemas.coverage :refer [deftest-contract-coverage]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage consolidated-m27-contracts-gate
  {:paths ["src/hive_mcp/tools/consolidated/addon.clj"
           "src/hive_mcp/tools/consolidated/config.clj"
           "src/hive_mcp/tools/consolidated/hivemind.clj"
           "src/hive_mcp/tools/consolidated/hot.clj"
           "src/hive_mcp/tools/consolidated/memory.clj"
           "src/hive_mcp/tools/consolidated/multi.clj"
           "src/hive_mcp/tools/consolidated/project.clj"
           "src/hive_mcp/tools/consolidated/roster.clj"
           "src/hive_mcp/tools/consolidated/session.clj"
           "src/hive_mcp/tools/consolidated/session_config.clj"
           "src/hive_mcp/tools/consolidated/transcript.clj"
           "src/hive_mcp/tools/consolidated/web.clj"
           "src/hive_mcp/tools/consolidated/workflow.clj"]
   :exempt {}})

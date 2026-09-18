;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.swarm.adapters.memory-scope
  "hive-mcp adapter for the hive-spi.swarm.ports.memory-scope SPI.

   One reify implements BOTH IProjectScope and IDiscStaleness, delegating to
   the host-owned hive-mcp namespaces that read .hive-project.edn files and
   the KG disc store:

   - IProjectScope/project-id-for-path  -> hive-mcp.tools.memory.scope/
     get-current-project-id (keeps alias resolution + the config cache)
   - IProjectScope/infer-scope-from-path -> hive-mcp.knowledge-graph.scope/
     infer-scope-from-path (keeps the register-project-config! side effect
     host-side)
   - IDiscStaleness/* -> hive-mcp.knowledge-graph.disc facade (whose
     staleness-warnings and kg-first-context do the filesystem/DataScript
     I/O; format-staleness-warnings is pure but carried on the port so the
     host owns the warning format)

   Install at addon init via install!. A standalone process without this
   adapter runs on the SPI's Noop."
  (:require [hive-spi.swarm.ports.memory-scope :as spi]
            [hive-mcp.tools.memory.scope :as memory-scope]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [hive-mcp.knowledge-graph.disc :as kg-disc]))

(defn make-adapter
  "Build the hive-mcp implementation of both memory-scope SPI protocols."
  []
  (reify spi/IProjectScope
    (project-id-for-path [_this path]
      (memory-scope/get-current-project-id path))
    (infer-scope-from-path [_this path]
      (kg-scope/infer-scope-from-path path))
    spi/IDiscStaleness
    (staleness-warnings [_this paths]
      (kg-disc/staleness-warnings paths))
    (format-staleness-warnings [_this warnings]
      (kg-disc/format-staleness-warnings warnings))
    (kg-first-context [_this paths]
      (kg-disc/kg-first-context paths))))

(defn install!
  "Install the hive-mcp adapter into the SPI slot. Idempotent by overwrite —
   the last installer wins, which is what a re-init should do."
  []
  (spi/set-memory-scope! (make-adapter)))

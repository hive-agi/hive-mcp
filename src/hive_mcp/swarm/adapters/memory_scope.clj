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

   All three host namespaces are hive-memory extraction targets in the kernel
   census, so none is required here: each function is resolved by symbol on
   the call (hive-mcp.swarm.adapters.soft). While the namespace is present
   the host answer passes through unchanged; once it has left, the method
   answers what the port's own Noop answers.

   Install at addon init via install!. A standalone process without this
   adapter runs on the SPI's Noop."
  (:require [hive-spi.swarm.ports.memory-scope :as spi]
            [hive-mcp.swarm.adapters.soft :as soft]))

(defn make-adapter
  "Build the hive-mcp implementation of both memory-scope SPI protocols."
  []
  (reify spi/IProjectScope
    (project-id-for-path [_this path]
      (soft/host-or 'hive-mcp.tools.memory.scope/get-current-project-id
                    #(spi/project-id-for-path spi/noop path)
                    path))
    (infer-scope-from-path [_this path]
      (soft/host-or 'hive-mcp.knowledge-graph.scope/infer-scope-from-path
                    #(spi/infer-scope-from-path spi/noop path)
                    path))
    spi/IDiscStaleness
    (staleness-warnings [_this paths]
      (soft/host-or 'hive-mcp.knowledge-graph.disc/staleness-warnings
                    #(spi/staleness-warnings spi/noop paths)
                    paths))
    (format-staleness-warnings [_this warnings]
      (soft/host-or 'hive-mcp.knowledge-graph.disc/format-staleness-warnings
                    #(spi/format-staleness-warnings spi/noop warnings)
                    warnings))
    (kg-first-context [_this paths]
      (soft/host-or 'hive-mcp.knowledge-graph.disc/kg-first-context
                    #(spi/kg-first-context spi/noop paths)
                    paths))))

(defn install!
  "Install the hive-mcp adapter into the SPI slot. Idempotent by overwrite —
   the last installer wins, which is what a re-init should do."
  []
  (spi/set-memory-scope! (make-adapter)))

(ns hive-mcp.agent.type-registry
  "Single source of truth for agent types and their properties.

   All consumers derive from this registry — no scattered enums.
   Leaf namespace: zero hive-mcp dependencies (safe to require anywhere).

   Design principle: Knowledge-Layer-First / SST (Single Source of Truth).
   Adding a new agent type = adding one entry here. All downstream
   validation, MCP schemas, depth mappings, and capabilities derive automatically.

   Sum type variants: coordinator, ling.

   Compat shim; lives in hive-spi.swarm.agent-types."

 (:require [hive-spi.swarm.agent-types :as spi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def registry spi/registry)
(def all-types spi/all-types)
(def all-type-strings spi/all-type-strings)
(def mcp-types spi/mcp-types)
(def type->depth spi/type->depth)
(def depth->type spi/depth->type)
(def type->capabilities spi/type->capabilities)
(def type->permissions spi/type->permissions)
(def type->spawn-modes spi/type->spawn-modes)
(def type->model-tier spi/type->model-tier)
(def type->slot-limit spi/type->slot-limit)
(def valid-type? spi/valid-type?)
(def type-depth spi/type-depth)
(def depth->agent-type spi/depth->agent-type)
(def spawnable? spi/spawnable?)
(def valid-spawn-mode? spi/valid-spawn-mode?)
(def has-capability? spi/has-capability?)
(def has-permission? spi/has-permission?)
(def can-chain-tools? spi/can-chain-tools?)
(def slot-limit spi/slot-limit)
(def default-model-tier spi/default-model-tier)
(def mcp-enum spi/mcp-enum)
(def describe spi/describe)

(ns hive-mcp.agent.type-registry
  "Single source of truth for agent types and their properties.

   All consumers derive from this registry — no scattered enums.
   Leaf namespace: zero hive-mcp dependencies (safe to require anywhere).

   Design principle: Knowledge-Layer-First / SST (Single Source of Truth).
   Adding a new agent type = adding one entry here. All downstream
   validation, MCP schemas, depth mappings, and capabilities derive automatically.

   Sum type variants: coordinator, ling.

   Compat shim; lives in hive-spi.swarm.agent-types. The data views are
   `def` aliases (values, nothing to reload). Every FUNCTION is a delegating
   `defn` that calls through the hive-spi var, so a reload or redef of the
   owner is seen by every caller of this name (card 20261116190025-31139c15)."

 (:require [hive-spi.swarm.agent-types :as spi]
           [malli.core :as m]))

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

(defn valid-type? [t] (spi/valid-type? t))
(defn type-depth [t] (spi/type-depth t))
(defn depth->agent-type [depth] (spi/depth->agent-type depth))
(defn spawnable? [agent-type] (spi/spawnable? agent-type))
(defn valid-spawn-mode? [agent-type spawn-mode] (spi/valid-spawn-mode? agent-type spawn-mode))
(defn has-capability? [agent-type capability] (spi/has-capability? agent-type capability))
(defn has-permission? [agent-type permission-key] (spi/has-permission? agent-type permission-key))
(defn can-chain-tools? [agent-type] (spi/can-chain-tools? agent-type))
(defn slot-limit [agent-type] (spi/slot-limit agent-type))
(defn default-model-tier [agent-type] (spi/default-model-tier agent-type))
(defn mcp-enum [] (spi/mcp-enum))
(defn describe [agent-type] (spi/describe agent-type))

;; Contracts mirror hive-spi.swarm.agent-types: instrumenting the owner never
;; reaches a caller of this facade, so the facade carries its own.
(m/=> valid-type? [:=> [:cat :any] :boolean])
(m/=> type-depth [:=> [:cat spi/TypeRef] :int])
(m/=> depth->agent-type [:=> [:cat :any] :keyword])
(m/=> spawnable? [:=> [:cat spi/TypeRef] :boolean])
(m/=> valid-spawn-mode? [:=> [:cat spi/TypeRef spi/TypeRef] :boolean])
(m/=> has-capability? [:=> [:cat spi/TypeRef spi/TypeRef] :boolean])
(m/=> has-permission? [:=> [:cat spi/TypeRef :any] :boolean])
(m/=> can-chain-tools? [:=> [:cat spi/TypeRef] :boolean])
(m/=> slot-limit [:=> [:cat spi/TypeRef] [:maybe :int]])
(m/=> default-model-tier [:=> [:cat spi/TypeRef] spi/ModelTier])
(m/=> mcp-enum [:=> [:cat] [:vector :string]])
(m/=> describe [:=> [:cat spi/TypeRef] [:maybe :string]])

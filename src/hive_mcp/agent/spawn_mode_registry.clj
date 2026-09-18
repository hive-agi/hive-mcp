(ns hive-mcp.agent.spawn-mode-registry
  "Single source of truth for ling spawn modes and their properties.

   All consumers derive from this registry — no scattered enums.
   Leaf namespace: zero hive-mcp dependencies (safe to require anywhere).

   Design principle: Knowledge-Layer-First / SST (Single Source of Truth).
   Adding a new spawn mode = adding one entry here. All downstream
   validation, MCP schemas, strategy dispatch, and slot limits derive automatically.

   Extensible at runtime via register-mode! for addon-contributed modes
   (e.g. :tmux from hive-tmux). Core modes are baked in; addon modes
   are registered during IAddon initialize! lifecycle.

   Sum type: claude | vterm | headless | agent-sdk | <addon-contributed>
   MCP surface: claude | vterm | headless | <addon-contributed with :mcp? true>

   NOTE: hive-mcp owns ONLY abstract/generic modes here. Concrete provider-
   or implementation-specific modes (e.g. :hive-agent, :tmux) are
   contributed by addons via `register-mode!`.

   Compat shim; lives in hive-spi.swarm.spawn-modes."

 (:require [hive-spi.swarm.spawn-modes :as spi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def registry spi/registry)
(def all-modes spi/all-modes)
(def all-mode-strings spi/all-mode-strings)
(def mcp-modes spi/mcp-modes)
(def internal-modes spi/internal-modes)
(def mode->slot-limit spi/mode->slot-limit)
(def emacs-modes spi/emacs-modes)
(def headless-modes spi/headless-modes)
(def alias-map spi/alias-map)
(def valid-mode? spi/valid-mode?)
(def resolve-alias spi/resolve-alias)
(def requires-emacs? spi/requires-emacs?)
(def slot-limit spi/slot-limit)
(def io-model spi/io-model)
(def capabilities spi/capabilities)
(def has-capability? spi/has-capability?)
(def mcp-enum spi/mcp-enum)
(def register-mode! spi/register-mode!)
(def deregister-mode! spi/deregister-mode!)

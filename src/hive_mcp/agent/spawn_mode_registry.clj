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

   Compat shim; lives in hive-spi.swarm.spawn-modes. The data views are
   `def` aliases (values, nothing to reload). Every FUNCTION is a delegating
   `defn` that calls through the hive-spi var, so a reload or redef of the
   owner is seen by every caller of this name (card 20261116190025-31139c15)."

 (:require [hive-spi.swarm.spawn-modes :as spi]
           [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def all-modes spi/all-modes)
(def all-mode-strings spi/all-mode-strings)
(def mcp-modes spi/mcp-modes)
(def internal-modes spi/internal-modes)
(def mode->slot-limit spi/mode->slot-limit)
(def emacs-modes spi/emacs-modes)
(def headless-modes spi/headless-modes)
(def alias-map spi/alias-map)

(defn registry [] (spi/registry))
(defn valid-mode? [m] (spi/valid-mode? m))
(defn resolve-alias [mode] (spi/resolve-alias mode))
(defn requires-emacs? [mode] (spi/requires-emacs? mode))
(defn slot-limit [mode] (spi/slot-limit mode))
(defn io-model [mode] (spi/io-model mode))
(defn capabilities [mode] (spi/capabilities mode))
(defn has-capability? [mode capability] (spi/has-capability? mode capability))
(defn mcp-enum [] (spi/mcp-enum))
(defn register-mode! [mode-kw mode-spec] (spi/register-mode! mode-kw mode-spec))
(defn deregister-mode! [mode-kw] (spi/deregister-mode! mode-kw))

;; Contracts mirror hive-spi.swarm.spawn-modes: instrumenting the owner never
;; reaches a caller of this facade, so the facade carries its own.
(m/=> registry [:=> [:cat] [:map-of :keyword spi/ModeSpec]])
(m/=> valid-mode? [:=> [:cat spi/ModeRef] :boolean])
(m/=> resolve-alias [:=> [:cat :any] :any])
(m/=> requires-emacs? [:=> [:cat :any] :boolean])
(m/=> slot-limit [:=> [:cat :any] [:maybe :int]])
(m/=> io-model [:=> [:cat :any] [:maybe :keyword]])
(m/=> capabilities [:=> [:cat :any] [:set :keyword]])
(m/=> has-capability? [:=> [:cat :any :any] :boolean])
(m/=> mcp-enum [:=> [:cat] [:vector :string]])
(m/=> register-mode! [:=> [:cat :keyword :map] :keyword])
(m/=> deregister-mode! [:=> [:cat :any] :any])

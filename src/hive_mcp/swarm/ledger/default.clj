(ns hive-mcp.swarm.ledger.default
  "Process-wide default swarm ledger, opened lazily on first append.

   Compat shim: moved to hive-agent.swarm.ledger.default in hive-agent.
   Every public var delegates there; see hive-mcp.swarm.delegate for behavior
   without the addon."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.ledger.default" sym))

(defn append! {:arglists '([event])} [& args] (apply (impl 'append!) args))
(defn reset-store! {:arglists '([])} [& args] (apply (impl 'reset-store!) args))
(defn set-store! {:arglists '([s])} [& args] (apply (impl 'set-store!) args))
(defn store {:arglists '([])} [& args] (apply (impl 'store) args))

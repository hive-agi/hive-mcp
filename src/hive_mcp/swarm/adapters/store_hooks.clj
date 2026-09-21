(ns hive-mcp.swarm.adapters.store-hooks
  "Installs the host hooks the swarm DataScript store (hive-datascript) calls.

   hive-datascript never names the orchestration layer or host config. This
   adapter composes them: ledger write-through (hive-agent's default ledger),
   the claim-release mirror into the logic db (hive-agent.swarm.logic), and
   the stale-ling threshold from config.edn. Every piece resolves late and
   is skipped when absent."
  (:require [taoensso.timbre :as log]
            [hive-mcp.session.identity :as sid]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- resolve-soft
  "The var SYM when its namespace loads, else nil."
  [sym]
  (try (requiring-resolve sym) (catch Throwable _ nil)))

(defn- stale-threshold-ms
  "config.edn swarm.stale-threshold-ms, or nil."
  []
  (when-let [get-value (resolve-soft 'hive-mcp.config.core/get-config-value)]
    (try (get-value "swarm.stale-threshold-ms") (catch Throwable _ nil))))

(defn scope-rows
  "Keep only the ROWS the :session-ref in OPTS owns, per
   hive-mcp.session.identity. With no valid session-ref the rows pass through
   unchanged (unscoped read)."
  [{:keys [session-ref parent-of]} session-key rows]
  (if-not (sid/valid? session-ref)
    rows
    (sid/harvestable {:parent-of    (or parent-of {})
                      :row->session session-key}
                     session-ref
                     rows)))

(defn install!
  "Install the store hooks into hive-datascript. Returns the installed map,
   or nil when hive-datascript is not on the classpath."
  []
  (if-let [install-hooks! (resolve-soft 'hive-datascript.swarm.hooks/install!)]
    (let [ledger-append (resolve-soft 'hive-agent.swarm.ledger.default/append!)
          claim-released (resolve-soft 'hive-agent.swarm.logic/release-claim-for-file!)
          threshold (stale-threshold-ms)]
      (install-hooks! (cond-> {:scope-rows scope-rows}
                        ledger-append (assoc :ledger-append ledger-append)
                        claim-released (assoc :claim-released claim-released)
                        threshold (assoc :stale-threshold-ms threshold))))
    (do (log/debug "hive-datascript absent; swarm store hooks not installed")
        nil)))

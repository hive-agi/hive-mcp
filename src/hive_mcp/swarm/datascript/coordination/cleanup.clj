(ns hive-mcp.swarm.datascript.coordination.cleanup
  "Stale-detection sweeps for coordinators and claims.

   Compat shim: moved to hive-datascript.swarm.coordination.cleanup in
   hive-datascript. Every public var delegates there; see
   hive-mcp.swarm.delegate for behavior without the library."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [hive-mcp.swarm.datascript.schema :as s]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-datascript.swarm.coordination.cleanup" sym))

(defn cleanup-old-claim-history! {:arglists '([& [{:keys [threshold-ms]}]])} [& args] (apply (impl 'cleanup-old-claim-history!) args))

;; Contracts transcribe hive-datascript.swarm.coordination.cleanup, where the
;; behaviour lives.
(m/=> cleanup-old-claim-history! [:=> [:cat s/ThresholdOpts] s/Count])
(defn cleanup-stale-claims! {:arglists '([& [{:keys [threshold-ms]}]])} [& args] (apply (impl 'cleanup-stale-claims!) args))

(m/=> cleanup-stale-claims! [:=> [:cat s/ThresholdOpts] s/Count])
(defn cleanup-stale-coordinators! {:arglists '([& [{:keys [threshold-ms]}]])} [& args] (apply (impl 'cleanup-stale-coordinators!) args))

;; The ids it marked stale, or nil when none were stale.
(m/=> cleanup-stale-coordinators! [:=> [:cat s/ThresholdOpts] [:maybe [:sequential :string]]])
(defn cleanup-terminal-tasks! {:arglists '([& [{:keys [threshold-ms]}]])} [& args] (apply (impl 'cleanup-terminal-tasks!) args))

(m/=> cleanup-terminal-tasks! [:=> [:cat s/ThresholdOpts] s/Count])
(def default-ledger-retain-ms @(impl 'default-ledger-retain-ms))
(defn sweep-ledger-cold! {:arglists '([& [opts]])} [& args] (apply (impl 'sweep-ledger-cold!) args))

(m/=> sweep-ledger-cold! [:=> [:cat s/ThresholdOpts]
                          [:map [:tasks s/Count] [:claim-history s/Count]]])

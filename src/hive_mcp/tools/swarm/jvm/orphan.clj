(ns hive-mcp.tools.swarm.jvm.orphan
  "Orphan process detection using composable predicates.

   Compat shim: moved to hive-agent.swarm.tools.jvm.orphan in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (requiring-resolve (symbol "hive-agent.swarm.tools.jvm.orphan" (name sym))))

(defn- impl!
  "impl, after making sure the host adapters fill the swarm port slots.
   The adapters are resolved late: a static require would close a load
   cycle through the events adapter. A failure here (for instance a call
   made while that cycle is still loading) leaves the ports on their
   noops until a later call installs them."
  [sym]
  (try ((requiring-resolve 'hive-mcp.swarm.adapters.boot/ensure!))
       (catch Throwable _ nil))
  (impl sym))

(defn age-orphan?
  {:arglists '([proc min-age-minutes])}
  [& args]
  (apply (impl! 'age-orphan?) args))

(defn enrich-with-parent-info
  {:arglists '([proc all-parents])}
  [& args]
  (apply (impl! 'enrich-with-parent-info) args))

(defn identify-orphan
  {:arglists '([proc & {:keys [protected-types true-orphans-only min-age-minutes], :or {protected-types #{}, true-orphans-only true, min-age-minutes 30}}])}
  [& args]
  (apply (impl! 'identify-orphan) args))

(defn identify-orphans
  {:arglists '([procs & {:keys [protected-types true-orphans-only min-age-minutes], :or {protected-types #{}, true-orphans-only true, min-age-minutes 30}}])}
  [& args]
  (apply (impl! 'identify-orphans) args))

(defn orphan-detector
  {:arglists '([& {:keys [protected-types true-orphans-only min-age-minutes], :or {protected-types #{}, true-orphans-only true, min-age-minutes 30}}])}
  [& args]
  (apply (impl! 'orphan-detector) args))

(defn protected-type?
  {:arglists '([proc protected-types])}
  [& args]
  (apply (impl! 'protected-type?) args))

(defn truly-orphaned?
  {:arglists '([proc])}
  [& args]
  (apply (impl! 'truly-orphaned?) args))

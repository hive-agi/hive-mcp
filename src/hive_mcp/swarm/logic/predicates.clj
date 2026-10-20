(ns hive-mcp.swarm.logic.predicates
  "Pure core.logic predicates and relations for swarm coordination.

   Compat shim: moved to hive-agent.swarm.logic.predicates in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.logic.predicates" sym))

(defn claims
  {:arglists '([& args])}
  [& args]
  (apply (impl 'claims) args))

;; A core.logic goal: a fn of a substitution (a relation applied to its
;; arguments answers one too).
(def Goal
  [:fn {:gen/schema :nil :gen/fmap (fn [_] (fn [a] a))} fn?])

(def Lvar
  "A goal argument: a ground value or a logic variable."
  :any)
;; Contracts transcribe hive-agent.swarm.logic.predicates (pldb relations and
;; core.logic goals). The relations take one argument per declared column.
(m/=> claims [:=> [:cat Lvar Lvar] Goal])

(defn depends-on
  {:arglists '([& args])}
  [& args]
  (apply (impl 'depends-on) args))

(m/=> depends-on [:=> [:cat Lvar Lvar] Goal])

(defn file-conflicto
  {:arglists '([file-path requesting-slave conflicting-slave])}
  [& args]
  (apply (impl 'file-conflicto) args))

(m/=> file-conflicto [:=> [:cat Lvar Lvar Lvar] Goal])

(defn reachable-fromo
  {:arglists '([source target])}
  [& args]
  (apply (impl 'reachable-fromo) args))

(m/=> reachable-fromo [:=> [:cat Lvar Lvar] Goal])

(defn slave
  {:arglists '([& args])}
  [& args]
  (apply (impl 'slave) args))

(m/=> slave [:=> [:cat Lvar Lvar] Goal])

(defn task
  {:arglists '([& args])}
  [& args]
  (apply (impl 'task) args))

(m/=> task [:=> [:cat Lvar Lvar Lvar] Goal])

(defn task-completedo
  {:arglists '([task-id])}
  [& args]
  (apply (impl 'task-completedo) args))

(m/=> task-completedo [:=> [:cat Lvar] Goal])

(defn task-files
  {:arglists '([& args])}
  [& args]
  (apply (impl 'task-files) args))

(m/=> task-files [:=> [:cat Lvar Lvar] Goal])

(defn task-pendingo
  {:arglists '([task-id])}
  [& args]
  (apply (impl 'task-pendingo) args))

(m/=> task-pendingo [:=> [:cat Lvar] Goal])

(defn would-deadlocko
  {:arglists '([task-a task-b])}
  [& args]
  (apply (impl 'would-deadlocko) args))

(m/=> would-deadlocko [:=> [:cat Lvar Lvar] Goal])

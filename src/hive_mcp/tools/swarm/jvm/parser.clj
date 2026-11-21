(ns hive-mcp.tools.swarm.jvm.parser
  "Process output parsing for JVM process management via multimethod dispatch by OS.

   Compat shim: moved to hive-agent.swarm.tools.jvm.parser in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.tools.jvm.parser" sym))

(defn- impl!
  "impl, after the host adapters are installed (see delegate/ensure-adapters!)."
  [sym]
  (delegate/ensure-adapters!)
  (impl sym))

(defn parse-etime-to-minutes
  {:arglists '([etime])}
  [& args]
  (apply (impl! 'parse-etime-to-minutes) args))

;; Any value that does not parse as [[DD-]HH:]MM:SS (nil included) is 0.
(m/=> parse-etime-to-minutes [:=> [:cat [:maybe :string]] [:int {:min 0}]])

(def parse-process-line @(impl 'parse-process-line))

(def parse-process-line-extended @(impl 'parse-process-line-extended))

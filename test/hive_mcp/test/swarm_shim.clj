(ns hive-mcp.test.swarm-shim
  "Test helper for the swarm compat shims (hive-mcp.swarm.* namespaces whose
   public fns delegate to hive-agent through hive-mcp.swarm.delegate).

   On the unit classpath hive-agent is absent, so a shim's only behaviour is
   WHERE it delegates: calling it raises :swarm/addon-missing naming the
   hive-agent ns and var it resolves. `targets` collects that mapping as data
   for a golden, so a shim that points a var at the wrong target (a typo in
   its `(impl 'sym)`, a moved namespace) changes the snapshot. The suites
   that run the shims against hive-agent itself live in test-swarm/."
  (:require [hive-mcp.swarm.delegate :as delegate]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn target
  "What calling shim fn F with ARGS answers: [:delegates-to ns var] when it
   refuses for a missing addon, [:value v] when it answers, [:threw msg] for
   any other throw."
  [f & args]
  (try [:value (apply f args)]
       (catch clojure.lang.ExceptionInfo e
         (let [d (ex-data e)]
           (if (:swarm/addon-missing d)
             [:delegates-to (:ns d) (:var d)]
             [:threw (ex-message e)])))
       (catch Throwable e [:threw (.getName (class e))])))

(defn targets
  "{var-name -> (target var)} for every var in VARS, each called with no args."
  [vars]
  (into (sorted-map) (map (fn [v] [(str (:name (meta v))) (target @v)])) vars))

(defn addon-absent?
  "True when NS-STR (a hive-agent namespace) is not on the classpath, which is
   the condition the shim goldens are recorded under."
  [ns-str]
  (not (delegate/available? ns-str)))

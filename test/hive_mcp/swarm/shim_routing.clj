(ns hive-mcp.swarm.shim-routing
  "Test helper: what a compat shim delegates to, as data.

   A shim's public fns reach an addon namespace through
   `hive-mcp.swarm.delegate`. `routing` answers {fn-name target} the same
   way whether or not the addon is on the classpath:

   - addon absent: each fn is CALLED and the target is read off the
     `:swarm/addon-missing` ex-data the stand-in raises, so the table is
     what the shim actually routes to;
   - addon present: each fn's expected target (from `expected`) is checked
     to RESOLVE, and kept only when it does.

   A golden over this table pins the routing of a shim in both worlds; in
   the absent world it catches a shim pointed at the wrong var."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn contracted-fns
  "Sorted names of the contracted public fns of `nsym`."
  [nsym]
  (sort (keys (get (m/function-schemas) nsym))))

(defn- called-target [f args]
  (try (apply f args)
       :returned-without-addon
       (catch clojure.lang.ExceptionInfo e
         (let [{:keys [ns var] :as d} (ex-data e)]
           (if (:swarm/addon-missing d) (str ns "/" var) [:other (ex-message e)])))))

(defn routing
  "{fn-name target} for the contracted fns of shim `nsym`.

   `addon-ns` is the namespace whose presence decides the world, `expected`
   is fn-name -> target string (read only when the addon is present), and
   `args` is fn-name -> argument vector for the call (default [])."
  [nsym addon-ns expected args]
  (into (sorted-map)
        (for [s (contracted-fns nsym)]
          [s (if (delegate/available? addon-ns)
               (let [t (get expected s)]
                 (if (and t (some? (requiring-resolve (symbol t)))) t [:unresolved t]))
               (called-target (var-get (ns-resolve nsym s)) (get args s [])))])))

(defn same-name-targets
  "fn-name -> \"<addon-ns>/<fn-name>\" for every contracted fn of `nsym`:
   the expected table of a shim whose fns keep their names."
  [nsym addon-ns]
  (into {} (for [s (contracted-fns nsym)] [s (str addon-ns "/" s)])))

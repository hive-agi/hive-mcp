(ns hive-mcp.swarm.addon-gate
  "Runs a swarm contract namespace with or without the swarm addon.

   The shims under hive-mcp.swarm.* delegate to hive-agent and
   hive-datascript, which test-swarm/ gets from local.deps.edn. A JVM started
   without them (the bare :test:test-swarm aliases) still loads every shim;
   a call raises the delegate's :swarm/addon-missing ex-info and a load-time
   value is nil. A namespace that installs `run` as its test-ns-hook runs
   every test when the addon is present, and without it only the tests that
   never reach the addon, plus its own addon-absent test, which pins the
   delegate's documented behaviour. A cold run without the addon then
   reports instead of aborting on the first fixture that touches the store."
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [hive-mcp.swarm.delegate :as delegate]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def swarm-addon
  "One namespace from each addon library the swarm shims resolve into."
  ["hive-agent.swarm.claim.span" "hive-datascript.swarm.connection"])

(defn present?
  "True when every swarm addon library is on the classpath."
  []
  (every? delegate/available? swarm-addon))

(defn addon-missing?
  "True when calling F raises the delegate's addon-missing ex-info."
  [f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e
         (true? (:swarm/addon-missing (ex-data e))))))

(defn offline
  "Predicate over a test name: true for the names in EXACT, the names
   starting with one of PREFIXES, and every mutants-present check (those
   only read the mutant table, never call the subject)."
  [exact prefixes]
  (fn [n]
    (or (contains? exact n)
        (str/ends-with? n "-mutants-present")
        (some #(str/starts-with? n %) prefixes))))

(defn run
  "test-ns-hook body for NS-SYM: every test var with the addon, only those
   whose name satisfies OFFLINE? without it. Vars run in source order,
   with the namespace's fixtures."
  [ns-sym offline?]
  (let [vars (->> (ns-interns ns-sym) vals
                  (filter (comp :test meta))
                  (sort-by (comp :line meta)))]
    (t/test-vars (if (present?)
                   vars
                   (filter #(offline? (name (:name (meta %)))) vars)))))

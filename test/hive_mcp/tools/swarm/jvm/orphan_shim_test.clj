(ns hive-mcp.tools.swarm.jvm.orphan-shim-test
  "Facets for the `hive-mcp.tools.swarm.jvm.orphan` compat shim, in the
   addon-free unit suite.

   The behaviour lives in hive-agent; its trifectas run in test-swarm/
   (hive-mcp.tools.swarm.jvm.contract-trifecta-test), where the addon is on
   the classpath. What hive-mcp owns here is the shim: for every contracted
   fn and every input its contract generates, a call either answers the
   contract's output (addon present) or raises addon-missing naming that fn
   (addon absent), never anything else.

   No in-JVM stub of the implementation is possible: `delegate/available?`
   looks for the implementation's SOURCE on the classpath, so the shim's
   port is the classpath itself."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.swarm.delegate :as delegate]
            [hive-mcp.tools.swarm.jvm.orphan :as shim]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private shim-ns 'hive-mcp.tools.swarm.jvm.orphan)
(def ^:private impl-ns "hive-agent.swarm.tools.jvm.orphan")

(defn- addon? [] (delegate/available? impl-ns))

(defn- shim-answer
  "Call the shim VAR on ARGS: [:ok out] or [:missing addon ns var]; any
   other throw propagates and fails the test."
  [v args]
  (try [:ok (apply @v args)]
       (catch clojure.lang.ExceptionInfo e
         (let [d (ex-data e)]
           (if (:swarm/addon-missing d)
             [:missing (:addon d) (:ns d) (:var d)]
             (throw e))))))

(def ^:private arities
  "Every [fn-sym input-schema output-schema] the namespace's contracts declare.
   orphan-detector answers a fn, which m/validate checks only as ifn?."
  (vec (for [[sym {:keys [schema]}] (sort-by key (get (m/function-schemas) shim-ns))
             :let [s (m/schema schema)]
             arity (if (= :function (m/type s)) (m/children s) [s])]
         [sym (first (m/children arity)) (second (m/children arity))])))

(deftest the-contracts-are-there
  (is (= 7 (count arities)) "every public fn of the shim carries an m/=> contract"))

(defspec every-contracted-shim-answers-its-contract-or-fails-loud 100
  (prop/for-all [[sym args out] (gen/bind (gen/elements arities)
                                          (fn [[sym in out]]
                                            (gen/fmap (fn [a] [sym a out]) (mg/generator in))))]
    (let [[tag & r] (shim-answer (ns-resolve shim-ns sym) args)]
      (if (addon?)
        (and (= :ok tag) (m/validate out (first r)))
        (= [:missing "hive-agent" impl-ns (name sym)] (into [tag] r))))))

(deftest-golden orphan-shim-answers
  "test/golden/tools/swarm/jvm/orphan-shim.edn"
  (if (addon?)
    :addon-present
    {:truly-orphaned (shim-answer #'shim/truly-orphaned? [{:truly-orphaned true}])
     :protected      (shim-answer #'shim/protected-type? [{:type :nrepl} #{"nrepl"}])
     :age            (shim-answer #'shim/age-orphan? [{:etime "45:00"} 30])
     :detector       (shim-answer #'shim/orphan-detector [:min-age-minutes 30])}))

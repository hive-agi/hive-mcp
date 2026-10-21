(ns hive-mcp.tools.swarm.jvm.parser-shim-test
  "Facets for the `hive-mcp.tools.swarm.jvm.parser` compat shim, in the
   addon-free unit suite.

   The behaviour lives in hive-agent; its trifectas run in test-swarm/
   (hive-mcp.tools.swarm.jvm.contract-trifecta-test). What hive-mcp owns
   here is the shim: for every input its contract generates, a call either
   answers the contract's output (addon present) or raises addon-missing
   (addon absent), never anything else; and its load-time aliases are nil
   without the addon. Named -shim-test because test-swarm already holds a
   hive-mcp.tools.swarm.jvm.parser-test."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.swarm.delegate :as delegate]
            [hive-mcp.tools.swarm.jvm.parser :as parser]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private impl-ns "hive-agent.swarm.tools.jvm.parser")

(defn- addon? [] (delegate/available? impl-ns))

(defn- shim-answer
  "Call F on ARGS: [:ok out] or [:missing ex-data]; anything else rethrows."
  [f args]
  (try [:ok (apply f args)]
       (catch clojure.lang.ExceptionInfo e
         (if (:swarm/addon-missing (ex-data e)) [:missing (ex-data e)] (throw e)))))

(def ^:private contract
  (m/schema (get-in (m/function-schemas) ['hive-mcp.tools.swarm.jvm.parser 'parse-etime-to-minutes :schema])))

(defspec parse-etime-shim-answers-its-contract-or-fails-loud 100
  (prop/for-all [args (mg/generator (first (m/children contract)))]
    (let [[tag v] (shim-answer parser/parse-etime-to-minutes args)]
      (if (addon?)
        (and (= :ok tag) (m/validate (second (m/children contract)) v))
        (and (= :missing tag) (= "hive-agent" (:addon v)) (= impl-ns (:ns v)))))))

(deftest-golden parser-shim-without-the-addon
  "test/golden/tools/swarm/jvm/parser-shim.edn"
  (if (addon?)
    :addon-present
    {:etime              (shim-answer parser/parse-etime-to-minutes ["01:30"])
     :etime-nil          (shim-answer parser/parse-etime-to-minutes [nil])
     :process-line       parser/parse-process-line
     :process-line-ext   parser/parse-process-line-extended}))

(deftest the-shim-declares-its-arglists
  (is (= '([etime]) (:arglists (meta #'parser/parse-etime-to-minutes)))))

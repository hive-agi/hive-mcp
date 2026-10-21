(ns hive-mcp.tools.migrate.optional-contract-test
  "Contract facets for the late-bound backend resolver: a present namespace
   answers its Var (callable as the fn), an absent one throws a directed
   error naming what is missing."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.migrate.optional :as opt]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema backend-var-resolves-present-vars
  #'opt/backend-var
  {:in [:cat [:= "clojure.string"] [:enum 'trim 'upper-case 'blank? "lower-case" :join]]
   :out opt/Var_
   :rel (fn [[ns-str sym] out] (= out (resolve (symbol ns-str (name sym)))))
   :num-tests 50})

(golden/deftest-golden backend-var-golden
  "test/golden/hive-mcp/tools/migrate/optional-backend-var.edn"
  {:present ((opt/backend-var "clojure.string" 'upper-case) "migrate")
   :absent  (try (opt/backend-var "clojure.string" 'no-such-fn)
                 (catch clojure.lang.ExceptionInfo e (ex-data e)))})

(deftest an-absent-namespace-throws
  (is (thrown? Exception (opt/backend-var "no.such.backend.m29" 'x))))

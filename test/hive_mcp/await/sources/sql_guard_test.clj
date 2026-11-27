(ns hive-mcp.await.sources.sql-guard-test
  "The read-only SELECT guard and $n binding of :sql-predicate. Pure."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.await.sources.sql-guard :as guard]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- refusal [sql & [params]]
  (try (guard/bind sql params) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest the-reference-query-binds-through-placeholders
  (is (= {:sql "select 1 from contributor.contributors where id=? and address_complement=?"
          :params [42 "Apto 3; drop table x"]}
         (guard/bind "select 1 from contributor.contributors where id=$1 and address_complement=$2"
                     [42 "Apto 3; drop table x"]))
      "a hostile VALUE is bound, never parsed: it is not in the SQL text"))

(deftest placeholders-bind-in-occurrence-order
  (is (= {:sql "select ? , ?, ?" :params ["b" "a" "b"]}
         (guard/bind "select $2 , $1, $2" ["a" "b"]))))

(deftest a-placeholder-inside-a-literal-is-text
  (is (= {:sql "select '$1' as s, ? as v" :params [7]}
         (guard/bind "select '$1' as s, $1 as v" [7]))))

(deftest keywords-inside-literals-do-not-count
  (is (= {:sql "select 1 from t where note = 'please delete; then insert' " :params []}
         (guard/bind "select 1 from t where note = 'please delete; then insert' ;" []))))

(deftest only-one-select-is-accepted
  (testing "writes and DDL"
    (doseq [sql ["insert into t values (1)" "update t set a = 1" "delete from t"
                 "drop table t" "truncate t" "create table x (a int)"
                 "with d as (delete from t returning *) select * from d"
                 "select * into newt from t" "select 1 for update"]]
      (is (= :await/invalid-subscription (:error (refusal sql))) sql)))
  (testing "a second statement, comments, dollar quotes, bare ?"
    (doseq [sql ["select 1; select 2" "select 1; delete from t" "select 1 -- x"
                 "select /* x */ 1" "select $$x$$" "select ?"]]
      (is (= :await/invalid-subscription (:error (refusal sql))) sql)))
  (testing "side-effecting server functions"
    (doseq [sql ["select pg_sleep(10)" "select pg_terminate_backend(1)"
                 "select set_config('a','b',false)" "select nextval('s')"
                 "select * from dblink('x','y')"]]
      (is (:function (refusal sql)) sql)))
  (testing "WITH ... SELECT is fine"
    (is (map? (guard/bind "with a as (select 1 as x) select x from a" [])))))

(deftest params-must-fit-placeholders
  (is (re-find #"\$2 has no value" (str (:placeholders (refusal "select $2" ["a"])) " "
                                        (try (guard/bind "select $1, $2" ["a"])
                                             (catch Exception e (.getMessage e))))))
  (is (= [2] (:unused (refusal "select $1" ["a" "b"]))))
  (is (:bad (refusal "select $1" [{:a 1}])) "only scalars bind"))

(deftest an-unterminated-literal-is-refused
  (is (= :await/invalid-subscription (:error (refusal "select 'abc")))))

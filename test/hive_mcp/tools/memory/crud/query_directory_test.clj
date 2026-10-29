(ns hive-mcp.tools.memory.crud.query-directory-test
  "memory query with no :directory resolved its project from the request
   context alone. On the MCP path that is nil, so the project became
   \"global\" and `memory query type=tool-friction` answered [] although the
   reports exist under their owner projects (card 20261024010152-071afc59)."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.context.request :as ctx]
            [hive-mcp.tools.memory.crud.query :as q]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest an-explicit-directory-wins
  (with-redefs [ctx/current-directory (constantly "/req")
                ctx/resolve-caller-directory (fn [_] "/caller")]
    (is (= "/mine" (q/query-directory "/mine")))))

(deftest an-omitted-directory-falls-back-to-the-request-then-the-caller
  (testing "the request context directory when it has one"
    (with-redefs [ctx/current-directory (constantly "/req")
                  ctx/resolve-caller-directory (fn [_] "/caller")]
      (is (= "/req" (q/query-directory nil)))
      (is (= "/req" (q/query-directory "  ")) "blank counts as absent")))
  (testing "the live bug: no request directory no longer means global"
    (with-redefs [ctx/current-directory (constantly nil)
                  ctx/resolve-caller-directory (fn [_] "/home/u/proj")]
      (is (= "/home/u/proj" (q/query-directory nil))))))

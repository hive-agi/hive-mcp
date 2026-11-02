(ns hive-mcp.agent.ling.backend-gap-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.ling.headless-registry :as hreg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest a-configured-backend-that-is-not-registered-is-a-gap
  (is (= {:configured :hive-agent :registered #{:claude-process}}
         (hreg/backend-gap :hive-agent #{:claude-process})))
  (testing "an empty registry is still a gap, naming no alternative"
    (is (= {:configured :hive-agent :registered #{}}
           (hreg/backend-gap :hive-agent nil))))
  (testing "a sequential registry is read as a set"
    (is (= #{:a :b} (:registered (hreg/backend-gap :hive-agent [:a :b :a]))))))

(deftest no-gap-when-the-choice-is-open-or-satisfied
  (testing "registered"
    (is (nil? (hreg/backend-gap :hive-agent #{:hive-agent :claude-process}))))
  (testing ":auto keeps priority selection"
    (is (nil? (hreg/backend-gap :auto #{:claude-process}))))
  (testing "nothing configured"
    (is (nil? (hreg/backend-gap nil #{:claude-process}))))
  (testing "a non-keyword is not a configured backend"
    (is (nil? (hreg/backend-gap "hive-agent" #{})))))

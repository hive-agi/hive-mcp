(ns hive-mcp.server.log-level-test
  "HIVE_MCP_LOG_LEVEL parsing: a known level is honoured, anything else is
   :info. Pure: the env value is passed in, the environment is not read."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.server.log-sink :as sink]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest defaults-to-info-test
  (is (= :info (sink/parse-log-level nil)))
  (is (= :info (sink/parse-log-level "")))
  (is (= :info (sink/parse-log-level "   ")))
  (is (= :info (sink/parse-log-level "verbose"))))

(deftest known-levels-are-honoured-test
  (is (= :debug (sink/parse-log-level "debug")))
  (is (= :warn (sink/parse-log-level ":warn")))
  (is (= :error (sink/parse-log-level " ERROR ")))
  (is (every? #(= % (sink/parse-log-level (name %))) sink/log-levels)))

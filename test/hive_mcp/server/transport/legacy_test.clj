(ns hive-mcp.server.transport.legacy-test
  "Facets for the legacy TCP channel starter. The configured port is one the
   socket layer refuses, so the start fails inside its rescue and the
   coordinator-running mark is never set."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.source :as src]
            [hive-mcp.server.transport.legacy :as legacy]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- with-port [port]
  (src/atom-source (atom {:services {:channel {:port port}}}) {}))

(use-fixtures :each (fn [f] (binding [config/*config-source* (with-port -1)] (f))))

(golden/deftest-golden legacy-start-golden
  "test/golden/hive-mcp/server/transport/legacy.edn"
  {:refused-port (legacy/start-legacy-channel!)
   :out-of-range (binding [config/*config-source* (with-port 70000)]
                   (legacy/start-legacy-channel!))})

(deftest a-failed-start-never-throws
  (is (nil? (legacy/start-legacy-channel!))))

(ns hive-mcp.tools.kg.queries-health-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.knowledge-graph.connection :as connection]
            [hive-mcp.knowledge-graph.edges :as edges]
            [hive-mcp.tools.core :as tools]
            [hive-mcp.tools.kg.queries :as queries]
            [malli.core :as m]
            [clojure.data.json :as json]))

(deftest kg-stats-includes-backend-health-test
  ;; Reads the real MCP response (JSON text) instead of redefining
  ;; tools/mcp-json: the handler's contract promises an McpResponse.
  (let [health {:status :healthy
                :backend :datahike
                :compatible? true}]
    (with-redefs [edges/edge-stats
                  (constantly {:total-edges 3
                               :by-relation {:implements 2}
                               :by-scope {"hive" 3}})
                  connection/backend-health (constantly health)]
      (let [response (queries/handle-kg-stats {})]
        (is (m/validate tools/McpResponse response))
        (is (= {:success true
                :total-edges 3
                :by-relation {:implements 2}
                :by-scope {:hive 3}
                :backend-health {:status "healthy"
                                 :backend "datahike"
                                 :compatible? true}}
               (json/read-str (:text response) :key-fn keyword)))))))

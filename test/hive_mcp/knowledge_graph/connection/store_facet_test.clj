(ns hive-mcp.knowledge-graph.connection.store-facet-test
  (:require [hive-mcp.knowledge-graph.connection.store :as subject]
            [hive-mcp.knowledge-graph.protocol :as proto]
            [hive-mcp.knowledge-graph.store.datascript :as ds-store]
            [hive-test.golden :as golden]))

(defn- live-with
  "store-live? with the process store set to STORE (nil clears it), restoring
   whatever store the suite had installed before."
  [store]
  (let [prior (when (proto/store-set?) (proto/get-store))]
    (try
      (if store (proto/set-store! store) (proto/clear-store!))
      (subject/store-live?)
      (finally
        (if prior (proto/set-store! prior) (proto/clear-store!))))))

(golden/deftest-golden store-golden
  "test/golden/hive-mcp/m13-connection-store.edn"
  {:no-store (live-with nil)
   :datascript-store (live-with (ds-store/create-store))})

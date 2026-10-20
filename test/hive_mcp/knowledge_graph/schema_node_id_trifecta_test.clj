(ns hive-mcp.knowledge-graph.schema-node-id-trifecta-test
  (:require [hive-mcp.knowledge-graph.schema :as schema]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.generator :as mg]))

;; The generator deliberately reaches valid, whitespace-only, placeholder,
;; and non-string arms instead of merely exercising the happy path.
(def NodeIdProbe [:enum "node-1" "x" " " "" "$0" "$ref:$0.id" nil 42])

(deftrifecta valid-node-id-contract
  hive-mcp.knowledge-graph.schema/valid-node-id?
  {:golden-path "test/golden/hive-mcp/m13-valid-node-id.edn"
   :cases {:valid "node-1" :short "x" :blank " " :empty ""
           :placeholder "$0" :reference "$ref:$0.id" :nil nil :number 42}
   :gen (mg/generator NodeIdProbe)
   :pred boolean?
   :apply? false
   :num-tests 500
   :mutations [["accept-all" (constantly true)]
               ["reject-all" (constantly false)]
               ["accept-unresolved-reference" (fn [x] (and (string? x)
                                                            (not (.isBlank ^String x))))]]})

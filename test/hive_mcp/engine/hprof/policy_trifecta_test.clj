(ns hive-mcp.engine.hprof.policy-trifecta-test
  "Generated hprof sizes exercise the contracted retention arithmetic."
  (:require [hive-mcp.engine.hprof.policy :as policy]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema total-bytes-trifecta
  hive-mcp.engine.hprof.policy/total-bytes
  {:in [:cat [:vector [:map [:bytes {:optional true} [:maybe [:int {:min 0 :max 1000000}]]]]]]
   :out [:int {:min 0}]
   :rel (fn [[files] total]
          (= total (reduce + 0 (map #(or (:bytes %) 0) files))))
   :num-tests 500
   :seed 109
   :n-cases 12
   :golden-path "test/golden/engine/hprof-total-bytes.edn"})

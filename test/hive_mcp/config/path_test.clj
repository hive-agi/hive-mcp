(ns hive-mcp.config.path-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.config.path :as path]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; The contract permits arbitrary environment values; the generator covers
;; the useful string/nil subset without narrowing the accepted input.
(def PathArgs [:cat [:maybe :string] :string])
(def PathResult :string)

(deftrifecta-from-schema path-selection hive-mcp.config.path/resolve-config-path
  {:in PathArgs :out PathResult
   :rel (fn [[env home] out]
          (= out (if (and (string? env) (not (.isBlank ^String env)))
                   env
                   (str home "/.config/hive-mcp/config.edn"))))
   :num-tests 500
   :golden-path "test/golden/config/path-selection.edn"})

(deftest path-examples
  (is (= "/tmp/.config/hive-mcp/config.edn" (path/resolve-config-path "  " "/tmp")))
  (is (= "alt.edn" (path/resolve-config-path "alt.edn" "/tmp"))))

(deftest-contract-coverage config-path-contracts
  {:paths ["src/hive_mcp/config/path.clj"]})

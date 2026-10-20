(ns hive-mcp.dns.validation-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.dns.validation :as v]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]))

(deftrifecta-from-schema snake-to-kebab-trifecta v/snake->kebab
  {:in [:cat [:string {:min 1}]]
   :out :string
   :rel (fn [[input] output]
          (= output (clojure.string/replace input "_" "-")))
   :num-tests 500})

(deftest validation-boundaries
  (is (nil? (v/validate-code "(+ 1 2)")))
  (is (= :missing (:reason (ex-data (try (v/validate-code nil)
                                         (catch clojure.lang.ExceptionInfo e e))))))
  (is (= :out-of-range (:reason (ex-data (try (v/validate-port 65536)
                                              (catch clojure.lang.ExceptionInfo e e))))))
  (is (= "\"hello\\\"world\"" (v/elisp-string "hello\"world")))
  (is (= {:some-key 1 :other-value 2}
         (v/normalize-params {:some_key 1 :other_value 2}))))

(deftest-contract-coverage dns-contracts
  {:paths ["src/hive_mcp/dns/validation.clj"
           "src/hive_mcp/dns/result/taxonomy.clj"
           "src/hive_mcp/dns/result/spec.clj"]})

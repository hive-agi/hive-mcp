(ns hive-mcp.dispatch.contract-coverage-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.dispatch.verbs :as verbs]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]))

(deftrifecta-from-schema verb-key-trifecta verbs/verb-key
  {:in [:cat [:enum "open" "save-file" :close]]
   :out :keyword
   :rel (fn [[input] result]
          (= result (if (keyword? input) input (keyword input))))
   :num-tests 500})

(deftest resolution-examples
  (is (= :static (:source (verbs/resolve-verb {:open inc} {} :open))))
  (is (= :unknown (:source (verbs/resolve-verb {} {} :open))))
  (is (= :contributed
         (:source (verbs/resolve-verb {} {:open {:handler inc :owner "addon"}} :open)))))

(deftest-contract-coverage dispatch-contracts
  {:paths ["src/hive_mcp/dispatch"]})

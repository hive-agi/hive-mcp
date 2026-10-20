(ns hive-mcp.dns.result.taxonomy-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.dns.result.taxonomy :as tax]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema known-error-trifecta tax/known-error?
  {:in [:cat [:enum :io/timeout :sdk/tool-not-found :unknown/error]]
   :out :boolean
   :rel (fn [[category] result]
          (= result (contains? #{:io/timeout :sdk/tool-not-found} category)))
   :num-tests 500})

(deftest taxonomy-examples
  (is (tax/known-error? :io/timeout))
  (is (tax/known-error? :sdk/tool-not-found))
  (is (false? (tax/known-error? :unknown/error)))
  (is (false? (tax/known-error? nil))))

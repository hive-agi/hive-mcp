(ns hive-mcp.config.headless-defaults-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.config.headless-defaults :as defaults]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema auto-sentinel hive-mcp.config.headless-defaults/auto?
  {:in [:maybe [:enum :auto :hive-agent :other]]
   :out :boolean
   :rel (fn [in out] (= (= :auto in) out))
   :golden-path "test/golden/config/auto-sentinel.edn"
   :num-tests 500})

(deftest explicit-resolution
  (is (= :custom (:default-backend (defaults/resolve! {:default-backend :custom}))))
  (is (keyword? (defaults/default-backend))))

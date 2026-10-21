(ns hive-mcp.spi.op-schema-registry.facet-test
  "Facets for hive-mcp.spi.op-schema-registry (cov-facets f07): the owner-
   tracking router is driven with private owners and :f07.op/* schema keys,
   and both registries (ownership here, hive-spi's core-op registry) are
   cleaned through deregister-by-owner!, the router's own teardown."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.spi.op-schema-registry :as osr]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- owned-by [owners]
  (into (sorted-map)
        (keep (fn [o] (when-let [ks (get (osr/owned) o)] [o (vec (sort ks))])))
        owners))

(deftest-golden op-schema-registry-golden
  "test/golden/hive-mcp/f07-op-schema-registry.edn"
  (try
    (let [r1 (osr/register-by-key! :f07/addon-a :op-schema/a
                                   {:f07.op/alpha [:map [:x :int]]
                                    :f07.op/beta  :string})
          r2 (osr/register-by-key! :f07/addon-b :op-schema/b {:f07.op/gamma :int})
          r3 (osr/register-by-key! :f07/addon-a :op-schema/bad [:not :a-map])
          owned-before (owned-by [:f07/addon-a :f07/addon-b])
          d1 (osr/deregister-by-owner! :f07/addon-a)
          d2 (osr/deregister-by-owner! :f07/addon-a)]
      {:registered  [(vec (sort r1)) (vec (sort r2)) r3]
       :owned       owned-before
       :deregister  [(vec (sort d1)) d2]
       :owned-after (owned-by [:f07/addon-a :f07/addon-b])})
    (finally
      (osr/deregister-by-owner! :f07/addon-a)
      (osr/deregister-by-owner! :f07/addon-b))))

(deftest owners-do-not-clobber-each-other
  (try
    (osr/register-by-key! :f07/one :op-schema/x {:f07.op/shared-1 :int})
    (osr/register-by-key! :f07/two :op-schema/y {:f07.op/shared-2 :int})
    (osr/deregister-by-owner! :f07/one)
    (is (= #{:f07.op/shared-2} (get (osr/owned) :f07/two)))
    (finally
      (osr/deregister-by-owner! :f07/one)
      (osr/deregister-by-owner! :f07/two))))

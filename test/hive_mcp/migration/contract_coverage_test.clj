(ns hive-mcp.migration.contract-coverage-test
  "Schema-driven migration contracts and the directory coverage gate."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.migration.core :as core]
            [hive-mcp.migration.adapter :as adapter]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :as hst]))

(deftest-contract-coverage migration-contracts
  {:paths ["src/hive_mcp/migration"]})

(hst/deftrifecta-from-schema backup-name-contract
  hive-mcp.migration.core/parse-backup-name
  {:in [:enum "kg-datalevin-20261019T120000.edn" "bad" "memory-chroma-20260101T000000.edn"]
   :out [:maybe [:map [:scope :keyword] [:backend :keyword]
                 [:timestamp :string] [:filename :string]]]
   :rel (fn [in out] (if (= in "bad") (nil? out)
                         (= (:filename out) in)))
   :golden-path "test/golden/hive-mcp/migration/backup-name-contract.edn"
   :num-tests 100})

(hst/deftrifecta-from-schema scope-compatibility-contract
  hive-mcp.migration.core/check-scope-compatibility
  {:in [:cat [:map [:backup/project-id {:optional true} [:enum "global" "alpha" "beta"]]]
        [:maybe [:enum "alpha" "beta"]]]
   :out [:map [:compatible? :boolean] [:backup-project-id :string]
         [:target-project-id [:maybe :string]] [:reason :string]]
   :rel (fn [[backup target] out]
          (= (:compatible? out)
             (or (nil? target) (= "global" (get backup :backup/project-id "global"))
                 (= target (:backup/project-id backup)))))
   :golden-path "test/golden/hive-mcp/migration/scope-compatibility-contract.edn"
   :num-tests 100})

(hst/deftrifecta-from-schema identity-export-contract
  hive-mcp.migration.adapter/transform-export
  {:in [:cat [:enum :edn] [:map [:item :int]]]
   :out [:map [:item :int]]
   :rel (fn [[_ data] out] (= data out))
   :golden-path "test/golden/hive-mcp/migration/identity-export-contract.edn"
   :num-tests 100})

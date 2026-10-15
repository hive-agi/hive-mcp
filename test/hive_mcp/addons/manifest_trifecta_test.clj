(ns hive-mcp.addons.manifest-trifecta-test
  "Trifecta for the pure fns of `hive-mcp.addons.manifest`: env-template
   stripping, the compact summary, load ordering and validation."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.addons.manifest :as manifest]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(hst/deftrifecta-from-schema strip-env-templates-contract
  hive-mcp.addons.manifest/strip-env-templates
  {:in        [:map-of :keyword [:or :int [:enum "${HOME}" "literal" "pre-${X}-post"]]]
   :out       :map
   :rel       (fn [in out]
                (and (every? #(not= "${HOME}" %) (vals out))
                     (every? (fn [[k v]] (or (= "${HOME}" v) (= v (get out k)))) in)))
   :num-tests 100
   :golden-path "test/golden/addons/manifest-strip-env.edn"})

(hst/deftrifecta-from-schema manifest-summary-contract
  hive-mcp.addons.manifest/manifest-summary
  {:in        [:map
               [:addon/id :string]
               [:addon/type [:enum :native :mcp-bridge :external]]
               [:addon/version {:optional true} [:enum "0.1.0" "2.0.0"]]
               [:addon/capabilities {:optional true} [:set [:enum :tools :mcp-bridge]]]
               [:addon/dependencies {:optional true} [:set [:enum "hive.memory" "hive.kg"]]]]
   :out       manifest/ManifestSummary
   ;; the optional keys are generated, so BOTH the carried and the defaulted
   ;; arm of version / capabilities / dependencies are reached
   :rel       (fn [in out]
                (and (= (:addon/id in) (:id out))
                     (= (:addon/type in) (:type out))
                     (= (get in :addon/version "0.0.0") (:version out))
                     (= (or (:addon/capabilities in) #{}) (:capabilities out))
                     (= (or (:addon/dependencies in) #{}) (:dependencies out))))
   :num-tests 100
   :golden-path "test/golden/addons/manifest-summary.edn"})

(defn- mk' [id deps]
  {:addon/id id :addon/type :native :addon/init-ns "n" :addon/init-fn "f"
   :addon/dependencies deps})

(deftest manifest-order-and-validation-test
  (let [{:keys [ordered cycles]} (manifest/manifests-load-order [(mk' "b" #{"a"}) (mk' "a" #{})])]
    (is (= ["a" "b"] (mapv :addon/id ordered)) "dependencies mount first")
    (is (= #{} cycles)))
  (is (seq (:cycles (manifest/manifests-load-order [(mk' "x" #{"y"}) (mk' "y" #{"x"})]))))
  (is (:valid? (manifest/validate-manifest (mk' "a" #{}))))
  (is (false? (:valid? (manifest/validate-manifest {:addon/id ""}))))
  (is (false? (:valid? (manifest/parse-manifest "{:unterminated")))))

(deftest manifest-schemas-generate-test
  (doseq [s [manifest/Validation manifest/ParseResult manifest/ManifestSummary
             manifest/LoadOrder manifest/ScanResult]]
    (is (seq (mg/sample s {:size 4})) (str "generates: " (pr-str s)))
    (is (m/schema s))))

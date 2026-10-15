(ns hive-mcp.addons.contract-coverage-test
  "Gate: every public fn under src/hive_mcp/addons carries an `m/=>` contract
   or a REASONED exemption, and no exemption is stale. Also the trifectas for
   the namespaces whose subjects are pure: runtime-ports (projections),
   manifest (config and ordering), di (service registry)."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.addons.di :as di]
            [hive-mcp.addons.manifest :as manifest]
            [hive-mcp.addons.runtime-ports :as ports]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; headless.clj defines the aliases `headless-id` ... by `(def x spi/x)`, which
;; the defn-reading universe never lists, so only `headless-addon?` is counted.
(deftest-contract-coverage addons-contracts-gate
  {:paths ["src/hive_mcp/addons"]
   :exempt {}})

;; ---- runtime-ports: pure projections ------------------------------------

(hst/deftrifecta-from-schema slave->ling-contract
  hive-mcp.addons.runtime-ports/slave->ling
  {:in        [:maybe [:map
                       [:slave/id {:optional true} :string]
                       [:slave/status {:optional true} [:enum :idle :working]]
                       [:slave/project-id {:optional true} :string]]]
   :out       [:maybe :map]
   :rel       (fn [in out] (if (nil? in) (nil? out) (= (:slave/id in) (:ling/id out))))
   :num-tests 100
   :golden-path "test/golden/addons/runtime-ports-slave-ling.edn"})

(hst/deftrifecta-from-schema ling-updates->slave-contract
  hive-mcp.addons.runtime-ports/ling-updates->slave
  {:in        [:map
               [:ling/status {:optional true} [:enum :idle :working]]
               [:ling/project-id {:optional true} :string]]
   :out       :map
   :rel       (fn [in out]
                (and (= (contains? in :ling/status) (contains? out :slave/status))
                     (= (contains? in :ling/project-id) (contains? out :slave/project-id))))
   :num-tests 100
   :golden-path "test/golden/addons/runtime-ports-ling-updates.edn"})

(deftest runtime-ports-shape-test
  (testing "the port map is keyword -> callable, with the documented keys"
    (let [p (ports/runtime-ports)]
      (is (m/validate ports/Ports p))
      (is (every? #(contains? p %) [:tools/invoke :memory/store :host/shared-jvm? :extension/get]))))
  (testing "shared-jvm? is true exactly when the injected system var holds a system"
    (is (true? (ports/shared-jvm? (constantly (atom {:up true})))))
    (is (false? (ports/shared-jvm? (constantly nil)))))
  (testing "invoke-registered-tool! refuses an unknown tool"
    (is (thrown? clojure.lang.ExceptionInfo
                 (ports/invoke-registered-tool! (constantly []) "nope" {})))
    (is (= {:a 1} (ports/invoke-registered-tool!
                   (constantly [{:name "t" :handler identity}]) "t" {:a 1})))))

;; ---- manifest: pure fns -------------------------------------------------

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
  {:in        [:map [:addon/id :string] [:addon/type [:enum :native :mcp-bridge :external]]]
   :out       manifest/ManifestSummary
   :rel       (fn [in out] (and (= (:addon/id in) (:id out)) (= "0.0.0" (:version out))))
   :num-tests 100})

(defn- mk' [id deps] {:addon/id id :addon/type :native :addon/init-ns "n" :addon/init-fn "f"
                      :addon/dependencies deps})

(deftest manifest-order-and-validation-test
  (let [{:keys [ordered cycles]} (manifest/manifests-load-order [(mk' "b" #{"a"}) (mk' "a" #{})])]
    (is (= ["a" "b"] (mapv :addon/id ordered)) "dependencies mount first")
    (is (= #{} cycles)))
  (is (seq (:cycles (manifest/manifests-load-order [(mk' "x" #{"y"}) (mk' "y" #{"x"})]))))
  (is (:valid? (manifest/validate-manifest (mk' "a" #{}))))
  (is (false? (:valid? (manifest/validate-manifest {:addon/id ""}))))
  (is (false? (:valid? (manifest/parse-manifest "{:unterminated")))))

(defn- mk' [id deps] {:addon/id id :addon/type :native :addon/init-ns "n" :addon/init-fn "f"
                      :addon/dependencies deps})

;; ---- di: the service registry, driven as a stateful sequence --------------

(deftest service-registry-roundtrip-test
  (di/clear-services!)
  (try
    (is (= :k (di/register-service! :k 1)))
    (is (di/service-registered? :k))
    (is (= 1 (di/get-service :k)))
    (is (= :dflt (di/get-service :absent :dflt)))
    (is (= #{:k} (di/list-services)))
    (is (thrown? clojure.lang.ExceptionInfo (di/require-service :absent)))
    (is (= {:satisfied? true :missing #{} :available #{}}
           (di/check-service-dependencies (Object.))))
    (is (nil? (di/resolve-symbols {:x 'no.such.ns/nothing})) "any unresolvable symbol -> nil")
    (is (= :k (di/deregister-service! :k)))
    (is (not (di/service-registered? :k)))
    (finally (di/clear-services!))))

(deftest schemas-generate-test
  (doseq [s [ports/Ports manifest/Validation manifest/ParseResult manifest/ManifestSummary
             manifest/LoadOrder manifest/ScanResult]]
    (is (seq (mg/sample s {:size 4})) (str "generates: " (pr-str s)))))

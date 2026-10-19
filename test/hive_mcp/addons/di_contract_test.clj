(ns hive-mcp.addons.di-contract-test
  "Contract tests for the service registry in `hive-mcp.addons.di`. The
   registry is one global atom, so it is driven as a stateful sequence over
   keys and values GENERATED from the schemas, never through a trifecta that
   would run its cases concurrently against the shared state."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.addons.di :as di]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f] (di/clear-services!) (try (f) (finally (di/clear-services!)))))

(def ^:private Key [:keyword {:gen/elements [:a :b/c :hive/kg-store :x.y/z]}])
(def ^:private Val [:or :int :string :boolean :keyword])

(deftest service-registry-roundtrip-test
  (doseq [[k v] (mg/sample [:tuple Key Val] {:size 10 :seed 7})]
    (testing (str "key " k)
      (di/clear-services!)
      (is (= k (di/register-service! k v)) "register answers the key")
      (is (true? (di/service-registered? k)))
      (is (= v (di/get-service k)))
      (is (= v (di/get-service k :dflt)))
      (is (= v (di/require-service k)) "even a false value is found")
      (is (= #{k} (di/list-services)))
      (is (= k (di/deregister-service! k)))
      (is (false? (di/service-registered? k)))
      (is (= :dflt (di/get-service k :dflt)))
      (is (thrown? clojure.lang.ExceptionInfo (di/require-service k))))))

(deftest batch-registration-test
  (is (= [:a :b] (sort (di/register-services! {:a 1 :b 2}))))
  (is (= #{:a :b} (di/list-services)))
  (is (nil? (di/clear-services!)))
  (is (= #{} (di/list-services))))

(deftest dependency-report-test
  (let [consumer (fn [req] (reify di/IServiceConsumer
                             (required-services [_] req)
                             (optional-services [_] #{})))]
    (di/register-service! :have 1)
    (is (= {:satisfied? true :missing #{} :available #{:have}}
           (di/check-service-dependencies (consumer #{:have}))))
    (is (= {:satisfied? false :missing #{:lack} :available #{:have}}
           (di/check-service-dependencies (consumer #{:have :lack}))))
    (is (= {:satisfied? true :missing #{} :available #{}}
           (di/check-service-dependencies (Object.))))))

(deftest symbol-resolution-contract-test
  (testing "a resolvable symbol answers its var, an unresolvable one nil"
    (is (var? (di/resolve-symbol 'clojure.core/inc)))
    (is (nil? (di/resolve-symbol 'no.such.ns/nothing)))
    (is (nil? (di/resolve-symbol 'clojure.core/no-such-fn-xyz))))
  (testing "resolve-symbols is all-or-nothing"
    (is (= {:i #'clojure.core/inc} (di/resolve-symbols {:i 'clojure.core/inc})))
    (is (nil? (di/resolve-symbols {:i 'clojure.core/inc :x 'no.such.ns/nothing})))))

(deftest contracts-discriminate-test
  (let [reg-schema (-> (m/function-schemas) (get 'hive-mcp.addons.di) (get 'register-service!) :schema)
        in-schema  (m/-function-info reg-schema)]
    (testing "register-service! admits only keyword keys and non-nil values"
      (is (some? in-schema))
      (is (not (m/validate (:input in-schema) [{:not :key} 1])))
      (is (not (m/validate (:input in-schema) [:k nil])))
      (is (m/validate (:input in-schema) [:k false])))))

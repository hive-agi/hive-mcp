(ns hive-mcp.test-support.registry-isolation-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.protocols.memory :as memory]
            [hive-mcp.test-support.registry-isolation :as isolation]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.protocols.memory-test :as memory-test]))

(deftrifecta empty-addon-registry
  isolation/safe-test-registry?
  {:golden-path "test/golden/hive-mcp/registry-isolation/empty-addon-registry.edn"
   :cases {:empty {:total 0 :active 0 :registered 0 :error 0}
           :active {:total 1 :active 1 :registered 0 :error 0}
           :missing-field {:total 0 :active 0 :registered 0}
           :unknown nil}
   :gen (gen/fmap (fn [n] {:total n :active n :registered 0 :error 0})
                  (gen/choose 0 20))
   :pred boolean?
   :property-type :pred
   :num-tests 50
   :mutations [["always-safe" (fn [_] true)]]})

(use-fixtures :each isolation/guard-live-registry-fixture)

(deftest isolated-registry-restores-after-success-and-throw
  (testing "a test-local store slot is removed even when the body throws"
    (let [store (memory-test/->StubStore :test-only)]
      (is (thrown? Exception
                   (isolation/with-isolated-store-registry
                    (fn []
                      (memory/register-store! :test-only store)
                      (throw (Exception. "body failed"))))))
      (is (empty? (memory/registered-stores)))
      (isolation/with-isolated-store-registry
       (fn [] (memory/register-store! :test-only store)))
      (is (empty? (memory/registered-stores))))))

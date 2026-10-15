(ns hive-mcp.agent.provider-registry-contract-test
  "Facets for the provider pipeline and the tool/agent registry.

   Both are impure (host config, secrets, atoms). The registry is driven
   through its own state with a fixture that seeds three tools; the contracted
   subjects are called directly. Provider results are validated against the
   contract schemas, arm by arm."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.provider :as p]
            [hive-mcp.agent.registry :as reg]
            [hive-mcp.agent.runtime-schema :as schema]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private seeded ["alpha" "beta" "gamma"])

(use-fixtures :each
  (fn [f]
    (reg/clear-agents!)
    (reset! reg/registry {})
    (reg/register! (for [n seeded] {:name n :description (str n " tool") :handler (fn [_] n)}))
    (try (f) (finally (reg/clear-agents!) (reset! reg/registry {})))))

(def ToolNames [:vector {:max 5} [:enum "alpha" "beta" "gamma" "delta"]])

(deftrifecta-from-schema get-schemas-trifecta
  hive-mcp.agent.registry/get-schemas
  {:in [:maybe ToolNames]
   :out [:vector [:map-of :any :any]]
   :rel (fn [names out]
          (and (every? #(not (contains? % :handler)) out)
               (= (count out)
                  (if names (count (filter (set seeded) (distinct names))) (count seeded)))))
   :mutation false
   :num-tests 100})

(deftrifecta-from-schema get-tool-trifecta
  hive-mcp.agent.registry/get-tool
  {:in [:enum "alpha" "beta" "delta"]
   :out [:maybe [:map-of :any :any]]
   :rel (fn [n out] (= (some? out) (boolean (some #{n} seeded))))
   :mutation false
   :num-tests 30})

(deftest registry-tools-and-agents
  (testing "tools"
    (is (= (set seeded) (set (reg/list-tools))))
    (is (= "alpha" (:name (reg/get-tool "alpha"))))
    (is (fn? (:handler (reg/get-tool "alpha"))) "the registry keeps handlers; get-schemas strips them")
    (is (= 2 (count (reg/get-schemas ["alpha" "beta"])))))
  (testing "refresh! replaces the table in one reset and returns the count"
    (binding [reg/*advertised-tools* (fn [] [{:name "only"}])]
      (is (= 1 (reg/refresh!)))
      (is (= ["only"] (vec (reg/list-tools)))))
    (binding [reg/*advertised-tools* (fn [] [])]
      (is (= 0 (reg/refresh!)))))
  (testing "agents"
    (is (= "a1" (reg/register-agent! {:id "a1"})))
    (is (= {:id "a1"} (reg/get-agent-by-id "a1")))
    (is (= 1 (count (reg/list-all-agents))))
    (is (= [] (vec (do (reg/clear-agents!) (reg/register-agent! {:id "a1"}) (reg/unregister-agent! "a1") (reg/list-lings)))) "empty registry has no lings")
    (is (= "a1" (reg/unregister-agent! "a1")))
    (is (nil? (reg/get-agent-by-id "a1")))
    (reg/register-agent! {:id "a2"})
    (reg/clear-agents!)
    (is (empty? (reg/list-all-agents)))))

(deftest ensure-registered-is-lazy-and-idempotent
  (reset! reg/registry {})
  (is (nil? (reg/ensure-registered!)) "returns nil whichever branch it takes")
  (is (nil? (reg/ensure-registered!))))

(deftest provider-results-validate-their-contract
  (testing "registry and order"
    (is (m/validate [:map-of :keyword [:map-of :any :any]] (p/effective-registry)))
    (is (every? keyword? (p/effective-priority)))
    (is (every? keyword? (p/available-providers)))
    (is (or (nil? (p/best-available-provider)) (keyword? (p/best-available-provider)))))
  (testing "validate-provider: both arms"
    (is (nil? (p/validate-provider :openrouter)))
    (let [err (p/validate-provider :definitely-not-a-provider)]
      (is (= :unknown-provider (:error err)))
      (is (seq (:available err)))))
  (testing "diagnostic rows follow the priority order"
    (is (= (vec (p/effective-priority)) (mapv :provider (p/provider-diagnostic)))))
  (testing "credential-refusal reads the probe answer"
    (is (nil? (p/credential-refusal :openrouter {:status 200 :ok? true})))
    (let [r (p/credential-refusal :openrouter {:status 401})]
      (is (= :provider-credential-rejected (:error r)))
      (is (= 401 (:status r)))))
  (testing "resolve-provider-model returns the routing when explicit"
    (is (= {:provider :openrouter :model "x"}
           (select-keys (p/resolve-provider-model {:provider :openrouter :model "x"}) [:provider :model]))))
  (testing "an unknown provider throws instead of answering"
    (is (thrown? clojure.lang.ExceptionInfo (p/resolve-provider-model {:provider :nope :model "x"})))))

(golden/deftest-golden-fn registry-surface-golden
  "test/golden/agent/registry-surface.edn"
  (fn []
    {:schemas (reg/get-schemas ["alpha" "delta"])
     :all-names (sort (reg/list-tools))
     :missing (reg/get-tool "delta")}))

(deftest-contract-coverage provider-registry-contract-coverage
  {:paths ["src/hive_mcp/agent/provider.clj" "src/hive_mcp/agent/registry.clj"]})

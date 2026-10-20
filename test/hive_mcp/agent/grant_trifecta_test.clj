(ns hive-mcp.agent.grant-trifecta-test
  "Facets for the grant wiring, derived from its malli contracts. The registry
   port is a stub (`constantly nil`, or a map used as a fn), never a redef."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-mcp.agent.grant :as grant]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private CallerId
  "Ids that reach both branches of slave-ids: bare, `a:b`, and coordinator lanes."
  [:or
   [:string {:min 1}]
   [:string {:min 1 :gen/fmap (fn [s] (str "worker-" s ":inst"))}]
   [:string {:min 1 :gen/fmap (fn [s] (str "coordinator:" s))}]])

(deftrifecta-from-schema slave-ids-trifecta
  hive-mcp.agent.grant/slave-ids
  {:in CallerId
   :out [:vector :string]
   :rel (fn [id out]
          (let [agent (first (str/split id #":" 2))]
            (and (= id (first out))
                 (= (if (and (not= agent id) (not (str/starts-with? agent "coordinator"))) 2 1)
                    (count out)))))
   :num-tests 200})

(deftrifecta-from-schema requested-grant-trifecta
  hive-mcp.agent.grant/requested-grant
  {:in [:or
        :nil
        [:map-of :keyword :string]
        [:string {:gen/fmap (fn [s] (str "{\"k\": \"" (str/replace s #"[\"\\]" "") "\"}"))}]
        [:string {:max 3 :gen/fmap (fn [s] (str " " s))}]
        [:int]]
   :out grant/RequestedGrant
   :rel (fn [x out]
          (cond
            (nil? x) (= {:ok nil} out)
            (map? x) (= {:ok x} out)
            (integer? x) (contains? out :refused)
            :else (or (contains? out :ok) (contains? out :refused))))
   :num-tests 200})

(def ^:private chain
  {"leaf" {:slave/id "leaf" :slave/parent "mid"}
   "mid"  {:slave/id "mid" :slave/parent {:slave/id "root"} :slave/depth 1}
   "root" {:slave/id "root" :slave/grant {:tools ["memory"]}}})

(deftest lineage-walks-to-root
  (is (= ["leaf" "mid" "root"] (mapv :slave/id (grant/lineage chain "leaf"))))
  (is (= {:tools ["memory"]} (grant/recorded-grant chain "leaf")))
  (is (nil? (grant/recorded-grant chain "")))
  (is (nil? (grant/recorded-grant chain nil)))
  (is (= 1 (grant/depth-of chain "mid")))
  (is (= 0 (grant/depth-of chain "nobody"))))

(deftest lineage-is-cycle-safe
  (let [cyc {"a" {:slave/id "a" :slave/parent "b"}
             "b" {:slave/id "b" :slave/parent "a"}}]
    (is (= ["a" "b"] (mapv :slave/id (grant/lineage cyc "a"))))))

(deftrifecta-from-schema lineage-empty-registry-trifecta
  hive-mcp.agent.grant/lineage
  {:in [:cat [:fn {:gen/elements [(constantly nil)]} ifn?] CallerId]
   :out [:vector :map]
   :rel (fn [_ out] (= [] out))
   :num-tests 100})

(deftest child-grant-decisions
  (testing "nothing asked, nothing recorded"
    (is (= {:grant nil} (grant/child-grant nil nil nil 1))))
  (testing "a grant is needed but the domain is absent"
    (is (str/includes? (:refused (grant/child-grant nil {"tools" []} nil 1)) "not loaded"))
    (is (str/includes? (:refused (grant/child-grant nil nil {"tools" []} 1)) "not loaded"))))

(deftest call-refusal-arms
  (is (nil? (grant/call-refusal nil (constantly nil) "x" "memory" "add")))
  (testing "remedy path is never gated"
    (is (nil? (grant/call-refusal nil chain "leaf" "hivemind" "ask")))
    (is (nil? (grant/call-refusal nil chain "leaf" "swarm" "hivemind ask"))))
  (testing "a grant with no domain fails closed"
    (is (str/includes? (grant/call-refusal nil chain "leaf" "memory" "add") "fails closed"))))

(golden/deftest-golden-fn grant-golden
  "test/golden/agent/grant-wiring.edn"
  (fn []
    {:slave-ids [(grant/slave-ids "solo") (grant/slave-ids "w:1") (grant/slave-ids "coordinator:9")]
     :requested [(grant/requested-grant nil) (grant/requested-grant "  ")
                 (grant/requested-grant {:a 1}) (grant/requested-grant "{\"a\":1}")
                 (select-keys (grant/requested-grant "[1]") [:refused])
                 (some? (:refused (grant/requested-grant 5)))]
     :lineage (mapv :slave/id (grant/lineage chain "leaf"))
     :depth [(grant/depth-of chain "mid") (grant/depth-of chain "zzz")]
     :always-permitted (vec (sort grant/always-permitted))
     :child-no-domain (some? (:refused (grant/child-grant nil {"a" 1} nil 1)))}))

(deftest-contract-coverage grant-contract-coverage
  {:paths ["src/hive_mcp/agent/grant.clj"]})

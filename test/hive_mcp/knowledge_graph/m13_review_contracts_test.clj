(ns hive-mcp.knowledge-graph.m13-review-contracts-test
  "Inputs the m13 contracts used to reject, called under instrumentation.

   Each case is a call the docstring or a production caller makes: a node with
   no edges, the backfill rescue branch, the optional opts of the migration
   and history functions."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.knowledge-graph.edges :as edges]
            [hive-mcp.knowledge-graph.grounding :as ground]
            [hive-mcp.knowledge-graph.migration :as mig]
            [hive-mcp.knowledge-graph.queries :as queries]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-mcp.knowledge-graph.versioning :as v]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.tools.kg :as kg-tools]
            [malli.core :as m]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private contracted
  #{#'queries/get-node-context #'queries/impact-analysis #'queries/find-connected-nodes
    #'queries/find-contradictions
    #'ground/backfill-grounding!
    #'mig/migrate-store! #'mig/sync-to-backend!
    #'mig/migrate-to-datahike! #'mig/migrate-to-datalevin!
    #'v/history #'v/create-versioned-store})

(use-fixtures :once
  (fn [f]
    (mi/instrument! {:filters [(mi/-filter-var contracted)]})
    (try (f) (finally (mi/unstrument! {:filters [(mi/-filter-var contracted)]})))))

(use-fixtures :each fixtures/datascript-fixture)

(defn- contract-of
  "The function schema registered for ns/name."
  [ns-sym fn-sym]
  (:schema (get-in (m/function-schemas) [ns-sym fn-sym])))

(defn- instrumented-stub
  "A stand-in instrumented with the REAL registered contract of ns/name, for an
   arity whose real body would write a store to disk."
  [ns-sym fn-sym ret]
  (m/-instrument {:schema (contract-of ns-sym fn-sym)} (fn [& _] ret)))

(deftest node-with-no-edges-has-nil-confidence
  (let [ctx (queries/get-node-context "review-absent-node")]
    (is (= "review-absent-node" (:node-id ctx)))
    (is (nil? (:confidence ctx)) "no scored edges: the body returns nil confidence")
    (is (zero? (get-in ctx [:incoming :count])))
    (is (= [] (:scopes ctx)))))

(deftest query-opts-are-optional
  (edges/add-edge! {:from "rv-a" :to "rv-b" :relation :implements :confidence 0.9})
  (testing "impact-analysis and find-connected-nodes default their opts"
    (let [impact (queries/impact-analysis "rv-b")]
      (is (= impact (queries/impact-analysis "rv-b" nil) (queries/impact-analysis "rv-b" {})))
      (is (nat-int? (:total-count impact))))
    (let [connected (queries/find-connected-nodes "rv-a")]
      (is (contains? connected "rv-b"))
      (is (= connected (queries/find-connected-nodes "rv-a" nil)))))
  (testing "find-contradictions without a scope"
    (is (= [] (vec (queries/find-contradictions))))))

(defn- failing-memory-store []
  (reify mem-proto/IMemoryStore
    (query-entries [_ _opts] (throw (ex-info "review store down" {})))))

(deftest backfill-rescue-branch-honours-contract
  (let [prior (:default (mem-proto/registered-stores))]
    (mem-proto/register-store! :default (failing-memory-store))
    (try
      (testing "1-arity: store failure answers the fallback map"
        (let [res (ground/backfill-grounding! {:limit 0})]
          (is (string? (:error res)))
          (is (= 0 (:total-scanned res) (:processed res)))))
      (testing "0-arity and nil opts reach the same fallback"
        (is (contains? (ground/backfill-grounding!) :error))
        (is (contains? (ground/backfill-grounding! nil) :error)))
      (finally
        (if prior
          (mem-proto/register-store! :default prior)
          (mem-proto/unregister-store! :default))))))

(deftest migrate-store-opts-are-optional
  (edges/add-edge! {:from "mg-a" :to "mg-b" :relation :refines :confidence 0.5})
  (testing "2-arity migrates in memory (install-store! writes the bound test store)"
    (let [res (mig/migrate-store! :datascript :datascript)]
      (is (false? (:dry-run res)))
      (is (= 1 (get-in res [:exported :edges])))
      (is (true? (get-in res [:validation :valid?])))))
  (testing "nil opts is the absent-opts case"
    (is (true? (:dry-run (mig/migrate-store! :datascript :datascript {:dry-run true}))))
    (is (map? (mig/migrate-store! :datascript :datascript nil))))
  (testing "sync-to-backend! without opts"
    (is (true? (get-in (mig/sync-to-backend! :datascript) [:validation :valid?])))))

(deftest convenience-migrations-take-optional-opts
  (testing "1-arity dry run through the real body"
    (is (true? (:dry-run (mig/migrate-to-datahike! {:dry-run true}))))
    (is (true? (:dry-run (mig/migrate-to-datalevin! {:dry-run true})))))
  (testing "0-arity accepted by the registered contract (real body would write to disk)"
    (doseq [f '[migrate-to-datahike! migrate-to-datalevin!]]
      (is (= {:ok f} ((instrumented-stub 'hive-mcp.knowledge-graph.migration f {:ok f}))))))
  (testing "create-versioned-store's 0-arity likewise"
    (is (nil? ((instrumented-stub 'hive-mcp.knowledge-graph.versioning
                                  'create-versioned-store nil)))))
  (testing "nil opts accepted"
    (is (= {:ok :nil} ((instrumented-stub 'hive-mcp.knowledge-graph.migration
                                          'migrate-to-datahike! {:ok :nil}) nil)))))

(deftest history-opts-are-optional
  (testing "no versioned system: nil for every opts shape"
    (is (nil? (v/history)))
    (is (nil? (v/history nil)))
    (is (nil? (v/history {:limit 3}))))
  (testing "the kg_history handler passes nil opts when limit is absent"
    (let [resp (kg-tools/handle-kg-history {})]
      (if (v/versioning-available?)
        (is (true? (:success (json/read-str (:text resp) :key-fn keyword)))
            (str "handler answered " (:text resp)))
        (is (re-find #"(?i)versioning" (:text resp)))))))

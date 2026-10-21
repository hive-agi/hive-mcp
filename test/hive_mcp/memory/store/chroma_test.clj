(ns hive-mcp.memory.store.chroma-test
  "Tests for ChromaMemoryStore protocol implementation.

   Tests verify:
   1. Protocol satisfaction (all 3 protocols implemented)
   2. Factory function creates valid store
   3. Store management (set-store!/get-store/store-set?)
   4. Method delegation to chroma.clj functions
   5. Store status and health reporting"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-mcp.memory.store.chroma :as chroma-store]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-test.golden :refer [deftest-golden]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-mcp.chroma.client :as client]
            [hive-mcp.chroma.connection :as chroma-conn]
            [hive-mcp.config.test-support :as cfg]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.test-fixtures :as fixtures]
            [hive-mcp.test.stub.chroma-transport :as transport]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; Fixtures
;;; ============================================================================

(defn clean-store-fixture
  "Reset active store before each test to avoid cross-test contamination."
  [f]
  (let [prev-store (when (mem-proto/store-set?)
                     (mem-proto/get-store))]
    (try
      (f)
      (finally
        ;; Restore previous store if one was set, otherwise clear
        (if prev-store
          (mem-proto/set-store! prev-store)
          (mem-proto/reset-registry!))))))

(use-fixtures :each clean-store-fixture)

;;; ============================================================================
;;; Protocol Satisfaction Tests
;;; ============================================================================

(deftest test-protocol-satisfaction
  (testing "ChromaMemoryStore satisfies all 3 protocols"
    (let [store (chroma-store/create-store)]
      (is (satisfies? mem-proto/IMemoryStore store)
          "Must satisfy IMemoryStore (core CRUD + search)")
      (is (satisfies? mem-proto/IMemoryStoreWithAnalytics store)
          "Must satisfy IMemoryStoreWithAnalytics (access/feedback tracking)")
      (is (satisfies? mem-proto/IMemoryStoreWithStaleness store)
          "Must satisfy IMemoryStoreWithStaleness (Bayesian staleness)"))))

(deftest test-protocol-predicates
  (testing "Protocol predicate functions work correctly"
    (let [store (chroma-store/create-store)]
      (is (true? (mem-proto/analytics-store? store))
          "analytics-store? should return true for ChromaMemoryStore")
      (is (true? (mem-proto/staleness-store? store))
          "staleness-store? should return true for ChromaMemoryStore"))))

;;; ============================================================================
;;; Factory Function Tests
;;; ============================================================================

(deftest test-create-store-defaults
  (testing "create-store with no args uses defaults"
    (let [store (chroma-store/create-store)]
      (is (instance? hive_mcp.memory.store.chroma.ChromaMemoryStore store))
      (is (= "localhost" (:host @(:config-atom store))))
      (is (= 8000 (:port @(:config-atom store))))
      (is (= "hive-mcp-memory" (:collection-name @(:config-atom store)))))))

(deftest test-create-store-with-opts
  (testing "create-store with custom options"
    (let [store (chroma-store/create-store {:host "chroma.example.com"
                                            :port 9000
                                            :collection-name "custom-collection"})]
      (is (instance? hive_mcp.memory.store.chroma.ChromaMemoryStore store))
      (is (= "chroma.example.com" (:host @(:config-atom store))))
      (is (= 9000 (:port @(:config-atom store))))
      (is (= "custom-collection" (:collection-name @(:config-atom store)))))))

;;; ============================================================================
;;; Store Management Tests
;;; ============================================================================

(deftest test-set-store-and-get-store
  (testing "set-store! accepts ChromaMemoryStore and get-store retrieves it"
    (let [store (chroma-store/create-store)]
      (mem-proto/set-store! store)
      (is (mem-proto/store-set?) "store-set? should be true after set-store!")
      (is (= store (mem-proto/get-store))
          "get-store should return the same store instance"))))

(deftest test-set-store-rejects-non-protocol
  (testing "set-store! rejects objects that don't implement IMemoryStore"
    (is (thrown? AssertionError
                 (mem-proto/set-store! {:not "a store"})))))

;;; ============================================================================
;;; Store Status Tests
;;; ============================================================================

(deftest test-store-status
  (testing "store-status returns correct shape"
    (let [store (chroma-store/create-store)
          status (mem-proto/store-status store)]
      (is (= "chroma" (:backend status)))
      (is (contains? status :configured?))
      (is (contains? status :entry-count))
      (is (contains? status :supports-search?)))))

(deftest test-connected-delegates-to-embedding-configured
  (testing "connected? reflects embedding provider state"
    (let [store (chroma-store/create-store)]
      ;; connected? delegates to chroma/embedding-configured?
      ;; The result depends on whether an embedding provider is set
      (is (boolean? (mem-proto/connected? store))
          "connected? should return a boolean"))))

;;; ============================================================================
;;; Health Check Tests
;;; ============================================================================

(deftest test-health-check-shape
  (testing "health-check returns correctly shaped map"
    (let [store (chroma-store/create-store)
          health (mem-proto/health-check store)]
      (is (contains? health :healthy?))
      (is (contains? health :latency-ms))
      (is (= "chroma" (:backend health)))
      (is (contains? health :errors))
      (is (vector? (:errors health)))
      (is (contains? health :checked-at))
      (is (string? (:checked-at health))))))

;;; ============================================================================
;;; Supports Semantic Search Tests
;;; ============================================================================

(deftest test-supports-semantic-search
  (testing "supports-semantic-search? delegates to embedding-configured?"
    (let [store (chroma-store/create-store)]
      (is (boolean? (mem-proto/supports-semantic-search? store))
          "Should return a boolean"))))

;;; ============================================================================
;;; Reset Store Tests
;;; ============================================================================

(deftest test-reset-store
  (testing "reset-store! clears collection cache and returns true"
    (let [store (chroma-store/create-store)]
      (is (true? (mem-proto/reset-store! store))))))

;;; ============================================================================
;;; Disconnect Tests
;;; ============================================================================

(deftest test-disconnect
  (testing "disconnect! clears collection cache"
    (let [store (chroma-store/create-store)
          result (mem-proto/disconnect! store)]
      (is (true? (:success? result)))
      (is (empty? (:errors result))))))

;;; ============================================================================
;;; Facets: pure projection helpers + CRUD over injected ports
;;; ============================================================================

(def ^:private FieldName
  [:or [:string {:gen/elements ["project_id" "content" "tags" "type" "a_b_c"]}]
   [:keyword {:gen/elements [:project-id :content :id :type]}]
   [:symbol {:gen/elements ['tags 'project_id]}]])

(deftrifecta-from-schema field-key-kebabs-milvus-columns
  #'chroma-store/field-key
  {:in [:cat FieldName]
   :out :keyword
   :rel (fn [[f] out] (= (name out) (.replace ^String (name f) "_" "-")))
   :golden-path "test/golden/hive-mcp/memory/store-chroma-field-key.edn"
   :num-tests 100})

(def ^:private Entry
  [:map [:id [:string {:gen/elements ["e1" "e2"]}]]
   [:content {:optional true} :string]
   [:project-id {:optional true} :string]
   [:tags {:optional true} [:vector :string]]])

(deftrifecta-from-schema project-fields-keeps-id-and-asked-keys
  #'chroma-store/project-fields
  {:in [:cat [:maybe [:sequential Entry]] [:maybe [:sequential FieldName]]]
   :out [:maybe [:sequential :map]]
   :rel (fn [[es fs] out]
          (if (seq fs)
            (let [ks (conj (set (map chroma-store/field-key fs)) :id)]
              (= out (mapv #(select-keys % ks) es)))
            (= out es)))
   :classify (fn [[_ fs] _] (if (seq fs) :projected :passthrough))
   :classify-domain #{:projected :passthrough}
   :num-tests 100})

(defn- with-chroma-ports
  "Run F with the in-memory Chroma transport and a mock embedder injected."
  [f]
  (let [prior-t (client/transport)
        prior-p (active/get-embedding-provider)]
    (client/set-transport! (transport/->transport))
    (active/set-embedding-provider! (fixtures/->MockEmbedder 8))
    (chroma-conn/reset-collection-cache!)
    (try (cfg/with-config {:embedder {:no-embed-types #{} :routes {}}} (f))
         (finally
           (chroma-conn/reset-collection-cache!)
           (client/set-transport! prior-t)
           (if prior-p (active/set-embedding-provider! prior-p) (active/reset-embedding-provider!))))))

(deftest-golden crud-over-injected-ports-golden
  "test/golden/hive-mcp/memory/store-chroma-crud.edn"
  (with-chroma-ports
    (fn []
      (let [s      (chroma-store/create-store)
            added  [(mem-proto/add-entry! s {:id "e1" :content "alpha" :type "note" :tags ["x"] :project-id "p"})
                    (mem-proto/add-entry! s {:id "e2" :content "beta" :type "decision" :tags ["y"] :project-id "p"})]
            got    (select-keys (mem-proto/get-entry s "e1") [:id :content :type :tags :project-id])
            upd    (:tags (mem-proto/update-entry! s "e1" {:tags ["x" "z"]}))
            notes  (vec (sort (map :id (mem-proto/query-entries s {:type "note"}))))
            all    (vec (sort (map :id (mem-proto/query-entries s {}))))
            del    (mem-proto/delete-entry! s "e1")]
        {:added added :got got :updated-tags upd :notes notes :all all
         :deleted del :after (mem-proto/get-entry s "e1")
         :connected? (mem-proto/connected? s)
         :semantic? (mem-proto/supports-semantic-search? s)}))))

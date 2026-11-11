(ns hive-mcp.memory.store.chroma-contract-runner-test
  "Runs the backend-agnostic IMemoryStore contract (hive-test) against
   ChromaMemoryStore.

   DEFAULT TIER: ChromaMemoryStore over the in-memory IChromaTransport
   (hive-mcp.test.stub.chroma-transport), so the adapter is held to the
   contract with no Chroma server. The e2e tier rebinds *backend* to a
   throwaway Chroma container (test-e2e/hive_mcp/e2e/chroma_test.clj) and runs
   these same deftests, which is what proves the stub faithful
   (axiom 20260729111910-400b5763).

   KNOWN DIVERGENCES: three contract cases fail IDENTICALLY on the stub and on
   a real Chroma 1.0.0 (measured 2026-11-11), so they are adapter-vs-contract
   gaps, not stub drift: :type comes back as a string, not the keyword it went
   in as (add-get-roundtrip, add-update-get), and cleanup-expired! does not
   remove an ephemeral entry whose :expires is overwritten on add
   (expiration-cleanup). They are listed in `adapter-gaps` and asserted to
   STILL fail, so the day the adapter is fixed this test goes red and the gap
   is removed instead of silently lingering."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-test.memory.store-contract :as contract]
            [hive-mcp.chroma.client :as client]
            [hive-mcp.chroma.connection :as chroma-conn]
            [hive-mcp.config.test-support :as cfg]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.memory.store.chroma :as chroma-store]
            [hive-mcp.protocols.memory :as proto]
            [hive-mcp.test-fixtures :as fixtures]
            [hive-mcp.test.stub.chroma-transport :as transport]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn stub-backend
  "The default backend: a fresh in-memory transport per test."
  []
  {:transport (transport/->transport)
   :config    {:host "stub.invalid" :port 1 :collection-name "hive-mcp-memory"}})

(def ^:dynamic *backend*
  "A no-arg fn answering {:transport IChromaTransport :config chroma-config}
   for one test. The e2e tier rebinds it to a real Chroma."
  stub-backend)

(defn backend-fixture
  "Install the backend's transport, a deterministic 384-d mock embedder and a
   pinned embedder config; bind *store-factory* to ChromaMemoryStore. Restores
   transport, provider and config afterwards."
  [f]
  (let [{:keys [transport config]} (*backend*)
        original-provider  (active/get-embedding-provider)
        original-transport (client/transport)
        original-config    (chroma-conn/get-config)]
    (client/set-transport! transport)
    (active/set-embedding-provider! (fixtures/->MockEmbedder 384))
    (chroma-conn/configure! config)
    (chroma-conn/reset-collection-cache!)
    (try
      (binding [contract/*store-factory* #(chroma-store/create-store (select-keys config [:host :port]))]
        (cfg/with-config {:embedder {:no-embed-types #{} :routes {}}} (f)))
      (finally
        (chroma-conn/reset-collection-cache!)
        (chroma-conn/configure! (select-keys original-config [:host :port :collection-name]))
        (client/set-transport! original-transport)
        (active/set-embedding-provider! original-provider)))))

(use-fixtures :each backend-fixture)

(def adapter-gaps
  "Contract cases ChromaMemoryStore fails on stub AND real Chroma alike."
  #{'test-add-get-roundtrip 'test-add-update-get 'test-expiration-cleanup})

(defn- run-gap
  "Run contract case SYM with reports captured; answer how many failed."
  [sym]
  (let [counters (ref clojure.test/*initial-report-counters*)]
    (binding [clojure.test/*report-counters* counters
              clojure.test/report (fn [m] (when (#{:fail :error} (:type m))
                                            (dosync (alter counters update (:type m) (fnil inc 0)))))]
      ((ns-resolve 'hive-test.memory.store-contract sym)))
    (+ (:fail @counters 0) (:error @counters 0))))

(defmacro ^:private gap-test
  "A deftest asserting contract case SYM still fails on this adapter."
  [test-name sym]
  `(deftest ~test-name
     (is (pos? (run-gap '~sym))
         ~(str sym " now passes on ChromaMemoryStore: remove it from adapter-gaps "
               "and make it an ordinary contract deftest"))))

;; Protocol Satisfaction (runner-specific — verify Chroma implements all 3)
;; =============================================================================

(deftest chroma-satisfies-all-protocols
  (let [store (chroma-store/create-store)]
    (testing "ChromaMemoryStore satisfies IMemoryStore"
      (is (satisfies? proto/IMemoryStore store)))
    (testing "ChromaMemoryStore satisfies IMemoryStoreWithAnalytics"
      (is (satisfies? proto/IMemoryStoreWithAnalytics store)))
    (testing "ChromaMemoryStore satisfies IMemoryStoreWithStaleness"
      (is (satisfies? proto/IMemoryStoreWithStaleness store)))))

;; =============================================================================
;; Contract Test Invocations — IMemoryStore: Connection Lifecycle
;; =============================================================================

(deftest chroma-lifecycle-connected
  (contract/test-lifecycle-connected))

(deftest chroma-lifecycle-health-check-shape
  (contract/test-lifecycle-health-check-shape))

(deftest chroma-lifecycle-store-status-shape
  (contract/test-lifecycle-store-status-shape))

(deftest chroma-lifecycle-connect-then-connected
  (contract/test-lifecycle-connect-then-connected))

(deftest chroma-disconnect-shape
  (contract/test-disconnect-shape))

;; =============================================================================
;; Contract Test Invocations — IMemoryStore: CRUD
;; =============================================================================

(gap-test chroma-add-get-roundtrip-gap test-add-get-roundtrip)

(deftest chroma-add-delete-get
  (contract/test-add-delete-get))

(gap-test chroma-add-update-get-gap test-add-update-get)

(deftest chroma-add-delete-count-invariant
  (contract/test-add-delete-count-invariant))

;; =============================================================================
;; Contract Test Invocations — IMemoryStore: Query & Search
;; =============================================================================

(deftest chroma-query-entries-by-type
  (contract/test-query-entries-by-type))

(deftest chroma-search-similar-behavioral
  (contract/test-search-similar-behavioral))

;; =============================================================================
;; Contract Test Invocations — IMemoryStore: Duplicate Detection
;; =============================================================================

(deftest chroma-find-duplicate-same-content
  (contract/test-find-duplicate-same-content))

(deftest chroma-find-duplicate-different-content
  (contract/test-find-duplicate-different-content))

;; =============================================================================
;; Contract Test Invocations — IMemoryStore: Expiration
;; =============================================================================

(gap-test chroma-expiration-cleanup-gap test-expiration-cleanup)

(deftest chroma-cleanup-expired-shape
  (contract/test-cleanup-expired-shape))

(deftest chroma-cleanup-expired-idempotent
  (contract/test-cleanup-expired-idempotent))

(deftest chroma-entries-expiring-soon
  (contract/test-entries-expiring-soon))

;; =============================================================================
;; Contract Test Invocations — IMemoryStore: Reset
;; =============================================================================

(deftest chroma-reset-store-idempotent
  (contract/test-reset-store-idempotent))

;; =============================================================================
;; Contract Test Invocations — IMemoryStoreWithAnalytics
;; =============================================================================

(deftest chroma-analytics-log-access
  (contract/test-analytics-log-access))

(deftest chroma-analytics-record-feedback
  (contract/test-analytics-record-feedback))

(deftest chroma-analytics-helpfulness-ratio
  (contract/test-analytics-helpfulness-ratio))

;; =============================================================================
;; Contract Test Invocations — IMemoryStoreWithStaleness
;; =============================================================================

(deftest chroma-staleness-update
  (contract/test-staleness-update))

(deftest chroma-staleness-get-stale-entries
  (contract/test-staleness-get-stale-entries))

(deftest chroma-staleness-propagate
  (contract/test-staleness-propagate))

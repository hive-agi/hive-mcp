(ns hive-mcp.tools.memory.crud.readback-test
  "Canonical receipts and legacy readback through registered stores."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [hive-mcp.protocols.memory :as mem]
            [hive-mcp.protocols.store-metrics :as metrics]
            [hive-mcp.test-support.registry-isolation :as isolation]
            [hive-mcp.tools.memory.crud.write :as write]))

(defn- with-default-store
  "Run F with STORE in the :default slot. Refuses a running addon server
   before the slot is touched and restores the exact prior slot map in one
   atomic swap, instead of unregistering and re-registering :default."
  [store f]
  (isolation/with-restored-store-registry
    (fn []
      (mem/register-store! :default store)
      (f))))

(defn- recording-store [entry reads]
  (reify mem/IMemoryStore
    (connect! [_ _] nil) (disconnect! [_] nil) (connected? [_] true)
    (health-check [_] {})
    (add-entry! [_ e] (:id e))
    (get-entry [_ id]
      (swap! reads inc)
      (when (= id (:id entry)) entry))
    (update-entry! [_ _ _] (throw (ex-info "unexpected edge update" {})))
    (delete-entry! [_ _] nil) (query-entries [_ _] [])
    (search-similar [_ _ _] []) (supports-semantic-search? [_] false)
    (cleanup-expired! [_] nil) (entries-expiring-soon [_ _ _] [])
    (find-duplicate [_ _ _ _] nil) (store-status [_] {})
    (reset-store! [_] nil)))

(deftest no-edge-add-reads-canonical-entry-once
  (let [id (str "readback-" (random-uuid))
        entry {:id id :type "note" :content "persisted" :tags ["scope:project:test"]
               :created "2026-01-01T00:00:00Z" :access-count 0
               :kg-incoming ["old-edge"] :document "internal"}
        reads (atom 0)]
    (with-default-store (recording-store entry reads)
      (fn []
        (let [resp (#'write/finalize-entry! id {} "test" nil
                    {:tags-with-scope ["scope:project:test"] :type "note"})
              body (json/read-str (:text resp) :key-fn keyword)]
          (is (not (:isError resp)))
          (is (= 1 @reads) "read the store's canonical entry, not an input reconstruction")
          (is (= {:id id :status "ok" :type "note" :tags ["scope:project:test"]
                  :chars 9 :preview "persisted"}
                 body))
          (is (not (contains? body :kg_edges_created))))))))

(deftest readback-retries-until-store-shape-is-visible
  (let [id (str "readback-" (random-uuid))
        entry {:id id :type "note" :content "canonical" :tags ["persisted"]
               :created "backend-created"}
        reads (atom 0)
        store (reify mem/IMemoryStore
                (connect! [_ _] nil) (disconnect! [_] nil) (connected? [_] true)
                (health-check [_] {})
                (add-entry! [_ e] (:id e))
                (get-entry [_ requested-id]
                  (when (= requested-id id)
                    (when (> (swap! reads inc) 1) entry)))
                (update-entry! [_ _ _] nil)
                (delete-entry! [_ _] nil) (query-entries [_ _] [])
                (search-similar [_ _ _] []) (supports-semantic-search? [_] false)
                (cleanup-expired! [_] nil) (entries-expiring-soon [_ _ _] [])
                (find-duplicate [_ _ _ _] nil) (store-status [_] {})
                (reset-store! [_] nil))]
    (is (= entry (#'write/fetch-with-retry store id)))
    (is (= 2 @reads))))

(deftest receipt-skips-read-and-preserves-response
  (let [entry {:id "receipt" :type "note" :content {:canonical true}
               :tags ["persisted"] :created "created" :updated "updated"
               :duration "long" :project-id "test" :content-hash "persisted-hash"
               :kg-outgoing ["edge"] :kg-incoming ["incoming"]}
        reads (atom 0)]
    (with-default-store (recording-store entry reads)
      (fn []
        (let [resp (#'write/finalize-entry! "receipt" {} "test" nil
                    {:type "note" :tags-with-scope [] :receipt entry})
              body (json/read-str (:text resp) :key-fn keyword)]
          (is (not (:isError resp)))
          (is (zero? @reads))
          (is (= {:id "receipt" :status "ok" :type "note" :tags ["persisted"]
                  :chars 18 :preview "{\"canonical\":true}" :duration "long"
                  :project-id "test" :content-hash "persisted-hash"}
                 body))
          (is (= body
                 (json/read-str
                  (:text (#'write/finalize-entry! "receipt" {} "test" nil
                           {:type "note" :tags-with-scope []}))
                  :key-fn keyword)))
          (is (= 1 @reads)))))))

(deftest mismatched-receipt-keeps-readback
  (let [entry {:id "canonical" :type "note" :content "stored" :tags []}
        reads (atom 0)]
    (with-default-store (recording-store entry reads)
      (fn []
        (#'write/finalize-entry! "canonical" {} "test" nil
         {:type "note" :tags-with-scope [] :receipt {:id "wrong"}})
        (is (= 1 @reads))))))

(deftest unsupported-store-keeps-id-contract
  (let [entry {:id "legacy"}]
    (is (= "legacy" (#'write/add-with-optional-receipt!
                    (recording-store entry (atom 0)) entry)))))

(deftest metered-store-forwards-receipts-and-failures
  (let [protocol (try
                   (some-> (requiring-resolve 'hive-spi.memory.receipt/IMemoryStoreAddReceipt) deref)
                   (catch java.io.FileNotFoundException _ nil))
        calls (atom []) observations (atom [])
        answer (atom {:id "receipt" :entry {:id "receipt" :content "canonical"}})
        store (reify mem/IMemoryStore
                (add-entry! [_ entry]
                  (when protocol (throw (ex-info "legacy write bypassed receipt" {})))
                  (swap! calls conj entry)
                  (:id @answer)))]
    (when protocol
      (extend (class store) protocol
              {:-add-entry-returning! (fn [_ entry] (swap! calls conj entry) @answer)}))
    (try
      (metrics/set-recorder! (fn [k _ outcome] (swap! observations conj [k outcome])))
      (let [wrapped (metrics/wrap-store :memory store)]
        (is (metrics/metered? wrapped))
        (if protocol
          (do
            (is (satisfies? protocol wrapped))
            (is (= @answer (#'write/add-with-optional-receipt! wrapped {:content "input"})))
            (doseq [failure [{:success? false :error :offline :id "failure"}
                             {:queued true :id "queued"}]]
              (reset! answer failure)
              (is (= failure (#'write/add-with-optional-receipt! wrapped {:content "input"}))))
            (is (= 3 (count @calls)))
            (is (= [[:-add-entry-returning! :ok]
                    [:-add-entry-returning! :ok]
                    [:-add-entry-returning! :ok]]
                   (mapv (fn [[k outcome]] [(last k) outcome]) @observations))))
          (do
            (is (= "receipt" (#'write/add-with-optional-receipt! wrapped {:content "input"})))
            (is (= [{:content "input"}] @calls))
            (is (= [[:add-entry! :ok]]
                   (mapv (fn [[k outcome]] [(last k) outcome]) @observations))))))
      (finally (metrics/rebind!)))))
(ns hive-mcp.tools.memory.crud.deferred-test
  "A failed embed must not lose the entry awaiting reembed."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.memory.crud.deferred :as deferred]
            [hive-test.trifecta :refer [deftrifecta]]
            [clojure.test.check.generators]
            [hive-mcp.protocols.memory]
            [hive-mcp.tools.memory.crud.write]
            [hive-mcp.tools.memory.crud.reembed]))

(defn tag-transition
  "Adapt the two-argument policy to trifecta's unary input port."
  [[tags deferred?]]
  (deferred/reembed-tags tags deferred?))

(deftrifecta pending-tag-policy
  hive-mcp.tools.memory.crud.deferred-test/tag-transition
  {:golden-path "test/golden/hive-mcp/memory/reembed-tags.edn"
   :cases {:defer [["note"] true]
           :dedupe [["note" "pending-reembed"] true]
           :drain [["note" "pending-reembed"] false]}
   :gen (clojure.test.check.generators/tuple
          (clojure.test.check.generators/vector
            (clojure.test.check.generators/elements ["note" "pending-reembed"]) 0 5)
          clojure.test.check.generators/boolean)
   :pred (fn [input]
           (let [[_ deferred?] input
                 result (tag-transition input)]
             (= (boolean deferred?) (boolean (some #{"pending-reembed"} result)))))
   :num-tests 80
   :mutations [["drops-deferred" (fn [[tags _]] tags)]
               ["always-pending" (fn [[tags _]] (conj (vec tags) "pending-reembed"))]]})

(deftest durable-queue-round-trip
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "reembed-test" (make-array java.nio.file.attribute.FileAttribute 0)))
        entry {:id "test-1" :type "note" :content "important" :tags ["note"]}]
    (binding [deferred/*queue-dir* (.getPath dir)]
      (deferred/park! entry)
      (is (= "important" (:content (deferred/lookup "test-1"))))
      (is (= ["pending-reembed"] (filter #{"pending-reembed"} (:tags (deferred/lookup "test-1")))))
      (deferred/remove! "test-1")
      (is (nil? (deferred/lookup "test-1"))))))

(deftest embed-failure-defers-but-store-failure-does-not
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "pending-write" (make-array java.nio.file.attribute.FileAttribute 0)))
        store (reify hive-mcp.protocols.memory/IMemoryStore
                (connect! [_ _] nil) (disconnect! [_] nil) (connected? [_] true)
                (health-check [_] {})
                (add-entry! [_ _] (throw (ex-info "embed-for-entry failed"
                                               {:result {:error :embedder/embed-failed}})))
                (get-entry [_ _] nil) (update-entry! [_ _ _] nil)
                (delete-entry! [_ _] nil) (query-entries [_ _] [])
                (search-similar [_ _ _] []) (supports-semantic-search? [_] false)
                (cleanup-expired! [_] nil) (entries-expiring-soon [_ _ _] [])
                (find-duplicate [_ _ _ _] nil) (store-status [_] {})
                (reset-store! [_] nil))
        snapshot (hive-mcp.protocols.memory/registered-stores)]
    (binding [deferred/*queue-dir* (.getPath dir)]
      (try
        (hive-mcp.protocols.memory/register-store! :default store)
        (let [id (#'hive-mcp.tools.memory.crud.write/index-entry!
                  {:type "note" :content "important" :tags-with-scope ["note"]
                   :project-id "test" :duration-str "long"})]
          (is (= "important" (:content (deferred/lookup id))))
          (is (some #{"pending-reembed"} (:tags (deferred/lookup id)))))
        (finally
          (hive-mcp.protocols.memory/unregister-store! :default)
          (when-let [old (:default snapshot)]
            (hive-mcp.protocols.memory/register-store! :default old)))))))

(deftest shared-gate-timeout-is-deferrable
  (is (true? (boolean (deferred/embedding-failure?
                        (ex-info "Embedding gate timed out"
                                 {:error :embedder/gate-timeout :lane :interactive})))))
  (is (false? (boolean (deferred/embedding-failure?
                         (ex-info "Vector store failed" {:error :store/unavailable}))))))

(deftest reembed-drains-only-after-successful-store-write
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "pending-drain" (make-array java.nio.file.attribute.FileAttribute 0)))
        accepted? (atom false)
        writes (atom [])
        entry {:id "drain-1" :type "note" :content "important" :tags ["note"]}
        store (reify hive-mcp.protocols.memory/IMemoryStore
                (connect! [_ _] nil) (disconnect! [_] nil) (connected? [_] true)
                (health-check [_] {})
                (add-entry! [_ e]
                  (swap! writes conj e)
                  (if @accepted? (:id e)
                      (throw (ex-info "embed-for-entry failed" {:error :embedder/embed-failed}))))
                (get-entry [_ _] nil) (update-entry! [_ _ _] nil)
                (delete-entry! [_ _] nil) (query-entries [_ _] [])
                (search-similar [_ _ _] []) (supports-semantic-search? [_] false)
                (cleanup-expired! [_] nil) (entries-expiring-soon [_ _ _] [])
                (find-duplicate [_ _ _ _] nil) (store-status [_] {})
                (reset-store! [_] nil))]
    (binding [deferred/*queue-dir* (.getPath dir)]
      (deferred/park! entry)
      (is (thrown? Exception (#'hive-mcp.tools.memory.crud.reembed/reembed-one! store "drain-1")))
      (is (some? (deferred/lookup "drain-1")))
      (reset! accepted? true)
      (is (= "drain-1" (:id (#'hive-mcp.tools.memory.crud.reembed/reembed-one! store "drain-1"))))
      (is (nil? (deferred/lookup "drain-1")))
      (is (not-any? #{"pending-reembed"} (:tags (last @writes)))))))

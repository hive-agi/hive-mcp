(ns hive-mcp.tools.memory.crud.deferred-test
  "A failed embed must not lose the entry awaiting reembed."
  (:require [clojure.test :refer [deftest is testing]]
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

(defn- temp-dir [prefix]
  (.getPath (.toFile (java.nio.file.Files/createTempDirectory
                      prefix (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- recording-store
  "IMemoryStore stub that accepts every add and records it."
  [writes]
  (reify hive-mcp.protocols.memory/IMemoryStore
    (connect! [_ _] nil) (disconnect! [_] nil) (connected? [_] true)
    (health-check [_] {})
    (add-entry! [_ e] (swap! writes conj e) (:id e))
    (get-entry [_ _] nil) (update-entry! [_ _ _] nil)
    (delete-entry! [_ _] nil) (query-entries [_ _] [])
    (search-similar [_ _ _] []) (supports-semantic-search? [_] false)
    (cleanup-expired! [_] nil) (entries-expiring-soon [_ _ _] [])
    (find-duplicate [_ _ _ _] nil) (store-status [_] {})
    (reset-store! [_] nil)))

(deftest a-corrupt-record-does-not-hide-the-rest-of-the-outbox
  (binding [deferred/*queue-dir* (temp-dir "pending-corrupt")]
    (deferred/park! {:id "good-1" :type "note" :content "a" :tags []})
    (deferred/park! {:id "bad-1" :type "note" :content "b" :tags []})
    (spit (java.io.File. deferred/*queue-dir* "6261642d31.edn") "{:id \"bad-1\" :content")
    (spit (java.io.File. deferred/*queue-dir* "pending-123.edn") "stray temp")
    (is (= ["bad-1" "good-1"] (deferred/pending-ids)))))

(deftest file-names-round-trip-to-ids
  (doseq [id ["20261003172907-4abbcf92" "../etc/passwd" "ção/λ"]]
    (binding [deferred/*queue-dir* (temp-dir "pending-names")]
      (deferred/park! {:id id :tags []})
      (is (= [id] (deferred/pending-ids)))))
  (is (nil? (deferred/id-of-file-name "pending-8812.edn")))
  (is (nil? (deferred/id-of-file-name "abc.edn"))))

(deftest a-kanban-entry-drains-into-the-kanban-slot-with-its-edges
  (let [default-writes (atom [])
        kanban-writes (atom [])
        snapshot (hive-mcp.protocols.memory/registered-stores)]
    (binding [deferred/*queue-dir* (temp-dir "pending-kanban")]
      (try
        (hive-mcp.protocols.memory/register-store! :default (recording-store default-writes))
        (hive-mcp.protocols.memory/register-store! :kanban (recording-store kanban-writes))
        (deferred/park! (deferred/with-store-key
                          {:id "k-1" :type "kanban" :content "card" :tags ["kanban"]}
                          :kanban))
        (deferred/amend! "k-1" {:kg-outgoing ["edge-1"]})
        (is (= {:total 1 :reembedded 1 :not-found 0 :errors 0}
               (hive-mcp.tools.memory.crud.reembed/drain-pending!)))
        (is (empty? @default-writes) "never drained into the caller's default slot")
        (is (= [{:id "k-1" :type "kanban" :content "card" :tags ["kanban"]
                 :kg-outgoing ["edge-1"]}]
               @kanban-writes))
        (is (empty? (deferred/pending-ids)))
        (testing "a second sweep has nothing to reembed"
          (is (zero? (:total (hive-mcp.tools.memory.crud.reembed/drain-pending!)))))
        (finally
          (hive-mcp.protocols.memory/unregister-store! :kanban)
          (hive-mcp.protocols.memory/unregister-store! :default)
          (doseq [[k s] snapshot]
            (hive-mcp.protocols.memory/register-store! k s)))))))

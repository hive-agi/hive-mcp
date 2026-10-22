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

(defn failure-stamp
  "Adapt with-failure to trifecta's unary input port."
  [[parked failure now-iso]]
  (deferred/with-failure parked failure now-iso))

(deftrifecta failure-stamp-policy
  hive-mcp.tools.memory.crud.deferred-test/failure-stamp
  {:golden-path "test/golden/hive-mcp/memory/embed-failure-stamp.edn"
   :cases {:first-failure [{:id "a"} {:error :embedder/embed-failed :message "boom"} "t1"]
           :repeat-failure [{:id "a" :deferred/attempts 2 :deferred/first-failed-at "t0"}
                            {:error :embedder/no-provider :message "cold"} "t9"]}
   :gen (clojure.test.check.generators/tuple
          (clojure.test.check.generators/fmap
            (fn [n] (cond-> {:id "a"} (pos? n) (assoc :deferred/attempts n
                                                      :deferred/first-failed-at "t0")))
            (clojure.test.check.generators/choose 0 5))
          (clojure.test.check.generators/return {:error :embedder/embed-failed :message "x"})
          (clojure.test.check.generators/return "t"))
   ;; The property takes the subject's own output: a stamped record always
   ;; counts at least one attempt and carries when and why it last failed.
   :pred (fn [result]
           (and (map? result)
                (pos? (get result :deferred/attempts 0))
                (some? (:deferred/first-failed-at result))
                (some? (:deferred/last-failed-at result))
                (= :embedder/embed-failed (get-in result [:deferred/last-error :error]))))
   :num-tests 40
   :mutations [["never-counts" (fn [[parked _ _]] parked)]
               ["forgets-cause" (fn [[parked _ now]]
                                  (-> parked
                                      (update :deferred/attempts (fnil inc 0))
                                      (assoc :deferred/first-failed-at now
                                             :deferred/last-failed-at now)))]]})

(deftest a-restart-warmup-failure-is-retryable-but-a-dim-mismatch-is-not
  (is (true? (boolean (deferred/embedding-failure?
                        (ex-info "x" {:result {:error :embedder/no-provider}})))))
  (is (false? (boolean (deferred/embedding-failure?
                         (ex-info "x" {:result {:error :embedder/dim-mismatch}}))))))

(deftest embed-retry-recovers-from-a-transient-failure-without-parking
  (let [calls (atom 0)
        sleeps (atom [])
        thunk (fn []
                (if (< (swap! calls inc) 3)
                  (throw (ex-info "embed-for-entry failed"
                                  {:result {:error :embedder/embed-failed}}))
                  "id-1"))
        outcome (deferred/call-with-embed-retry thunk deferred/default-retry
                  #(swap! sleeps conj %))]
    (is (= {:ok "id-1"} outcome))
    (is (= 3 @calls))
    (is (= [250 500] @sleeps) "backs off between tries and sleeps only between them")))

(deftest embed-retry-gives-up-loudly-with-the-cause-and-attempt-count
  (let [calls (atom 0)
        outcome (deferred/call-with-embed-retry
                  (fn [] (swap! calls inc)
                    {:error :embedder/no-provider :message "provider chain cold"})
                  {:attempts 3 :delay-ms 1} (fn [_] nil))]
    (is (= 3 @calls))
    (is (= 3 (:attempts outcome)))
    (is (= :embedder/no-provider (get-in outcome [:failure :error])))
    (is (= "provider chain cold" (get-in outcome [:failure :message])))))

(deftest a-gate-timeout-is-not-retried-in-place
  (let [calls (atom 0)
        outcome (deferred/call-with-embed-retry
                  (fn [] (swap! calls inc)
                    (throw (ex-info "gate" {:error :embedder/gate-timeout})))
                  deferred/default-retry (fn [_] nil))]
    (is (= 1 @calls))
    (is (= :embedder/gate-timeout (get-in outcome [:failure :error])))))

(deftest a-store-failure-is-never-retried-or-deferred
  (let [calls (atom 0)
        thrown (try
                 (deferred/call-with-embed-retry
                   (fn [] (swap! calls inc) (throw (ex-info "store down" {:error :store/unavailable})))
                   deferred/default-retry (fn [_] nil))
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
    (is (= "store down" (ex-message thrown)))
    (is (= 1 @calls))))

(defn- failing-store
  "IMemoryStore stub whose add-entry! throws an embed failure for the first
   `fail-times` calls and then accepts."
  [fail-times calls]
  (reify hive-mcp.protocols.memory/IMemoryStore
    (connect! [_ _] nil) (disconnect! [_] nil) (connected? [_] true)
    (health-check [_] {})
    (add-entry! [_ e]
      (if (<= (swap! calls inc) fail-times)
        (throw (ex-info "embed-for-entry failed"
                        {:result {:error :embedder/no-provider}}))
        (:id e)))
    (get-entry [_ _] nil) (update-entry! [_ _ _] nil)
    (delete-entry! [_ _] nil) (query-entries [_ _] [])
    (search-similar [_ _ _] []) (supports-semantic-search? [_] false)
    (cleanup-expired! [_] nil) (entries-expiring-soon [_ _ _] [])
    (find-duplicate [_ _ _ _] nil) (store-status [_] {})
    (reset-store! [_] nil)))

(deftest a-write-that-embeds-on-the-second-try-is-stored-not-parked
  (let [calls (atom 0)
        snapshot (hive-mcp.protocols.memory/registered-stores)]
    (binding [deferred/*queue-dir* (temp-dir "pending-warm")]
      (try
        (hive-mcp.protocols.memory/register-store! :default (failing-store 1 calls))
        (let [id (#'hive-mcp.tools.memory.crud.write/index-entry!
                  {:type "note" :content "warm" :tags-with-scope ["note"]
                   :project-id "test" :duration-str "long"})]
          (is (string? id))
          (is (= 2 @calls))
          (is (nil? (deferred/lookup id)) "a recovered write leaves nothing in the outbox"))
        (finally
          (hive-mcp.protocols.memory/unregister-store! :default)
          (doseq [[k s] snapshot]
            (hive-mcp.protocols.memory/register-store! k s)))))))

(deftest a-write-whose-embed-keeps-failing-is-parked-with-its-cause
  (let [calls (atom 0)
        snapshot (hive-mcp.protocols.memory/registered-stores)]
    (binding [deferred/*queue-dir* (temp-dir "pending-cold")]
      (try
        (hive-mcp.protocols.memory/register-store! :default (failing-store 100 calls))
        (let [id (#'hive-mcp.tools.memory.crud.write/index-entry!
                  {:type "note" :content "cold" :tags-with-scope ["note"]
                   :project-id "test" :duration-str "long"})
              parked (deferred/lookup id)]
          (is (= 3 @calls) "retried in place before parking")
          (is (some #{"pending-reembed"} (:tags parked)))
          (is (= :embedder/no-provider (get-in parked [:deferred/last-error :error])))
          (is (= 1 (:deferred/attempts parked))))
        (finally
          (hive-mcp.protocols.memory/unregister-store! :default)
          (doseq [[k s] snapshot]
            (hive-mcp.protocols.memory/register-store! k s)))))))

(deftest a-failed-drain-counts-the-attempt-and-reports-the-stuck-ids
  (let [snapshot (hive-mcp.protocols.memory/registered-stores)]
    (binding [deferred/*queue-dir* (temp-dir "pending-stuck")]
      (try
        (hive-mcp.protocols.memory/register-store! :default (failing-store 100 (atom 0)))
        (deferred/park! {:id "stuck-1" :type "note" :content "c" :tags []})
        (let [first-sweep (hive-mcp.tools.memory.crud.reembed/drain-pending!)
              second-sweep (hive-mcp.tools.memory.crud.reembed/drain-pending!)
              parked (deferred/lookup "stuck-1")]
          (is (= {:total 1 :reembedded 0 :not-found 0 :errors 1 :stuck ["stuck-1"]}
                 first-sweep))
          (is (= ["stuck-1"] (:stuck second-sweep)))
          (is (= 2 (:deferred/attempts parked)))
          (is (some? (:deferred/first-failed-at parked)))
          (is (= :embedder/no-provider (get-in parked [:deferred/last-error :error])))
          (is (not-any? #(= "deferred" (namespace %))
                        (filter keyword? (keys (deferred/->store-entry parked))))
              "outbox bookkeeping never reaches the store"))
        (finally
          (hive-mcp.protocols.memory/unregister-store! :default)
          (doseq [[k s] snapshot]
            (hive-mcp.protocols.memory/register-store! k s)))))))

(deftest recording-a-failure-never-loses-or-throws-on-a-corrupt-record
  (binding [deferred/*queue-dir* (temp-dir "pending-corrupt-stamp")]
    (deferred/park! {:id "bad-2" :type "note" :content "b" :tags []})
    (spit (java.io.File. deferred/*queue-dir* "6261642d32.edn") "{:id \"bad-2\" :content")
    (is (nil? (deferred/record-failure! "bad-2" (ex-info "x" {}) "t")))
    (is (nil? (deferred/record-failure! "absent" (ex-info "x" {}) "t")))
    (is (= ["bad-2"] (deferred/pending-ids)))))

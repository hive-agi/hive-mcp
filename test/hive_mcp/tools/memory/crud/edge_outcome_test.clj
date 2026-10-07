(ns hive-mcp.tools.memory.crud.edge-outcome-test
  "A failed KG edge write must not hide the id of an entry that was stored."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-mcp.tools.memory.crud.edge-outcome :as edge-outcome]
            [hive-mcp.tools.memory.crud.write]))

(defn- stub-ids [requests]
  (mapv #(str "edge-" (:relation %) "-" (:to %)) requests))

(defn outcome
  "Adapt attempt-edges to trifecta's unary port: [kg-params fail?] drives a
   stub edge writer that throws when fail? is true. Returns {:in input :out
   outcome} so the property can relate the two."
  [[kg-params fail? :as input]]
  (let [requests (edge-outcome/edge-requests kg-params)]
    {:in  input
     :out (edge-outcome/attempt-edges
           (fn [] (if fail?
                    (throw (ex-info "kg transact failed" {:error :kg/down}))
                    (stub-ids requests)))
           requests)}))

(def ^:private gen-targets
  (gen/vector (gen/elements ["a" "b" "c"]) 0 3))

(deftrifecta kg-edge-failure-keeps-entry
  hive-mcp.tools.memory.crud.edge-outcome-test/outcome
  {:golden-path "test/golden/hive-mcp/memory/edge-outcome.edn"
   :cases {:ok      [{:kg_depends_on ["a"] :kg_refines ["b"]} false]
           :failed  [{:kg_implements ["a"] :kg_depends_on ["b" "c"]} true]
           :no-edge [{} true]}
   :gen (gen/tuple
         (gen/hash-map :kg_implements gen-targets :kg_supersedes gen-targets
                       :kg_depends_on gen-targets :kg_refines gen-targets)
         gen/boolean)
   :pred (fn [{[kg-params fail?] :in {:keys [edge-ids edge-errors]} :out}]
           (let [requests (edge-outcome/edge-requests kg-params)]
             (if fail?
               (and (empty? edge-ids)
                    (= requests (mapv #(dissoc % :error) edge-errors))
                    (every? (comp string? :error) edge-errors))
               (and (empty? edge-errors)
                    (= (stub-ids requests) edge-ids)))))
   :num-tests 100
   :mutations [["swallows-errors"
                (fn [input] {:in input :out {:edge-ids [] :edge-errors []}})]
               ["drops-target"
                (fn [[_ fail? :as input]]
                  {:in  input
                   :out {:edge-ids []
                         :edge-errors (if fail? [{:error "kg transact failed"}] [])}})]]})

(defn- recording-store
  "IMemoryStore stub that serves `entry` by id and records updates."
  [entry updates]
  (reify mem-proto/IMemoryStore
    (connect! [_ _] nil) (disconnect! [_] nil) (connected? [_] true)
    (health-check [_] {})
    (add-entry! [_ e] (:id e))
    (get-entry [_ id] (when (= id (:id entry)) entry))
    (update-entry! [_ id u] (swap! updates conj [id u]) (merge entry u))
    (delete-entry! [_ _] nil) (query-entries [_ _] [])
    (search-similar [_ _ _] []) (supports-semantic-search? [_] false)
    (cleanup-expired! [_] nil) (entries-expiring-soon [_ _ _] [])
    (find-duplicate [_ _ _ _] nil) (store-status [_] {})
    (reset-store! [_] nil)))

(deftest add-response-names-entry-when-an-edge-fails
  (fixtures/global-datascript-fixture
   (fn []
     (let [entry    {:id "entry-1" :type "note" :content "kept" :tags ["note"]}
           updates  (atom [])
           snapshot (mem-proto/registered-stores)]
       (try
         (mem-proto/register-store! :default (recording-store entry updates))
         (let [resp (#'hive-mcp.tools.memory.crud.write/finalize-entry!
                     "entry-1" {:kg_depends_on [""]} "test" nil
                     {:tags-with-scope ["note"] :type "note"})
               body (json/read-str (:text resp) :key-fn keyword)]
           (is (not (:isError resp)))
           (is (= "entry-1" (:id body)))
           (is (= [{:relation "depends-on" :to ""}]
                  (mapv #(dissoc % :error) (:kg_edge_errors body))))
           (is (empty? @updates) "no :kg-outgoing link is written for failed edges"))
         (finally
           (mem-proto/unregister-store! :default)
           (when-let [old (:default snapshot)]
             (mem-proto/register-store! :default old))))))))

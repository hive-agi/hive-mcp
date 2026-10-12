(ns hive-mcp.tools.memory.crud.edit-dry-run-test
  "batch-edit dry-run must run the same checks a real edit runs, and write nothing."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.tools.memory.crud.edit :as edit]))

(def ^:private stored
  {"e1" {:id "e1" :type "note" :content "alpha beta beta"}})

(defn verdict
  "Adapt dry-run-op to trifecta's unary port: one op against `stored`.
   Returns {:in op :out verdict}."
  [op]
  {:in op :out (edit/dry-run-op stored op)})

(defn- expected-ok?
  [{:keys [id find] :as op}]
  (and (empty? (edit/unrecognised-params op))
       (contains? stored id)
       (or (not (contains? op :find))
           (let [^String text (:content (stored id))]
             (= 1 (count (filter #(.startsWith text ^String find (int %))
                                 (range (count text)))))))))

(deftrifecta batch-edit-dry-run-validates
  hive-mcp.tools.memory.crud.edit-dry-run-test/verdict
  {:golden-path "test/golden/hive-mcp/memory/edit-dry-run.edn"
   :cases {:ok-content  {:id "e1" :content "new"}
           :ok-find     {:id "e1" :find "alpha" :replace "gamma"}
           :ambiguous   {:id "e1" :find "beta" :replace "x"}
           :missing     {:id "nope" :content "x"}
           :bad-param   {:id "e1" :colour "red"}
           :bad-type    {:id "e1" :type "not a type!"}}
   :gen (gen/one-of
         [(gen/fmap (fn [id] {:id id :content "c"}) (gen/elements ["e1" "e2"]))
          (gen/fmap (fn [f] {:id "e1" :find f :replace "z"})
                    (gen/elements ["alpha" "beta" "gamma"]))])
   :pred (fn [{op :in {:keys [id ok error]} :out}]
           (and (= id (:id op))
                (= (boolean ok) (expected-ok? op))
                (= (not ok) (string? error))))
   :num-tests 100
   :mutations [["echo-ok" (fn [op] {:in op :out {:id (:id op) :ok true}})]
               ["existence-only"
                (fn [op] {:in  op
                          :out (if (contains? stored (:id op))
                                 {:id (:id op) :ok true}
                                 {:id (:id op) :ok false
                                  :error (str "Entry not found: " (:id op))})})]]})

(defn- recording-store
  "IMemoryStore stub serving `stored` and recording every write."
  [writes]
  (reify mem-proto/IMemoryStore
    (connect! [_ _] nil) (disconnect! [_] nil) (connected? [_] true)
    (health-check [_] {})
    (add-entry! [_ e] (swap! writes conj [:add e]) (:id e))
    (get-entry [_ id] (stored id))
    (update-entry! [_ id u] (swap! writes conj [:update id u]) nil)
    (delete-entry! [_ id] (swap! writes conj [:delete id]) nil)
    (query-entries [_ _] [])
    (search-similar [_ _ _] []) (supports-semantic-search? [_] false)
    (cleanup-expired! [_] nil) (entries-expiring-soon [_ _ _] [])
    (find-duplicate [_ _ _ _] nil) (store-status [_] {})
    (reset-store! [_] nil)))

(deftest dry-run-reports-per-op-and-writes-nothing
  (let [writes   (atom [])
        snapshot (mem-proto/registered-stores)]
    (try
      (mem-proto/register-store! :default (recording-store writes))
      (let [resp (edit/handle-batch-edit
                  {:dry-run true
                   :operations [{:id "e1" :find "alpha" :replace "z"}
                                {:id "e1" :find "beta" :replace "z"}
                                {:id "gone" :content "x"}]})
            body (json/read-str (:text resp) :key-fn keyword)]
        (is (= [true false false] (mapv :ok (:results body))))
        (is (= 1 (:valid body)))
        (is (= 2 (:invalid body)))
        (is (empty? @writes) "a dry run must not touch the store"))
      (finally
        (mem-proto/unregister-store! :default)
        (when-let [old (:default snapshot)]
          (mem-proto/register-store! :default old))))))

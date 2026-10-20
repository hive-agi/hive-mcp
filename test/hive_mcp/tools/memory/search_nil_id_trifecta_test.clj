(ns hive-mcp.tools.memory.search-nil-id-trifecta-test
  "A search port may return a stale hit without an ID; pin the public response."
  (:require [clojure.test :refer [is]]
            [clojure.data.json :as json]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.tools.memory.search :as search]
            [hive-mcp.protocols.memory :as mem-proto]))

(defrecord SearchPort [hits]
  mem-proto/IMemoryStore
  (connect! [_ _] nil)
  (disconnect! [_] nil)
  (connected? [_] true)
  (health-check [_] {:healthy? true})
  (add-entry! [_ _] nil)
  (get-entry [_ _] nil)
  (update-entry! [_ _ _] nil)
  (delete-entry! [_ _] nil)
  (query-entries [_ _] nil)
  (search-similar [_ _ _] hits)
  (supports-semantic-search? [_] true)
  (cleanup-expired! [_] nil)
  (entries-expiring-soon [_ _ _] nil)
  (find-duplicate [_ _ _ _] nil)
  (store-status [_] {:configured? true})
  (reset-store! [_] nil))

(defn run-search
  "Install a search port for one call and restore the previous registration."
  [hits]
  (let [prior (get (mem-proto/registered-stores) :default)]
    (try
      (mem-proto/register-store! :default (->SearchPort hits))
      (let [response (search/handle-search-semantic
                      {:query "needle" :limit 20 :scope "all"
                       :exclude_tags [] :directory "/tmp"})]
        (if (:isError response)
          {:error (:text response)}
          (select-keys (json/read-str (:text response) :key-fn keyword)
                       [:results :count :dropped-null-hits])))
      (finally
        (if prior
          (mem-proto/register-store! :default prior)
          (mem-proto/unregister-store! :default))))))

(defn- hit [id]
  {:id id :content "needle" :type "note" :tags [] :distance 0.1})

(def ^:private input-of (atom nil))

(defn- run-case [hits]
  (reset! input-of hits)
  (run-search hits))

(defn- valid-answer? [answer]
  (let [hits @input-of
        ids (mapv :id (:results answer))]
    (and (not (:error answer))
         (= (count (filter (comp nil? :id) hits)) (:dropped-null-hits answer))
         (= (count ids) (:count answer))
         (= (set (keep :id hits)) (set ids))
         (= (count ids) (count (distinct ids))))))

(deftrifecta nil-id-search-contract
  hive-mcp.tools.memory.search-nil-id-trifecta-test/run-case
  {:golden-path "test/golden/tools/memory/search-nil-id.edn"
   :cases {:mixed [(hit "good-a") (hit nil) (hit "good-b")]
           :only-null [(hit nil) (hit nil)]
           :all-valid [(hit "good-a") (hit "good-b")]
           :empty []}
   :gen (gen/fmap (fn [ids] (mapv hit ids))
                  (gen/vector (gen/one-of [(gen/return nil)
                                           (gen/elements ["a" "b" "c"])]) 0 7))
   :pred valid-answer?
   :num-tests 30
   :mutations [["allow null IDs" (fn [hits]
                                  {:results (mapv (fn [e] {:id (:id e)}) hits)
                                   :count (count hits) :dropped-null-hits 0})]
               ["count only valid hits" (fn [hits]
                                          (let [ids (keep :id hits)]
                                            {:results (mapv (fn [id] {:id id}) ids)
                                             :count (count ids) :dropped-null-hits 0}))]]
   :assert (fn []
             (let [answer (run-case [(hit "survives") (hit nil)])]
               (is (= ["survives"] (mapv :id (:results answer))))
               (is (= 1 (:count answer)))
               (is (= 1 (:dropped-null-hits answer)))))} )

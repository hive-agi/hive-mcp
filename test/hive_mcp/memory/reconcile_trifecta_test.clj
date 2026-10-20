(ns hive-mcp.memory.reconcile-trifecta-test
  "Pin the vector-index vs entry-store diff and the `memory reconcile` verb.
   The core is pure; the verb is exercised through an IReconcileSources stub,
   so no live store or vector index is touched."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.memory.reconcile :as reconcile]
            [hive-mcp.tools.memory.reconcile :as verb]))

(defn- subject
  "One-argument adapter: INPUT is [store-ids vector-ids]."
  [[store-ids vector-ids]]
  (reconcile/diff-ids store-ids vector-ids))

(defn- valid-diff?
  [diff [store-ids vector-ids]]
  (let [store   (set (keep identity store-ids))
        vectors (set (keep identity vector-ids))]
    (and (= (vec (sort (remove store vectors))) (:orphan-vector-ids diff))
         (= (vec (sort (remove vectors store))) (:unindexed-store-ids diff))
         (= (count (filter nil? vector-ids)) (:nil-vector-ids diff))
         (= (count store) (:store-count diff))
         (= (count vectors) (:vector-count diff))
         (= (count (filter store vectors)) (:consistent-count diff))
         (= (and (empty? (:orphan-vector-ids diff))
                 (empty? (:unindexed-store-ids diff))
                 (zero? (:nil-vector-ids diff)))
            (:consistent? diff)))))

(def ^:private last-input (atom nil))

(defn- run-case [input]
  (reset! last-input input)
  (subject input))

(defn- pred [diff] (valid-diff? diff @last-input))

(deftrifecta reconcile-diff-contract
  hive-mcp.memory.reconcile-trifecta-test/run-case
  {:golden-path "test/golden/memory/reconcile-diff.edn"
   :cases {:in-sync [["a" "b"] ["b" "a"]]
           :orphan-vector [["a"] ["a" "ghost"]]
           :unindexed-entry [["a" "lost"] ["a"]]
           :nil-vector-id [["a"] ["a" nil]]
           :both-empty [[] []]}
   :gen (gen/tuple
         (gen/vector (gen/elements ["a" "b" "c" "d"]) 0 6)
         (gen/vector (gen/one-of [(gen/return nil)
                                  (gen/elements ["a" "b" "c" "e"])]) 0 6))
   :pred pred
   :num-tests 40
   :mutations [["report nothing" (fn [_] {:orphan-vector-ids [] :unindexed-store-ids []
                                          :nil-vector-ids 0 :store-count 0
                                          :vector-count 0 :consistent-count 0
                                          :consistent? true})]
               ["swap the two sides" (fn [[s v]] (reconcile/diff-ids v s))]]
   :assert (fn []
             (let [d (run-case [["a" "lost"] ["a" "ghost" nil]])]
               (is (= ["ghost"] (:orphan-vector-ids d)))
               (is (= ["lost"] (:unindexed-store-ids d)))
               (is (= 1 (:nil-vector-ids d)))
               (is (false? (:consistent? d)))))})

(defrecord StubSources [store vectors writes]
  reconcile/IReconcileSources
  (store-ids [_] store)
  (vector-ids [_] vectors))

(defn- call-verb [sources params]
  (try
    (verb/set-sources! sources)
    (json/read-str (:text (verb/handle-reconcile params)) :key-fn keyword)
    (finally (verb/clear-sources!))))

(use-fixtures :each (fn [t] (verb/clear-sources!) (t) (verb/clear-sources!)))

(deftest verb-reports-drift-through-port-stub
  (testing "orphan and unindexed ids are reported, status ok"
    (let [r (call-verb (->StubSources ["a" "lost"] ["a" "ghost"] nil) {})]
      (is (= "ok" (:status r)))
      (is (= ["ghost"] (:orphan-vector-ids r)))
      (is (= ["lost"] (:unindexed-store-ids r)))
      (is (= 1 (:orphan-count r)))
      (is (false? (:consistent? r))))))

(deftest verb-limit-truncates-listings
  (let [ghosts (mapv #(str "g" %) (range 5))
        r (call-verb (->StubSources [] ghosts nil) {:limit 2})]
    (is (= 2 (count (:orphan-vector-ids r))))
    (is (= 5 (:orphan-count r)))
    (is (true? (:truncated? r)))))

(deftest verb-without-sources-is-not-wired
  (let [r (json/read-str (:text (verb/handle-reconcile {})) :key-fn keyword)]
    (is (= "not-wired" (:status r)))))

(deftest verb-in-sync-is-consistent
  (let [r (call-verb (->StubSources ["a" "b"] ["b" "a"] nil) {})]
    (is (true? (:consistent? r)))
    (is (empty? (:orphan-vector-ids r)))))

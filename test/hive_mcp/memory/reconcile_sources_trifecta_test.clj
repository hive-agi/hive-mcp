(ns hive-mcp.memory.reconcile-sources-trifecta-test
  "Pin `read-all` paging and the `paged-sources` port over page-function
   stubs (vectors sliced by offset/limit); no live store or index is touched."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.memory.reconcile :as reconcile]
            [hive-mcp.memory.reconcile-sources :as src]))

(defn- pager
  "A page function over the fixed vector IDS."
  [ids]
  (fn [offset limit] (->> ids (drop offset) (take limit) vec)))

(def ^:private last-input (atom nil))

(defn- run-case
  "INPUT is [ids page-size max-pages]."
  [[ids page-size max-pages :as input]]
  (reset! last-input input)
  (src/read-all (pager ids) page-size max-pages))

(defn- valid-listing?
  [{:keys [ids pages exhausted?]}]
  (let [[all size cap] @last-input
        need (inc (quot (count all) size))]
    (if (<= need cap)
      (and exhausted? (= all ids) (= need pages))
      (and (not exhausted?) (= cap pages) (= (vec (take (* cap size) all)) ids)))))

(deftrifecta read-all-paging-contract
  hive-mcp.memory.reconcile-sources-trifecta-test/run-case
  {:golden-path "test/golden/memory/reconcile-sources-read-all.edn"
   :cases {:empty         [[] 3 5]
           :short-page    [["a" "b"] 3 5]
           :exact-page    [["a" "b" "c"] 3 5]
           :several-pages [["a" "b" "c" "d" "e" "f" "g"] 3 5]
           :capped        [["a" "b" "c" "d" "e" "f" "g"] 2 2]}
   :gen (gen/tuple (gen/vector (gen/elements ["a" "b" "c" nil "d"]) 0 20)
                   (gen/choose 1 6)
                   (gen/choose 1 6))
   :pred valid-listing?
   :num-tests 60
   :mutations [["stop after the first page" (fn [[ids size _]] (src/read-all (pager ids) size 1))]
               ["claim exhaustion always" (fn [in] (assoc (run-case in) :exhausted? true))]]
   :assert (fn []
             (is (= {:ids ["a" "b" "c" "d"] :pages 2 :exhausted? false}
                    (run-case [["a" "b" "c" "d" "e"] 2 2]))))})

(deftest paged-sources-feed-the-reconcile-report
  (let [sources (src/paged-sources (pager ["a" "lost" "b"]) (pager ["a" "b" "ghost" nil])
                                   {:page-size 2})]
    (testing "before a report nothing was read"
      (is (false? (src/exhausted? sources))))
    (let [r (reconcile/report sources)]
      (is (= ["ghost"] (:orphan-vector-ids r)))
      (is (= ["lost"] (:unindexed-store-ids r)))
      (is (= 1 (:nil-vector-ids r)))
      (is (src/exhausted? sources))))
  (testing "a listing cut by max-pages is reported as partial"
    (let [sources (src/paged-sources (pager (mapv str (range 10))) (pager ["0"])
                                     {:page-size 2 :max-pages 2})]
      (reconcile/report sources)
      (is (false? (src/exhausted? sources))))))

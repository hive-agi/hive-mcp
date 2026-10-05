(ns hive-mcp.tools.memory.review-test
  "Pure queue grouping and query-port handler regression for multiple gates."
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.tools.memory.review :as review]))

(defn grouped [rows]
  (review/group-queues rows))

(deftrifecta grouped-queues-trifecta
  hive-mcp.tools.memory.review-test/grouped
  {:golden-path "test/golden/memory/review-queues.edn"
   :cases {:two-gates [["axiom-candidate" [{:id "a"}]]
                       ["friction-hypothesis-candidate" [{:id "f"}]]]
           :empty-gate [["axiom-candidate" []]
                        ["friction-hypothesis-candidate" [{:id "f"}]]]
           :no-gates []}
   :gen (gen/vector (gen/tuple (gen/elements ["axiom-candidate" "friction-hypothesis-candidate"])
                               (gen/vector (gen/hash-map :id gen/string-alphanumeric) 0 5)) 0 5)
   :pred (fn [result] (and (map? result) (every? vector? (vals result))))
   :num-tests 100
   :mutations [["drop-second" (fn [rows] (into (sorted-map) (take 1 rows)))]
               ["flatten" (fn [rows] (vec (mapcat second rows)))]]})

(deftest lists-every-gate-through-query-port
  (let [calls (atom [])
        port (fn [params]
               (swap! calls conj params)
               {:type "text" :text (json/generate-string
                                     [{:id (:type params) :type (:type params)}])})]
    (binding [review/*query-port* port]
      (let [result (json/parse-string (:text (review/handle-review {:limit 3})) true)]
        (is (= #{:axiom-candidate :friction-hypothesis-candidate} (set (keys result))))
        (is (= ["axiom-candidate"] (mapv :id (:axiom-candidate result))))
        (is (= ["friction-hypothesis-candidate"]
               (mapv :id (:friction-hypothesis-candidate result))))
        (is (= #{"axiom-candidate" "friction-hypothesis-candidate"}
               (set (map :type @calls))))
        (is (every? #(= {:limit 3 :scope "all" :verbosity "metadata"}
                        (dissoc % :type)) @calls))))
    (reset! calls [])
    (binding [review/*query-port* port]
      (let [result (json/parse-string
                     (:text (review/handle-review {:type "friction-hypothesis-candidate"})) true)]
        (is (= #{:friction-hypothesis-candidate} (set (keys result))))
        (is (= ["friction-hypothesis-candidate"] (mapv :type @calls)))))))

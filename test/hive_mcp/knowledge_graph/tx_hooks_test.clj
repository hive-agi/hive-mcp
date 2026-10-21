(ns hive-mcp.knowledge-graph.tx-hooks-test
  "KG tx-hooks: tx-data passes untouched with no hook, hooks rewrite in id
   order, a throwing hook refuses the transaction, and unregister removes."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.knowledge-graph.tx-hooks :as tx-hooks]))

(defn- clean [t]
  (doseq [id (tx-hooks/registered)] (tx-hooks/unregister! id))
  (t)
  (doseq [id (tx-hooks/registered)] (tx-hooks/unregister! id)))

(use-fixtures :each clean)

(def ^:private edge {:kg-edge/id "e1" :kg-edge/from "a" :kg-edge/to "b" :kg-edge/relation :refines})

(deftest no-hook-passes-tx-data-untouched
  (is (= [edge] (tx-hooks/apply-hooks [edge]))))

(deftest hooks-rewrite-in-id-order
  (tx-hooks/register! :b/second (fn [tx] (conj tx :second)))
  (tx-hooks/register! :a/first (fn [tx] (conj tx :first)))
  (is (= [:a/first :b/second] (tx-hooks/registered)))
  (is (= [edge :first :second] (tx-hooks/apply-hooks [edge]))))

(deftest a-throwing-hook-refuses-the-transaction
  (tx-hooks/register! :x/refuse (fn [_] (throw (ex-info "refused" {:id :x}))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"refused" (tx-hooks/apply-hooks [edge]))))

(defn tag-hook [tx] (conj tx :tagged))

(deftest a-var-is-a-hook
  (tx-hooks/register! :x/var #'tag-hook)
  (is (= [edge :tagged] (tx-hooks/apply-hooks [edge]))))

(deftest register-replaces-and-unregister-removes
  (tx-hooks/register! :x/h (fn [tx] (conj tx 1)))
  (tx-hooks/register! :x/h (fn [tx] (conj tx 2)))
  (is (= [edge 2] (tx-hooks/apply-hooks [edge])))
  (tx-hooks/unregister! :x/h)
  (is (= [edge] (tx-hooks/apply-hooks [edge])))
  (testing "unregistering an absent id is safe"
    (is (= :x/none (tx-hooks/unregister! :x/none)))))

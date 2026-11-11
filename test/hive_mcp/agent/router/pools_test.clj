(ns hive-mcp.agent.router.pools-test
  "Tests for hive-mcp.agent.router.pools: model pools, work-class mapping,
   and pool resolution. All pure data — no fixtures, no with-redefs."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.router.pools :as sut]
            [malli.core :as m]))

(defn valid-member?
  [m]
  (and (string? (:provider m))
       (string? (:model m))
       (pos-int? (:weight m))))

(defn valid-pools?
  [pools-map]
  (and (map? pools-map)
       (every? #{:cheap :mid :frontier} (keys pools-map))
       (every? vector? (vals pools-map))
       (every? (fn [members] (every? valid-member? members))
               (vals pools-map))))

(defn valid-work-classes?
  [wc]
  (and (map? wc)
       (every? keyword? (keys wc))
       (every? keyword? (vals wc))))

(deftest defaults-validate-against-schemas-test
  (testing "default-pools validates against PoolMember schemas"
    (is (valid-pools? sut/default-pools)))
  (testing "each pool member matches PoolMember schema"
    (doseq [[pool-kw members] sut/default-pools
            member members]
      (is (m/validate sut/PoolMember member)
          (str "Member " member " in pool " pool-kw " fails PoolMember schema"))))
  (testing "each pool vector validates as part of Pools schema"
    (is (m/validate sut/Pools sut/default-pools)
        "default-pools validates against Pools schema"))
  (testing "default-work-classes validates against WorkClasses schema"
    (is (m/validate sut/WorkClasses sut/default-work-classes)
        "default-work-classes validates against WorkClasses schema")))

(deftest config-override-replaces-single-pool-test
  (testing "a config override replaces one pool without affecting the others"
    (let [config {:router {:pools {:cheap [{:provider "test" :model "test-model" :weight 1}]}}}
          result (sut/pools config)]
      (is (= [{:provider "test" :model "test-model" :weight 1}]
             (:cheap result))
          "cheap pool is replaced")
      (is (= (:mid sut/default-pools) (:mid result))
          "mid pool is unchanged")
      (is (= (:frontier sut/default-pools) (:frontier result))
          "frontier pool is unchanged")))
  (testing "work-class config override merges on top of defaults"
    (let [config {:router {:work-classes {:review :cheap}}}
          result (sut/work-classes config)]
      (is (= :cheap (:review result))
          "review is overridden to cheap")
      (is (= :cheap (:grunt result))
          "grunt stays from defaults"))))

(deftest unknown-work-class-returns-frontier-test
  (testing "an unknown work class returns the :frontier pool"
    (let [result (sut/pool-for {} :nonexistent)]
      (is (= :frontier (:pool result))
          "unknown work class pool is :frontier")
      (is (= (:frontier sut/default-pools) (:members result))
          "members come from the frontier pool")))
  (testing "a nil work class returns the :frontier pool"
    (let [result (sut/pool-for {} nil)]
      (is (= :frontier (:pool result))
          "nil work class pool is :frontier"))))

(deftest work-class-mapped-to-an-undeclared-pool-returns-frontier-test
  (let [result (sut/pool-for {:router {:work-classes {:grunt :bargain}}} :grunt)]
    (is (= :frontier (:pool result)))
    (is (= (:frontier sut/default-pools) (:members result))))
  (testing "an emptied pool also falls to :frontier"
    (is (= :frontier (:pool (sut/pool-for {:router {:pools {:cheap []}}} :grunt))))))

(deftest answers-satisfy-their-contracts-test
  (is (m/validate sut/Pools (sut/pools nil)))
  (is (m/validate sut/WorkClasses (sut/work-classes {})))
  (is (m/validate sut/PoolChoice (sut/pool-for {} :grunt)))
  (is (m/validate sut/PoolChoice (sut/pool-for {} nil))))

(deftest sensitive-work-classes-never-cheap-test
  (testing ":money never resolves to :cheap under defaults"
    (is (= :frontier (:pool (sut/pool-for {} :money)))))
  (testing ":auth never resolves to :cheap under defaults"
    (is (= :frontier (:pool (sut/pool-for {} :auth)))))
  (testing ":migration never resolves to :cheap under defaults"
    (is (= :frontier (:pool (sut/pool-for {} :migration)))))
  (testing ":production never resolves to :cheap under defaults"
    (is (= :frontier (:pool (sut/pool-for {} :production)))))
  (testing ":secrets never resolves to :cheap under defaults"
    (is (= :frontier (:pool (sut/pool-for {} :secrets))))))

(deftest pool-for-return-shape-test
  (testing "pool-for returns the expected shape"
    (let [result (sut/pool-for {} :grunt)]
      (is (contains? result :pool))
      (is (contains? result :members))
      (is (vector? (:members result)))
      (is (= :cheap (:pool result)))))
  (testing "pool-for with config override still returns correct shape"
    (let [config {:router {:pools {:cheap [{:provider "x" :model "y" :weight 2}]}}}
          result (sut/pool-for config :grunt)]
      (is (= :cheap (:pool result)))
      (is (= [{:provider "x" :model "y" :weight 2}] (:members result))))))

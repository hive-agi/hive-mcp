(ns hive-mcp.hotspot-batch3-test
  "Pure-stage tests for the batch-3 hotspot decompositions (golden, property
   and edge cases). The stages under test are private seams, reached by var."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.server.routes.middleware :as mw]
            [hive-mcp.scheduler.dag-waves :as dw]
            [hive-mcp.agent.headless :as hl]
            [hive-mcp.embeddings.service :as svc]))

(def ^:private fit-catchup-blocks @#'mw/fit-catchup-blocks)
(def ^:private catchup-wire-size @#'mw/catchup-wire-size)
(def ^:private plan-wave @#'dw/plan-wave)
(def ^:private next-drain-deadline @#'hl/next-drain-deadline)
(def ^:private reader-action @#'hl/reader-action)
(def ^:private script-aware-tokens @#'svc/script-aware-tokens)

(deftest fit-catchup-blocks-golden
  (let [a (catchup-wire-size :a "x")
        b (catchup-wire-size :b "yy")]
    (testing "everything fits"
      (is (= [{:a "x" :b "yy"} nil] (fit-catchup-blocks [[:a "x"] [:b "yy"]] 1000))))
    (testing "first oversized block defers itself and everything after it"
      (is (= [{} {:a "x" :b "yy"}] (fit-catchup-blocks [[:a "x"] [:b "yy"]] (dec a)))))
    (testing "later blocks defer once the budget is spent"
      (is (= [{:a "x"} {:b "yy"}] (fit-catchup-blocks [[:a "x"] [:b "yy"]] (+ a (dec b))))))
    (testing "nothing in, nothing out"
      (is (= [{} nil] (fit-catchup-blocks nil 10))))))

(defspec fit-catchup-blocks-partitions 100
  (prop/for-all [bodies (gen/vector gen/string-alphanumeric 0 6)
                 budget (gen/choose 0 400)]
    (let [pairs (map-indexed (fn [i b] [(keyword (str "t" i)) b]) bodies)
          [deliver defer] (fit-catchup-blocks pairs budget)]
      (and (= (count pairs) (+ (count deliver) (count defer)))
           (<= (reduce + 0 (map (fn [[t b]] (catchup-wire-size t b)) deliver)) budget)))))

(deftest plan-wave-golden
  (is (= {:to-dispatch [1 2] :available 2 :skipped 1}
         (plan-wave [1 2 3] 3 {:x :y})))
  (is (= {:to-dispatch [] :available 0 :skipped 2}
         (plan-wave [1 2] 1 {:x :y :z :w})))
  (is (= {:to-dispatch [] :available 5 :skipped 0}
         (plan-wave [] 5 nil))))

(deftest drain-deadline-and-reader-action
  (is (nil? (next-drain-deadline nil false 100 50)))
  (is (= 150 (next-drain-deadline nil true 100 50)))
  (is (= 120 (next-drain-deadline 120 true 100 50)))
  (is (= :stop (reader-action 100 true true 101)))
  (is (= :sleep (reader-action 100 true false 90)))
  (is (= :read (reader-action nil false false 90)))
  (is (= :read (reader-action 100 true true 90))))

(deftest script-aware-tokens-golden
  (testing "empty string costs the base one"
    (is (= 1 (script-aware-tokens "" 5.0))))
  (testing "ASCII word: ceil(len/cpt) + base"
    (is (= 2 (script-aware-tokens "hello" 5.0)))
    (is (= 4 (script-aware-tokens "hello" 2.0))))
  (testing "digits and punctuation cost one each"
    (is (= 4 (script-aware-tokens "123" 5.0))))
  (testing "CJK costs one per code point"
    (is (= 3 (script-aware-tokens "日本" 5.0)))))

(defspec script-aware-tokens-monotone-in-append 100
  (prop/for-all [s gen/string-alphanumeric]
    (>= (script-aware-tokens (str s "!") 5.0) (script-aware-tokens s 5.0))))

(ns hive-mcp.channel.drain-metrics-test
  "Contract tests for the Context Codec metrics over a drain batch.

   The load-bearing test is `critical-atom-recall-is-one-by-construction`: it
   does not assert a number the code happens to produce, it runs entries
   through the real projection and asserts the guarantee survives it."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.channel.commitment :as commit]
            [hive-mcp.channel.drain-metrics :as dm]
            [hive-mcp.channel.drain-projection :as proj]
            [hive-mcp.channel.drain-rank :as rank]))

(defn- entry
  [id t chars]
  {:id id :T t :C (apply str (repeat chars "x")) :tags []})

(def ^:private a-buffer
  (into [(entry "ax-1" "axiom" 4000)
         (entry "ax-2" "axiom" 4000)]
        (for [i (range 12)] (entry (str "note-" i) "note" 2000))))

(deftest critical-atom-recall-is-one-by-construction
  (testing "through the REAL projection, not a hand-built batch"
    (let [projected (proj/project a-buffer {:policy :index})
          m (dm/metrics projected)]
      (is (= 1.0 (:critical-atom-recall m))
          "an axiom that was pointerised would show here")
      (is (= 2 (get (:by-class m) :safety-boundary)))
      (is (nil? (get (:withheld-by-class m) :safety-boundary))
          "no safety boundary may be withheld")
      (is (pos? (get (:withheld-by-class m) :evidence))
          "the pool entries WERE pointerised, so the test is not vacuous"))))

(deftest recall-and-recoverability-are-different-questions
  (let [projected (proj/project a-buffer {:policy :index})
        m (dm/metrics projected)]
    (is (= 1.0 (:round-trip-recoverability m))
        "every pointer still carries the id a pull needs")
    (is (< (:weighted-atom-recall m) 1.0)
        "content was withheld, so weighted recall must not read as perfect")))

(deftest an-empty-population-has-no-recall-rather-than-zero
  (let [m (dm/metrics [(entry "n" "note" 10)])]
    (is (nil? (:critical-atom-recall m))
        "no axioms present, so the number is undefined, not 0.0"))
  (let [m (dm/metrics [])]
    (is (zero? (:atoms m)))
    (is (nil? (:round-trip-recoverability m)))
    (is (zero? (:commitment-density m)))))

(deftest a-pointer-scores-zero-recall-and-one-recoverability
  (let [a (dm/atom-of {:id "x" :T "note" :ref true})]
    (is (false? (:whole? a)))
    (is (true? (:recoverable? a)))
    (is (= 0.0 (dm/round-trip-recoverability [(assoc a :recoverable? false)])))))

(deftest an-entry-with-no-id-is-not-recoverable
  (is (false? (:recoverable? (dm/atom-of {:T "note" :ref true})))
      "a pointer with no address is content that is simply gone"))

(deftest density-counts-commitments-per-thousand-chars
  (is (= 0.0 (dm/commitment-density [] 0)))
  (is (= 1.0 (dm/commitment-density [{} {}] 2000))))

(deftest the-string-spelling-of-a-type-is-the-one-production-sends
  (is (= :safety-boundary (:class (dm/atom-of {:id "a" :T "axiom"}))))
  (is (= (dm/atom-of {:id "a" :T "axiom"})
         (dm/atom-of {:id "a" :T :axiom}))))

(deftest critical-classes-agrees-with-the-shipped-floor
  (is (= rank/floor-types
         (into #{} (filter commit/critical?) (keys commit/type->class)))))

(def ^:private gen-entry
  (gen/let [id (gen/not-empty gen/string-alphanumeric)
            t (gen/elements ["axiom" "note" "decision" "convention" "snippet"])
            ref? gen/boolean]
    (cond-> {:id id :T t :C "body"}
      ref? (assoc :ref true))))

(defspec every-metric-stays-inside-its-bounds 200
  (prop/for-all [batch (gen/vector gen-entry 0 30)]
    (let [m (dm/metrics batch)]
      (every? (fn [k] (let [v (get m k)]
                        (or (nil? v) (and (<= 0.0 v) (<= v 1.0)))))
              [:critical-atom-recall :weighted-atom-recall
               :round-trip-recoverability]))))

(defspec projection-never-pointerises-a-critical-atom 200
  ;; Stated as a DELTA, not as an absolute. An entry that arrives already
  ;; marked :ref keeps that mark, and projection cannot mint that state for an
  ;; axiom, so the guarantee is "projection did not change it", not "it is
  ;; never set". Asserting the absolute made the generator, not the code, the
  ;; thing under test.
  (prop/for-all [batch (gen/vector gen-entry 1 20)]
    (let [after (proj/project batch {:policy :index})]
      (every? (fn [[before entry]]
                (or (not (commit/critical? (:T entry)))
                    (= (boolean (:ref before)) (boolean (:ref entry)))))
              (map vector batch after)))))

(defspec a-critical-atom-that-arrived-whole-is-recalled 200
  (prop/for-all [batch (gen/vector (gen/fmap #(dissoc % :ref) gen-entry) 1 20)]
    (let [m (dm/metrics (proj/project batch {:policy :index}))]
      (and (nil? (get (:withheld-by-class m) :safety-boundary))
           (contains? #{nil 1.0} (:critical-atom-recall m))))))

(defspec a-batch-with-every-id-present-is-fully-recoverable 200
  (prop/for-all [batch (gen/vector gen-entry 1 20)]
    (= 1.0 (dm/round-trip-recoverability (map dm/atom-of batch)))))

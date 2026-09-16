(ns hive-mcp.channel.commitment-test
  "Conformance tests for the commitment taxonomy.

   The load-bearing test is `every-core-memory-type-is-classified`: its
   universe is the type registry, never the classification map, so a memory
   type added without a class fails here instead of silently defaulting."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.channel.commitment :as c]
            [hive-mcp.channel.drain-rank :as rank]
            [hive-mcp.memory.type-registry :as reg]))

(def ^:private core-types
  (set (keys @#'reg/base-registry)))

(def ^:private classified-types
  (into core-types (keys c/type->class)))

(deftest every-core-memory-type-is-classified
  (testing "universe drawn from the registry, not from the classification"
    (is (<= 20 (count core-types))
        "vacuous universe — base-registry did not load")
    (is (empty? (remove c/classified? core-types))
        "registered memory types carrying no commitment class")))

(deftest the-mapping-the-plan-specifies
  (doseq [[t expected] {:axiom      :safety-boundary
                        :decision   :decision
                        :convention :constraint
                        :principle  :constraint
                        :note       :evidence
                        :knowledge  :evidence}]
    (is (= expected (c/classify t)) (str t))))

(deftest the-string-spelling-of-a-type-is-the-one-production-sends
  (is (= :safety-boundary (c/classify "axiom")))
  (is (c/critical? "axiom"))
  (is (= (c/classify :note) (c/classify "note"))))

(deftest critical-agrees-with-the-shipped-floor
  (is (= rank/floor-types
         (into #{} (filter c/critical?) classified-types))))

(deftest an-unclassified-type-defaults-to-a-protected-class
  (let [t :some-addon-registered-type]
    (is (not (c/classified? t)))
    (is (= c/default-class (c/classify t)))
    (is (> (c/weight t) (c/class-weight :evidence))
        "an unclassified type must not be more compressible than evidence")))

(deftest weights-descend-with-binding-strength
  (let [ws (mapv c/class-weight c/classes)]
    (is (= ws (vec (reverse (sort ws)))))
    (is (= 1.0 (first ws)))
    (is (apply distinct? ws)))
  (is (nil? (c/class-weight :not-a-commitment-class))))

(deftest a-missing-type-is-still-classified
  (is (= c/default-class (c/classify nil)))
  (is (= c/default-class (c/classify ""))))

(def ^:private token-gen
  (gen/one-of [(gen/elements (vec (keys c/type->class)))
               (gen/fmap name (gen/elements (vec (keys c/type->class))))
               gen/keyword
               gen/string-alphanumeric]))

(defspec classification-is-total 200
  (prop/for-all [t token-gen]
    (contains? (set c/classes) (c/classify t))))

(defspec a-type-classifies-the-same-whichever-way-it-is-spelled 200
  (prop/for-all [t (gen/elements (vec (keys c/type->class)))]
    (= (c/classify t)
       (c/classify (name t))
       (c/classify (symbol (name t))))))

(defspec weight-is-bounded-and-positive 200
  (prop/for-all [t token-gen]
    (let [w (c/weight t)]
      (and (< 0.0 w) (<= w 1.0)))))

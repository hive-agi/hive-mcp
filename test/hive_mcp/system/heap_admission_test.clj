(ns hive-mcp.system.heap-admission-test
  "Heap admission (friction 20261111145728-573b7979): only the HARD watermark
   refuses, a soft reading admits, and every failure admits. The heap is never
   sampled by the decision tests: `hard-pressure` takes the probe as an
   argument."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.system.heap-admission :as ha]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private wm {:soft 0.80 :hard 0.92})

(defn- probe-returning
  "A probe answering `reading`, recording the watermarks it was asked with."
  [reading seen]
  (fn [w] (reset! seen w) reading))

(deftest hard-pressure-refuses-only-at-hard
  (let [seen (atom nil)]
    (testing ":hard answers the reading, with the watermarks the probe was asked with"
      (let [reading {:level :hard :basis :post-gc :heap-pct 94 :now-pct 97
                     :post-gc-pct 94 :pool "G1 Old Gen"}]
        (is (= (assoc reading :watermarks wm)
               (ha/hard-pressure (probe-returning reading seen) wm)))
        (is (= wm @seen))))
    (testing ":soft admits (the gate used to refuse here)"
      (is (nil? (ha/hard-pressure (probe-returning {:level :soft :basis :post-gc :heap-pct 85} seen)
                                  wm))))
    (testing ":ok admits"
      (is (nil? (ha/hard-pressure (probe-returning {:level :ok :basis :post-gc :heap-pct 40} seen)
                                  wm))))
    (testing "95% in use now but 60% after GC is an :ok reading, and admits"
      (is (nil? (ha/hard-pressure (probe-returning {:level :ok :basis :post-gc :heap-pct 60
                                                    :now-pct 95 :post-gc-pct 60}
                                                   seen)
                                  wm))))))

(deftest hard-pressure-fails-open
  (testing "no governor on the classpath (nil reading) admits"
    (is (nil? (ha/hard-pressure (constantly nil) wm))))
  (testing "a probe that throws admits"
    (is (nil? (ha/hard-pressure (fn [_] (throw (ex-info "probe down" {}))) wm)))))

(deftest heap-probe-reads-this-jvm
  (testing "nil (no hive-cache) or a Reading with a level and a basis"
    (let [r (ha/heap-probe wm)]
      (is (or (nil? r)
              (and (m/validate ha/Reading r)
                   (contains? #{:post-gc :instant} (:basis r))))
          (pr-str (dissoc r :snapshot))))))

(deftest the-production-gate-answers-without-throwing
  (testing "the 0-arity reads config and the live heap and answers nil or a hard Reading"
    (let [r (ha/hard-pressure)]
      (is (or (nil? r) (= :hard (:level r)))))))

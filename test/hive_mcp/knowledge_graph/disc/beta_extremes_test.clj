(ns hive-mcp.knowledge-graph.disc.beta-extremes-test
  "beta-lower-bound over the whole positive parameter range: the extremes the
   m14 coverage batch found (long overflow, a zero denominator, NaN) return a
   finite score, and a trifecta over the widened BetaDisc contract holds."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.knowledge-graph.disc.schema :as ds]
            [hive-mcp.knowledge-graph.disc.volatility :as vol]
            [hive-schemas.test :as hst]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- score? [x]
  (and (number? x) (not (Double/isNaN (double x))) (<= 0.0 x 1.0)))

(defn- disc [a b] {:disc/certainty-alpha a :disc/certainty-beta b})

(def ^:private extremes
  {"long overflow past alpha+beta ~2.09e6" (disc 1 2097151)
   "both longs at Long/MAX_VALUE"          (disc Long/MAX_VALUE Long/MAX_VALUE)
   "tiny doubles underflowing sum^3"       (disc 1.1e-162 2.8e-163)
   "smallest subnormal"                    (disc Double/MIN_VALUE Double/MIN_VALUE)
   "huge doubles overflowing a*b"          (disc 9e307 9e307)
   "Double/MAX_VALUE on both"              (disc Double/MAX_VALUE Double/MAX_VALUE)
   "mixed extremes"                        (disc Double/MAX_VALUE Double/MIN_VALUE)
   "bigint past Long range"                (disc (bigint 1e30) 3)})

(deftest beta-lower-bound-is-a-finite-score-at-extremes
  (doseq [[label d] extremes]
    (testing label
      (let [out (vol/beta-lower-bound d)]
        (is (score? out) (str label " -> " out))))))

(deftest beta-lower-bound-keeps-its-values-in-the-normal-range
  (is (< (Math/abs (- (vol/beta-lower-bound nil)
                      (- (/ 5.0 7.0) (* 2.0 (Math/sqrt (/ 10.0 (* 49.0 8.0)))))))
         1e-12))
  (is (< (vol/beta-lower-bound (disc 1.0 1.0)) 0.5))
  (testing "a near-certain huge alpha approaches 1"
    (is (> (vol/beta-lower-bound (disc 1e12 1.0)) 0.999))))

(defn- mean
  "Beta mean in scaled double space, so the oracle itself cannot overflow."
  [d]
  (let [a (min Double/MAX_VALUE (double (or (:disc/certainty-alpha d) 5.0)))
        b (min Double/MAX_VALUE (double (or (:disc/certainty-beta d) 2.0)))
        mx (max a b)]
    (/ (/ a mx) (+ (/ a mx) (/ b mx)))))

(hst/deftrifecta-from-schema beta-lower-bound-wide-contract
  hive-mcp.knowledge-graph.disc.volatility/beta-lower-bound
  {:in [:cat [:maybe ds/BetaDisc]]
   :out (let [s (get-in (m/function-schemas)
                        ['hive-mcp.knowledge-graph.disc.volatility 'beta-lower-bound :schema])]
          (second (m/children s)))
   ;; the lower bound of the credible interval never exceeds its mean
   :rel (fn [[d] out] (and (score? out) (<= out (+ 1e-9 (mean d)))))
   :num-tests 300})

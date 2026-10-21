(ns hive-mcp.tools.memory.duration-contract-test
  "Contract facets for hive-mcp.tools.memory.duration."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.memory.duration :as dur]
            [hive-mcp.tools.memory.schema :as ms]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- idx [d] (.indexOf ^java.util.List dur/duration-order (or d "long")))

(deftrifecta-from-schema shift-duration-clamps-to-the-order
  #'dur/shift-duration
  ;; Boundary durations and a zero delta are weighted so the :held arm is
  ;; reached as often as :moved.
  {:in [:cat {:gen/schema [:or
                           [:cat [:maybe ms/DurationName] [:int {:min -3 :max 3}]]
                           [:cat [:maybe ms/DurationName] [:= 0]]
                           [:cat [:= "permanent"] [:int {:min 1 :max 3}]]
                           [:cat [:= "ephemeral"] [:int {:min -3 :max -1}]]]}
        [:maybe ms/DurationName] [:int {:min -3 :max 3}]]


   :out ms/ShiftResult
   :rel (fn [[d delta] {:keys [new-duration changed? at-boundary?]}]
          (let [i (idx d)
                j (max 0 (min (dec (count dur/duration-order)) (+ i delta)))]
            (and (= new-duration (nth dur/duration-order j))
                 (= changed? (not= j i))
                 (= at-boundary? (= j i)))))
   :classify (fn [_ out] (if (:changed? out) :moved :held))
   :classify-domain #{:moved :held}
   :golden-path "test/golden/hive-mcp/tools/memory/shift-duration.edn"
   :num-tests 200})

(deftrifecta-from-schema valid-duration-is-membership
  #'dur/valid-duration?
  {:in [:cat [:or ms/DurationName [:enum "forever" "" nil]]]
   :out :boolean
   :rel (fn [[d] out] (= out (contains? (set dur/duration-order) d)))
   :golden-path "test/golden/hive-mcp/tools/memory/valid-duration.edn"
   :num-tests 100})

(deftest calculate-expires-is-nil-only-for-permanent
  (is (nil? (dur/calculate-expires "permanent")))
  (doseq [d ["ephemeral" "short" "medium" "long" nil]]
    (is (string? (dur/calculate-expires d)) (str d))))

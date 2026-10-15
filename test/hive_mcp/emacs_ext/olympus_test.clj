(ns hive-mcp.emacs-ext.olympus-test
  "The `:emacs/olympus-*` façade forwards to whatever layout calculator the
   extension registry holds, and answers nil when hive-emacs is not loaded.
   The calculators are the in-process stub contributors
   (hive-mcp.test.stub.olympus), registered through the registry seam."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.emacs-ext.olympus :as olympus]
            [hive-mcp.test.stub.extensions :as ext-stub]
            [hive-mcp.test.stub.olympus :as stub]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each stub/olympus-fixture)

(deftrifecta-from-schema calculate-layout-trifecta olympus/calculate-layout
  {:in [:cat [:int {:min 0 :max 40}]]
   :out olympus/Layout
   :rel (fn [[n] layout]
          (= layout (stub/calculate-layout n)))
   :num-tests 200})

(deftrifecta-from-schema position-for-cell-trifecta olympus/position-for-cell
  {:in [:cat olympus/Positions [:int {:min 0 :max 2}] [:int {:min 0 :max 2}]
        [:maybe [:int {:min 0 :max 2}]]]
   :out [:maybe :string]
   :rel (fn [[positions row col tab] id]
          (if (nil? id)
            (not-any? #(= {:row row :col col :tab tab} (select-keys % [:row :col :tab]))
                      (vals positions))
            (= {:row row :col col :tab tab}
               (select-keys (get positions id) [:row :col :tab]))))
   :num-tests 200})

(deftest grid-and-tabbed-arms-are-distinct
  (let [grid   (olympus/calculate-layout 3)
        tabbed (olympus/calculate-layout 9)]
    (is (m/validate olympus/GridLayout grid))
    (is (not (m/validate olympus/TabbedLayout grid)))
    (is (m/validate olympus/TabbedLayout tabbed))
    (is (not (m/validate olympus/GridLayout tabbed)))))

(deftest assign-positions-places-every-ling
  (let [lings     (mapv #(hash-map :slave/id (str "ling-" %)) (range 6))
        layout    (olympus/calculate-layout (count lings))
        positions (olympus/assign-positions lings layout)]
    (is (m/validate olympus/Positions positions))
    (is (= (set (map :slave/id lings)) (set (keys positions))))
    (is (= "ling-5" (olympus/position-for-cell positions 0 1 1)))))

(deftest without-hive-emacs-every-answer-is-nil
  (ext-stub/without-extensions
   (keys stub/builders)
   (fn []
     (testing "a cold registry answers nil, never throws"
       (is (nil? (olympus/calculate-layout 3)))
       (is (nil? (olympus/assign-positions [{:slave/id "a"}] {:rows 1 :cols 1})))
       (is (nil? (olympus/position-for-cell {} 0 0 nil)))))))

(ns hive-mcp.channel.row-transforms-trifecta-test
  "apply-transforms as a trifecta over generated hivemind rows: the chain may
   drop or rewrite rows but never grow them, and an empty read stays empty."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.channel.row-transforms :as rt]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- chain
  "Unary subject: the transform chain over ROWS with a fixed read ctx."
  [rows]
  (rt/apply-transforms rows {:reader "m05-reader" :project-id "p"}))

(def ^:private gen-row
  (gen/hash-map :a (gen/elements ["ling-1" "ling-2" "coordinator"])
                :e (gen/elements ["progress" "completed" "error"])
                :m (gen/fmap #(apply str %) (gen/vector gen/char-alpha 0 20))
                :ts gen/nat))

(deftrifecta transform-chain
  hive-mcp.channel.row-transforms-trifecta-test/chain
  {:golden-path "test/golden/channel/row-transforms-chain.edn"
   :cases {:empty []
           :one [{:a "ling-1" :e "progress" :m "hi" :ts 1}]
           :two [{:a "ling-1" :e "completed" :m "done" :ts 2}
                 {:a "coordinator" :e "error" :m "x" :ts 3}]}
   :gen (gen/vector gen-row 0 5)
   :pred (fn [out] (and (vector? out) (every? #(and (string? (:a %)) (string? (:e %))) out)))
   :num-tests 100
   :mutations [["grows" (fn [rows] (into (vec rows) rows))]
               ["drops-agent" (fn [rows] (mapv #(dissoc % :a) rows))]]})

(deftest keys-are-vectors-of-keywords-under-instrumentation
  (let [vs #{#'rt/core-transform-keys #'rt/transform-keys #'rt/apply-transforms}]
    (mi/instrument! {:filters [(mi/-filter-var vs)]})
    (try
      (is (every? keyword? (rt/core-transform-keys)))
      (is (every? keyword? (rt/transform-keys)))
      (is (= [] (rt/apply-transforms [] {})))
      (finally (mi/unstrument! {:filters [(mi/-filter-var vs)]})))))

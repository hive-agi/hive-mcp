(ns hive-mcp.agent.ring-buffer-trifecta-test
  "Facets for the stdout/stderr ring buffer, derived from its malli contracts.
   The :in schemas generate ATOMS (ring-buffer/RingBuffer), so the derived
   goldens are off and a hand-built golden over plain values pins behaviour."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.ring-buffer :as rb]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema ring-buffer-stats-trifecta
  hive-mcp.agent.ring-buffer/ring-buffer-stats
  {:in rb/RingBuffer
   :out rb/BufferStats
   :rel (fn [buf stats]
          (and (= (count (:lines @buf)) (:current-lines stats))
               (= (:capacity @buf) (:capacity stats))
               (= (:total-lines-seen @buf) (:total-lines-seen stats))))
   :num-tests 200})

(deftrifecta-from-schema ring-buffer-contents-trifecta
  hive-mcp.agent.ring-buffer/ring-buffer-contents
  {:in rb/RingBuffer
   :out [:vector :any]
   :rel (fn [buf out] (= out (:lines @buf)))
   :mutation false
   :num-tests 200})

(deftrifecta-from-schema ring-buffer-contents-since-trifecta
  hive-mcp.agent.ring-buffer/ring-buffer-contents-since
  {:in [:cat rb/RingBuffer [:int {:min 0 :max 1000000}]]
   :out [:vector rb/LineEntry]
   :rel (fn [[buf since] out]
          (let [{:keys [lines timestamps]} @buf
                expected (vec (for [[l t] (map vector lines timestamps) :when (> t since)] {:text l :ts t}))]
            (= expected out)))
   :mutation false
   :num-tests 200})

(deftrifecta-from-schema ring-buffer-append-trifecta
  hive-mcp.agent.ring-buffer/ring-buffer-append!
  {:in [:cat rb/RingBuffer :string]
   :out rb/BufferState
   :rel (fn [[buf line] st]
          (and (= line (peek (:lines st)))
               (<= (count (:lines st)) (:capacity st))
               (= (count (:lines st)) (count (:timestamps st)))))
   :mutation false
   :num-tests 200})

(deftest bounded-drop-oldest
  (let [b (rb/create-ring-buffer 3)]
    (doseq [l ["a" "b" "c" "d" "e"]] (rb/ring-buffer-append! b l))
    (is (= ["c" "d" "e"] (rb/ring-buffer-contents b)))
    (is (= ["d" "e"] (rb/ring-buffer-contents b {:last-n 2})))
    (is (= {:current-lines 3 :capacity 3 :total-lines-seen 5 :dropped 2} (rb/ring-buffer-stats b)))
    (is (= 5000 (:capacity (rb/ring-buffer-stats (rb/create-ring-buffer)))))))

(deftest contents-since-is-strictly-after
  (let [b (atom {:lines ["x" "y" "z"] :timestamps [10 20 30] :capacity 9 :total-lines-seen 3 :dropped 0})]
    (is (= [{:text "z" :ts 30}] (rb/ring-buffer-contents-since b 20)))
    (is (= [] (rb/ring-buffer-contents-since b 30)))
    (is (= 3 (count (rb/ring-buffer-contents-since b nil))))
    (is (= 3 (count (rb/ring-buffer-contents-since b 9))))))

(golden/deftest-golden-fn ring-buffer-golden
  "test/golden/agent/ring-buffer.edn"
  (fn []
    (let [b (rb/create-ring-buffer 2)]
      (doseq [l ["one" "two" "three"]] (rb/ring-buffer-append! b l))
      {:contents (rb/ring-buffer-contents b)
       :last-1   (rb/ring-buffer-contents b {:last-n 1})
       :stats    (rb/ring-buffer-stats b)
       :timestamps-aligned? (= (count (:lines @b)) (count (:timestamps @b)))})))

(deftest-contract-coverage ring-buffer-contract-coverage
  {:paths ["src/hive_mcp/agent/ring_buffer.clj"]})

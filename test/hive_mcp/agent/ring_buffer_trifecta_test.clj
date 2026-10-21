(ns hive-mcp.agent.ring-buffer-trifecta-test
  "Facets for the stdout/stderr ring buffer, derived from its malli contracts.
   The :in schemas generate ATOMS (ring-buffer/RingBuffer), so the derived
   goldens are off and a hand-built golden over plain values pins behaviour."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.headless]
            [hive-mcp.agent.ring-buffer :as rb]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [hive-test.mutation :refer [deftest-mutations]]
            [malli.core :as m]))

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
   :num-tests 200})

(deftrifecta-from-schema ring-buffer-contents-since-trifecta
  hive-mcp.agent.ring-buffer/ring-buffer-contents-since
  {:in [:cat rb/RingBuffer [:int {:min 0 :max 1000000}]]
   :out [:vector rb/LineEntry]
   :rel (fn [[buf since] out]
          (let [{:keys [lines timestamps]} @buf
                expected (vec (for [[l t] (map vector lines timestamps) :when (> t since)] {:text l :ts t}))]
            (= expected out)))
   :num-tests 200})

(deftrifecta-from-schema ring-buffer-append-trifecta
  hive-mcp.agent.ring-buffer/ring-buffer-append!
  {:in [:cat rb/RingBuffer :string]
   :out rb/BufferState
   :rel (fn [[buf line] st]
          (and (= line (peek (:lines st)))
               (<= (count (:lines st)) (:capacity st))
               (= (count (:lines st)) (count (:timestamps st)))))
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

;; Behavioural mutants. The schema-derived ones above only break the output
;; SHAPE; these keep the shape and break the semantics (bound, ordering,
;; strictness), which is where a ring buffer actually goes wrong.

(defn- fixed-buffer []
  (atom {:lines ["x" "y" "z"] :timestamps [10 20 30] :capacity 3
         :total-lines-seen 3 :dropped 0}))

(deftest-mutations ring-buffer-contents-mutations
  hive-mcp.agent.ring-buffer/ring-buffer-contents
  [["ignores last-n" (fn ([b] (:lines @b)) ([b _] (:lines @b)))]
   ["keeps the oldest n" (fn ([b] (:lines @b))
                           ([b {:keys [last-n]}]
                            (if last-n (subvec (:lines @b) 0 (min last-n (count (:lines @b)))) (:lines @b))))]
   ["drops the newest line" (fn ([b] (pop (:lines @b))) ([b _] (pop (:lines @b))))]]
  (fn []
    (let [b (fixed-buffer)]
      (is (= ["x" "y" "z"] (rb/ring-buffer-contents b)))
      (is (= ["y" "z"] (rb/ring-buffer-contents b {:last-n 2})))
      (is (= ["x" "y" "z"] (rb/ring-buffer-contents b {:last-n 9}))))))

(deftest-mutations ring-buffer-contents-since-mutations
  hive-mcp.agent.ring-buffer/ring-buffer-contents-since
  [["inclusive of since" (fn [b since]
                           (let [{:keys [lines timestamps]} @b]
                             (vec (for [[l t] (map vector lines timestamps)
                                        :when (or (nil? since) (>= t since))]
                                    {:text l :ts t}))))]
   ["ignores since" (fn [b _]
                      (let [{:keys [lines timestamps]} @b]
                        (mapv (fn [l t] {:text l :ts t}) lines timestamps)))]]
  (fn []
    (let [b (fixed-buffer)]
      (is (= [{:text "z" :ts 30}] (rb/ring-buffer-contents-since b 20)))
      (is (= [] (rb/ring-buffer-contents-since b 30)))
      (is (= 3 (count (rb/ring-buffer-contents-since b nil)))))))

(deftest-mutations ring-buffer-append-mutations
  hive-mcp.agent.ring-buffer/ring-buffer-append!
  [["never drops" (fn [b line]
                    (swap! b (fn [st] (-> st
                                          (update :lines conj line)
                                          (update :timestamps conj 0)
                                          (update :total-lines-seen inc)))))]
   ["drops the newest instead of the oldest"
    (fn [b line]
      (swap! b (fn [{:keys [lines capacity] :as st}]
                 (if (< (count lines) capacity)
                   (-> st (update :lines conj line) (update :timestamps conj 0)
                       (update :total-lines-seen inc))
                   (-> st (update :total-lines-seen inc) (update :dropped inc))))))]
   ["does not count dropped lines"
    (fn [b line]
      (swap! b (fn [{:keys [lines timestamps capacity] :as st}]
                 (let [ls (conj lines line) ts (conj timestamps 0)
                       over (max 0 (- (count ls) capacity))]
                   (assoc st :lines (subvec ls over) :timestamps (subvec ts over)
                          :total-lines-seen (inc (:total-lines-seen st)))))))]]
  (fn []
    (let [b (rb/create-ring-buffer 2)]
      (doseq [l ["one" "two" "three"]] (rb/ring-buffer-append! b l))
      (is (= ["two" "three"] (:lines @b)))
      (is (= 2 (count (:timestamps @b))))
      (is (= {:current-lines 2 :capacity 2 :total-lines-seen 3 :dropped 1}
             (rb/ring-buffer-stats b))))))

;; headless.clj re-exports the ring-buffer fns as Var aliases and carries
;; their contracts, so the facade sits under the same gate.
(deftest-contract-coverage ring-buffer-contract-coverage
  {:paths ["src/hive_mcp/agent/ring_buffer.clj"
           "src/hive_mcp/agent/headless.clj"]})

(deftest headless-facade-aliases-carry-the-ring-buffer-contracts
  (let [fns (get (m/function-schemas) 'hive-mcp.agent.headless)]
    (doseq [f '[create-ring-buffer ring-buffer-append! ring-buffer-contents
                ring-buffer-contents-since ring-buffer-stats]]
      (testing (str f)
        (is (contains? fns f) "facade alias has no m/=> contract")))
    (testing "the facade delegates to the owner"
      (let [b (hive-mcp.agent.headless/create-ring-buffer 2)]
        (hive-mcp.agent.headless/ring-buffer-append! b "a")
        (is (= ["a"] (hive-mcp.agent.headless/ring-buffer-contents b)))
        (is (= 1 (:current-lines (hive-mcp.agent.headless/ring-buffer-stats b))))
        (is (= ["a"] (mapv :text (hive-mcp.agent.headless/ring-buffer-contents-since b nil))))))))

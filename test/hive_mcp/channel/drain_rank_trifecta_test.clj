(ns hive-mcp.channel.drain-rank-trifecta-test
  "Schema trifectas for the two-lane drain ranker: key normalisation, lane
   assignment, scoring and batch selection, each against its own contract."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.channel.drain-rank :as rank]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private EntryType [:enum :axiom :principle :decision :note "axiom" "note" "convention"])

(def ^:private Entry
  "A buffered piggyback entry, sized for generation."
  [:map
   [:id [:enum "e1" "e2" "e3" "e4" "e5"]]
   [:T EntryType]
   [:C {:optional true} [:string {:gen/max 40}]]
   [:tags {:optional true} [:vector {:gen/max 3} [:enum "carto" "drain" "scope:project:x" "memory"]]]
   [:A {:optional true} [:int {:min 0 :max 9}]]
   [:F {:optional true} [:int {:min -3 :max 3}]]])

(def ^:private TokenSet [:set {:gen/max 3} [:enum "carto" "drain" "memory" "zzz"]])

(deftrifecta-from-schema keyword-normalisation
  hive-mcp.channel.drain-rank/kw
  {:in [:or :keyword :symbol [:enum "note" ":axiom" "  decision " "" "   " nil]]
   :out [:maybe :keyword]
   :rel (fn [x out]
          (if (or (nil? x) (and (string? x) (empty? (.trim ^String x))))
            (nil? out)
            (and (keyword? out) (nil? (namespace out)))))
   :idempotent? false
   :golden-path "test/golden/channel/drain-rank-kw.edn"
   :num-tests 200})

(deftrifecta-from-schema lane-assignment
  hive-mcp.channel.drain-rank/lane
  {:in [:cat Entry [:maybe [:set {:gen/max 2} [:enum "e1" "e2"]]]]
   :out [:enum :floor :pool]
   :rel (fn [[entry pins] out]
          (= out (if (or (= :axiom (rank/kw (:T entry)))
                         (contains? (set pins) (:id entry)))
                   :floor :pool)))
   :golden-path "test/golden/channel/drain-rank-lane.edn"
   :num-tests 200})

(deftrifecta-from-schema entry-score
  hive-mcp.channel.drain-rank/score
  {:in [:cat Entry TokenSet [:int {:min 0 :max 10}]]
   :out :double
   :rel (fn [[entry tokens offers] out]
          ;; more prior offers never raise a score, and decay is capped
          (let [fresh (rank/score entry tokens 0)]
            (and (<= out fresh)
                 (>= out (- fresh rank/max-offer-decay 1e-9))
                 (or (zero? offers) (<= out (rank/score entry tokens (dec offers)))))))
   :golden-path "test/golden/channel/drain-rank-score.edn"
   :num-tests 200})

(deftrifecta-from-schema batch-selection
  hive-mcp.channel.drain-rank/select-batch
  {:in [:cat [:vector {:gen/max 6} Entry]
        [:map
         [:tokens {:optional true} TokenSet]
         [:char-budget {:optional true} [:int {:min 1 :max 400}]]
         [:floor-cap {:optional true} [:int {:min 0 :max 3}]]]]
   :out [:map [:ordered [:vector :map]] [:batch [:vector :map]] [:taken [:int {:min 0}]]]
   :rel (fn [[pending _] {:keys [ordered batch taken]}]
          (and (= (frequencies pending) (frequencies ordered))
               (= batch (subvec ordered 0 taken))
               (= (empty? pending) (zero? taken))))
   :golden-path "test/golden/channel/drain-rank-select-batch.edn"
   :num-tests 200})

(deftest score-relation-reaches-decay
  (testing "decay arm: a repeatedly offered note sinks, by at most the cap"
    (let [e {:id "e1" :T :note :tags ["carto"]}]
      (is (< (rank/score e #{"carto"} 3) (rank/score e #{"carto"} 0)))
      (is (= (rank/score e #{} 100) (- (rank/score e #{} 0) rank/max-offer-decay))))))

(deftest select-batch-accepts-token-sets-under-instrumentation
  (mi/instrument! {:filters [(mi/-filter-var #{#'rank/select-batch #'rank/score})]})
  (try
    (is (= 1 (:taken (rank/select-batch [{:id "a" :T :note}] {:tokens #{"x"} :pins #{"a"}}))))
    (is (double? (rank/score {:id "a" :T :note} #{"x"} 0)))
    (finally (mi/unstrument! {:filters [(mi/-filter-var #{#'rank/select-batch #'rank/score})]}))))

(deftest tokens-schema-arms
  (is (m/validate rank/Tokens #{"a"}))
  (is (m/validate rank/Tokens ["a"]))
  (is (m/validate rank/Tokens nil))
  (is (not (m/validate rank/Tokens #{:a}))))

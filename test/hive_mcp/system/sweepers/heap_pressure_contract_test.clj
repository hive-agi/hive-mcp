(ns hive-mcp.system.sweepers.heap-pressure-contract-test
  "Contract facets for hive-mcp.system.sweepers.heap-pressure: the pure GC
   policy gets schema-driven trifectas; heap-snapshot is checked against its
   own schema. No collection is provoked."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.system.sweepers.heap-pressure :as hp]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private gib (* 1024 1024 1024))

(def ^:private HeapIn
  [:map
   [:committed [:maybe [:enum 0 gib (* 3 gib) (* 5 gib) (* 9 gib) (* 12 gib)]]]
   [:used [:maybe [:enum 0 gib (* 3 gib) (* 4 gib) (* 10 gib)]]]])

(deftrifecta-from-schema reclaimable-is-the-empty-committed-heap
  #'hp/reclaimable
  {:in HeapIn
   :out [:int {:min 0}]
   :rel (fn [{:keys [committed used]} out] (= out (max 0 (- (or committed 0) (or used 0)))))
   :golden-path "test/golden/hive-mcp/system/heap-reclaimable.edn"
   :num-tests 100})

(deftrifecta-from-schema worth-collecting-needs-both-thresholds
  #'hp/worth-collecting?
  {:in HeapIn
   :out :boolean
   :rel (fn [{:keys [committed used]} out]
          (let [c (or committed 0) u (or used 0)]
            (= out (and (>= (- c u) hp/min-waste-bytes)
                        (> c (* hp/min-waste-ratio (max u 1)))))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :classify-floor 5
   :golden-path "test/golden/hive-mcp/system/heap-worth-collecting.edn"
   :num-tests 200})

(deftest heap-snapshot-reads-the-runtime
  (let [s (hp/heap-snapshot)]
    (is (m/validate hp/HeapSnap s))
    (is (<= (:used s) (:committed s)))))

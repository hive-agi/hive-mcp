(ns hive-mcp.tools.kg.synthetics-contract-test
  "Contract facets for hive-mcp.tools.kg.synthetics: the pure partition and
   cursor rotation, and the store-reading enumerations and reports against a
   fresh in-memory DataScript KG with no memory store registered."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-mcp.tools.core :as core]
            [hive-mcp.tools.kg.schema :as ks]
            [hive-mcp.tools.kg.synthetics :as synthetics]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each fixtures/datascript-fixture)

(def ^:private Ids
  [:vector {:max 5} ks/SynthId])

(deftrifecta-from-schema partition-sources-splits-on-the-record-set
  #'synthetics/partition-sources
  {:in [:cat Ids [:map {:closed true}
                  [:visible? :boolean]
                  [:records [:set {:max 3} ks/SynthId]]]]
   :out ks/Partition
   :rel (fn [[sources {:keys [visible? records]}] {:keys [registered orphans]}]
          (if visible?
            (and (= registered (filterv records sources))
                 (= orphans (filterv (complement records) sources)))
            (and (= registered sources) (= [] orphans))))
   :classify (fn [[_ v] _] (if (:visible? v) :visible :blind))
   :classify-domain #{:visible :blind}
   :golden-path "test/golden/hive-mcp/tools/kg/partition-sources.edn"
   :num-tests 200})

(deftrifecta-from-schema rotate-after-is-a-rotation
  #'synthetics/rotate-after
  {:in [:cat [:vector {:max 5} ks/SynthId] [:maybe ks/SynthId]]
   :out [:vector ks/SynthId]
   :rel (fn [[ids after] out]
          (and (= (frequencies ids) (frequencies out))
               (or (nil? after)
                   (= out (into (filterv #(pos? (compare % after)) ids)
                                (remove #(pos? (compare % after))) ids)))))
   :golden-path "test/golden/hive-mcp/tools/kg/rotate-after.edn"
   :num-tests 200})

(deftrifecta-from-schema record-visibility-is-blind-without-records
  #'synthetics/record-visibility
  {:in [:cat Ids]
   :out ks/RecordVisibility
   :rel (fn [[sources] out]
          ;; the empty store holds no :kg-synthetic/id record
          (= (empty? sources) (:visible? out)))
   :golden-path "test/golden/hive-mcp/tools/kg/record-visibility.edn"
   :num-tests 60})

(deftrifecta-from-schema reap-orphans-on-an-empty-graph-reaps-nothing
  #'synthetics/reap-orphans!
  {:in [:cat ks/ReapOpts]
   :out ks/ReapResult
   :rel (fn [_ out] (and (zero? (:reaped out)) (zero? (:edges-removed out))))
   :num-tests 40})

(deftrifecta-from-schema cleanup-without-a-store-refuses-or-scans-nothing
  #'synthetics/cleanup-synthetics!
  {:in [:cat ks/CleanupOpts]
   :out ks/CleanupResult
   :rel (fn [_ out] (zero? (:scanned out)))
   :num-tests 40})

(defn- action-ok?
  "Oracle for parse-action: nil and values that are neither string nor
   keyword read as :delete."
  [action]
  (cond
    (keyword? action) (contains? #{:delete :demote} action)
    (string? action)  (contains? #{"delete" "demote"} action)
    :else             true))

(deftrifecta-from-schema cleanup-handler-validates-before-scanning
  #'synthetics/handle-kg-cleanup-synthetics
  {:in [:cat ks/CleanupParams]
   :out core/McpResponse
   :rel (fn [[{:keys [action threshold limit min_live_members]}] out]
          (let [bad? (boolean
                      (or (not (action-ok? action))
                          (and threshold (or (not (number? threshold))
                                             (< threshold 0.0) (> threshold 1.0)))
                          (and limit (or (not (integer? limit)) (neg? limit)))
                          (and min_live_members (or (not (integer? min_live_members))
                                                    (neg? min_live_members)))))]
            (= bad? (boolean (:isError out)))))
   :classify (fn [_ out] (if (:isError out) :refused :ran))
   :classify-domain #{:refused :ran}
   :num-tests 100})

(deftest store-enumerations-on-an-empty-graph
  (is (= [] (synthetics/list-synthetic-source-nodes)))
  (is (= #{} (synthetics/synthetic-record-ids)))
  (is (= 0 (synthetics/count-projection-edges)))
  (is (m/validate ks/RetractResult
                  (synthetics/retract-node-bounded! "synth-a" {:budget 5})))
  (is (= {:removed 0 :remaining 0 :complete? true}
         (synthetics/retract-node-bounded! "synth-a" {}))))

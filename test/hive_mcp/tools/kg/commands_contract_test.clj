(ns hive-mcp.tools.kg.commands-contract-test
  "Contract facets for hive-mcp.tools.kg.commands: the bulk-edge envelope
   (pure), the bulk write through an injected transact port, the flush
   decorator, and the grounding handlers against an empty KG."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-mcp.tools.core :as core]
            [hive-mcp.tools.kg.commands :as cmd]
            [hive-mcp.tools.kg.schema :as ks]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each fixtures/global-datascript-fixture)

(def ^:private Classified
  [:vector {:max 5} ks/ClassifiedOp])

(def ^:private Written
  [:or
   [:map {:closed true} [:ids [:vector {:max 5} [:enum "e1" "e2" "e3" "e4" "e5"]]]]
   [:map {:closed true} [:error [:enum "tx failed" "down"]]]])

(deftrifecta-from-schema batch-envelope-pairs-ops-with-written-ids
  #'cmd/batch-envelope
  {:in [:cat Classified Written]
   :out ks/BatchEnvelope
   :rel (fn [[classified {:keys [ids error]}] {:keys [results summary write_error]}]
          (let [n-ok (count (filter #(= :ok (first %)) classified))
                ok   (min n-ok (count ids))]
            (and (= (count classified) (count results) (:total summary))
                 (= ok (:success summary))
                 (= (- (count classified) ok) (:failed summary))
                 (= error write_error)
                 (= (take ok ids)
                    (keep #(get-in % [:result :edge-id]) results)))))
   :classify (fn [[_ w] _] (if (:error w) :write-failed :written))
   :classify-domain #{:write-failed :written}
   :golden-path "test/golden/hive-mcp/tools/kg/batch-envelope-contract.edn"
   :num-tests 200})

(def ^:private failing-transact
  (fn [_] (throw (ex-info "kg down" {}))))

;; No golden facet: the input carries the transact fn, which has no EDN form.
(deftrifecta-from-schema add-edges-with-a-failing-port-fails-every-valid-op
  #'cmd/add-edges-with
  {:in [:cat [:= failing-transact] ks/AddEdgesParams]
   :out core/McpResponse
   :rel (fn [[_ {:keys [operations]}] out]
          (if (empty? operations)
            (true? (:isError out))
            (let [body (json/read-str (:text out) :key-fn keyword)]
              (and (= (count operations) (get-in body [:summary :total]))
                   (zero? (get-in body [:summary :success]))))))
   :num-tests 60})

(deftrifecta-from-schema add-edges-writes-every-valid-op
  #'cmd/handle-kg-add-edges
  {:in [:cat ks/AddEdgesParams]
   :out core/McpResponse
   :rel (fn [[{:keys [operations]}] out]
          (or (and (empty? operations) (true? (:isError out)))
              (let [body (json/read-str (:text out) :key-fn keyword)]
                (= (count operations) (get-in body [:summary :total])))))
   :num-tests 40})

(deftrifecta-from-schema reground-refuses-or-reports
  #'cmd/handle-kg-reground
  {:in [:cat ks/RegroundParams]
   :out ks/KgResponse
   :rel (fn [[{:keys [entry_id]}] out]
          (if (and (string? entry_id) (seq entry_id) (not= \$ (first entry_id)))
            (m/validate core/McpResponse out)
            (m/validate ks/NodeIdError out)))
   :num-tests 40})

(deftrifecta-from-schema backfill-without-a-memory-store-is-an-error
  #'cmd/handle-kg-backfill-grounding
  {:in [:cat ks/BackfillParams]
   :out core/McpResponse
   :rel (fn [_ out] (m/validate core/McpResponse out))
   :num-tests 30})

(deftest with-kg-flush-returns-the-handler-result
  (let [h (cmd/with-kg-flush (fn [p] {:type "text" :text (pr-str p)}))]
    (is (= {:type "text" :text "{:a 1}"} (h {:a 1})))))

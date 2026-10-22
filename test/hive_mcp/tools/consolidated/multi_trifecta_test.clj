(ns hive-mcp.tools.consolidated.multi-trifecta-test
  "Schema-derived trifectas over the pure helpers of the multi tool:
   malformed-operations, malformed-operations-message and batch-route."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.consolidated.multi :as mu]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-mcp.test.stub.isolated-registry :as iso]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(clojure.test/use-fixtures :each iso/with-fresh-registries)

(def Operations
  "A batch whose entries mix op maps with the non-map shapes callers send."
  [:vector {:max 5} [:or [:map [:tool [:enum "memory" "kg"]]] :string :int :nil [:vector :int]]])

(deftrifecta-from-schema malformed-operations-contract
  hive-mcp.tools.consolidated.multi/malformed-operations
  {:in          Operations
   :out         [:vector mu/MalformedOperation]
   :rel         (fn [ops out]
                  (and (= (count out) (count (remove map? ops)))
                       (every? (fn [{:keys [index value]}] (= value (nth ops index))) out)))
   :golden-path "test/golden/tools/consolidated/multi-malformed-operations.edn"
   :num-tests   200})

(deftrifecta-from-schema malformed-operations-message-contract
  hive-mcp.tools.consolidated.multi/malformed-operations-message
  {:in          [:vector {:min 1 :max 3} mu/MalformedOperation]
   :out         [:string {:min 1}]
   :rel         (fn [malformed out]
                  (every? #(str/includes? out (str "index " (:index %) " = ")) malformed))
   :golden-path "test/golden/tools/consolidated/multi-malformed-message.edn"
   :num-tests   100})

(def RouteParams
  "Every route params shape batch-route distinguishes, one arm per branch, so
   the generator reaches each branch rather than mostly the non-batch one."
  [:or
   [:map [:command {:optional true} [:enum "add" "batch-add"]]]
   [:map [:tool [:maybe [:enum "" " "]]]
    [:operations [:vector {:max 3} [:map [:id :string]]]]]
   [:map [:tool [:enum "memory" "kanban"]] [:command [:enum "batch-add" "batch-get"]]
    [:operations [:vector {:max 3} [:map [:id :string]]]]]
   [:map [:tool [:enum "memory" "kanban"]] [:command {:optional true} [:maybe [:enum "add" ""]]]
    [:operations [:vector {:max 3} [:map [:tool {:optional true} [:enum "kg" ""]]]]]]])

(deftrifecta-from-schema batch-route-contract
  hive-mcp.tools.consolidated.multi/batch-route
  {:in          RouteParams
   :out         [:maybe :map]
   :rel         (fn [{:keys [tool command operations] :as in} out]
                  (cond
                    (nil? operations) (nil? out)
                    (str/blank? (str tool)) (= in out)
                    (str/starts-with? (str command) "batch-") (nil? out)
                    :else (and (not (contains? out :tool))
                               (= (count operations) (count (:operations out)))
                               (every? #(not (str/blank? (str (:tool %)))) (:operations out)))))
   :classify    (fn [{:keys [tool operations]} out]
                  (cond (nil? operations) :not-batch
                        (str/blank? (str tool)) :bare
                        (nil? out) :own-batch
                        :else :inherited))
   :classify-domain #{:not-batch :bare :own-batch :inherited}
   :golden-path "test/golden/tools/consolidated/multi-batch-route-schema.edn"
   :num-tests   300})

(deftest handle-multi-unknown-tool-names-the-roster
  (let [r (mu/handle-multi {:tool "m27-no-such-tool" :command "x"})]
    (is (true? (:isError r)))
    (is (str/includes? (:text r) "Unknown tool: m27-no-such-tool"))
    (is (vector? (mu/tool-names)))
    (is (nil? (mu/get-tool-handler nil)))))

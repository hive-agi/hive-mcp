(ns hive-mcp.tools.consolidated.memory-trifecta-test
  "Schema-derived trifectas over the pure bare-batch refusal of the memory
   tool, and the handler built from it with an injected delegate port."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.consolidated.memory :as mem]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def BareBatchParams
  "Params that reach every arm of bare-batch-refusal: operations as a vector
   or a (possibly blank) string, files as paths or a dir, or nothing."
  [:map
   [:operations {:optional true} [:or [:vector {:max 2} :map] [:enum "" " " "[{}]"]]]
   [:paths {:optional true} [:or [:vector {:max 2} [:enum "a.md" "b.org"]] [:enum "" "a.md"]]]
   [:dir {:optional true} [:maybe [:enum "" "  " "/notes"]]]])

(defn- present? [v]
  (if (string? v) (not (str/blank? v)) (seq v)))

(defn expected-refusal?
  "Oracle: a refusal is due exactly when operations are present, or when no
   file (paths or a non-blank dir) is named."
  [{:keys [operations paths dir]}]
  (boolean (or (present? operations)
               (not (or (present? paths) (and (string? dir) (not (str/blank? dir))))))))

(deftrifecta-from-schema bare-batch-refusal-contract
  hive-mcp.tools.consolidated.memory/bare-batch-refusal
  {:in          BareBatchParams
   :out         [:maybe [:string {:min 1}]]
   :rel         (fn [in out] (= (expected-refusal? in) (some? out)))
   :classify    (fn [in out] (cond (nil? out) :delegated
                                   (present? (:operations in)) :ops-refused
                                   :else :empty-refused))
   :classify-domain #{:delegated :ops-refused :empty-refused}
   :golden-path "test/golden/tools/consolidated/memory-bare-batch-refusal.edn"
   :num-tests   300})

(defn handler-answer
  "Unary adapter: run the bare-batch handler over PARAMS with a delegate that
   echoes the command it was handed."
  [params]
  ((mem/make-bare-batch-handler (fn [p] {:type "text" :text (:command p)})) params))

(deftrifecta-from-schema make-bare-batch-handler-contract
  hive-mcp.tools.consolidated.memory-trifecta-test/handler-answer
  {:in          BareBatchParams
   :out         [:map [:type [:= "text"]] [:text :string] [:isError {:optional true} [:= true]]]
   :rel         (fn [in out] (if (expected-refusal? in)
                               (true? (:isError out))
                               (= "ingest batch" (:text out))))
   :golden-path "test/golden/tools/consolidated/memory-bare-batch-handler.edn"
   :num-tests   200})

(deftest tool-defs-advertise-one-object-schema
  (let [[td :as tds] (mem/tool-defs)]
    (is (= 1 (count tds)))
    (is (m/validate mem/AdvertisedToolDef td))
    (is (= "memory" (:name td)))
    (is (= (sort (get-in td [:inputSchema :properties "relation" :enum]))
           (get-in td [:inputSchema :properties "relation" :enum]))
        "the relation enum is sorted so the cached tools span stays byte-stable")))

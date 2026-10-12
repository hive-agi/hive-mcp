(ns hive-mcp.tools.consolidated.multi-batch-route-test
  "Contract for `batch-route`: an `operations` array is always a batch, even
   when a top-level :tool / :command is also set (card 20260901172001-0c2d3bcc).
   Before the fix that shape fell through to single dispatch and ran nothing."
  (:require [clojure.test :refer [is]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.tools.consolidated.multi :as multi]
            [hive-test.trifecta :refer [deftrifecta]]))

(def ^:private edge-op
  {:id "e1" :tool "kg" :command "edge" :from "a" :to "b" :relation "relates"})

(def ^:private gen-params
  (gen/let [tool    (gen/elements [nil "" "kg" "memory"])
            command (gen/elements [nil "edge" "add" "batch-edge"])
            ops     (gen/elements [nil [] [edge-op] [{:id "x" :from "a"}]])]
    (cond-> {}
      (some? tool)    (assoc :tool tool)
      (some? command) (assoc :command command)
      (some? ops)     (assoc :operations ops))))

(deftrifecta batch-route-contract
  hive-mcp.tools.consolidated.multi/batch-route
  {:golden-path "test/golden/tools/consolidated/multi-batch-route.edn"
   :cases       {:no-ops             {:tool "kg" :command "edge"}
                 :ops-no-tool        {:operations [edge-op]}
                 :ops-with-tool      {:tool "kg" :command "edge" :operations [edge-op]}
                 :ops-inherit-tool   {:tool "kg" :command "edge"
                                      :operations [{:id "x" :from "a" :to "b"}]}
                 :tool-own-batch     {:tool "kg" :command "batch-edge"
                                      :operations [edge-op]}}
   :gen         gen-params
   :pred        (fn [r] (or (nil? r) (and (map? r) (contains? r :operations))))
   :num-tests   200
   :mutations   [["old routing: tool set means never a batch"
                  (fn [{:keys [tool operations] :as p}]
                    (when (and (some? operations) (or (nil? tool) (= "" tool))) p))]
                 ["no inheritance: ops keep missing tool"
                  (fn [{:keys [operations] :as p}]
                    (when (some? operations) (dissoc p :tool :command)))]]
   :assert      (fn []
                  (let [r (multi/batch-route {:tool "kg" :command "edge"
                                              :operations [edge-op]})]
                    (is (some? r) "operations with a top-level tool is a batch")
                    (is (not (contains? r :tool)) "top-level tool is consumed"))
                  (is (= "kg" (-> (multi/batch-route
                                   {:tool "kg" :command "edge"
                                    :operations [{:id "x" :from "a" :to "b"}]})
                                  :operations first :tool))
                      "an op without :tool inherits the top-level one")
                  (is (nil? (multi/batch-route {:tool "kg" :command "batch-edge"
                                                :operations [edge-op]}))
                      "a tool's own batch-* command keeps single dispatch")
                  (is (nil? (multi/batch-route {:tool "kg" :command "edge"}))
                      "no operations, no batch"))})

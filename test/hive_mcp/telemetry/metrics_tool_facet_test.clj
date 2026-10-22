(ns hive-mcp.telemetry.metrics-tool-facet-test
  "Facets for hive-mcp.telemetry.metrics-tool: the pure MCP-params ->
   hotspots-opts translation (property + golden)."
  (:require [hive-mcp.telemetry.metrics-tool :as tool]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private by-values #{"total" "count" "mean" "p99" "max" "errors"})

(def ^:private Params
  "MCP params as they arrive: keyword or string keys, by as keyword or string
   (known or not), limit as int, numeric string or junk, area present or blank."
  [:map
   [:by {:optional true} [:enum "total" "count" "mean" "p99" "max" "errors" "bogus" :p99 :mean]]
   [:limit {:optional true} [:or [:int {:min 0 :max 100}] [:enum "5" "40" "x"]]]
   ["area" {:optional true} [:enum "" "mcp/tool" "kg/edge"]]])

(defn- expected-limit [l]
  (cond (integer? l) l
        (= "5" l) 5
        (= "40" l) 40
        :else 20))

(deftrifecta-from-schema hotspots-opts hive-mcp.telemetry.metrics-tool/hotspots-opts
  {:in  [:maybe Params]
   :out [:map [:by :keyword] [:limit [:maybe :int]] [:area {:optional true} :string]]
   :rel (fn [in out]
          (let [by (some-> (:by in) name)
                area (get in "area")]
            (and (= (:by out) (keyword (if (by-values by) by "total")))
                 (= (:limit out) (expected-limit (:limit in)))
                 (= (:area out) (not-empty area)))))
   :classify (fn [in _] (if (by-values (some-> (:by in) name)) :known-by :default-by))
   :classify-domain #{:known-by :default-by}
   :num-tests 200})

(golden/deftest-golden hotspots-opts-golden
  "test/golden/hive-mcp/telemetry/metrics-tool-hotspots-opts.edn"
  (into (sorted-map)
        (for [[label p] {"nil"          nil
                         "empty"        {}
                         "string-keys"  {"by" "p99" "limit" "7" "area" "mcp/tool"}
                         "keyword-by"   {:by :errors :limit 3}
                         "unknown-by"   {:by "bogus" :limit "junk"}
                         "blank-area"   {:area ""}}]
          [label {:in p :out (tool/hotspots-opts p)}])))

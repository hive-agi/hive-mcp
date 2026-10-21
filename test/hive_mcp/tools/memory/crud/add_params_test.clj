(ns hive-mcp.tools.memory.crud.add-params-test
  "memory add refuses `embedder` with an MCP error instead of dropping it."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.core :as core]
            [hive-mcp.tools.memory.crud :as crud]
            [hive-mcp.tools.memory.crud.add-params :as ap]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private AddArgs
  [:map
   [:type {:optional true} [:enum "note" "decision"]]
   [:content {:optional true} [:enum "x" "a longer body"]]
   [:tags {:optional true} [:vector {:max 2} [:enum "a" "b"]]]
   [:embedder {:optional true} [:maybe [:enum "noop" "ollama" ""]]]])

(deftrifecta-from-schema refuse-unsupported-names-embedder
  #'ap/refuse-unsupported
  {:in [:cat [:maybe AddArgs]]
   :out [:maybe core/McpResponse]
   :rel (fn [[args] out]
          (if (contains? args :embedder)
            (and (true? (:isError out)) (str/includes? (:text out) "embedder"))
            (nil? out)))
   :classify (fn [[args] _] (contains? args :embedder))
   :classify-domain #{true false}
   :num-tests 100})

(deftest memory-add-with-embedder-is-an-mcp-error
  (let [out (crud/handle-add {:type "note" :content "x" :embedder "noop"})]
    (is (m/validate core/McpResponse out))
    (is (true? (:isError out)))
    (is (str/includes? (:text out) "embedder"))
    (is (str/includes? (:text out) "Nothing was written"))))

(deftest guard-passes-other-args-through
  (let [seen (atom nil)
        h (ap/guard (fn [args] (reset! seen args) {:type "text" :text "ok"}))]
    (is (= {:type "text" :text "ok"} (h {:type "note" :content "x"})))
    (is (= {:type "note" :content "x"} @seen))
    (is (true? (:isError (h {:type "note" :embedder "noop"}))))))

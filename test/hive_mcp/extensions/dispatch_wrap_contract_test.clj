(ns hive-mcp.extensions.dispatch-wrap-contract-test
  "Contract facets for the addon dispatch wrap: the MCP refusal it answers
   during an unmount, and the wrap with no drain gate on the classpath."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.extensions.dispatch-wrap :as dw]
            [hive-mcp.extensions.registry :as ext]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema refused-answer-is-an-mcp-error
  #'dw/refused-answer
  {:in [:cat [:maybe [:string {:max 40}]] [:enum "hive.carto" "probe.a" :probe/b]]
   :out [:map {:closed true} [:type [:= "text"]] [:text [:maybe :string]] [:isError [:= true]]]
   :rel (fn [[message _] out] (= message (:text out)))
   :golden-path "test/golden/hive-mcp/extensions/refused-answer.edn"
   :num-tests 100})

(deftest without-a-drain-gate-only-the-inner-wrap-applies
  (let [h (fn [_] :ran)
        inner (ext/get-extension :addon/wrap-handler)]
    (binding [dw/*dispatcher* (constantly nil)]
      (let [wrapped (dw/wrap-addon-handler "m11.probe" h)]
        (if inner
          (is (ifn? wrapped))
          (is (identical? h wrapped)))
        (is (= :ran (wrapped {})))))))

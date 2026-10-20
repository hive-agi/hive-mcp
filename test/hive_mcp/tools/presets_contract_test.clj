(ns hive-mcp.tools.presets-contract-test
  "Contract facets for the preset handlers. No embedding provider is
   configured in the unit JVM, so the Chroma-free branches answer: writes
   refuse, the lazy header is built locally, list/status degrade. Each
   trifecta points at the contracted handler var; a golden pins the rest."
  (:require [clojure.string :as str]
            [hive-mcp.tools.presets :as presets]
            [hive-mcp.tools.result-bridge :as rb]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema preset-add-refuses-without-chroma
  #'presets/handle-preset-add
  {:in [:cat [:map [:name [:enum "tdd" "reviewer" "x"]]
              [:content {:optional true} [:enum "body"]]]]
   :out rb/McpResponse
   :rel (fn [_ out] (and (true? (:isError out))
                         (= "Chroma not configured" (:text out))))
   :golden-path "test/golden/hive-mcp/tools/presets-add-gated.edn"
   :num-tests 30})

(deftrifecta-from-schema preset-header-lazy-names-every-preset
  #'presets/handle-preset-header
  {:in [:cat [:map [:presets [:vector {:min 1 :max 3} [:enum "tdd" "clarity" "reviewer"]]]]]
   :out rb/McpResponse
   :rel (fn [[{ps :presets}] out]
          (and (not (:isError out))
               (every? #(str/includes? (:text out) %) ps)
               (str/includes? (:text out) "\"mode\":\"lazy\"")))
   :golden-path "test/golden/hive-mcp/tools/presets-header-lazy.edn"
   :num-tests 40})

(golden/deftest-golden preset-envelopes-golden
  "test/golden/hive-mcp/tools/presets-envelopes.edn"
  {:search    (presets/handle-preset-search {:query "q"})
   :migrate   (presets/handle-preset-migrate {:directory "/d"})
   :delete    (presets/handle-preset-delete {:name "n"})
   :list      (presets/handle-preset-list {})
   :list-slim (presets/handle-preset-list-slim {})
   :status    (presets/handle-preset-status {})
   :get       (presets/handle-preset-get {:name "zz-no-such-preset"})
   :core      (presets/handle-preset-core {:name "zz-no-such-preset"})
   :header-0  (presets/handle-preset-header {})})

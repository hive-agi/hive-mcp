(ns hive-mcp.agent.error-summary-trifecta-test
  "Facets for error-summary, derived from its contracts: `truncate`
   keeps a short string and cuts a long one to n chars plus an ellipsis,
   and `summarize-error` always answers an ErrorSummary."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [clojure.string :as str]
            [hive-mcp.agent.error-summary :as es]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema truncate-trifecta
  hive-mcp.agent.error-summary/truncate
  {:in [:cat [:int {:min 0 :max 20}] [:string {:max 30}]]
   :out :string
   :rel (fn [[n s] out]
          (if (<= (count s) n)
            (= out s)
            (= out (str (subs s 0 n) "…"))))
   :num-tests 200})

(deftrifecta-from-schema summarize-string-error-trifecta
  hive-mcp.agent.error-summary/summarize-error
  {:in [:string {:max 40}]
   :out es/ErrorSummary
   :rel (fn [s out] (and (= "java.lang.String" (:class out)) (= [] (:frames out))
                         (str/starts-with? s (str/replace (:message out) #"…$" ""))))
   :num-tests 100})

(golden/deftest-golden-fn error-summary-golden
  "test/golden/agent/error-summary.edn"
  (fn []
    {:truncate [(es/truncate 5 "abcdefghij") (es/truncate 5 "abc") (es/truncate 0 "abc") (es/truncate 3 nil)]
     :string (es/summarize-error "boom")
     :map (es/summarize-error {:message "m" :class "C"})
     :line (es/summary->line {:class "X" :message "m" :frames []})}))

(deftest-contract-coverage error-summary-contract-coverage
  {:paths ["src/hive_mcp/agent/error_summary.clj"]})

(ns hive-mcp.tools.memory.gaps-contract-test
  "Contract facets for hive-mcp.tools.memory.gaps."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.memory.gaps :as gaps]
            [hive-mcp.tools.memory.schema :as ms]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Text
  [:maybe [:enum "What is this thing for?" "TODO: wire the adapter" "FIXME later"
           "unclear: who owns it" "missing: tests" "assuming a single writer"
           "plain prose." "" "TODO: a. What owns this module?"]])

(deftrifecta-from-schema knowledge-gaps-are-capped-and-prefixed
  #'gaps/extract-knowledge-gaps
  {:in [:cat Text]
   :out [:sequential :string]
   :rel (fn [[c] out]
          (and (<= (count out) 10)
               (every? #(<= (count %) 80) out)
               (if (str/blank? (str c)) (empty? out) true)
               (every? #(some (fn [p] (str/starts-with? % p))
                              ["question: " "todo: " "fixme: " "uncertain: " "missing: "
                               "assumption: " "tbd: " "hack: " "xxx: "])
                       out)))
   :classify (fn [_ out] (if (seq out) :gaps :none))
   :classify-domain #{:gaps :none}
   :golden-path "test/golden/hive-mcp/tools/memory/knowledge-gaps.edn"
   :num-tests 200})

(deftrifecta-from-schema questions-end-in-a-question-mark
  #'gaps/extract-questions
  {:in [:cat ms/GapText]
   :out [:vector :string]
   :rel (fn [_ out] (every? #(str/ends-with? % "?") out))
   :golden-path "test/golden/hive-mcp/tools/memory/extract-questions.edn"
   :num-tests 100})

(deftest each-extractor-names-its-marker
  (is (= ["todo: wire the adapter"] (gaps/extract-todo-markers "TODO: wire the adapter")))
  (is (= ["uncertain: unclear who owns it"] (gaps/extract-uncertainty "unclear: who owns it")))
  (is (= ["missing: missing tests"] (gaps/extract-missing "missing: tests")))
  (is (= ["assumption: a single writer"] (gaps/extract-assumptions "assuming a single writer"))))

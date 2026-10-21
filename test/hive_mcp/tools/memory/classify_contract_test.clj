(ns hive-mcp.tools.memory.classify-contract-test
  "Contract facets for hive-mcp.tools.memory.classify."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.memory.classify :as cl]
            [hive-mcp.tools.memory.schema :as ms]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private SignalTags
  ;; One tag per input, so each signal level is reached evenly.
  [:enum ["axiom"] ["convention"] ["lint"] ["plain"] nil])



(deftrifecta-from-schema tag-level-signal-ranks-the-strongest-tag
  #'cl/tag-level-signal
  {:in [:cat SignalTags]
   :out ms/SignalLevel
   :rel (fn [[tags] out]
          (let [t (set tags)]
            (= out (cond (some t ["axiom" "ADR"]) 4
                         (some t ["convention" "pattern"]) 3
                         (some t ["lint" "kondo"]) 1
                         :else nil))))
   :classify (fn [_ out] (or out :none))
   :classify-domain #{4 3 1 :none}
   :golden-path "test/golden/hive-mcp/tools/memory/tag-level-signal.edn"
   :num-tests 200})

(def ^:private Content
  [:maybe [:enum "This MUST ALWAYS hold." "Pattern: retry with backoff."
           "see file: src/a.clj at line 12" "plain words" "" "  "]])

(deftrifecta-from-schema content-keyword-level-reads-the-signal
  #'cl/content-keyword-level
  {:in [:cat Content]
   :out ms/SignalLevel
   :rel (fn [[c] out]
          (= out (get {"This MUST ALWAYS hold." 4 "Pattern: retry with backoff." 3
                       "see file: src/a.clj at line 12" 1} c)))
   :golden-path "test/golden/hive-mcp/tools/memory/content-keyword-level.edn"
   :num-tests 100})

(deftest classify-abstraction-level-prefers-content-then-tags
  (is (= 4 (cl/classify-abstraction-level "note" "must never break" nil)))
  (is (= 3 (cl/classify-abstraction-level "note" "plain" ["convention"])))
  (is (<= 1 (cl/classify-abstraction-level "zzz" nil nil) 4)))

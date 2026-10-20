(ns hive-mcp.tools.composite-contract-test
  "Contract facets for the composite tool builder's pure surface: the
   one-line contribution summary, the contributed command names, and the
   subdomain prefix stripper. Tool names that no addon contributes to are
   used, so the command registry is read but never written."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.composite :as composite]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Text
  [:enum nil "" "   " "short" "first line\nsecond line" (apply str (repeat 230 "x"))])

(deftrifecta-from-schema contribution-summary-is-one-capped-line
  #'composite/contribution-summary
  {:in [:cat [:map [:summary {:optional true} Text] [:description {:optional true} Text]]]
   :out [:maybe :string]
   :rel (fn [[{:keys [summary description]}] out]
          ;; Per the docstring: its :summary whole, else the FIRST LINE of
          ;; its :description; trimmed and capped at 200 chars.
          (let [src (cond (not (str/blank? summary)) summary
                          (not (str/blank? description)) (first (str/split-lines description)))]
            (if (str/blank? src)
              (nil? out)
              (and (<= (count out) 200)
                   (str/starts-with? (str/trim src) (str/replace out "…" ""))))))
   :classify (fn [_ out] (cond (nil? out) :none (str/ends-with? out "…") :capped :else :line))
   :classify-domain #{:none :capped :line}
   :golden-path "test/golden/hive-mcp/tools/composite-contribution-summary.edn"
   :num-tests 200})

(def ^:private Cmds
  [:map-of {:max 3} [:enum "ship" "belt" "carto"]
   [:map [:handler {:optional true} [:or [:= :leaf] [:map-of {:max 2} [:enum :land :scan] [:= :h]]]]]])

(deftrifecta-from-schema contributed-command-names-spell-every-verb
  #'composite/contributed-command-names
  {:in [:cat Cmds]
   :out [:vector :string]
   :rel (fn [[cmds] out]
          (= (set out)
             (set (for [[c {:keys [handler]}] cmds
                        x (cons c (when (map? handler) (map #(str c " " (name %)) (keys handler))))]
                    x))))
   :num-tests 150})

(golden/deftest-golden composite-text-golden
  "test/golden/hive-mcp/tools/composite-text.edn"
  {:text      (composite/contributed-commands-text
               {"ship" {:summary "Land a branch"} "belt" {:description "Scan\nmore"} "zz" {}})
   :empty     (composite/contributed-commands-text {})
   :names     (composite/contributed-command-names {"ship" {:handler {:land :h}}})
   :stripped  (:command ((composite/subdomain-handler "carto" identity) {:command "carto search"}))
   :kept      (:command ((composite/subdomain-handler "carto" identity) {:command "kg traverse"}))
   :no-addon  (composite/build-merged-tool {:name "m24-no-such-tool" :description "d"})
   :lazy-miss (composite/lazy-resolve-handlers 'm24.no-such/handlers)})

(deftest unknown-tools-answer-empty-shapes
  (is (= {} (composite/lazy-resolve-schema-props 'm24.no-such/tool-def)))
  (is (= [:help] (keys (composite/build-composite-handlers "m24-no-such-tool"))))
  (is (= {:a 1} (composite/build-merged-handlers "m24-no-such-tool" {:a 1})))
  (is (m/validate composite/ToolDef (composite/build-composite-tool "m24-no-such-tool" "x"))))

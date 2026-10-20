(ns hive-mcp.presets.m16-facet-test
  "Trifecta for the pure preset-core extraction; the store-backed fns are
   covered by presets_test.clj against the in-memory vector stub."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [hive-mcp.presets.core :as presets]
            [hive-schemas.test :as hst]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- out-schema [nsym fsym]
  (-> (m/function-schemas) (get-in [nsym fsym :schema]) m/children second))

(def ^:private PresetMap
  [:map {:gen/fmap identity}
   [:name {:optional true} [:enum "tdd" "clarity" "review"]]
   [:title {:optional true} :string]
   [:category {:optional true} [:enum "testing" "coding" "general"]]
   [:_content {:optional true} :string]
   [:content {:optional true} :string]])

(hst/deftrifecta-from-schema extract-preset-core-contract
  hive-mcp.presets.core/extract-preset-core
  {:in [:cat PresetMap]
   :out (out-schema 'hive-mcp.presets.core 'extract-preset-core)
   :rel (fn [[_in] out]
          (and (string? (:summary out))
               (vector? (:key-points out))
               (every? string? (:key-points out))
               (<= (count (:key-points out)) 5)
               (string? (:category out))))
   :golden-path "test/golden/hive-mcp/m16-extract-preset-core.edn"})

(def ^:private md-sample
  "# Testing Discipline\n\nWrite the test first. Red, green, refactor.\n\n- small steps\n- run often\n- keep green\n- delete duplication\n- name the intent\n- sixth bullet is dropped")

(golden/deftest-golden preset-core-golden
  "test/golden/hive-mcp/m16-preset-core.edn"
  {:full (presets/extract-preset-core {:name "tdd" :_content md-sample})
   :content-key (presets/extract-preset-core {:name "x" :content "# T\n\nPara one.\n\n- a\n- b"})
   :empty (presets/extract-preset-core {:name "bare"})
   :no-bullets (presets/extract-preset-core {:name "nb" :content "# T\n\nOnly prose."})
   :title-fallback (presets/extract-preset-core {:name "nm" :content "no title"})})

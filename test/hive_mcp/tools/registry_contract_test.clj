(ns hive-mcp.tools.registry-contract-test
  "Contract facets for the tool registry: a schema-derived trifecta over the
   pure advertised-tools fold (precedence addon > dynamic > base, absorbed
   and excluded names dropped, every surviving name exactly once, the
   visibility gate applied) and over child-ling-excluded?, plus a golden
   over the gate."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.registry :as reg]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Name [:enum "memory" "swarm" "kg" "carto" "web"])

(defn- layer
  "A source layer whose tools all carry SRC, so the winner is observable."
  [src opts]
  [:vector (merge {:gen/fmap (fn [ts] (mapv #(assoc % :src src) ts))} opts)
   [:map [:name Name] [:src [:= src]]]])

(def ^:private Inputs
  "SurfaceInputs over a small name universe, so collisions between layers
   and with the absorbed / excluded / visible sets are frequent. Base is
   never empty and :visible is a non-empty set half the time, so the gate
   is reached as often as it is skipped."
  [:map
   [:base (layer :base {:min 1 :max 5})]
   [:dynamic (layer :dynamic {:max 3})]
   [:addon (layer :addon {:max 3})]
   [:absorbed [:set {:max 1} Name]]
   [:excluded [:set {:max 1} Name]]
   [:visible [:or :nil [:set {:min 1 :max 2} Name]]]])

(defn- expected-winner
  "The tool of NAME from the highest-precedence layer, last-in-layer wins."
  [inputs nm]
  (some (fn [layer] (last (filter #(= nm (:name %)) (get inputs layer))))
        [:addon :dynamic :base]))

(deftrifecta-from-schema advertised-tools-folds-by-precedence
  #'reg/advertised-tools
  {:in [:cat Inputs]
   :out reg/ToolDefs
   :rel (fn [[{:keys [absorbed excluded visible] :as in}] out]
          (let [names    (map :name out)
                expected (->> (mapcat in [:base :dynamic :addon]) (map :name) distinct
                              (remove (into (set absorbed) excluded)) set)]
            (and (= (count names) (count (distinct names)))
                 (= expected (set names))
                 (every? (fn [t]
                           (let [w (expected-winner in (:name t))]
                             (and (= (:src w) (:src t))
                                  (= (boolean (and (seq visible) (not (visible (:name t)))))
                                     (boolean (:deprecated t))))))
                         out))))
   :classify (fn [[in] _] (if (seq (:visible in)) :gated :ungated))
   :classify-domain #{:gated :ungated}
   :golden-path "test/golden/hive-mcp/tools/registry-advertised-tools.edn"
   :num-tests 200})

(deftrifecta-from-schema child-ling-exclusion-is-exact-name-membership
  #'reg/child-ling-excluded?
  {:in [:cat [:map [:name [:enum "swarm" "multi" "emacs" "swarm-x" "memory" "Swarm"]]
              [:deprecated {:optional true} :boolean]]]
   :out :boolean
   :rel (fn [[t] out] (= out (contains? #{"swarm" "multi" "emacs"} (:name t))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :golden-path "test/golden/hive-mcp/tools/registry-child-excluded.edn"
   :num-tests 100})

(golden/deftest-golden visibility-gate-golden
  "test/golden/hive-mcp/tools/registry-visibility-gate.edn"
  (let [ts [{:name "a"} {:name "b" :deprecated true} {:name "c"}]]
    {:gated   (reg/apply-visibility-gate ts #{"a"})
     :ungated (reg/apply-visibility-gate ts nil)
     :empty   (reg/apply-visibility-gate ts #{})
     :none    (reg/apply-visibility-gate nil #{"a"})}))

(deftest live-surfaces-validate
  (is (m/validate reg/ToolDefs (reg/get-advertised-tools)))
  (is (m/validate reg/ToolDefs (reg/get-advertised-tools {:compact-schema? true})))
  (is (every? :consolidated (reg/get-consolidated-tools)))
  (is (not-any? reg/child-ling-excluded? (reg/get-child-ling-tools)))
  (is (nil? (reg/get-tool-by-name "no-such-tool"))))

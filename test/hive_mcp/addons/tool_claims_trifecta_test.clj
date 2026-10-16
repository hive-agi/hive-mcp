(ns hive-mcp.addons.tool-claims-trifecta-test
  "Trifecta for the pure tool-name resolution (`hive-mcp.addons.tool-claims`).

   Property + mutation facets are SYNTHESIZED from the malli contracts by
   hive-schemas.test; the golden pins the seeded cases. The input schemas below
   are narrowings of the contract's own `CoreTool` / `Contribution` (names and
   addon ids drawn from small enums so that clashes, the interesting part of
   the rules, actually occur), never a second description of the same shape."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.addons.tool-claims :as tc]
            [hive-schemas.test :as hst]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Name [:enum "a" "b" "c" "d"])

(def ^:private CoreIn
  [:vector {:max 4}
   [:map [:name Name] [:consolidated {:optional true} [:maybe :boolean]]]])

(def ^:private ContributionIn
  "Contributions with DISTINCT addon ids, as the registry always supplies."
  [:vector {:max 4
            :gen/fmap (fn [cs] (vec (vals (into (sorted-map) (map (juxt (comp str :addon-id) identity)) cs))))}
   [:map
    [:addon-id [:enum :x :y :z]]
    [:addon-type {:optional true} [:maybe [:enum :native :external]]]
    [:tools [:vector {:max 4} [:map [:name Name] [:consolidated {:optional true} [:maybe :boolean]]]]]
    [:excluded {:optional true} [:maybe [:set Name]]]]])

(defn- tool-count [contributions]
  (reduce + (map (comp count :tools) contributions)))

(hst/deftrifecta-from-schema resolve-claims-contract
  hive-mcp.addons.tool-claims/resolve-claims
  {:in        [:cat CoreIn ContributionIn]
   :out       tc/Resolution
   :rel       (fn [[_core contributions] {:keys [installed refused]}]
                ;; every contributed tool-def is installed XOR refused, once,
                ;; and an installed name is held by exactly one addon
                (and (= (tool-count contributions) (+ (count installed) (count refused)))
                     (= (count installed) (count (distinct (map :name installed))))
                     (every? :addon-source installed)))
   :num-tests 200
   :golden-path "test/golden/addons/tool-claims-resolve.edn"})

(hst/deftrifecta-from-schema refusals-by-addon-contract
  hive-mcp.addons.tool-claims/refusals-by-addon
  {:in        tc/Resolution
   :out       [:map-of :any [:vector tc/Refusal]]
   :rel       (fn [{:keys [refused]} grouped]
                (and (= (count refused) (reduce + (map count (vals grouped))))
                     (every? (fn [[id rs]] (every? #(= id (:addon-id %)) rs)) grouped)))
   :num-tests 100})

(hst/deftrifecta-from-schema claimed-core-names-contract
  hive-mcp.addons.tool-claims/claimed-core-names
  {:in        [:map [:claims [:vector tc/Claim]]]
   :out       [:set :string]
   :rel       (fn [{:keys [claims]} names]
                (= names (set (map :tool (filter #(= :core (:over %)) claims)))))
   :num-tests 100})

(deftest contracts-discriminate-test
  (testing "a Resolution missing a bucket is not a Resolution"
    (is (not (m/validate tc/Resolution {:installed [] :refused []}))))
  (testing "a refusal with an unknown reason is rejected"
    (is (not (m/validate tc/Refusal {:addon-id :x :tool "a" :reason :nope :holder :core}))))
  (testing "a claim over neither :core nor :addon is rejected"
    (is (not (m/validate tc/Claim {:addon-id :x :tool "a" :how :declared :over :both})))))

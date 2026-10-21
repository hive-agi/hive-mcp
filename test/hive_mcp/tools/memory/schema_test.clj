(ns hive-mcp.tools.memory.schema-test
  "Facets for the memory tool value objects.

   `params` is the one fn: a trifecta from its contract (it answers a malli
   schema object, so the golden pins its FORM instead). The golden also pins
   the arms of the union schemas (Promotion, EntryTags) so a change in what
   they admit shows up as a diff, and a property checks that every value
   object here generates values it accepts."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.tools.memory.schema :as ms]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema params-extends-mcp-params
  hive-mcp.tools.memory.schema/params
  {:in        [:fn {:gen/elements [[:map [:x {:optional true} :int]]
                                   [:map [:feedback [:enum "helpful"]]]
                                   [:map [:min_projects {:optional true} :int]]]}
               some?]
   :out       [:fn {:gen/elements [ms/McpParams]} m/schema?]
   :rel       (fn [extra out]
                (let [keys-of #(set (map first (m/children (m/schema %))))]
                  (and (every? (keys-of out) (keys-of ms/McpParams))
                       (every? (keys-of out) (keys-of extra)))))
   ;; the :out schema is a schema? predicate over malli objects: no shape to mutate
   :mutation  false
   :num-tests 50})

(def ^:private value-objects
  {:entry ms/Entry :mcp-params ms/McpParams :tier-walk ms/TierWalk :decay-plan ms/DecayPlan
   :decay-cycle ms/DecayCycle :xpoll-plan ms/XpollPlan :promotion ms/Promotion
   :xpoll-cycle ms/XpollCycle :cleanup ms/CleanupResult :entry-tags ms/EntryTags
   :reconcile ms/ReconcileResponse :directory-input ms/DirectoryInput
   :search-payload ms/SearchPayload :runner-opts ms/RunnerOpts})

(defspec every-value-object-accepts-what-it-generates 50
  (prop/for-all [[_ s] (gen/elements (vec value-objects))
                 seed gen/nat]
    (m/validate s (mg/generate s {:seed seed :size 5}))))

(def ^:private promotion-arms
  {:not-promoted {:id "e1" :promoted false :duration "permanent"}
   :promoted     {:id "e1" :promoted true :old_duration "short" :new_duration "long" :tiers_promoted 2}
   :zero-tiers   {:id "e1" :promoted true :old_duration "short" :new_duration "long" :tiers_promoted 0}})

(deftest-golden memory-schema-forms-and-arms
  "test/golden/tools/memory/schema.edn"
  {:params-form     (m/form (ms/params [:map [:feedback {:optional true} [:enum "helpful"]]]))
   :promotion-arms  (update-vals promotion-arms #(m/validate ms/Promotion %))
   :entry-tags      (into (sorted-map)
                          (for [[k v] {:vector ["a"] :set #{"a"} :joined "a,b" :nil nil :int 3}]
                            [k (m/validate ms/EntryTags v)]))
   :tier-walk-open? (m/validate ms/TierWalk {:final-duration "long" :tiers-promoted 1 :extra 1})
   :preview-len     [(m/validate ms/PreviewLen 5) (m/validate ms/PreviewLen 6)]})

(deftest params-keeps-the-shared-keys-optional
  (is (m/validate (ms/params [:map [:x {:optional true} :int]]) {}))
  (is (not (m/validate (ms/params [:map [:x :int]]) {}))))

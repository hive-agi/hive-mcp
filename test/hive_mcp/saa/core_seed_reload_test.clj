(ns hive-mcp.saa.core-seed-reload-test
  "Contract: every load of hive-mcp.saa.core-seed converges the :saa/core seed.

   R1 a load over an image that already loaded core-seed installs every seed the
      registry is missing, across all child registries.
   R1b the upgrade-reload case: a child registry new to the image starts empty
      and the load fills it, leaving the other seeds as they were.
   R2 a load replaces :saa/core entries left by an earlier load with the ones
      the loaded code builds."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.saa.registry :as registry]
            [hive-mcp.saa.registry.phase-providers :as r-providers]
            [hive-mcp.saa.registry.scorers :as r-scorers]
            [hive-mcp.saa.registry.planners :as r-planners]
            [hive-mcp.saa.registry.tool-intents :as r-intents]
            [hive-mcp.saa.registry.dispatch-modes :as r-dispatch]
            [hive-mcp.saa.types :as types]
            [hive-mcp.saa.support :as support]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each support/with-fresh-registry)

(defn- load-core-seed!
  "Load hive-mcp.saa.core-seed again into an image that has already loaded it."
  []
  (require 'hive-mcp.saa.core-seed :reload))

(defn- core-seed-view
  "The :saa/core seed as a comparable value: provider, scorer and planner
   entries, the :saa/core tool slice of every registered intent, and the owner
   of the :dag-wave dispatch mode."
  []
  {:provider       (some-> (r-providers/lookup :saa/default) (select-keys [:provider :owner]))
   :scorer         (some-> (r-scorers/lookup :saa/default) (select-keys [:scorer :owner]))
   :planner        (some-> (r-planners/lookup :saa/default) (select-keys [:planner :owner]))
   :intents        (into {}
                         (keep (fn [i]
                                 (when-let [slice (r-intents/lookup-owner-slice :saa/core i)]
                                   [i slice])))
                         (r-intents/all-intents))
   :dag-wave-owner (:owner (r-dispatch/lookup :dag-wave))})

(defn- assert-seeded!
  "The fixture's install! seeded every slot, so a later equality is not vacuous."
  [view]
  (is (= :saa/core (get-in view [:provider :owner])))
  (is (= :saa/core (get-in view [:scorer :owner])))
  (is (= :saa/core (get-in view [:planner :owner])))
  (is (seq (:intents view)))
  (is (= :saa/core (:dag-wave-owner view))))

(defn- stale-dispatch
  [_plan _agent-id _ctx]
  {:wave-id "stale" :result {:status :stale}})

(defn- stale-entries
  "One :saa/core entry per seeded kind, standing in for a seed an earlier load left."
  []
  [(types/saa-registry-entry :saa/phase-provider {:provider :stale :owner :saa/core})
   (types/saa-registry-entry :saa/scorer {:scorer :stale :owner :saa/core})
   (types/saa-registry-entry :saa/planner {:planner :stale :owner :saa/core})
   (types/saa-registry-entry :saa/tool-intent {:intent :read :tools ["stale"] :owner :saa/core})
   (types/saa-registry-entry :saa/dispatch-mode {:mode :dag-wave :dispatch stale-dispatch
                                                 :owner :saa/core})])

(deftest r1-load-installs-every-missing-seed
  (testing "a load of core-seed over an emptied registry puts every :saa/core seed back"
    (let [pristine (core-seed-view)]
      (assert-seeded! pristine)
      (registry/reset-for-test!)
      (is (nil? (registry/lookup-dispatch-mode :dag-wave)) "precondition: the wipe landed")
      (is (empty? (:intents (core-seed-view))) "precondition: the wipe landed")
      (load-core-seed!)
      (is (= pristine (core-seed-view)))
      (is (ifn? (registry/lookup-dispatch-mode :dag-wave))))))

(deftest r1b-load-fills-a-child-registry-new-to-the-image
  (testing "only the dispatch-mode registry is empty, as after a reload that adds it"
    (let [pristine (core-seed-view)]
      (assert-seeded! pristine)
      (r-dispatch/reset-for-test!)
      (is (nil? (registry/lookup-dispatch-mode :dag-wave)) "precondition: the wipe landed")
      (load-core-seed!)
      (is (= pristine (core-seed-view)))
      (is (ifn? (registry/lookup-dispatch-mode :dag-wave))))))

(deftest r2-load-replaces-seeds-left-by-an-earlier-load
  (testing "a load rebuilds :saa/core entries that are already present"
    (let [pristine (core-seed-view)]
      (assert-seeded! pristine)
      (is (not-any? #{:conflict :ignored}
                    (registry/register-by-key! :saa/core :saa/core (stale-entries)))
          "precondition: every stale entry was accepted under :saa/core")
      (is (identical? stale-dispatch (registry/lookup-dispatch-mode :dag-wave))
          "precondition: the stale seed is in place")
      (is (= #{"stale"} (r-intents/lookup-owner-slice :saa/core :read))
          "precondition: the stale intent slice is in place")
      (load-core-seed!)
      (is (= pristine (core-seed-view)))
      (is (not (identical? stale-dispatch (registry/lookup-dispatch-mode :dag-wave)))))))

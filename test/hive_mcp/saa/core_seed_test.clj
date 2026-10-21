(ns hive-mcp.saa.core-seed-test
  "Facets for the :saa/core seed. dag-wave-dispatch-fn is driven with its
   scheduler port (start-dag!) injected as a recording stub, so the property
   checks what reached the scheduler and what the dispatch fn answered;
   install! is pinned by a golden over the registries it re-seeds, inside
   support's fixture that snapshots and restores every SAA child registry."
  (:require [clojure.test :refer [use-fixtures]]
            [hive-mcp.saa.core-seed :as core-seed]
            [hive-mcp.saa.registry :as registry]
            [hive-mcp.saa.registry.tool-intents :as tool-intents]
            [hive-mcp.saa.support :as support]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each support/with-fresh-registry)

(def ^:private calls (atom []))

(defn- stub-start-dag!
  "The scheduler port: records each call and answers {:plan-id ..} as the
   kernel's start-dag! does."
  [plan-id opts]
  (swap! calls conj [plan-id opts])
  {:plan-id (str "sched-" plan-id) :started true})

(def ^:private DispatchArgs
  [:cat
   [:maybe [:map [:id [:enum "plan-1" "plan-2"]]]]
   [:maybe [:enum "agent-x"]]
   [:maybe [:map
            [:directory {:optional true} [:enum "/repo" "/tmp"]]
            [:run-id {:optional true} [:enum "run-7" "run-8"]]]]])

(defn- dispatch-under-stub
  "Build the dispatch fn over the stub port and apply it, answering the
   dispatch result and the single scheduler call it made."
  [plan agent-id ctx]
  (reset! calls [])
  (let [out ((core-seed/dag-wave-dispatch-fn stub-start-dag!) plan agent-id ctx)]
    {:out out :call (first @calls)}))

;; dag-wave-dispatch-fn returns a FUNCTION, which has no value a trifecta's
;; golden or mutants can compare, so its behaviour is pinned by a golden over
;; the dispatch fn it builds, applied with the scheduler port stubbed.
(golden/deftest-golden dag-wave-dispatch-golden
  "test/golden/hive-mcp/saa/core-seed-dag-wave-dispatch.edn"
  {:with-run-id (dispatch-under-stub {:id "plan-1"} "agent-x" {:directory "/repo" :run-id "run-7"})
   :scheduler-id (dispatch-under-stub {:id "plan-2"} nil {:directory "/tmp"})
   :nil-ctx (dispatch-under-stub {:id "plan-1"} nil nil)
   :nil-plan (dispatch-under-stub nil "agent-x" {:run-id "run-8"})
   :conforms (every? #(m/validate (-> (m/function-schemas)
                                      (get-in ['hive-mcp.saa.core-seed 'dag-wave-dispatch-fn :schema])
                                      m/children second m/children last)
                                  (:out %))
                     [(dispatch-under-stub {:id "p"} nil {:run-id "r"})
                      (dispatch-under-stub {:id "p"} nil {})])})

(golden/deftest-golden install-reseeds-core-golden
  "test/golden/hive-mcp/saa/core-seed-reseed-registries.edn"
  (let [_ (registry/reset-for-test!)
        counts (core-seed/install!)]
    {:counts counts
     :installed-shape (m/validate core-seed/SeedCounts core-seed/installed)
     :tool-intents (into (sorted-map)
                         (map (fn [i] [i (tool-intents/lookup i)]))
                         (tool-intents/all-intents))
     :dag-wave? (fn? (registry/lookup-dispatch-mode :dag-wave))
     :reinstall (core-seed/install!)}))

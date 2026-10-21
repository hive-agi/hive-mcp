(ns hive-mcp.workflows.forge-belt-contract-test
  "Contracts and hive-test facets for the forge-belt extension stubs and the
   FOSS defaults that back them.

   forge-belt resolves every operation from the extension registry. Both sides
   of that seam are pinned under instrumentation: the noop arm (registry empty)
   and the delegated arm (defaults registered through
   hive-mcp.test.stub.forge-belt). The delegated dispatch predicates get
   trifectas over generated belt state, related to the documented rule."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.test.stub.forge-belt :as stub]
            [hive-mcp.workflows.forge-belt :as belt]
            [hive-mcp.workflows.forge-belt-defaults :as fbd]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private contracted
  (into #{#'fbd/register-forge-belt-defaults!}
        (for [s '[quenched? has-tasks? no-tasks? continuous? single-shot? always handle-start
                  handle-smite handle-survey handle-spark handle-end handle-halt handle-error
                  on-smite-count-change on-spark-count-change compile-belt run-belt
                  run-single-strike run-continuous-belt]]
          (ns-resolve 'hive-mcp.workflows.forge-belt s))))

(defn- without-belt-extensions [f]
  (let [prior (into {} (map (fn [k] [k (ext/get-extension k)])) stub/belt-keys)]
    (doseq [k stub/belt-keys] (ext/deregister! k))
    (try (f)
         (finally (doseq [[k v] prior] (when v (ext/register! k v)))))))

(use-fixtures :each
  (fn [f]
    (mi/instrument! {:filters [(mi/-filter-var contracted)]})
    (try (f) (finally (mi/unstrument!)))))

;; =============================================================================
;; Delegated predicates (defaults registered): trifectas
;; =============================================================================

(defn- with-defaults [f] (fn [x] (stub/with-forge-belt-defaults #(f x))))

(def ^:private BeltState
  [:map
   [:quenched? {:optional true} [:maybe :boolean]]
   [:continuous? {:optional true} [:maybe :boolean]]
   [:survey-result {:optional true} [:map [:count {:optional true} [:int {:min 0 :max 3}]]]]])

(defn has-tasks-with-defaults
  "Unary adapter: belt/has-tasks? with the FOSS defaults registered."
  [d] (stub/with-forge-belt-defaults #(belt/has-tasks? d)))

(defn continuous-with-defaults
  "Unary adapter: belt/continuous? with the FOSS defaults registered."
  [d] (stub/with-forge-belt-defaults #(belt/continuous? d)))

(deftrifecta-from-schema delegated-has-tasks
  hive-mcp.workflows.forge-belt-contract-test/has-tasks-with-defaults
  {:in BeltState :out :boolean
   :rel (fn [d out] (= out (pos? (get-in d [:survey-result :count] 0))))
   :num-tests 100})

(deftrifecta-from-schema delegated-continuous
  hive-mcp.workflows.forge-belt-contract-test/continuous-with-defaults
  {:in BeltState :out :boolean
   :rel (fn [d out] (= out (true? (:continuous? d))))
   :num-tests 100})

(deftest delegated-predicates-partition
  (stub/with-forge-belt-defaults
    (fn []
      (doseq [d [{} {:survey-result {:count 0}} {:survey-result {:count 2}}
                 {:continuous? true} {:continuous? false} {:quenched? true}]]
        (is (not= (belt/has-tasks? d) (belt/no-tasks? d)))
        (is (not= (belt/continuous? d) (belt/single-shot? d))))
      (is (true? (belt/quenched? {:quenched? true})))
      (is (false? (belt/quenched? {:quenched? "yes"}))))))

;; =============================================================================
;; Noop arm (registry empty)
;; =============================================================================

(deftest noop-arm
  (without-belt-extensions
   (fn []
     (is (= [false false true false true true]
            ((juxt belt/quenched? belt/has-tasks? belt/no-tasks? belt/continuous?
                   belt/single-shot? belt/always) {})))
     (is (= {:a 1} (belt/handle-start {} {:a 1})))
     (is (= {:count 0 :smited [] :failed []} (:smite-result (belt/handle-smite {} {}))))
     (is (= {:tasks [] :count 0} (:survey-result (belt/handle-survey {} {}))))
     (is (= {} (belt/handle-end {} {:data {}})))
     (is (= {:data {}} (belt/handle-halt {} {:data {}})))
     (is (nil? (belt/handle-error {} {:error "e"})))
     (is (nil? (belt/compile-belt)))
     (is (= "Extension not available" (:message (belt/run-belt nil {}))))
     (is (false? (:success (belt/run-continuous-belt {})))))))

;; =============================================================================
;; Delegated arm: the defaults through the stub seam
;; =============================================================================

(defn- resources []
  {:agent-ops  {:kill-fn  (fn [_ _] {:smited ["z"] :failed [] :count 1})
                :spawn-fn (fn [_] {:spawned ["l1"] :failed [] :count 1})}
   :kanban-ops {:list-fn (fn [_] {:tasks [{:id "t1"}] :count 1}) :update-fn (fn [_] nil)}
   :config     {:max-slots 2}
   :context-gather? false
   :directory  "/tmp/belt"
   :clock-fn   (constantly "2026-01-01T00:00:00Z")})

(deftest-golden forge-belt-single-strike
  "test/golden/workflows/forge-belt-single-strike.edn"
  (stub/with-forge-belt-defaults #(belt/run-single-strike (resources))))

(deftest delegated-arm
  (stub/with-forge-belt-defaults
    (fn []
      (is (map? (belt/compile-belt)))
      (let [out (belt/run-single-strike (resources))]
        (is (= [true 1 1 1] ((juxt :success :strike-count :total-smited :total-sparked) out))))
      (is (= :hive-mcp.workflows.forge-belt-defaults/halted
             (get-in (belt/handle-halt {} {:data {} :fsm {}}) [:data :phase])))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Forge belt error"
                            (belt/handle-error {} {:error "e" :data {:phase :x}})))
      (is (nil? (belt/on-smite-count-change [:total-smited] 0 1))))))

(deftest register-defaults-installs-every-key
  (without-belt-extensions
   (fn []
     (is (nil? (fbd/register-forge-belt-defaults!)))
     (is (every? ext/get-extension stub/belt-keys)))))

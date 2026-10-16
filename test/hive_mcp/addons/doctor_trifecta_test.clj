(ns hive-mcp.addons.doctor-trifecta-test
  "Trifecta for the addon doctor (`hive-mcp.addons.doctor`).

   `run-doctor` is the composition root over ports. Every port is INJECTED
   (`run-doctor`'s own DIP seam: discover, solve, constructor, registry entry,
   health, clock), so the facets run over a deterministic stub world: no redefs,
   no live registry, no Emacs. The adapter turns a scenario keyword + addon id
   into the stage statuses, which is what the golden pins."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-dsl.result :as r]
            [hive-mcp.addons.doctor :as doctor]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]
            [malli.generator :as mg])
  (:import [java.time Instant]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private spec-of
  {:addon/id "stub.addon" :addon/type :native
   :addon/init-ns "stub.ns" :addon/init-fn "->stub"
   :addon/capabilities #{:tools}})

(defn- ports
  "A stub world. `scenario` shapes what the ports answer."
  [scenario]
  {:discover-fn            (fn [] {:specs (if (= scenario :undiscovered) [] [spec-of]) :errors []})
   :solve-fn               (fn [_] {:ordered [spec-of] :missing {} :cycles #{} :duplicates {}
                                    :unmet-capabilities {}})
   :resolve-constructor-fn (fn [_] (when (not= scenario :unresolvable) (fn [& _] nil)))
   :scan-project-fn        (fn [_] {:status :pass :evidence {}})
   :get-entry-fn           (fn [_] (when (not= scenario :unregistered)
                                     {:addon ::addon :state (if (= scenario :inactive) :registered :active)
                                      :registered-at (Instant/parse "2026-10-14T00:00:00Z")}))
   :validate-addon-fn      (fn [_] (r/ok true))
   :health-fn              (fn [_] {:status :ok})
   :capabilities-fn        (fn [_] #{:tools})
   :addon-id-fn            (fn [_] "stub.addon")
   :addon-type-fn          (fn [_] :native)
   :now-fn                 (constantly (Instant/parse "2026-10-14T12:00:00Z"))})

(defn run-scenario
  "Unary adapter: scenario keyword -> {:ok? :stages {stage status}} or the refusal."
  [scenario]
  (let [res (doctor/run-doctor {:addon-id "stub.addon"} (ports scenario))]
    (if (r/ok? res)
      (let [rep (:ok res)]
        {:ok?    (:ok? rep)
         :summary (:summary rep)
         :stages (into (sorted-map) (map (juxt :stage :status)) (:stages rep))})
      {:refused (:error res)})))

(def ^:private gen-scenario
  (gen/elements [:healthy :undiscovered :unresolvable :unregistered :inactive]))

(deftrifecta doctor-scenarios-contract
  hive-mcp.addons.doctor-trifecta-test/run-scenario
  {:golden-path "test/golden/addons/doctor-scenarios.edn"
   :cases {:healthy      :healthy
           :undiscovered :undiscovered
           :unresolvable :unresolvable
           :unregistered :unregistered
           :inactive     :inactive}
   :gen gen-scenario
   :pred (fn [r] (and (map? r)
                      (or (contains? r :refused)
                          ;; the headline is exactly "no stage failed"
                          (= (:ok? r) (zero? (:fail (:summary r)))))))
   :num-tests 50
   :mutations [["everything passes" (fn [_] {:ok? true :summary {:pass 6 :fail 0 :skip 0} :stages {}})]
               ["never ok" (fn [_] {:ok? false :summary {:pass 0 :fail 6 :skip 0} :stages {}})]
               ["refuses" (constantly {:refused :addon-doctor/invalid-input})]]
   :assert (fn []
             (is (true? (:ok? (run-scenario :healthy))))
             (is (false? (:ok? (run-scenario :undiscovered))))
             (is (= :fail (get-in (run-scenario :unregistered) [:stages :lifecycle-smoke]))
                 "an addon absent from the registry fails the lifecycle stage")
             (is (= :skip (get-in (run-scenario :undiscovered) [:stages :constructor-resolution]))
                 "no manifest -> the constructor stage is skipped, not failed"))})

(deftest run-doctor-arms-test
  (testing "invalid input is the ONLY err arm, and validates against DoctorResult"
    (let [res (doctor/run-doctor {:addon-id ""} (ports :healthy))]
      (is (r/err? res))
      (is (m/validate doctor/DoctorResult res))))
  (testing "a clean world is an ok arm whose report satisfies the report contract"
    (let [res (doctor/run-doctor {:addon-id "stub.addon"} (ports :healthy))]
      (is (r/ok? res))
      (is (m/validate doctor/DoctorResult res))
      (is (doctor/report-valid? (:ok res)))
      (is (not (m/validate doctor/DoctorResult {:ok {:report/type :nope}})))))
  (testing "the err arm is not mistaken for a report"
    (is (not (doctor/report-valid? {:error :addon-doctor/invalid-input})))))

(deftest scan-project-boundary-fragments-test
  (testing "no directory -> skip"
    (is (= :skip (:status (doctor/scan-project-boundary nil))))
    (is (= :skip (:status (doctor/scan-project-boundary "  ")))))
  (testing "a directory that does not exist -> fail with evidence"
    (let [frag (doctor/scan-project-boundary "/nonexistent/hive-doctor-probe")]
      (is (= :fail (:status frag)))
      (is (m/validate doctor/ScanFragment frag)))))

(deftest schemas-generate-test
  (doseq [s [doctor/FeatureName doctor/DoctorInput doctor/StageName doctor/Stage
             doctor/DoctorReport doctor/ScanFragment doctor/Ports]]
    (is (seq (mg/sample s {:size 5})) (str "generates: " (pr-str s)))))

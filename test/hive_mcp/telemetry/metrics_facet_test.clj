(ns hive-mcp.telemetry.metrics-facet-test
  "Facets for hive-mcp.telemetry.metrics: record! routes every observation
   to the installed recorder and never throws (property), and the recorder
   install/restore lifecycle (golden). The recorder is the namespace's own
   injection port; the previous recorder is restored after each test."
  (:require [clojure.test :refer [use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.telemetry.metrics :as metrics]
            [hive-test.golden :as golden]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [t]
    (let [prev (metrics/install-recorder! nil)]
      (try (t) (finally (metrics/install-recorder! prev))))))

(def ^:private Observations
  "Observations, each with a flag saying whether the recorder throws on it."
  [:vector {:gen/max 8}
   [:tuple [:vector {:min 2 :max 3} [:enum :mcp :kg "tool" "op"]] [:int {:min 0 :max 1000000}]
    [:enum :ok :error] :boolean]])

(defspec record-routes-to-recorder-and-never-throws 100
  (prop/for-all [obs (mg/generator Observations)]
    (let [seen (atom [])
          errors-before (metrics/recorder-error-count)]
      (metrics/install-recorder!
       (fn [k ns oc]
         (if (= :boom (last k))
           (throw (ex-info "recorder failed" {}))
           (swap! seen conj [k ns oc]))))
      (let [answers (mapv (fn [[k ns oc throw?]]
                            (metrics/record! (if throw? (conj k :boom) k) ns oc))
                          obs)
            throws (count (filter #(nth % 3) obs))]
        (metrics/install-recorder! nil)
        (and (every? nil? answers)
             (= (vec (for [[k ns oc throw?] obs :when (not throw?)] [k ns oc])) @seen)
             (= throws (- (metrics/recorder-error-count) errors-before)))))))

(defn- lifecycle-trace []
  (let [seen (atom [])
        rec (fn [k ns oc] (swap! seen conj [k ns oc]))
        _ (metrics/install-recorder! rec)
        _ (metrics/record! [:mcp :tool "kg"] 42 :ok)
        timed-value (metrics/timed [:mcp :tool "memory"] :answer)
        timed-raised (try (metrics/timed [:mcp :tool "bad"] (throw (ex-info "x" {})))
                          (catch clojure.lang.ExceptionInfo _ :rethrown))
        replaced-is-rec (identical? rec (metrics/install-recorder! nil))
        _ (metrics/record! [:mcp :tool "after-restore"] 1 :ok)]
    {:recorded (mapv (fn [[k _ oc]] [k oc]) @seen)
     :timed-value timed-value
     :timed-raised timed-raised
     :install-answers-previous? replaced-is-rec}))

(golden/deftest-golden recorder-lifecycle-golden
  "test/golden/hive-mcp/telemetry/metrics-recorder-lifecycle.edn"
  (lifecycle-trace))

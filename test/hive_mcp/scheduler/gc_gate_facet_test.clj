(ns hive-mcp.scheduler.gc-gate-facet-test
  "Facets for hive-mcp.scheduler.gc-gate: the fragmentation predicate
   (property against the docstring's rule) and gc-hint! over injected probe
   and gc! ports (property + golden). Nothing touches the real heap."
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.scheduler.gc-gate :as gate]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private gb (* 1024 1024 1024))

(def ^:private Usage
  "Heap readings around the default thresholds (2x, 1 GB gap)."
  [:map
   [:committed [:int {:min 0 :max (* 8 gb)}]]
   [:used [:int {:min 0 :max (* 4 gb)}]]])

(deftrifecta-from-schema fragmented hive-mcp.scheduler.gc-gate/fragmented?
  {:in  [:cat Usage]
   :out :boolean
   :rel (fn [[{:keys [committed used]}] out]
          (= out (and (pos? used) (> committed (* 2.0 used)) (> (- committed used) gb))))
   :classify (fn [_ out] (if out :fragmented :compact))
   :classify-domain #{:fragmented :compact}
   :num-tests 300})

(def ^:private Scenario
  "A first reading, a second (post-gc) reading, and which port fails."
  [:tuple Usage Usage [:enum :none :probe :gc]])

(defn- run [[before after fails]]
  (let [reads (atom [before after])
        gcs (atom 0)
        probe (fn [] (if (= fails :probe)
                       (throw (Exception. "probe down"))
                       (let [r (first @reads)] (swap! reads #(if (next %) (vec (rest %)) %)) r)))
        gc! (fn [] (if (= fails :gc) (throw (Exception. "gc refused")) (swap! gcs inc)))]
    {:out (gate/gc-hint! {:probe probe :gc! gc!}) :gcs @gcs}))

(defspec gc-hint-collects-only-when-fragmented 200
  (prop/for-all [[before _ fails :as sc] (mg/generator Scenario)]
    (let [{:keys [out gcs]} (run sc)]
      (and (m/validate gate/GcHintResult out)
           (= (:reason out)
              (cond (= fails :probe) :probe-failed
                    (not (gate/fragmented? before)) :not-fragmented
                    (= fails :gc) :gc-failed
                    :else :fragmented))
           (= (:gc? out) (= :fragmented (:reason out)))
           (= gcs (if (:gc? out) 1 0))))))

(golden/deftest-golden gc-hint-golden
  "test/golden/hive-mcp/scheduler/gc-gate-hint.edn"
  (into (sorted-map)
        (for [[label sc] {"compact"     [{:committed (* 2 gb) :used gb} {:committed (* 2 gb) :used gb} :none]
                          "fragmented"  [{:committed (* 6 gb) :used gb} {:committed (* 3 gb) :used (quot gb 2)} :none]
                          "probe-fails" [{:committed 0 :used 0} {:committed 0 :used 0} :probe]
                          "gc-fails"    [{:committed (* 6 gb) :used gb} {:committed (* 6 gb) :used gb} :gc]
                          "zero-used"   [{:committed (* 6 gb) :used 0} {:committed (* 6 gb) :used 0} :none]}]
          [label (run sc)])))

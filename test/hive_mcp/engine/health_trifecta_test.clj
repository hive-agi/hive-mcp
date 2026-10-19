(ns hive-mcp.engine.health-trifecta-test
  "Schema-derived property, golden and mutation facets for the contracted
   health-budget check. A fixture seeds a fixed set of subsystems with known
   counters, so the subject's output depends only on the generated budget."
  (:require [clojure.test :refer [use-fixtures]]
            [hive-mcp.engine.health :as h]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private seeded
  "subsystem-id -> [bytes-delta elapsed-ms restarts]."
  {:sub/idle  [0 0 0]
   :sub/light [5 3 1]
   :sub/heavy [40 25 6]})

(defn- seed-health! []
  (h/reset-all!)
  (doseq [[sid [bytes ms restarts]] seeded]
    (h/record-cycle! sid bytes ms)
    (dotimes [_ restarts] (h/record-restart! sid))))

(use-fixtures :each
  (fn [t] (seed-health!) (try (t) (finally (h/reset-all!)))))

(def ^:private Ceiling [:maybe [:int {:min 0 :max 30}]])

(defn- expected-breaches
  "Independent oracle: the breach vector the docstring promises."
  [sid {:keys [max-alloc-bytes max-cycle-ms max-restarts]}]
  (when-let [[bytes ms restarts] (get seeded sid)]
    (cond-> []
      (and max-alloc-bytes (> bytes max-alloc-bytes)) (conj :alloc-bytes)
      (and max-cycle-ms (> ms max-cycle-ms))          (conj :cycle-ms)
      (and max-restarts (> restarts max-restarts))    (conj :restarts))))

(deftrifecta-from-schema budget-exceeded-trifecta
  hive-mcp.engine.health/budget-exceeded?
  {:in [:cat [:enum :sub/idle :sub/light :sub/heavy :sub/unregistered]
        [:map
         [:max-alloc-bytes {:optional true} Ceiling]
         [:max-cycle-ms {:optional true} Ceiling]
         [:max-restarts {:optional true} Ceiling]
         [:max-cycle-ms-once {:optional true} Ceiling]]]
   :out [:maybe [:vector [:enum :alloc-bytes :cycle-ms :restarts :cycle-ms-once]]]
   :rel (fn [[sid budget] out] (= out (expected-breaches sid budget)))
   :num-tests 300
   :seed 91
   :n-cases 12
   :golden-path "test/golden/engine/health-budget-exceeded.edn"})

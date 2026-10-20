(ns hive-mcp.telemetry.health-contract-test
  "Contract facets for hive-mcp.telemetry.health on the unit classpath.

   The store-backed paths (persist, health-summary, get-recent-errors) need
   hive-datascript, which only the :test-swarm alias carries, so this namespace
   pins what is reachable without a store: validation refuses an unknown type
   or severity BEFORE any metric, channel or store is touched, and the
   HealthEvent / HealthRow schemas accept the documented shapes and reject
   others."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.telemetry.health :as health]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- refusal
  "The ex-data of emit-health-event!'s refusal, sets sorted for a stable golden."
  [event]
  (try (health/emit-health-event! event)
       :not-refused
       (catch clojure.lang.ExceptionInfo e
         (let [d (ex-data e)]
           (-> d
               (update-vals #(if (set? %) (vec (sort %)) %))
               (assoc :message (ex-message e)))))))

(golden/deftest-golden emit-health-event-refusals-golden
  "test/golden/hive-mcp/telemetry/health-refusals.edn"
  {:unknown-type     (refusal {:type :not-a-type :severity :error :message "x"})
   :unknown-severity (refusal {:type :harvest-failed :severity :loud :message "x"})
   :both-unknown     (refusal {:type :nope :severity :nope})})

(golden/deftest-golden health-vocabulary-golden
  "test/golden/hive-mcp/telemetry/health-vocabulary.edn"
  {:severities  (vec (sort health/severities))
   :error-types (vec (sort health/error-types))
   :schema-keys (vec (sort (keys health/health-event-schema)))})

(deftest health-schemas-accept-the-documented-shapes
  (is (m/validate health/HealthEvent {:type :harvest-failed :severity :error :message "m"
                                      :context {:a 1} :recoverable? true}))
  (is (not (m/validate health/HealthEvent {:severity :error})))
  (is (m/validate health/HealthRow {:health-event/id "health-event-1"
                                    :health-event/type :nrepl-timeout
                                    :health-event/timestamp (java.util.Date.)}))
  (is (not (m/validate health/HealthRow {:health-event/type :nrepl-timeout}))))

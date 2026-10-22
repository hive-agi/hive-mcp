(ns hive-mcp.server.routes.metrics-seam-facet-test
  "Facets for hive-mcp.server.routes.metrics-seam: `record!` hands every
   observation to the metrics bridge and answers nil (property), and `timed`
   records :ok / :error and rethrows (golden). Observed through the bridge's
   own injection port, hive-mcp.telemetry.metrics/install-recorder!; the
   previous recorder is restored after each test."
  (:require [clojure.test :refer [use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.server.routes.metrics-seam :as seam]
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

(def ^:private Observation
  [:tuple [:enum [:http :post "/mcp"] [:mcp :tool "kg"] :k] [:int {:min 0}] [:enum :ok :error]])

(defspec record-reaches-the-bridge 100
  (prop/for-all [obs (mg/generator [:vector {:gen/max 6} Observation])]
    (let [seen (atom [])]
      (metrics/install-recorder! (fn [& args] (swap! seen conj (vec args))))
      (let [answers (mapv #(apply seam/record! %) obs)]
        (metrics/install-recorder! nil)
        (and (every? nil? answers)
             (= obs @seen))))))

(defn- timed-trace []
  (let [seen (atom [])]
    (metrics/install-recorder! (fn [k ns oc] (swap! seen conj [k (nat-int? ns) oc])))
    (let [ok (seam/timed [:http :get "/health"] {:status 200})
          raised (try (seam/timed [:http :post "/mcp"] (throw (ex-info "boom" {})))
                      (catch clojure.lang.ExceptionInfo e (ex-message e)))
          recorder-throws (do (metrics/install-recorder! (fn [& _] (throw (ex-info "rec" {}))))
                              (seam/record! [:k] 1 :ok))]
      {:value ok :rethrown raised :recorded @seen :answer-when-recorder-throws recorder-throws})))

(golden/deftest-golden timed-golden
  "test/golden/hive-mcp/server/routes/metrics-seam-timed.edn"
  (timed-trace))

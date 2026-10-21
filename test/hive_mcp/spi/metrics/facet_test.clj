(ns hive-mcp.spi.metrics.facet-test
  "Facets for hive-mcp.spi.metrics (cov-facets f07): a recording IMetrics
   adapter is installed through the facade's install!, the four calls are
   driven through it, and the port laws run against the adapter. The
   late-bound host-namespace path is covered in metrics_test.clj."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.spi.metrics :as metrics]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord RecordingMetrics [seen]
  metrics/IMetrics
  (-inc-events! [_ type severity] (swap! seen conj [:inc-events type severity]) nil)
  (-observe-request-duration! [_ tool seconds] (swap! seen conj [:duration tool seconds]) nil)
  (-set-lings-active! [_ n] (swap! seen conj [:lings n]) nil)
  (-handle-effect! [_ data] (swap! seen conj [:effect data]) nil))

(deftest-golden metrics-facade-golden
  "test/golden/hive-mcp/f07-spi-metrics-facade.edn"
  (let [seen (atom [])]
    (metrics/install! (->RecordingMetrics seen))
    (try
      {:returns [(metrics/inc-events! :agent/spawn :info)
                 (metrics/inc-events! :wave/failed :error)
                 (metrics/observe-request-duration! "memory" 0.25)
                 (metrics/set-lings-active! 3)
                 (metrics/handle-effect! {:counter :wave_failure})
                 (metrics/handle-effect! nil)]
       :calls @seen}
      (finally (metrics/uninstall!)))))

(deftest-port-contract recording-metrics-port-laws
  {:port/methods
   {metrics/-inc-events!               {:in [:cat :keyword [:enum :info :warn :error :fatal]] :out :nil}
    metrics/-observe-request-duration! {:in [:cat [:string {:min 1}] [:double {:min 0 :max 60}]] :out :nil}
    metrics/-set-lings-active!         {:in [:cat [:int {:min 0 :max 50}]] :out :nil}
    metrics/-handle-effect!            {:in [:cat [:maybe [:map [:counter :keyword]]]] :out :nil}}}
  (->RecordingMetrics (atom [])))

(deftest current-is-the-installed-impl
  (let [impl (->RecordingMetrics (atom []))]
    (metrics/install! impl)
    (try (is (identical? impl (metrics/current)))
         (finally (metrics/uninstall!)))))

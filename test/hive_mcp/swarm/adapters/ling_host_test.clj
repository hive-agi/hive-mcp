(ns hive-mcp.swarm.adapters.ling-host-test
  "Facets for hive-mcp.swarm.adapters.ling-host.

   On the unit classpath the host readiness check needs hive-datascript, which
   is absent, so the adapter's own degrade path is what is reachable: the
   readiness port answers {:ready? false :phase :host-error ...} instead of
   throwing, and catchup answers nil. The golden pins those answers against
   the port's Noop; the port law suite runs ILingReadiness with the adapter
   injected over any agent id and spawn mode."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.swarm.adapters.ling-host :as ling-host]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-spi.swarm.ports.ling-host :as lh]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (lh/clear-ling-host!) (try (f) (finally (lh/clear-ling-host!)))))

(def ^:private Readiness
  [:map [:ready? :boolean] [:phase :keyword] [:elapsed-ms :int] [:attempts :int]])

(deftest-golden readiness-and-catchup-golden
  "test/golden/hive-mcp/swarm/adapters/ling-host-answers.edn"
  (let [a (ling-host/make-adapter)]
    {:adapter-readiness (lh/wait-for-ling-ready a "f00-no-such-ling" :headless)
     :noop-readiness    (lh/wait-for-ling-ready lh/noop "f00-no-such-ling" :headless)
     :adapter-catchup   (lh/ling-catchup a {:agent-id "f00-no-such-ling"})
     :noop-catchup      (lh/ling-catchup lh/noop {})}))

(deftest install-fills-the-slot-test
  (let [a (ling-host/install!)]
    (is (identical? a (lh/get-ling-host)))
    (is (lh/ling-host-set?))))

(deftest-port-contract ling-readiness-port-laws
  {:port/methods
   {lh/wait-for-ling-ready {:in  [:cat [:string {:min 1}] [:enum :headless :claude :vterm :bogus]]
                            :out Readiness
                            :rel (fn [_ out] (false? (:ready? out)))}}}
  (ling-host/make-adapter)
  :n 5)

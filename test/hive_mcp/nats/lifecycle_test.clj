(ns hive-mcp.nats.lifecycle-test
  "NatsShutdown is the IShutdownHook (priority 200, client band) that closes
   the NATS connection. The law suite runs against the record itself; with no
   server connected shutdown! is the documented idempotent no-op."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.nats.lifecycle :as nats-lc]
            [hive-mcp.protocols.lifecycle :as lifecycle]
            [hive-mcp.system.registry :as reg]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-port-contract nats-shutdown-port-laws
  {:port/methods
   {lifecycle/shutdown-priority {:in [:cat] :out [:= 200]}
    lifecycle/shutdown-name     {:in [:cat] :out [:= "nats/close"]}
    lifecycle/shutdown!         {:in [:cat [:map [:reason :keyword]]] :out :nil}}}
  (nats-lc/->NatsShutdown))

(defn- nats-hooks []
  (filterv #(instance? hive_mcp.nats.lifecycle.NatsShutdown %)
           (reg/registered-shutdown-hooks)))

(deftest-golden nats-lifecycle-golden
  "test/golden/hive-mcp/f07-nats-lifecycle.edn"
  (let [before (count (nats-hooks))
        _      (nats-lc/register!)
        after  (count (nats-hooks))]
    {:registered-on-load (pos? before)
     :register-idempotent (= before after)
     :hook-name (lifecycle/shutdown-name (first (nats-hooks)))
     :hook-priority (lifecycle/shutdown-priority (first (nats-hooks)))}))

(deftest shutdown-is-safe-twice
  (let [h (nats-lc/->NatsShutdown)]
    (is (nil? (lifecycle/shutdown! h {})))
    (is (nil? (lifecycle/shutdown! h {})))))

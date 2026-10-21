(ns hive-mcp.nats.backbone-test
  "NatsBackbone is an IEventBackbone adapter over hive-mcp.nats.client. Unit
   runs have no NATS server, so the record is exercised in its graceful-
   degradation state: the port laws hold (publish!/unsubscribe! return nil,
   subscribe! answers nil when disconnected, backbone-id is :nats), and the
   lifecycle fns never throw."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.nats.backbone :as nats-bb]
            [hive-mcp.protocols.event-backbone :as eb]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Subject [:string {:min 1 :max 24}])
(def ^:private Handler [:fn {:gen/elements [identity (constantly nil)]} ifn?])

(deftest-port-contract nats-backbone-port-laws
  {:port/methods
   {eb/backbone-id  {:in [:cat] :out [:= :nats]}
    eb/connected?   {:in [:cat] :out [:maybe :boolean]
                     ;; no server in unit runs: never reports connected
                     :rel (fn [_ out] (not out))}
    eb/publish!     {:in [:cat Subject [:map [:x :int]]] :out :nil}
    eb/subscribe!   {:in [:cat Subject Handler] :out :nil}
    eb/unsubscribe! {:in [:cat Subject] :out :nil}}}
  (nats-bb/create-backbone))

(deftest-golden nats-backbone-golden
  "test/golden/hive-mcp/f07-nats-backbone.edn"
  (let [bb (nats-bb/create-backbone)]
    {:record-type   (str (type bb))
     :satisfies?    (satisfies? eb/IEventBackbone bb)
     :backbone-id   (eb/backbone-id bb)
     :stop          (nats-bb/stop-backbone! bb)
     :publish-down  (eb/publish! bb "hive.v1.test" {:a 1})}))

(deftest create-backbone-is-a-fresh-record
  (is (= (nats-bb/create-backbone) (nats-bb/create-backbone)))
  (is (instance? hive_mcp.nats.backbone.NatsBackbone (nats-bb/create-backbone))))

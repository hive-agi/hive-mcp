(ns hive-mcp.hivemind.messaging.local-fanout-test
  "A shout reaches this process's own IDeliveryChannels whether or not the
   event backbone is connected.

   Why: shout! used to publish ONLY to the backbone when it was connected, and
   the bridge drops the backbone's echo of our own publishes, so every
   in-process reader went deaf the moment NATS connected. The routing step now
   fans out locally (marked :via :local-origin) and publishes to the backbone
   in addition; NatsChannel skips the local marker so nothing double-publishes.

   No NATS server, no swarm addon: the backbone is a stub record and the
   fanout / publish boundaries are redefined."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.delivery.channels :as channels]
            [hive-mcp.hivemind.messaging :as msg]
            [hive-mcp.nats.bridge :as bridge]
            [hive-mcp.protocols.delivery-channel :as dc]
            [hive-mcp.protocols.event-backbone :as eb]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private route-shout! @#'msg/route-shout!)

(defn- stub-backbone
  "IEventBackbone whose connectivity is fixed at `connected?`."
  [connected?]
  (reify eb/IEventBackbone
    (backbone-id [_] :stub)
    (connected? [_] connected?)
    (publish! [_ _ _] nil)
    (subscribe! [_ _ _] nil)
    (unsubscribe! [_ _] nil)))

(def ^:private payload
  {:agent-id "ling-1" :event-type :completed :project-id "p"
   :shout-id "s-1" :message "done" :data {}})

(defn- route-capturing
  "Run route-shout! against a backbone with the given connectivity.
   -> {:fanned [payload...] :published [payload...]}"
  [connected?]
  (let [fanned (atom []) published (atom [])]
    (with-redefs [dc/fanout!           (fn [p] (swap! fanned conj p))
                  bridge/publish-shout! (fn [p] (swap! published conj p))]
      (route-shout! (stub-backbone connected?) payload))
    {:fanned @fanned :published @published}))

(deftest connected-backbone-still-fans-out-locally
  (let [{:keys [fanned published]} (route-capturing true)]
    (testing "same-JVM channels see the shout while the backbone is connected"
      (is (= 1 (count fanned)))
      (is (= :local-origin (:via (first fanned))))
      (is (= "s-1" (:shout-id (first fanned)))))
    (testing "the backbone gets exactly one publish, without the local marker"
      (is (= [payload] published)))))

(deftest disconnected-backbone-fans-out-locally-only
  (let [{:keys [fanned published]} (route-capturing false)]
    (is (= [(assoc payload :via :local-origin)] fanned))
    (is (empty? published) "nothing is published to a disconnected backbone")))

;; =============================================================================
;; NatsChannel: never republishes a local or inbound payload
;; =============================================================================

(defn- nats-channel-publishes
  "Payloads NatsChannel hands to the bridge for `event`."
  [event]
  (let [published (atom [])]
    (with-redefs [bridge/publish-shout! (fn [p] (swap! published conj p))]
      (dc/deliver! (channels/create-nats-channel) event))
    @published))

(deftest nats-channel-skips-exempt-origins
  (doseq [via [:local-origin :nats-inbound "local-origin" "nats-inbound"]]
    (testing (str "via " (pr-str via))
      (is (empty? (nats-channel-publishes (assoc payload :via via)))))))

(deftest nats-channel-republishes-unmarked-payloads
  (let [[p :as all] (nats-channel-publishes payload)]
    (is (= 1 (count all)))
    (is (= "ling-1" (:agent-id p)))
    (is (= "done" (:message p)))))

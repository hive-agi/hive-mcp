(ns hive-mcp.swarm.adapters.messaging-test
  "Facets for hive-mcp.swarm.adapters.messaging.

   The adapter is plumbing over hive-mcp.channel.*: every method delegates.
   Its pure delegations are pinned by a property against the host function
   they forward to, the stateful ones by a golden over a fresh project id
   and an explicit clock, and the whole IContextStore / IBroadcastGovernance
   surface by a port law suite with the adapter injected. (emit! has its own
   suite in messaging-emit-test.)"
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.channel.payload-ref :as payload-ref]
            [hive-mcp.swarm.adapters.messaging :as messaging]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-spi.swarm.ports.messaging :as msg]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defspec elide-delegates-to-payload-ref 200
  (prop/for-all [s   gen/string-alphanumeric
                 cap (gen/choose 0 30)]
    (let [put! (fn [_] "ref-1")]
      (= (payload-ref/elide! s cap put!)
         (msg/elide! (messaging/make-adapter) s cap put!)))))

(defn- governance-round-trip
  "Broadcast accounting through the adapter on a project id no other test
   uses, with an explicit clock so the window is deterministic."
  []
  (let [a      (messaging/make-adapter)
        p      (str "f00-messaging-" (System/nanoTime))
        ;; sequenced in the let: a large map literal evaluates in hash order
        before (msg/spent-recently a p 1000)
        _      (msg/record-broadcast! a p 1000)
        _      (msg/record-broadcast! a p 2000)
        after  (msg/spent-recently a p 2500)]
    {:before  before
     :after   after
     :refused (msg/apply-policy a {:broadcast? true} {})
     :short   (msg/elide! a "short" (fn [_] "r"))
     :cut     (msg/elide! a (apply str (repeat 50 "x")) 10 (fn [_] "r"))
     :conv    (msg/fetch-conversation a "f00-no-such-context" nil)
     :reader? (msg/coordinator-reader? a "f00-no-such-reader")
     :session (msg/coordinator-session a "f00-no-such-reader")}))

(deftest-golden governance-and-context-golden
  "test/golden/hive-mcp/swarm/adapters/messaging-governance.edn"
  (governance-round-trip))

(deftest frontend-status-and-ids-test
  (let [a (messaging/make-adapter)]
    (is (m/validate [:map [:channel-connected? :boolean] [:ws-connected? :boolean] [:ws-clients :int]]
                    (msg/frontend-status a)))
    (is (not= (msg/new-conversation-id a) (msg/new-conversation-id a)))
    (is (vector? (msg/delivery-channels a)))))

(deftest-port-contract messaging-port-laws
  {:port/methods
   {msg/elide!              {:in  [:cat :string [:int {:min 0 :max 40}] [:= {} (fn [_] "ref")]]
                             :out [:map [:text :string] [:ref {:optional true} :string]]
                             :rel (fn [[s cap _] out]
                                    (if (> (count s) cap)
                                      (= "ref" (:ref out))
                                      (= {:text s} out)))}
    msg/fetch-conversation  {:in [:cat [:string {:min 1}] [:maybe [:map [:limit {:optional true} pos-int?]]]]
                             :out [:sequential :any]}
    msg/coordinator-reader? {:in [:cat [:string {:min 1}]] :out :boolean}}}
  (messaging/make-adapter))

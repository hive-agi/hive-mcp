(ns hive-mcp.protocols.event-backbone-contract-test
  "Contract facets for the event-backbone slot, its watches and the headless
   fallback. Every test owns the slot and puts the prior backbone back."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.protocols.event-backbone :as eb]
            [hive-mcp.protocols.registry :as reg]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden])
  (:import [hive_mcp.protocols.event_backbone NoopBackbone]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-the-slot [f]
  (let [prior (when (eb/backbone-set?) (eb/get-backbone))]
    (eb/clear-backbone!)
    (try (f)
         (finally (eb/clear-backbone!)
                  (when prior (eb/set-backbone! prior))))))

(use-fixtures :each own-the-slot)

(defrecord StubBackbone [id]
  eb/IEventBackbone
  (backbone-id [_] id)
  (connected? [_] true)
  (publish! [_ _ _] true)
  (subscribe! [_ subject _] {:subject subject})
  (unsubscribe! [_ _] nil))

(deftrifecta-from-schema noop-backbone-is-disconnected
  #'eb/noop-backbone
  {:in [:cat]
   :out eb/Backbone
   :rel (fn [_ out] (and (instance? NoopBackbone out) (= :noop (eb/backbone-id out))
                         (false? (eb/connected? out))
                         (nil? (eb/subscribe! out "s" identity))))
   :num-tests 10})

;; The watch fn is a generated keyword (an ifn); clear-backbone! on an empty
;; slot never transitions it, so it is registered and removed without firing.
(deftrifecta-from-schema watches-return-the-slot-and-are-removable
  #'eb/add-backbone-watch!
  {:in [:cat [:enum ::w1 ::w2 ::w3] ifn?]
   :out reg/SlotValue
   :rel (fn [[k _] out]
          (let [had? (contains? (.getWatches ^clojure.lang.IRef (:state out)) k)]
            (eb/remove-backbone-watch! k)
            (and had? (not (contains? (.getWatches ^clojure.lang.IRef (:state out)) k)))))
   :num-tests 30})

(golden/deftest-golden slot-lifecycle-golden
  "test/golden/hive-mcp/protocols/event-backbone-lifecycle.edn"
  (let [seen (atom [])
        _ (eb/add-backbone-watch! ::m17 (fn [old new] (swap! seen conj [(some-> old eb/backbone-id)
                                                                         (some-> new eb/backbone-id)])))
        before {:set? (eb/backbone-set?) :id (eb/backbone-id (eb/get-backbone))}
        installed (eb/set-backbone! (->StubBackbone :m17-nats))
        during {:set? (eb/backbone-set?) :id (eb/backbone-id (eb/get-backbone))
                :same? (identical? installed (eb/get-backbone))
                :subscribed (eb/subscribe! (eb/get-backbone) "hive.events" identity)}
        cleared (eb/clear-backbone!)
        _ (eb/remove-backbone-watch! ::m17)
        _ (eb/set-backbone! (->StubBackbone :m17-after-unwatch))]
    {:before before :during during :cleared cleared
     :transitions @seen}))

(deftest set-rejects-a-non-backbone
  (is (thrown? AssertionError (eb/set-backbone! "nats://nowhere")))
  (is (false? (eb/backbone-set?))))

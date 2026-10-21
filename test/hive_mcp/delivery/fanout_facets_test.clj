(ns hive-mcp.delivery.fanout-facets-test
  "Golden over isolated delivery fanout. The IDeliveryChannel port is
   satisfied by stub channels registered through the channel registry, one
   per outcome arm: delivers, unavailable, available? throws, deliver!
   throws, deliver! hangs past the timeout. A second policy with
   :max-failures 1 drives the throwing channel's breaker open. wrap-envelope
   gets its own golden over the id and idempotence arms."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.delivery.fanout :as fanout]
            [hive-mcp.protocols.delivery-channel :as dc]
            [hive-test.golden :refer [deftest-golden deftest-golden-fn]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord StubChannel [id mode seen]
  dc/IDeliveryChannel
  (channel-id [_] id)
  (available? [_]
    (case mode
      :unavailable false
      :avail-throws (throw (ex-info "probe failed" {}))
      true))
  (deliver! [_ event]
    (case mode
      :throws (throw (ex-info "write failed" {}))
      ;; longer than the default 500 ms per-channel timeout too
      :hangs  (do (Thread/sleep 1500) :late)
      (do (swap! seen conj (:kind event)) :delivered))))

(defn- with-channels [chans f]
  (let [before (vec (dc/get-channels))]
    (dc/clear-channels!)
    (fanout/reset-breakers!)
    (try
      (run! dc/register-channel! chans)
      (f)
      (finally
        (dc/clear-channels!)
        (fanout/reset-breakers!)
        (run! dc/register-channel! before)))))

(use-fixtures :each (fn [f] (fanout/reset-breakers!) (f) (fanout/reset-breakers!)))

(def ^:private policy {:per-channel-timeout-ms 100 :availability-timeout-ms 100})

(defn- scenario []
  (let [seen  (atom [])
        chans [(->StubChannel :ok :ok seen)
               (->StubChannel :unavailable :unavailable seen)
               (->StubChannel :avail-throws :avail-throws seen)
               (->StubChannel :throws :throws seen)
               (->StubChannel :hangs :hangs seen)]]
    (with-channels chans
      (fn []
        (let [first-run (fanout/fanout! {:kind :one} policy)
              strict    (assoc policy :max-failures 1)
              second    (fanout/fanout! {:kind :two} strict)
              third     (fanout/fanout! {:kind :three} strict)]
          {:first        (into (sorted-map) first-run)
           :second       (into (sorted-map) second)
           :breaker-open (into (sorted-map) third)
           :delivered    @seen
           :via-protocol (into (sorted-map) (do (fanout/reset-breakers!) (dc/fanout! {:kind :four})))
           :empty        (with-channels [] #(fanout/fanout! {:kind :none}))})))))

(deftest-golden-fn fanout-outcomes-golden
  "test/golden/delivery/fanout-outcomes.edn"
  scenario)

(deftest-golden wrap-envelope-golden
  "test/golden/delivery/fanout-wrap-envelope.edn"
  (let [given    (fanout/wrap-envelope {:k 1} "parent-1")
        fresh    (fanout/wrap-envelope {:k 1})
        rewrap   (fanout/wrap-envelope given "other")]
    {:given-id     (::fanout/id given)
     :ts-is-int    (int? (::fanout/ts given))
     :fresh-id     (string? (::fanout/id fresh))
     :idempotent   (= given rewrap)
     :payload-kept (:k given)}))

(deftest reset-breakers-clears-snapshot
  (with-channels [(->StubChannel :throws :throws (atom []))]
    (fn []
      (fanout/fanout! {:kind :x} policy)
      (is (seq (fanout/breaker-snapshot)))
      (fanout/reset-breakers!)
      (is (empty? (fanout/breaker-snapshot))))))

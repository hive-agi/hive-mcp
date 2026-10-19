(ns hive-mcp.protocols.delivery-channel-contract-test
  "Contract facets for the delivery-channel registry and fanout. Each test owns
   the registry: it starts with three stub channels and the prior channels are
   registered again afterwards."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.protocols.delivery-channel :as dc]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord StubChannel [id up? seen]
  dc/IDeliveryChannel
  (channel-id [_] id)
  (available? [_] up?)
  (deliver! [_ event] (swap! seen conj (:type event)) nil))

(def ^:private stub-ids [:m17-alpha :m17-beta :m17-gamma])

(defn- own-the-registry [f]
  (let [prior (vec (dc/get-channels))]
    (dc/clear-channels!)
    (doseq [id stub-ids] (dc/register-channel! (->StubChannel id (not= id :m17-beta) (atom []))))
    (try (f)
         (finally (dc/clear-channels!)
                  (run! dc/register-channel! prior)))))

(use-fixtures :each own-the-registry)

(deftrifecta-from-schema get-channel-finds-what-was-registered
  #'dc/get-channel
  {:in [:cat [:enum :m17-alpha :m17-beta :m17-gamma :m17-absent]]
   :out [:maybe dc/Channel]
   :rel (fn [[id] out] (if (= id :m17-absent) (nil? out) (= id (dc/channel-id out))))
   :num-tests 50})

(deftrifecta-from-schema noop-channel-is-never-available
  #'dc/noop-channel
  {:in [:cat]
   :out dc/Channel
   :rel (fn [_ out] (and (= :noop (dc/channel-id out)) (false? (dc/available? out))
                         (nil? (dc/deliver! out {:type :x}))))
   :num-tests 10})

(golden/deftest-golden registry-lifecycle-golden
  "test/golden/hive-mcp/protocols/delivery-channel-lifecycle.edn"
  (let [ids #(set (map dc/channel-id (dc/get-channels)))
        start (ids)
        replaced (dc/channel-id (dc/register-channel! (->StubChannel :m17-alpha false (atom []))))
        alpha-up? (dc/available? (dc/get-channel :m17-alpha))
        unregistered (dc/unregister-channel! :m17-gamma)
        after-unregister (ids)
        cleared (dc/clear-channels!)]
    {:start start :replaced replaced :alpha-up-after-replace alpha-up?
     :unregistered unregistered :after-unregister after-unregister
     :cleared cleared :after-clear (dc/get-channels)}))

(golden/deftest-golden fanout-delivers-to-available-channels-golden
  "test/golden/hive-mcp/protocols/delivery-channel-fanout.edn"
  (let [outcomes (dc/fanout! {:type :m17/ping})]
    {:outcomes (into (sorted-map) (select-keys outcomes stub-ids))
     :seen (into (sorted-map)
                 (for [id stub-ids] [id @(:seen (dc/get-channel id))]))}))

(deftest register-rejects-a-non-channel
  (is (thrown? AssertionError (dc/register-channel! {:id :not-a-channel}))))

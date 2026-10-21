(ns hive-mcp.memory.write-events-test
  "The deprecated write-events facade: ->event / event->write as a trifecta
   over every write op (round trip through the kernel vocabulary), and the
   listener registry driven through a recording listener."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.memory.write-events :as mwe]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Op [:enum :added :updated :deleted])

(def ^:private Payload
  [:map
   [:id :string]
   [:memory-type {:optional true} :string]
   [:tags {:optional true} [:vector :string]]
   [:project-id {:optional true} :string]
   [:junk {:optional true} :int]])

(deftrifecta-from-schema to-event-keeps-payload-keys-and-types-the-op
  #'mwe/->event
  {:in [:cat Op Payload]
   :out :map
   :rel (fn [[op payload] out]
          (and (= (mwe/op->channel-type op) (:type out))
               (= (dissoc payload :junk) (dissoc out :type))
               ;; inverse: the channel event reads back as the same write
               (= (assoc (dissoc payload :junk) :op op) (mwe/event->write out))))
   :classify (fn [[op] _] op)
   :classify-domain #{:added :updated :deleted}
   :golden-path "test/golden/hive-mcp/memory/write-events-to-event.edn"
   :num-tests 100})

(deftest-golden listener-registry-golden
  "test/golden/hive-mcp/memory/write-events-listeners.edn"
  (let [seen (atom [])
        reg  (mwe/register-listener! ::f04-recorder (fn [& args] (swap! seen conj (vec args))))
        ks   (contains? (mwe/listener-keys) ::f04-recorder)
        ret  (mwe/notify! :updated {:id "m1" :tags ["x"]})
        un   (mwe/unregister-listener! ::f04-recorder)
        _    (mwe/notify! :deleted {:id "m2"})]
    {:registered reg :listed? ks :notify ret :unregister un
     :seen @seen :after? (contains? (mwe/listener-keys) ::f04-recorder)
     :non-write (mwe/event->write {:type :something-else :id "x"})
     :channel-types mwe/channel-types}))

(deftest listener-keys-is-a-set
  (is (set? (mwe/listener-keys))))

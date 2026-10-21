(ns hive-mcp.swarm.adapters.inbox.piggyback-test
  "Facets for hive-mcp.swarm.adapters.inbox.piggyback.

   The sink's port is hive-spi.swarm.ports.messaging/IInboxSink; its
   transport is hive-mcp.channel.piggyback's instruction queue, which this
   suite reads back with drain-instructions! on recipients no other test
   uses:

     property  accepts? is exactly 'a non-blank string'
     golden    an envelope delivered through the sink, drained from the queue
     port      IInboxSink law suite with the sink injected"
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.channel.piggyback :as channel-piggyback]
            [hive-mcp.swarm.adapters.inbox.piggyback :as piggyback]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-spi.swarm.ports.messaging :as msg]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defspec accepts-every-non-blank-string-recipient 200
  (prop/for-all [r (gen/one-of [gen/string (gen/elements ["" " " "\t" nil :kw 42 "ling-a"])])]
    (= (and (string? r) (not (str/blank? r)))
       (msg/accepts? (piggyback/make-sink) r))))

(defn- deliver-and-drain
  "Deliver ENVELOPE to a fresh recipient through the sink; the receipt's
   shape and what the recipient's queue holds afterwards."
  [envelope]
  (let [to      (str "f00-piggyback-" (System/nanoTime))
        receipt (msg/-deliver! (piggyback/make-sink) (assoc envelope :inbox/to to))]
    {:receipt-valid? (m/validate piggyback/Receipt receipt)
     :delivered?     (:delivered? receipt)
     :queued         (channel-piggyback/drain-instructions! to)
     :drained-again  (channel-piggyback/drain-instructions! to)}))

(deftest-golden deliver-golden
  "test/golden/hive-mcp/swarm/adapters/inbox-piggyback-deliver.edn"
  {:plain     (deliver-and-drain {:inbox/body "hello"})
   :full      (deliver-and-drain {:inbox/body "do x" :inbox/kind :task :inbox/from "coord"
                                  :inbox/context-id "ctx-1" :inbox/attrs {:a 1}})
   :non-string-body (deliver-and-drain {:inbox/body 42 :inbox/kind :note})})

(deftest sink-identity-test
  (let [s (piggyback/make-sink)]
    (is (= :piggyback (msg/sink-id s)))
    (is (= 1000 (:inbox/priority (meta s))))))

(deftest-port-contract piggyback-sink-port-laws
  {:port/methods
   {msg/sink-id  {:in [:cat] :out [:= :piggyback]}
    msg/accepts? {:in  [:cat [:or :string :nil :keyword]]
                  :out :boolean
                  :rel (fn [[r] out] (= out (and (string? r) (not (str/blank? r)))))}}}
  (piggyback/make-sink))

(ns hive-mcp.await.sources.manual
  "The :manual await source: events are injected by hand (`swarm await
   fire`, or `fire!` from a test). It is the reference source: every other
   source (:ling, :inbox, :broker) does what this one does, with a real
   producer in place of `fire!`.

   Subscription: {:type :manual :filter {..} :dedup-key k}. An injected
   event is {:type :manual :data <map>}; the awaiter's filter and dedup are
   applied by the pure step, not here."
  (:require [hive-mcp.await.ports :as ports]
            [malli.core :as m]
            [hive-mcp.await.domain :as domain]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private listeners
  ;; {awaiter-id deliver!}: who an injected event is offered to.
  (atom {}))

(defmethod ports/open-source :manual
  [_subscription awaiter deliver!]
  (let [id (:id awaiter)]
    (swap! listeners assoc id deliver!)
    (fn close [] (swap! listeners dissoc id))))

(defn listening
  "The awaiter ids the :manual source currently delivers to."
  []
  (vec (sort (keys @listeners))))

(m/=> listening [:=> [:cat] [:vector :string]])

(defn fire!
  "Inject one :manual event with `data` (a map). With `awaiter-id`, offer
   it to that awaiter only; otherwise to every listening awaiter (each
   one's filter decides). Answers the ids it was offered to."
  ([data] (fire! data nil))
  ([data awaiter-id]
   (let [event   {:type :manual :data (or data {})}
         targets (if awaiter-id
                   (select-keys @listeners [(str awaiter-id)])
                   @listeners)]
     (doseq [[_ deliver!] targets] (deliver! event))
     (vec (sort (keys targets))))))

(m/=> fire! [:function
             [:=> [:cat [:maybe :map]] [:vector :string]]
             [:=> [:cat [:maybe :map] [:maybe domain/AwaiterId]] [:vector :string]]])

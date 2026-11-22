(ns hive-mcp.await.sources.manual
  "The :manual await source: events are injected by hand (`swarm await
   fire`, or `fire!` from a test). It is the reference source: every other
   source (:ling, :inbox, :broker) does what this one does, with a real
   producer in place of `fire!`.

   Subscription: {:type :manual :filter {..} :dedup-key k}. An injected
   event is {:type :manual :data <map>}; the awaiter's filter and dedup are
   applied by the pure step, not here.

   Isolation: a fire that names no awaiter (a broadcast) is offered only to
   awaiters whose filter is non-empty. An unfiltered awaiter matches every
   event, so it hears only a fire that names its id; otherwise one
   coordinator's broadcast would wake every other coordinator's lings."
  (:require [hive-mcp.await.ports :as ports]
            [malli.core :as m]
            [hive-mcp.await.domain :as domain]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private listeners
  ;; {awaiter-id {:deliver! f :broadcast? bool}}: who an injected event is
  ;; offered to. :broadcast? is true when the awaiter's subscription carries
  ;; a non-empty filter, so a fire that names no awaiter may be offered to it.
  (atom {}))

(defmethod ports/open-source :manual
  [subscription awaiter deliver!]
  (let [id (:id awaiter)]
    (swap! listeners assoc id {:deliver!   deliver!
                               :broadcast? (boolean (seq (:filter subscription)))})
    (fn close [] (swap! listeners dissoc id))))

(defn listening
  "The awaiter ids the :manual source currently delivers to."
  []
  (vec (sort (keys @listeners))))

(m/=> listening [:=> [:cat] [:vector :string]])

(defn fire!
  "Inject one :manual event with `data` (a map). With `awaiter-id`, offer
   it to that awaiter only. Without one (a broadcast), offer it only to the
   listening awaiters that carry a filter, and each one's filter decides. An
   awaiter with a nil or empty filter would match ANY broadcast, so it is
   offered only a fire that names its id (friction 20261122150418-7fef4903:
   one coordinator's fire woke every other coordinator's unfiltered lings).
   Answers the ids it was offered to."
  ([data] (fire! data nil))
  ([data awaiter-id]
   (let [event   {:type :manual :data (or data {})}
         targets (if awaiter-id
                   (select-keys @listeners [(str awaiter-id)])
                   (into {} (filter (fn [[_ l]] (:broadcast? l))) @listeners))]
     (doseq [[_ l] targets] ((:deliver! l) event))
     (vec (sort (keys targets))))))

(m/=> fire! [:function
             [:=> [:cat [:maybe :map]] [:vector :string]]
             [:=> [:cat [:maybe :map] [:maybe domain/AwaiterId]] [:vector :string]]])

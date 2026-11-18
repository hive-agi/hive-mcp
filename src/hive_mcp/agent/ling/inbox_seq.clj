(ns hive-mcp.agent.ling.inbox-seq
  "Per-ling sequence numbers for the messages hive-mcp dispatches to a ling.

   A headless ling is reached over two routes: its NATS inbox and the
   in-process :headless-ling hand-off. While NATS blips the :delivery chain
   falls back from one to the other, so a message sent second could reach
   the ling's loop first. Every dispatch is therefore stamped, at send time,
   with

     {:epoch e :seq n :after m}

   E names this host process (seqs restart with it), N numbers the message
   among the ling's dispatches, and M is the highest seq of the ling whose
   send had already been ACCEPTED (published to the inbox, or queued by the
   headless backend) when N was stamped, nil when none had. The ling's loop
   (hive-agent.loop.inbox-order) holds N back until M was handed off to it,
   whichever route M took. A send that fails is never accepted, so no later
   message waits for it; two sends in flight at once name no order between
   them, and none is imposed.

   The stamp rides in the dispatch task opts under :ling-seq, and in the
   NATS envelope's :meta under :ling-seq."
  (:require [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def LingSeq
  [:map
   [:epoch :string]
   [:seq :int]
   [:after [:maybe :int]]])

(defonce ^{:doc "This host process's epoch: seqs are compared only within it."}
  epoch
  (str (java.util.UUID/randomUUID)))

(defonce ^:private state
  ;; ling-id -> {:next n :accepted m}
  (atom {}))

(defn stamp!
  "The next ling sequence stamp of LING-ID."
  [ling-id]
  (let [[_ after] (swap-vals! state update ling-id
                              (fn [{:keys [next] :as s}]
                                (assoc s :next (inc (or next 0)))))
        {:keys [next accepted]} (get after ling-id)]
    {:epoch epoch :seq next :after accepted}))

(defn accepted!
  "Record that the send stamped STAMP was accepted by its route, so later
   stamps of LING-ID name it (or a later one) as their predecessor."
  [ling-id {:keys [seq] :as stamp}]
  (when (and (int? seq) (= epoch (:epoch stamp)))
    (swap! state update ling-id
           (fn [{:keys [accepted] :as s}]
             (assoc s :accepted (max seq (or accepted 0))))))
  nil)

(defn forget!
  "Drop LING-ID's counters (the ling is gone)."
  [ling-id]
  (swap! state dissoc ling-id)
  nil)

(m/=> stamp! [:=> [:cat :any] LingSeq])
(m/=> accepted! [:=> [:cat :any :any] :nil])
(m/=> forget! [:=> [:cat :any] :nil])

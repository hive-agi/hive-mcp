(ns hive-mcp.system.sweepers.async-result
  "Periodic reclamation for the async result buffers.

   `hive-mcp.channel.async-result/buffers` is only ever emptied by `drain!`,
   and `drain!` drops a buffer solely when the caller has taken everything in
   it. A caller that disconnects mid-batch therefore leaves its entries in the
   atom for the lifetime of the JVM. `gc-expired!` is the reclamation half, and
   this is the thing that calls it.

   DORMANT AS WRITTEN. Registering a sweep is not the same as running one:
   `hive-mcp.system.sweep-coordinator/start!` has no caller anywhere in the
   source tree, so the coordinator never starts, and this namespace is not
   required by anything, so even the registration below never executes. Both
   gaps are tracked on the kanban card. The code is here, correct and tested,
   so that turning the subsystem on is a wiring decision rather than a
   development one."
  (:require [hive-mcp.channel.async-result :as async-result]
            [hive-mcp.protocols.lifecycle :as lifecycle]
            [hive-mcp.system.registry :as reg]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const sweep-interval-s
  "How often the buffers are swept, in seconds.

   Matched to `async-result/ttl-seconds`, because that is the age at which a
   DELIVERED entry becomes reclaimable and so the shortest interval at which
   this sweep has anything to do. Undelivered entries live far longer and do
   not need a faster cadence."
  300)

(defrecord AsyncResultSweep [gc-fn]
  lifecycle/ISweepable
  (sweep-interval-s [_] sweep-interval-s)
  (sweep-name [_] "channels/async-result-gc")
  (sweep! [_ _ctx]
    ;; `gc-fn` is a constructor-injected seam, nil in production. A test needs
    ;; to choose what the reclaim does (throw, return a count) and the record
    ;; can simply hold the collaborator, which beats `with-redefs` standing in
    ;; for one (20260726172005-562432b9).
    (let [gc (or gc-fn async-result/gc-expired!)]
      (try
        {:swept (gc) :errors []}
        (catch Throwable t
          ;; Caught here as well as in the coordinator: the coordinator's catch
          ;; keeps the heartbeat alive but discards the outcome, and a sweep
          ;; that reports {:swept 0 :errors [...]} is distinguishable from one
          ;; that genuinely had nothing to do. A silent zero is not.
          (log/error t "async-result sweep: gc-expired! threw")
          {:swept 0 :errors [{:sweep "channels/async-result-gc"
                              :error (str (.getName (class t)) ": " (.getMessage t))}]})))))

(defonce ^:private -registered?
  (do (reg/register-sweep! (->AsyncResultSweep nil)) true))

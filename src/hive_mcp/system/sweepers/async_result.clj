(ns hive-mcp.system.sweepers.async-result
  "Periodic reclamation for the async result buffers.

   `hive-mcp.channel.async-result/buffers` is only ever emptied by `drain!`,
   and `drain!` drops a buffer solely when the caller has taken everything in
   it. A caller that disconnects mid-batch therefore leaves its entries in the
   atom for the lifetime of the JVM. `gc-expired!` is the reclamation half, and
   this is the thing that calls it.

   LIVE since `:hive/sweep-coordinator` was added to system.edn. Registering
   a sweep is not the same as running one, and neither is requiring the
   namespace: the `defonce` below only fires because the coordinator's
   init-key requires this namespace by name, and the coordinator only sweeps
   because an Integrant key starts it. Drop either and this sweep goes quiet
   with no other symptom. Confirmed on the live process 2026-09-16: the
   coordinator's :run-counts carried \"channels/async-result-gc\"."
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

(def ^:private -registered?
  "Auto-registration. `def`, deliberately NOT `defonce`.

   Registration is keyed by sweep-name and overwrites, so a repeated load
   cannot duplicate this sweep. `defonce` only guaranteed that a hot reload
   would keep running the OLD code, and that an unregister could never be
   undone (kanban 20260916134011-1246379c)."
  (do (reg/register-sweep! (->AsyncResultSweep nil)) true))

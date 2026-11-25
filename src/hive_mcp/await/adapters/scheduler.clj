(ns hive-mcp.await.adapters.scheduler
  "Adapter: the process IPollScheduler polling await sources run on.

   A small pool of daemon threads (no await thread can hold the JVM open)
   and FIXED-DELAY scheduling, so a slow probe (a query near its
   statement_timeout, a kubectl call near its timeout) never stacks a second
   probe of the same awaiter behind it. Created on first use."
  (:require [hive-mcp.await.sources.boundary :as b]
            [malli.core :as m])
  (:import (java.util.concurrent Executors ScheduledExecutorService
                                 ScheduledFuture ThreadFactory TimeUnit)
           (java.util.concurrent.atomic AtomicLong)))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def pool-size
  "Threads shared by every polling awaiter of the process."
  2)

(defn- daemon-factory []
  (let [n (AtomicLong. 0)]
    (reify ThreadFactory
      (newThread [_ r]
        (doto (Thread. ^Runnable r (str "hive-await-poll-" (.incrementAndGet n)))
          (.setDaemon true))))))

(defrecord ExecutorScheduler [^ScheduledExecutorService executor]
  b/IPollScheduler
  (schedule-repeating! [_ f interval-ms]
    (let [^ScheduledFuture fut
          (.scheduleWithFixedDelay executor ^Runnable (fn [] (f))
                                   0 (long interval-ms) TimeUnit/MILLISECONDS)]
      (fn cancel [] (.cancel fut false) nil))))

(defn make-scheduler
  "A scheduler over its own pool of `n` daemon threads."
  ([] (make-scheduler pool-size))
  ([n] (->ExecutorScheduler (Executors/newScheduledThreadPool (int n) (daemon-factory)))))

(m/=> make-scheduler [:function [:=> [:cat] :any] [:=> [:cat pos-int?] :any]])

(defonce ^:private process (delay (make-scheduler)))

(defn process-scheduler
  "The process scheduler, created on first use."
  []
  @process)

(m/=> process-scheduler [:=> [:cat] :any])

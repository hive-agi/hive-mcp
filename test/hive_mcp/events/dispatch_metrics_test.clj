(ns hive-mcp.events.dispatch-metrics-test
  "`dispatch` records `[:event/dispatch event-id]` through the late-bound
   hive-ttracking metrics port, observed with a recording stub sink."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.events.dispatch :as dispatch]
            [hive.events.router :as router]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- port
  "The metrics port's [install! current-sink observe!-protocol-var], or nil."
  []
  (try
    (when-let [install (requiring-resolve 'hive-ttracking.metrics/install!)]
      [install (requiring-resolve 'hive-ttracking.metrics/current-sink)
       (requiring-resolve 'hive-ttracking.metrics/IMetricsSink)])
    (catch Throwable _ nil)))

(defn- recording-sink
  "A stub IMetricsSink appending [k outcome] to `log`. Built with eval because
   the port is bound late, so the protocol cannot be named at compile time."
  [proto-var log]
  (let [psym (symbol (str (:ns (meta proto-var))) (str (:name (meta proto-var))))]
    ((eval `(fn [l#]
              (reify ~psym
                (~'observe! [~'_ k# ~'_ns outcome#] (swap! l# conj [k# outcome#]) nil)
                (~'snapshot [~'_] {}))))
     log)))

(defn- with-recording
  [f]
  (if-let [[install current proto] (port)]
    (let [log  (atom [])
          prev (current)]
      (install (recording-sink proto log))
      (try (f) @log
           (finally (install prev))))
    (do (f) ::no-port)))

(deftest record-dispatch-is-total
  (testing "recording never throws and answers nil, with or without the port"
    (is (nil? (dispatch/record-dispatch! :x/y (System/nanoTime) :ok)))))

(deftest dispatch-records-ok-and-error
  (let [id  :metrics-test/ok
        log (with-recording
              (fn []
                (router/reg-event-fx id (fn [_ _] {}))
                (try
                  (dispatch/dispatch [id {}])
                  (finally (router/clear-event id)))
                (is (thrown? Exception (dispatch/dispatch [:metrics-test/unregistered {}])))))]
    (when (vector? log)
      (is (some #{[[:event/dispatch id] :ok]} log))
      (is (some #{[[:event/dispatch :metrics-test/unregistered] :error]} log)))))

(ns hive-mcp.nrepl.classloader-gc.facet-test
  "Facets for hive-mcp.nrepl.classloader-gc (cov-facets f07). The middleware is
   driven with an injected nREPL Transport stub (recording sends) and a stub
   downstream handler that answers like nREPL's session middleware: a session-
   less eval gets a fresh session id on its first response. No socket is
   opened. The golden pins the pin -> reuse -> rotate -> re-pin sequence."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.nrepl.classloader-gc :as gc]
            [hive-test.golden :refer [deftest-golden]]
            [nrepl.transport :as transport]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord RecordingTransport [sent]
  transport/Transport
  (recv [_] nil)
  (recv [_ _timeout] nil)
  (send [this response] (swap! sent conj response) this))

(defn- session-middleware-stub
  "Downstream handler faithful to nREPL's session middleware for this path:
   a request without :session gets a NEW id (s-<n>) on its response; a request
   carrying :session answers on that session."
  [seen]
  (let [n (atom 0)]
    (fn [{:keys [session transport] :as msg}]
      (swap! seen conj (select-keys msg [:op :session :id]))
      (when transport
        (transport/send transport
                        {:id (:id msg)
                         :session (or session (str "s-" (swap! n inc)))})))))

(defn- drive
  "Run msgs through wrap-shared-classloader from a freshly rotated state with
   rotation threshold `threshold`; answer what the downstream saw, what the
   client transport got, and the status after each message."
  [threshold msgs]
  (let [seen (atom []) sent (atom [])
        client (->RecordingTransport sent)
        h (gc/wrap-shared-classloader (session-middleware-stub seen))]
    (gc/set-rotation-threshold! threshold)
    (gc/force-rotate!)
    (try
      (let [statuses (mapv (fn [m]
                             (h (assoc m :transport client))
                             (select-keys (gc/status)
                                          [:shared-session-id :eval-count]))
                           msgs)]
        {:downstream @seen :client-got @sent :status statuses})
      (finally
        (gc/set-rotation-threshold! 10000)
        (gc/force-rotate!)))))

(deftest-golden shared-classloader-golden
  "test/golden/hive-mcp/f07-classloader-gc-middleware.edn"
  {:pin-reuse-rotate
   (drive 3 [{:op "eval" :id "1"}
             {:op "eval" :id "2"}
             {:op "eval" :id "3"}
             {:op "eval" :id "4"}
             {:op "eval" :id "5"}])
   :explicit-session-passthrough
   (drive 3 [{:op "eval" :id "a" :session "cider-1"}
             {:op "describe" :id "b"}])
   :rotation-disabled
   (drive 0 [{:op "eval" :id "x"} {:op "eval" :id "y"} {:op "eval" :id "z"}])})

(deftest-golden classloader-gc-status-golden
  "test/golden/hive-mcp/f07-classloader-gc-status.edn"
  (let [_ (gc/force-rotate!)
        st (gc/status)
        cl (gc/count-dynamic-classloaders)]
    {:status-keys (sort (keys st))
     :rotated-session (:shared-session-id st)
     :rotated-count (:eval-count st)
     :cl-keys (sort (keys cl))
     :cl-depth-positive (pos? (:chain-depth cl))
     :cl-count-bounded (<= 0 (:dynamic-classloader-count cl) (:chain-depth cl))}))

(deftest set-threshold-is-observable
  (try
    (gc/set-rotation-threshold! 7)
    (is (= 7 (:rotation-threshold (gc/status))))
    (finally (gc/set-rotation-threshold! 10000))))

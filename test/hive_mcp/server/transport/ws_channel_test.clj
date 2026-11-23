(ns hive-mcp.server.transport.ws-channel-test
  "Facets for the WebSocket channel starter and its option builder, plus the
   contract-coverage gate for server/transport.

   The healing starter is only driven with a monitor that is already set, so
   no go-loop is spawned, and with a port the socket layer refuses, so the
   initial start fails inside its rescue."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.channel.inbound :as inbound]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.source :as src]
            [hive-mcp.server.transport.schema :as ts]
            [hive-mcp.server.transport.ws-channel :as wsc]
            [hive-mcp.channel.websocket]
            [taoensso.timbre]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema channel-opts-route-client-messages
  #'wsc/channel-opts
  {:in [:cat ts/Port]
   :out wsc/ChannelOpts
   :rel (fn [[port] out]
          (and (= port (:port out))
               (= inbound/handle-client-message! (:on-message out))))
   :num-tests 100})

(golden/deftest-golden channel-opts-golden
  "test/golden/hive-mcp/server/transport/channel-opts.edn"
  ;; :on-message is a fn and does not print readably; the golden pins
  ;; whether it is the inbound handler
  (into (sorted-map)
        (for [port [nil 0 9999 7911]]
          [(str port) (update (wsc/channel-opts port) :on-message
                              #(= inbound/handle-client-message! %))])))

(golden/deftest-golden healing-start-with-a-live-monitor-golden
  "test/golden/hive-mcp/server/transport/ws-channel-healing.edn"
  (let [monitor (atom ::already-monitoring)
        out (binding [config/*config-source*
                      (src/atom-source (atom {:services {:ws-channel {:port -1}}}) {})]
              (wsc/start-ws-channel-with-healing! monitor))]
    {:out out :monitor-kept? (= ::already-monitoring @monitor)}))

;;; ---------------------------------------------------------------------------
;;; Regression: two-JVM port-conflict handling (card 20261114171700-30987cc0)
;;;
;;; Root cause (corrected 2026-11-14): the "server died, attempting restart"
;;; loop was TWO hive-mcp JVMs competing for 127.0.0.1:9999. The second JVM's
;;; start! threw BindException, which the old result/rescue nil swallowed, so
;;; the monitor warned every 30s. The fix: detect BindException and back off
;;; for `bind-backoff-ms`, logging ONE clear "port in use" warning per window.
;;; ---------------------------------------------------------------------------

(deftest bind-exception?-recognises-direct-and-wrapped
  (testing "direct BindException"
    (is (true? (#'wsc/bind-exception?
                (java.net.BindException. "Address already in use")))))
  (testing "wrapped BindException (aleph/netty style)"
    (is (true? (#'wsc/bind-exception?
                (ex-info "start-server failed" {}
                         (java.net.BindException. "Address already in use: bind"))))))
  (testing "unrelated exception is not a bind conflict"
    (is (false? (#'wsc/bind-exception?
                 (ex-info "boom" {} (IllegalStateException. "nope"))))))
  (testing "nil is not a bind conflict"
    (is (false? (#'wsc/bind-exception? nil)))))

(deftest check-channel!-backs-off-on-bind-conflict
  (testing "BindException triggers ONE warn then suppresses further restart attempts"
    (let [port-holder (java.net.ServerSocket. 0) ; ephemeral "other process"
          port        (.getLocalPort port-holder)
          stop-calls  (atom 0)
          start-calls (atom 0)
          warns       (atom [])
          check!      #'wsc/check-channel!
          conflict-until @#'wsc/bind-conflict-until]
      (try
        (reset! conflict-until 0)
        (with-redefs [hive-mcp.channel.websocket/status (fn [] {:running? false})
                      hive-mcp.channel.websocket/stop!  (fn [] (swap! stop-calls inc) nil)
                      hive-mcp.channel.websocket/start! (fn [_opts]
                                                          (swap! start-calls inc)
                                                          (throw (ex-info "bind failed" {}
                                                                          (java.net.BindException.
                                                                           "Address already in use"))))
                      taoensso.timbre/-log! (fn [_config level & args]
                                              (when (= :warn level)
                                                (swap! warns conj
                                                       (map #(if (delay? %) (force %) %) args)))
                                              nil)]
          ;; First pass: port held by `port-holder` — one warn, backoff armed.
          (check! {:port port :host "127.0.0.1"} "127.0.0.1" port)
          (is (= 1 @stop-calls))
          (is (= 1 @start-calls))
          (is (= 1 (count @warns)))
          (is (some #(re-find #"already in use" (str %)) (first @warns)))
          (is (pos? @conflict-until))
          ;; Subsequent passes inside the backoff window: no restart attempts,
          ;; no additional warns.
          (dotimes [_ 5]
            (check! {:port port :host "127.0.0.1"} "127.0.0.1" port))
          (is (= 1 @stop-calls) "stop! not re-called during backoff")
          (is (= 1 @start-calls) "start! not re-attempted during backoff")
          (is (= 1 (count @warns)) "no log spam during backoff"))
        (finally
          (reset! conflict-until 0)
          (.close port-holder))))))

(deftest check-channel!-non-bind-failures-log-every-pass
  (testing "non-BindException restart failures still warn every pass"
    (let [stop-calls  (atom 0)
          start-calls (atom 0)
          warns       (atom [])
          check!      #'wsc/check-channel!
          conflict-until @#'wsc/bind-conflict-until]
      (reset! conflict-until 0)
      (with-redefs [hive-mcp.channel.websocket/status (fn [] {:running? false})
                    hive-mcp.channel.websocket/stop!  (fn [] (swap! stop-calls inc) nil)
                    hive-mcp.channel.websocket/start! (fn [_opts]
                                                        (swap! start-calls inc)
                                                        (throw (ex-info "kaboom" {})))
                    taoensso.timbre/-log! (fn [_config level & args]
                                            (when (= :warn level)
                                              (swap! warns conj args))
                                            nil)]
        (dotimes [_ 3]
          (check! {:port 0 :host "127.0.0.1"} "127.0.0.1" 0))
        (is (= 3 @stop-calls))
        (is (= 3 @start-calls))
        (is (= 3 (count @warns)) "non-bind failures warn on every pass"))
      (reset! conflict-until 0))))

;;; ---------------------------------------------------------------------------
;;; Coverage gate — every public fn under server/transport
;;; ---------------------------------------------------------------------------

(def ^:private transport-nss
  '[hive-mcp.server.transport.a2a-gateway hive-mcp.server.transport.legacy
    hive-mcp.server.transport.mcp-http hive-mcp.server.transport.nrepl
    hive-mcp.server.transport.olympus-ws hive-mcp.server.transport.websocket-mcp
    hive-mcp.server.transport.ws-channel])

(run! require transport-nss)

(coverage/deftest-contract-coverage server-transport-contracts
  {:paths ["src/hive_mcp/server/transport"]})

(deftest server-transport-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths ["src/hive_mcp/server/transport"]})]
    ;; 14 = 10 + nrepl's STD-04 socket API (socket-path-for, admit-socket-path,
    ;; start-socket-nrepl!, stop-socket-nrepl!), each with an m/=> contract
    (is (= 14 (count (:universe report))))
    (is (= 14 (count (:covered report))))))

(deftest every-transport-contract-generates-both-sides
  (doseq [ns-sym transport-nss
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (testing (str ns-sym "/" fn-sym)
      (is (= 3 (count (mg/sample side {:size 3 :seed 7})))))))

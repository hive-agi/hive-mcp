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
    (is (= 10 (count (:universe report))))
    (is (= 10 (count (:covered report))))))

(deftest every-transport-contract-generates-both-sides
  (doseq [ns-sym transport-nss
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (testing (str ns-sym "/" fn-sym)
      (is (= 3 (count (mg/sample side {:size 3 :seed 7})))))))

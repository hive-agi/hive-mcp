(ns hive-mcp.transport.olympus.http-test
  "Facets for hive-mcp.transport.olympus.http (cov-facets f07), stopped state
   only. start! binds a real aleph server on a port, so a unit run pins what
   is observable without a socket: status in the stopped state, its shape
   while an in-memory client sits in stream/clients, and that stop! is an
   idempotent nil when nothing runs. The started path is exercised by
   server/transport/olympus_ws_test.clj."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.transport.olympus.http :as ohttp]
            [hive-mcp.transport.olympus.stream :as stream]
            [hive-test.golden :refer [deftest-golden]]
            [manifold.stream :as s]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-golden olympus-http-stopped-golden
  "test/golden/hive-mcp/f07-olympus-http-stopped.edn"
  (let [prev @stream/clients]
    (ohttp/stop!)
    (try
      (let [stopped     (ohttp/status)
            client      (s/stream)
            _           (reset! stream/clients #{client})
            with-client (ohttp/status)]
        (s/close! client)
        {:stopped     stopped
         :with-client with-client
         :stop-again  (ohttp/stop!)})
      (finally (reset! stream/clients prev)))))

(deftest status-reports-not-running
  (is (false? (:running? (ohttp/status)))))

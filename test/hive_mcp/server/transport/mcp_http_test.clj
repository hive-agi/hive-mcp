(ns hive-mcp.server.transport.mcp-http-test
  "Facets for the MCP-HTTP starter. An enabled start registers the :mcp-http
   tool surface before it tries to listen, so each test starts with the
   surface absent and puts back whatever was registered before it; the port
   is one the socket layer refuses, so nothing binds."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.source :as src]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.server.transport.mcp-http :as mh]
            [hive-mcp.server.transport.schema :as ts]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [hive-mcp.transport.mcp-http :as http]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-the-surface
  "Run the test with an empty config, no :mcp-http surface and no MCP-HTTP
   server held by the transport (its `start!` answers an already-running
   server's port instead of starting), then restore the surface that was
   registered before the test, if any."
  [f]
  (let [prior (get (routes/registered-surfaces) mh/surface-id)]
    (routes/unregister-surface! mh/surface-id)
    (http/stop!)
    (try
      (binding [config/*config-source* (src/empty-source)] (f))
      (finally
        (http/stop!)
        (routes/unregister-surface! mh/surface-id)
        (when prior (routes/register-surface! mh/surface-id prior))))))

(use-fixtures :each own-the-surface)

(golden/deftest-golden mcp-http-start-paths-golden
  "test/golden/hive-mcp/server/transport/mcp-http.edn"
  {:no-config (mh/start-mcp-http!)
   :disabled  (mh/start-mcp-http! {:enabled false :port -1})
   :enabled   (mh/start-mcp-http! {:enabled true :port -1 :bind "127.0.0.1"})})

(deftest an-enabled-start-registers-the-surface-and-reports-failure
  (let [out (mh/start-mcp-http! {:enabled true :port -2})]
    (is (m/validate ts/StartStatus out))
    (is (= {:status :failed :port -2} out))
    (is (contains? (routes/registered-surfaces) mh/surface-id)
        "the tool surface is registered even when the listen fails")))

(deftest a-disabled-start-touches-nothing
  (is (nil? (mh/start-mcp-http! {:enabled false})))
  (is (not (contains? (routes/registered-surfaces) mh/surface-id))))

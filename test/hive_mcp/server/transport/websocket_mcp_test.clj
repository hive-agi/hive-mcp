(ns hive-mcp.server.transport.websocket-mcp-test
  "Facets for the WebSocket MCP starter. Runs under the empty config source
   and only ever asks for ports the socket layer refuses, so nothing binds."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.source :as src]
            [hive-mcp.server.transport.schema :as ts]
            [hive-mcp.server.transport.websocket-mcp :as wsm]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (binding [config/*config-source* (src/empty-source)] (f))))

(def ^:private UnbindableConfig
  [:maybe
   [:map
    [:enabled :boolean]
    [:port [:enum -1 -2 -70000]]]])

(deftrifecta-from-schema websocket-start-fails-loudly-on-a-bad-port
  #'wsm/start-websocket-server!
  {:in [:cat UnbindableConfig]
   :out ts/MaybeStartStatus
   :rel (fn [[cfg] out]
          (if (:enabled cfg)
            (= {:status :failed :port (:port cfg)} out)
            (nil? out)))
   :classify (fn [_ out] (:status out :disabled))
   :classify-domain #{:disabled :failed}
   :num-tests 150 :classify-floor 11})

(golden/deftest-golden websocket-start-paths-golden
  "test/golden/hive-mcp/server/transport/websocket-mcp.edn"
  {:no-config (wsm/start-websocket-server!)
   :disabled  (wsm/start-websocket-server! {:enabled false})
   :enabled   (wsm/start-websocket-server! {:enabled true :port -1})})

(deftest the-runtime-config-port-is-read-when-the-component-has-none
  (binding [config/*config-source*
            (src/atom-source (atom {:services {:websocket {:enabled true :port -4}}}) {})]
    (is (= {:status :failed :port -4} (wsm/start-websocket-server!)))))

(ns hive-mcp.server.transport.a2a-gateway-test
  "Facets for the A2A gateway starter. Every test runs under the empty config
   source and asks for port -1, which the socket address rejects before any
   bind, so nothing listens and nothing depends on the developer's config."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.source :as src]
            [hive-mcp.server.transport.a2a-gateway :as a2a]
            [hive-mcp.server.transport.schema :as ts]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [hive-mcp.transport.a2a :as a2a-http]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-the-gateway
  "Empty config, and no A2A server held by the transport: its `start!`
   answers an already-running server's port instead of starting."
  [f]
  (a2a-http/stop!)
  (try (binding [config/*config-source* (src/empty-source)] (f))
       (finally (a2a-http/stop!))))

(use-fixtures :each own-the-gateway)

(def ^:private UnbindableConfig
  ;; ports a socket address refuses, so an enabled start never binds
  [:maybe
   [:map
    [:enabled :boolean]
    [:port [:enum -1 -2 -70000]]]])

(deftrifecta-from-schema a2a-gateway-answers-only-when-enabled
  #'a2a/start-a2a-gateway!
  {:in [:cat UnbindableConfig]
   :out ts/MaybeStartStatus
   ;; BUG (pinned): start! swallows its bind failure and answers nil, so the
   ;; gateway reports :running although nothing listens
   :rel (fn [[cfg] out]
          (if (:enabled cfg)
            (= {:status :running :port (:port cfg)} out)
            (nil? out)))
   :classify (fn [_ out] (:status out :disabled))
   :classify-domain #{:disabled :running}
   :num-tests 60})

(golden/deftest-golden a2a-gateway-paths-golden
  "test/golden/hive-mcp/server/transport/a2a-gateway.edn"
  {:no-config  (a2a/start-a2a-gateway!)
   :nil-config (a2a/start-a2a-gateway! nil)
   :disabled   (a2a/start-a2a-gateway! {:enabled false :port -1})
   :enabled    (a2a/start-a2a-gateway! {:enabled true :port -1 :bind "127.0.0.1"})})

(deftest the-runtime-config-enables-the-gateway-too
  (binding [config/*config-source*
            (src/atom-source (atom {:services {:a2a {:enabled true :port -3}}}) {})]
    (is (= {:status :running :port -3} (a2a/start-a2a-gateway!)))))

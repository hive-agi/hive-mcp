(ns hive-mcp.server.transport.mcp-http
  "MCP over HTTP for remote clients.

   Single responsibility: start the MCP HTTP transport if enabled via config."
  (:require [hive-mcp.config.core :as config]
            [hive-mcp.dns.result :as result]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- build-dispatch
  "The live tool set behind the SDK method table."
  []
  (let [build-spec  (requiring-resolve 'hive-mcp.server.routes/build-server-spec)
        create-ctx! (requiring-resolve 'io.modelcontext.clojure-sdk.server/create-context!)
        sdk-dispatch (requiring-resolve 'hive-mcp.transport.mcp-http/sdk-dispatch)]
    (sdk-dispatch (create-ctx! (assoc (build-spec) :server-id (random-uuid))))))

(defn start-mcp-http!
  "Start the MCP HTTP transport.

   Enabled by the system component config, or by the runtime config /
   HIVE_MCP_HTTP_ENABLED. Port and bind host come from the component config
   first, the runtime config second. The token comes only from the runtime
   config / HIVE_MCP_HTTP_TOKEN, never from a profile file.

   Returns nil when not enabled, {:status :running :port n} when listening,
   {:status :failed :port n} when the start failed. Never throws."
  ([] (start-mcp-http! nil))
  ([component-config]
   (let [enabled? (or (:enabled component-config)
                      (config/get-service-value :mcp-http :enabled
                                                :env "HIVE_MCP_HTTP_ENABLED"
                                                :parse #(= "true" %)
                                                :default false))
         port     (or (:port component-config)
                      (config/get-service-value :mcp-http :port
                                                :env "HIVE_MCP_HTTP_PORT"
                                                :parse parse-long
                                                :default 7921))
         bind     (or (:bind component-config)
                      (config/get-service-value :mcp-http :bind
                                                :env "HIVE_MCP_HTTP_BIND"))]
     (when enabled?
       (let [actual (result/rescue nil
                      ((requiring-resolve 'hive-mcp.transport.mcp-http/start!)
                       {:dispatch (build-dispatch)
                        :port     port
                        :bind     bind
                        :token    (config/get-service-value :mcp-http :token
                                                            :env "HIVE_MCP_HTTP_TOKEN")}))]
         (if actual
           {:status :running :port actual}
           (do (log/warn "MCP HTTP failed to start, NOT listening" {:port port})
               {:status :failed :port port})))))))

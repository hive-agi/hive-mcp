(ns hive-mcp.await.adapters.http
  "Adapter: the IHttpGet the :prom-predicate source queries Prometheus
   through (clj-http). Query params are encoded by the client, never spliced
   into the URL; a non-2xx status is answered, not thrown, so the source
   decides what it means."
  (:require [clj-http.client :as http]
            [hive-mcp.await.sources.boundary :as b]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord CljHttpClient []
  b/IHttpGet
  (http-get! [_ url query-params {:keys [timeout-ms]}]
    (let [t (int (or timeout-ms 10000))
          r (http/get url {:query-params      query-params
                           :socket-timeout    t
                           :connection-timeout t
                           :throw-exceptions  false
                           ;; never follow a redirect off the configured host
                           :redirect-strategy :none
                           :as                :string})]
      {:status (:status r) :body (:body r)})))

(def default-client
  "The process HTTP client."
  (->CljHttpClient))

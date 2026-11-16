(ns hive-mcp.transport.origin
  "Which browser Origins may reach hive's unauthenticated local servers."
  (:require [hive-mcp.dns.result :refer [rescue]]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private loopback-hosts #{"localhost" "127.0.0.1" "[::1]" "::1"})

(defn allowed?
  "Pure: may a request carrying `origin` (the Origin header value, nil when
   absent) reach the server? Absent: yes, a non-browser client. Present: only
   an http(s) origin whose host is localhost, 127.0.0.1 or [::1]."
  [origin]
  (if (nil? origin)
    true
    (let [uri (rescue nil (java.net.URI. (str origin)))]
      (boolean (and uri
                    (#{"http" "https"} (.getScheme ^java.net.URI uri))
                    (loopback-hosts (.getHost ^java.net.URI uri)))))))

(m/=> allowed? [:=> [:cat [:maybe :string]] :boolean])

(defn forbidden
  "The 403 answered to a request from a disallowed `origin`."
  [origin]
  {:status  403
   :headers {"Content-Type" "text/plain"}
   :body    (str "Forbidden: Origin " origin " is not loopback")})

(m/=> forbidden [:=> [:cat [:maybe :string]] [:map [:status [:= 403]]]])

(defn wrap-loopback-origin
  "Ring middleware: answer `forbidden` to any request whose Origin header is
   not `allowed?`, before `handler` sees it."
  [handler]
  (fn [req]
    (let [origin (get-in req [:headers "origin"])]
      (if (allowed? origin)
        (handler req)
        (forbidden origin)))))

(m/=> wrap-loopback-origin [:=> [:cat ifn?] ifn?])
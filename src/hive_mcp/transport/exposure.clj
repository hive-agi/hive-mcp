(ns hive-mcp.transport.exposure
  "Exposure policy shared by every network transport (a2a, mcp-http).

   One rule, stated once: a listener with no secret answers anyone who can
   reach it, so it binds loopback only, whatever was requested. With a
   secret the requested host wins, default all interfaces, and every call
   must carry `Authorization: Bearer <secret>`, compared in constant time so
   a caller cannot learn the secret by timing.

   Pure: no I/O, no state."
  (:require [clojure.string :as str])
  (:import [java.security MessageDigest]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def loopback "127.0.0.1")

(def all-interfaces "0.0.0.0")

(defn bind-host
  "The interface to listen on. Loopback when SECRET is blank, whatever was
   REQUESTED. Otherwise REQUESTED, default all interfaces."
  [secret requested]
  (cond
    (str/blank? secret)    loopback
    (str/blank? requested) all-interfaces
    :else                  requested))

(defn bearer-ok?
  "True when SECRET is blank (no auth configured), or AUTH-HEADER is exactly
   `Bearer SECRET`. The comparison is constant-time."
  [secret auth-header]
  (or (str/blank? secret)
      (MessageDigest/isEqual
       (.getBytes (str auth-header) "UTF-8")
       (.getBytes (str "Bearer " secret) "UTF-8"))))

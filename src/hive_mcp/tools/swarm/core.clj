(ns hive-mcp.tools.swarm.core
  "Core utilities for swarm tool handlers including response builders and swarm availability check."
  (:require [hive-spi.editor.services :as svc]
            [clojure.data.json :as json]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn swarm-addon-available?
  "True when a :vessel :dispatch capability is registered and reports the swarm addon loaded."
  []
  (= "t" (:result (svc/invoke :vessel :dispatch {:op :swarm/available?} 2000))))

(defn mcp-success
  "Build a successful MCP response."
  [data]
  {:type "text" :text (if (string? data) data (json/write-str data))})

(defn mcp-error
  "Build an error MCP response."
  [message]
  {:type "text" :text message :isError true})

(defn mcp-error-json
  "Build an error MCP response with JSON payload."
  [data]
  {:type "text" :text (json/write-str data) :isError true})

(defn mcp-timeout-error
  "Build a timeout error response."
  [operation & {:keys [extra-data]}]
  (let [base {:error (str operation " timed out")
              :status "timeout"}
        data (if extra-data (merge base extra-data) base)]
    {:type "text" :text (json/write-str data) :isError true}))

(defn addon-not-loaded-error
  "Return the standard error when no swarm host answers."
  []
  {:type "text" :text "Swarm host unavailable: no :vessel :dispatch capability is registered, or the vessel reports its swarm addon unloaded." :isError true})

(defmacro with-swarm
  "Execute body only if swarm addon is available."
  [& body]
  `(if (swarm-addon-available?)
     (do ~@body)
     (addon-not-loaded-error)))

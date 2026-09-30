(ns hive-mcp.agent.provider.preflight
  "Provider BOUNDARY for spawn preflight: can the resolved provider be called
   right now?

   Composes the pipeline (`hive-mcp.agent.provider`) with one network read per
   provider that declares an :auth-probe. The probe kinds are an open set:
   `probe-credential` dispatches on the probe's :kind, and a provider without
   a probe is only checked for a configured secret.

   `*http-get*` is the transport seam: (fn [url headers timeout-ms]) ->
   {:status int :body map-or-nil}, read at call time."
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]
            [hive-mcp.agent.provider :as provider]
            [taoensso.timbre :as log]
            [hive-dsl.result :as r]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private default-timeout-ms
  "Budget for one credential probe, connect and read each."
  5000)

(defn- parse-json-body
  "The JSON body `s` as a keywordized map, or nil when it is not JSON."
  [s]
  (when (string? s)
    (r/rescue nil (json/read-str s :key-fn keyword))))

(defn- clj-http-get
  "GET `url` with `headers`; never throws on an HTTP status."
  [url headers timeout-ms]
  (let [resp (http/get url {:headers            headers
                            :throw-exceptions   false
                            :socket-timeout     timeout-ms
                            :connection-timeout timeout-ms})]
    {:status (:status resp)
     :body   (parse-json-body (:body resp))}))

(def ^:dynamic *http-get*
  "Transport for credential probes: (fn [url headers timeout-ms]) ->
   {:status int :body map-or-nil}."
  clj-http-get)

(defmulti probe-credential
  "Ask the provider whether `secret` is usable now, as described by the entry's
   :auth-probe `probe`. Returns {:status int :remaining number-or-nil}, or nil
   when no method knows the probe's :kind."
  (fn [probe _secret] (:kind probe)))

(defmethod probe-credential :default [_ _] nil)

(defmethod probe-credential :openrouter-key-info
  [{:keys [url timeout-ms]} secret]
  (let [{:keys [status body]} (*http-get* url
                                          {"Authorization" (str "Bearer " secret)}
                                          (or timeout-ms default-timeout-ms))]
    {:status    status
     :remaining (get-in body [:data :limit_remaining])}))

(defn- probe-refusal
  "The refusal a credential probe of `provider` returns, or nil. A probe that
   cannot complete (network error, timeout) is inconclusive: it logs and passes."
  [provider]
  (when-let [{:keys [probe secret]} (provider/credential-probe provider)]
    (try
      (some->> (probe-credential probe secret)
               (provider/credential-refusal provider))
      (catch Exception e
        (log/warn "Provider credential probe inconclusive; spawn proceeds"
                  {:provider provider :kind (:kind probe) :error (ex-message e)})
        nil))))

(defn refusal
  "nil when `provider` can be called now, else an error map with :error,
   :provider and :fix. Checks the configured secret first, then the provider's
   credential probe when it declares one."
  [provider]
  (when provider
    (or (provider/missing-secret provider)
        (probe-refusal provider))))

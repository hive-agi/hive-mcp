(ns hive-mcp.await.sources.prom-predicate
  "The :prom-predicate await source: poll a PromQL instant query until a
   comparison against a threshold holds.

   Subscription:
     {:type :prom-predicate
      :poll-ms 30000                          ; >= 10 s, default 30 s
      :spec {:query \"sum(rate(sisf_sync_rows_total[5m]))\"
             :op \">\" :threshold 0
             :match \"any\"?                    ; any (default) | all series
             :url \"http://prometheus:9090\"?}} ; default: config services.prometheus.url
   plus a REQUIRED deadline (<= 48 h).

   The query goes to /api/v1/query as a parameter (never spliced into the
   URL), under a timeout. A vector result holds when ANY (or ALL) of its
   series compares true; a scalar result is compared as is; an EMPTY vector
   never holds (no series is not 'zero'). The wake carries the series that
   matched (at most 5) and the value observed.

   Ports (DIP): *http* (IHttpGet) is read when the source opens."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-mcp.await.ports :as ports]
            [hive-mcp.await.sources.boundary :as b]
            [hive-mcp.await.sources.poll :as poll]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def timing
  "Poll interval bounds: at least 10 s, 30 s by default."
  {:min-poll-ms 10000 :default-poll-ms 30000})

(def max-query-chars 4000)
(def http-timeout-ms 10000)
(def sample-series 5)
(def fallback-url "http://localhost:9090")

(def Parsed
  [:map
   [:query :string]
   [:op :string]
   [:threshold number?]
   [:match [:enum :any :all]]
   [:url {:optional true} [:maybe :string]]])

(defn- refuse [message & [data]]
  (throw (poll/refusal :prom-predicate message (or data {}))))

(defn parse
  "The Parsed spec of `subscription`, or a refusal. Pure."
  [subscription]
  (let [spec (poll/spec-of subscription)
        q    (:query spec)
        op   (some-> (:op spec) name str/trim)
        th   (poll/->number (:threshold spec))
        mt   (some-> (or (:match spec) "any") name str/lower-case keyword)
        url  (some-> (:url spec) str str/trim not-empty)]
    (when-not (and (string? q) (not (str/blank? q))) (refuse ":query (PromQL) is required"))
    (when (> (count q) max-query-chars) (refuse (str ":query is longer than " max-query-chars)))
    (when-not (poll/comparator-of op) (refuse (str ":op must be one of " (str/join " " (sort (keys poll/comparators))))))
    (when-not th (refuse ":threshold must be a number"))
    (when-not (#{:any :all} mt) (refuse ":match must be any or all"))
    (when (and url (not (re-matches #"https?://[^\s]+" url))) (refuse ":url must be an http(s) URL"))
    (cond-> {:query q :op op :threshold th :match mt}
      url (assoc :url (str/replace url #"/+$" "")))))

(m/=> parse [:=> [:cat :map] Parsed])

(defn- sample-value [v]
  (poll/->number (second v)))

(defn evaluate
  "The probe answer for a Prometheus `response` {:status :body}. A non-200
   or a status other than success is a failed poll (thrown). Pure."
  [parsed {:keys [status body]}]
  (when-not (= 200 status)
    (throw (ex-info (str "prometheus answered HTTP " status) {:error :await/poll-failed :status status})))
  (let [doc  (json/read-str (str body) :key-fn keyword)
        _    (when-not (= "success" (:status doc))
               (throw (ex-info (str "prometheus query failed: " (:error doc)) {:error :await/poll-failed})))
        {:keys [resultType result]} (:data doc)
        holds? #(and (some? %) (poll/compare-values (:op parsed) % (:threshold parsed)))
        series (case resultType
                 "vector" (mapv (fn [s] {:metric (:metric s) :value (sample-value (:value s))}) result)
                 "scalar" [{:metric {} :value (sample-value result)}]
                 (throw (ex-info (str "prometheus result type " resultType " is not an instant vector or scalar")
                                 {:error :await/poll-failed})))
        hits   (filterv (comp holds? :value) series)
        ok?    (and (seq series)
                    (if (= :all (:match parsed)) (= (count hits) (count series)) (seq hits)))]
    {:match? (boolean ok?)
     :data   {:query (:query parsed) :op (:op parsed) :threshold (:threshold parsed)
              :match (:match parsed)
              :observed (:value (first (or (seq hits) series)))
              :series-count (count series)
              :matched (vec (take sample-series hits))}}))

(m/=> evaluate [:=> [:cat Parsed :map] poll/ProbeResult])

;; =============================================================================
;; Ports and the source
;; =============================================================================

(def ^:dynamic *http*
  "The IHttpGet, read when the source opens. nil: the clj-http adapter."
  nil)

(defn- current-http []
  (or *http* @(requiring-resolve 'hive-mcp.await.adapters.http/default-client)))

(defn base-url
  "The Prometheus URL for `parsed`: its own, else config, else localhost."
  [parsed]
  (or (:url parsed)
      (some-> (try ((requiring-resolve 'hive-mcp.config.core/get-in-config) [:services :prometheus :url])
                   (catch Throwable _ nil))
              str (str/replace #"/+$" ""))
      fallback-url))

(defmethod ports/check-subscription :prom-predicate
  [_subscription awaiter]
  (poll/check! :prom-predicate timing awaiter parse))

(defmethod ports/open-source :prom-predicate
  [subscription awaiter deliver!]
  (let [parsed (parse subscription)
        client (current-http)
        url    (str (base-url parsed) "/api/v1/query")]
    (poll/open-polling :prom-predicate timing awaiter deliver!
                       (fn probe []
                         (evaluate parsed (b/http-get! client url {"query" (:query parsed)}
                                                       {:timeout-ms http-timeout-ms}))))))

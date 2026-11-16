(ns hive-mcp.nats.streams
  "JetStream streams hive owns, declared as data and ensured on every connect.

   Strata (CPPB):
     Value objects  StreamSpec, EnsureReport
     Pure           events-stream, inbox-stream, default-streams, spec->config
     Boundary       ensure-stream!, ensure-streams! (one JetStreamManagement
                    round-trip per stream; a failure is a report row, never a
                    throw)"
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]
            [malli.core :as m])
  (:import [io.nats.client Connection JetStreamApiException]
           [io.nats.client.api StreamConfiguration StorageType DiscardPolicy]
           [java.time Duration]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Value objects
;; =============================================================================

(def StreamSpec
  [:map
   [:name [:re #"^[A-Za-z0-9_-]+$"]]
   [:subjects [:vector {:min 1} :string]]
   [:max-age-ms pos-int?]
   [:duplicate-window-ms pos-int?]
   [:max-bytes {:optional true} pos-int?]
   [:discard {:optional true} [:enum :old :new]]])

(def EnsureReport
  [:map
   [:stream :string]
   [:action [:enum :created :updated :failed]]
   [:error {:optional true} :string]])

;; =============================================================================
;; Pure: the streams
;; =============================================================================

(def ^:private hour-ms (* 60 60 1000))

(def events-stream
  "Bus events: shouts, agent lifecycle, system events, tool notifications.
   Excludes hive.v1.inbox.> (HIVE_INBOX) and hive.v1.wave.> (HIVE_WAVE)."
  {:name                "HIVE_EVENTS"
   :subjects            ["hive.v1.shout.>" "hive.v1.agent.>"
                         "hive.v1.event.>" "hive.v1.tool.>"]
   :max-age-ms          (* 72 hour-ms)
   :max-bytes           (* 2 1024 1024 1024)
   :discard             :old
   :duplicate-window-ms (* 2 60 1000)})

(def inbox-stream
  "Per-coordinator inboxes: hive.v1.inbox.<inbox-id>, one durable each."
  {:name                "HIVE_INBOX"
   :subjects            ["hive.v1.inbox.>"]
   :max-age-ms          (* 7 24 hour-ms)
   :duplicate-window-ms (* 2 60 1000)})

(def default-streams [events-stream inbox-stream])

(defn spec->config
  "Pure: a StreamSpec as a jnats StreamConfiguration (file storage)."
  [{:keys [name subjects max-age-ms duplicate-window-ms max-bytes discard]}]
  (cond-> (-> (StreamConfiguration/builder)
              (.name name)
              (.subjects ^"[Ljava.lang.String;" (into-array String subjects))
              (.storageType StorageType/File)
              (.maxAge (Duration/ofMillis max-age-ms))
              (.duplicateWindow (Duration/ofMillis duplicate-window-ms)))
    max-bytes (.maxBytes (long max-bytes))
    discard   (.discardPolicy (if (= :new discard) DiscardPolicy/New DiscardPolicy/Old))
    true      (.build)))

(m/=> spec->config [:=> [:cat StreamSpec] [:fn #(instance? StreamConfiguration %)]])

;; =============================================================================
;; Boundary
;; =============================================================================

(def ^:private stream-not-found 10059)

(defn- stream-exists? [jsm stream-name]
  (try (.getStreamInfo jsm stream-name) true
       (catch JetStreamApiException e
         (if (= stream-not-found (.getApiErrorCode e)) false (throw e)))))

(defn ensure-stream!
  "Create the stream, or update it to `spec` when it exists. Never throws."
  [^Connection conn spec]
  (try
    (let [jsm (.jetStreamManagement conn)
          cfg (spec->config spec)]
      (if (stream-exists? jsm (:name spec))
        (do (.updateStream jsm cfg) {:stream (:name spec) :action :updated})
        (do (.addStream jsm cfg) {:stream (:name spec) :action :created})))
    (catch Exception e
      (log/warn "[NATS] ensure-stream failed for" (:name spec) ":" (.getMessage e))
      {:stream (:name spec) :action :failed
       :error (or (some-> (.getMessage e) str/trim) (str (class e)))})))

(m/=> ensure-stream! [:=> [:cat :any StreamSpec] EnsureReport])

(defn ensure-streams!
  "Ensure every spec; one report row per stream, in order."
  [conn specs]
  (mapv #(ensure-stream! conn %) specs))

(m/=> ensure-streams! [:=> [:cat :any [:sequential StreamSpec]] [:vector EnsureReport]])

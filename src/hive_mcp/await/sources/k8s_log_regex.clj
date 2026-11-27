(ns hive-mcp.await.sources.k8s-log-regex
  "The :k8s-log-regex await source: poll a pod's (or a selector's) logs with
   `kubectl logs` until a line matches a regex. It wakes on the first
   matching line and carries that line. A log line proves a job RAN; when
   the question is whether the data landed, use :sql-predicate.

   Subscription:
     {:type :k8s-log-regex
      :poll-ms 30000                         ; >= 10 s, default 30 s
      :spec {:namespace \"sisf\"
             :pod \"sisf-sync-7d9\"            ; or :selector \"app=sisf-sync\"
             :container \"sync\"?
             :context \"blw\"?                 ; kubectl context
             :regex \"Scheduled sync completed\"
             :lookback-ms 0?                 ; also match lines this old at create
             :tail 2000?}}                   ; lines read per poll, <= 10000
   plus a REQUIRED deadline (<= 48 h).

   Bounded poll, not `-f`: each poll is one `kubectl logs --since-time=T
   --timestamps --tail=N` call under a timeout and an output cap, where T is
   the awaiter's creation time minus :lookback-ms. So a line logged before
   the wait began never wakes it (unless lookback says so), a dead or
   restarted pod is just a failed poll, and close leaves no process behind.

   Arguments are passed as an argv vector (no shell), and every name is
   checked against the Kubernetes name grammar, so none can start a flag.

   Ports (DIP): *process* (IProcessRunner) is read when the source opens."
  (:require [clojure.string :as str]
            [hive-mcp.await.ports :as ports]
            [hive-mcp.await.sources.boundary :as b]
            [hive-mcp.await.sources.poll :as poll]
            [malli.core :as m])
  (:import (java.time Instant)
           (java.util.regex Pattern PatternSyntaxException)))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def timing
  "Poll interval bounds: at least 10 s, 30 s by default."
  {:min-poll-ms 10000 :default-poll-ms 30000})

(def default-tail 2000)
(def max-tail 10000)
(def max-regex-chars 500)
(def max-lookback-ms (* 24 60 60 1000))
(def kubectl-timeout-ms 20000)
(def max-output-bytes (* 4 1024 1024))
(def max-line-chars
  "The longest matched line a wake event carries."
  2000)

(def ^:private name-re #"[a-z0-9]([-a-z0-9.]{0,251}[a-z0-9])?")
(def ^:private selector-re #"[A-Za-z0-9]([-A-Za-z0-9_./=!,() ]{0,500})")
(def ^:private context-re #"[A-Za-z0-9]([-A-Za-z0-9_.@:/]{0,250})")

(def Parsed
  [:map
   [:namespace :string]
   [:pod {:optional true} [:maybe :string]]
   [:selector {:optional true} [:maybe :string]]
   [:container {:optional true} [:maybe :string]]
   [:context {:optional true} [:maybe :string]]
   [:regex :string]
   [:lookback-ms :int]
   [:tail pos-int?]])

(defn- refuse [message & [data]]
  (throw (poll/refusal :k8s-log-regex message (or data {}))))

(defn- checked [v re what]
  (when (some? v)
    (let [s (str/trim (str v))]
      (when-not (re-matches re s) (refuse (str what " " (pr-str s) " is not a valid name")))
      s)))

(defn parse
  "The Parsed spec of `subscription`, or a refusal. Pure."
  [subscription]
  (let [spec  (poll/spec-of subscription)
        ns'   (checked (:namespace spec) name-re ":namespace")
        pod   (checked (:pod spec) name-re ":pod")
        sel   (checked (:selector spec) selector-re ":selector")
        rx    (:regex spec)
        tail  (or (:tail spec) default-tail)
        look  (or (:lookback-ms spec) 0)]
    (when-not ns' (refuse ":namespace is required"))
    (when (= (boolean pod) (boolean sel)) (refuse "name exactly one of :pod or :selector"))
    (when-not (and (string? rx) (not (str/blank? rx))) (refuse ":regex is required"))
    (when (> (count rx) max-regex-chars) (refuse (str ":regex is longer than " max-regex-chars)))
    (try (Pattern/compile rx)
         (catch PatternSyntaxException e (refuse (str ":regex does not compile: " (.getDescription e)))))
    (when-not (and (integer? tail) (<= 1 tail max-tail)) (refuse (str ":tail must be 1.." max-tail)))
    (when-not (and (integer? look) (<= 0 look max-lookback-ms))
      (refuse (str ":lookback-ms must be 0.." max-lookback-ms)))
    (cond-> {:namespace ns' :regex rx :lookback-ms (long look) :tail (long tail)}
      pod (assoc :pod pod)
      sel (assoc :selector sel)
      (:container spec) (assoc :container (checked (:container spec) name-re ":container"))
      (:context spec)   (assoc :context (checked (:context spec) context-re ":context")))))

(m/=> parse [:=> [:cat :map] Parsed])

(defn since-time
  "The RFC 3339 instant logs are read from: `created` minus lookback. Pure."
  [parsed created]
  (str (Instant/ofEpochMilli (- (long created) (long (:lookback-ms parsed))))))

(m/=> since-time [:=> [:cat Parsed :int] :string])

(defn argv
  "The kubectl argv for one poll of `parsed` from `since`. Pure."
  [parsed since]
  (cond-> ["kubectl"]
    (:context parsed) (into ["--context" (:context parsed)])
    true (into ["logs" "-n" (:namespace parsed)])
    (:pod parsed) (conj (:pod parsed))
    (:selector parsed) (into ["-l" (:selector parsed) "--prefix" "--max-log-requests" "10"])
    (:container parsed) (into ["-c" (:container parsed)])
    true (into ["--timestamps" (str "--since-time=" since) (str "--tail=" (:tail parsed))])))

(m/=> argv [:=> [:cat Parsed :string] [:vector :string]])

(defn first-match
  "The first line of `out` the regex finds, as event data, or nil. Pure."
  [parsed out]
  (let [p (Pattern/compile (:regex parsed))]
    (some (fn [line]
            (let [mt (.matcher p ^String line)]
              (when (.find mt)
                {:line  (subs line 0 (min (count line) max-line-chars))
                 :match (.group mt)
                 :namespace (:namespace parsed)
                 :target (or (:pod parsed) (:selector parsed))})))
          (str/split-lines (or out "")))))

(m/=> first-match [:=> [:cat Parsed [:maybe :string]] [:maybe :map]])

(defn evaluate
  "The probe answer for one kubectl `result`. A non-zero exit or a timeout
   is a failed poll (thrown), so polling goes on. Pure."
  [parsed {:keys [exit out err timed-out?]}]
  (when timed-out? (throw (ex-info "kubectl logs timed out" {:error :await/poll-timeout})))
  (when-not (zero? (long exit))
    (throw (ex-info (str "kubectl logs exited " exit ": " (subs (str err) 0 (min 300 (count (str err)))))
                    {:error :await/poll-failed :exit exit})))
  (if-let [m (first-match parsed out)]
    {:match? true :data m}
    {:match? false}))

(m/=> evaluate [:=> [:cat Parsed :map] poll/ProbeResult])

;; =============================================================================
;; Ports and the source
;; =============================================================================

(def ^:dynamic *process*
  "The IProcessRunner, read when the source opens. nil: the process
   adapter, resolved late."
  nil)

(defn- current-process []
  (or *process* @(requiring-resolve 'hive-mcp.await.adapters.process/default-runner)))

(defmethod ports/check-subscription :k8s-log-regex
  [_subscription awaiter]
  (poll/check! :k8s-log-regex timing awaiter parse))

(defmethod ports/open-source :k8s-log-regex
  [subscription awaiter deliver!]
  (let [parsed (parse subscription)
        runner (current-process)
        since  (since-time parsed (or (:created awaiter) (b/now-ms poll/*clock*)))
        cmd    (argv parsed since)]
    (poll/open-polling :k8s-log-regex timing awaiter deliver!
                       (fn probe []
                         (evaluate parsed (b/run-process! runner cmd
                                                          {:timeout-ms kubectl-timeout-ms
                                                           :max-bytes max-output-bytes}))))))

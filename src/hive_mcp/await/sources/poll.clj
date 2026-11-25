(ns hive-mcp.await.sources.poll
  "Wake bounded context: the poll engine every predicate-style await source
   (:sql-predicate, :k8s-log-regex, :prom-predicate) runs on.

   A polling source is three things: a pure `parse` of its subscription
   (refusing what it cannot honour), a `probe` that does ONE bounded read at
   the I/O boundary and answers {:match? bool :data map}, and the engine
   here that repeats the probe every :poll-ms until the first match, the
   awaiter's deadline, or close.

   Shared rules, enforced before anything is recorded (check):
     - a deadline is REQUIRED: a poll that can never end is refused;
     - the deadline is at most `max-deadline-ms` away;
     - :poll-ms is at least the source's minimum (default: its default).
   The engine never wakes on the deadline itself: the registry's own
   deadline timer does that (:reason :timeout). The engine only stops
   polling once the deadline is reached, so a late probe cannot deliver.

   Stop is clean: close cancels the schedule and flips a stopped flag that
   an in-flight probe checks before it delivers, so after close (cancel,
   wake, deadline, shutdown) nothing more is delivered.

   The clock and the scheduler are ports (hive-mcp.await.sources.boundary),
   read from *clock* and *scheduler* when the source OPENS, so a test binds
   stubs around create!. The first probe runs on the scheduler, not inside
   open-source, so a predicate already true at the first poll reaches the
   registry through the same offer! path as any later one."
  (:require [clojure.string :as str]
            [hive-mcp.await.sources.boundary :as b]
            [malli.core :as m]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def max-deadline-ms
  "The furthest away a polling awaiter's deadline may be: 48 hours."
  (* 48 60 60 1000))

(def ProbeResult
  "What a probe answers: whether the predicate holds, and the event data."
  [:map
   [:match? :boolean]
   [:data {:optional true} [:maybe :map]]])

(def PollSpec
  "The timing a source declares: its minimum and default poll interval."
  [:map
   [:min-poll-ms pos-int?]
   [:default-poll-ms pos-int?]])

;; =============================================================================
;; Pure calculations
;; =============================================================================

(defn refusal
  "The ex-info a polling source throws when it refuses `subscription`. Pure."
  [source message data]
  (ex-info (str "await " (name source) ": " message)
           (merge {:error :await/invalid-subscription :source source} data)))

(defn kw-keys
  "`x` with kebab keyword keys, recursively through nested maps: JSON
   objects arrive with string keys and snake_case names (connection_ref ->
   :connection-ref). Vectors are walked; their scalars are kept. Pure."
  [x]
  (cond
    (map? x) (into {} (map (fn [[k v]]
                             [(if (or (string? k) (keyword? k))
                                (keyword (str/replace (name k) "_" "-"))
                                k)
                              (kw-keys v)]))
                   x)
    (vector? x) (mapv kw-keys x)
    (sequential? x) (mapv kw-keys x)
    :else x))

(m/=> kw-keys [:=> [:cat :any] :any])

(defn spec-of
  "The source-specific spec of `subscription`: its :spec map, keywordised.
   Pure."
  [subscription]
  (kw-keys (or (:spec subscription) {})))

(m/=> spec-of [:=> [:cat :map] :map])

(defn closed-spec
  "The spec of `subscription`, refused when it carries a key outside
   `allowed`. A spec is persisted in the registry file, so an unknown key
   (a `url`, a `password`) must never be accepted and stored. Pure."
  [source subscription allowed]
  (let [spec (spec-of subscription)
        extra (sort (map name (remove (set allowed) (keys spec))))]
    (when (seq extra)
      (throw (refusal source (str "unknown spec key(s): " (str/join ", " extra)
                                  "; accepted: " (str/join ", " (sort (map name allowed))))
                      {:unknown (vec extra)})))
    spec))

(m/=> closed-spec [:=> [:cat :keyword :map [:set :keyword]] :map])

(defn poll-interval
  "The poll interval `subscription` asks for, or `timing`'s default. Pure."
  [subscription {:keys [default-poll-ms]}]
  (let [p (:poll-ms subscription)]
    (long (if (integer? p) p default-poll-ms))))

(m/=> poll-interval [:=> [:cat :map PollSpec] :int])

(defn timing-errors
  "Why `awaiter` cannot be polled under `timing` at `now`, as a vector of
   messages (empty when it can). Pure."
  [awaiter timing now]
  (let [sub (:subscription awaiter)
        p   (:poll-ms sub)
        d   (:deadline awaiter)]
    (cond-> []
      (and (some? p) (not (integer? p)))
      (conj ":poll-ms must be an integer (ms)")

      (and (integer? p) (< (long p) (long (:min-poll-ms timing))))
      (conj (str ":poll-ms " p " is under the minimum " (:min-poll-ms timing) " ms"))

      (nil? d)
      (conj "a deadline is required (deadline or timeout_ms): a poll must end")

      (and (some? d) (some? now) (> (- (long d) (long now)) max-deadline-ms))
      (conj (str "the deadline is more than " (quot max-deadline-ms 3600000) " h away")))))

(m/=> timing-errors [:=> [:cat :map PollSpec [:maybe :int]] [:vector :string]])

(def comparators
  "Comparison operators a predicate may name, by their text."
  {">" > ">=" >= "<" < "<=" <= "==" == "=" == "!=" not=})

(defn comparator-of
  "The comparison fn `op` names (a string or keyword), or nil. Pure."
  [op]
  (get comparators (some-> op name str/trim)))

(m/=> comparator-of [:=> [:cat :any] [:maybe ifn?]])

(defn ->number
  "`x` as a number when it is one or reads as one, else nil. Pure."
  [x]
  (cond
    (number? x) x
    (string? x) (let [s (str/trim x)]
                  (try (if (re-matches #"[+-]?\d+" s)
                         (parse-long s)
                         (let [d (Double/parseDouble s)] (when-not (Double/isNaN d) d)))
                       (catch NumberFormatException _ nil)))
    :else nil))

(m/=> ->number [:=> [:cat :any] [:maybe number?]])

(defn compare-values
  "True when `observed` `op` `expected` holds. Numbers compare numerically
   (a numeric string is a number); anything else only under = / == / !=,
   by its string form. A NaN or a nil never holds. Pure."
  [op observed expected]
  (let [f  (comparator-of op)
        on (->number observed)
        en (->number expected)]
    (boolean
     (cond
       (or (nil? f) (nil? observed)) false
       (and on en) (if (#{"!="} (name op)) (not= (double on) (double en)) (f on en))
       (#{"=" "=="} (name op)) (= (str observed) (str expected))
       (= "!=" (name op)) (not= (str observed) (str expected))
       :else false))))

(m/=> compare-values [:=> [:cat :any :any :any] :boolean])

;; =============================================================================
;; Boundary defaults
;; =============================================================================

(defrecord SystemClock []
  b/IClock
  (now-ms [_] (System/currentTimeMillis)))

(def ^:dynamic *clock*
  "The IClock polling sources read, when the source opens."
  (->SystemClock))

(def ^:dynamic *scheduler*
  "The IPollScheduler polling sources run on, read when the source opens.
   nil means the process scheduler (hive-mcp.await.adapters.scheduler),
   resolved late so loading this namespace starts no thread."
  nil)

(defn- current-scheduler []
  (or *scheduler*
      ((requiring-resolve 'hive-mcp.await.adapters.scheduler/process-scheduler))))

;; =============================================================================
;; The engine
;; =============================================================================

(defn check!
  "Throw the refusal for `awaiter` when its timing cannot be polled under
   `timing`, or `parse` refuses its subscription. `parse` is the source's
   pure (fn [subscription]) -> parsed spec, throwing a refusal. Answers nil."
  [source timing awaiter parse]
  ;; measured from the awaiter's own creation stamp (the registry's clock),
  ;; so a deadline is judged on the clock that set it
  (let [errs (timing-errors awaiter timing (or (:created awaiter) (b/now-ms *clock*)))]
    (when (seq errs)
      (throw (refusal source (str/join "; " errs) {:errors errs}))))
  (parse (:subscription awaiter))
  nil)

(defn open-polling
  "Start polling for `awaiter`: every interval run (probe) and, on each
   {:match? true}, hand (deliver! {:type source :data data}) until closed.

   A match is delivered on EVERY matching poll, not only the first: the
   registry closes the source when the awaiter wakes (deliver! releases
   it), and the pure step ignores an event for an awaiter that already
   woke. Delivering again is what keeps a wake from being lost when the
   first one raced arm! while the awaiter was being created.

   A probe that throws is logged and counted on the next delivered event
   (:poll-errors) and polling goes on: a database or a pod that is not up
   yet is not a reason to give up before the deadline. Polling stops at the
   deadline (the registry wakes the awaiter with :timeout). Answers the
   zero-arg close fn; closing twice is harmless."
  [source timing awaiter deliver! probe]
  (let [clock     *clock*
        scheduler (current-scheduler)
        interval  (poll-interval (:subscription awaiter) timing)
        deadline  (:deadline awaiter)
        stopped?  (atom false)
        polls     (atom 0)
        errors    (atom 0)
        cancel    (atom nil)
        stop!     (fn []
                    (when (compare-and-set! stopped? false true)
                      (when-let [c @cancel] (c))))
        tick      (fn []
                    (try
                      (cond
                        @stopped? nil

                        (and deadline (>= (b/now-ms clock) (long deadline)))
                        (stop!)

                        :else
                        (let [n (swap! polls inc)
                              r (try (probe)
                                     (catch Throwable t
                                       (swap! errors inc)
                                       ;; the ex-data :error keyword and the
                                       ;; source's own message only: an
                                       ;; adapter keeps secrets out of both
                                       (log/warn "await: poll failed; polling on"
                                                 {:source source :id (:id awaiter) :poll n
                                                  :error (or (:error (ex-data t)) (.getName (class t)))
                                                  :message (when (instance? clojure.lang.ExceptionInfo t)
                                                             (.getMessage t))})
                                       nil))]
                          (when (and (:match? r) (not @stopped?))
                            (deliver! {:type source
                                       :data (merge (:data r)
                                                    {:source source :poll n
                                                     :poll-errors @errors})}))))
                      (catch Throwable t
                        (log/warn t "await: poll tick crashed" {:source source :id (:id awaiter)}))))]
    (reset! cancel (b/schedule-repeating! scheduler tick interval))
    ;; a close that raced the schedule: cancel what was just scheduled
    (when @stopped? (when-let [c @cancel] (c)))
    (fn close [] (stop!))))

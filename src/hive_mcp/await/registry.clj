(ns hive-mcp.await.registry
  "Wake bounded context, L2 boundary: the awaiter registry.

   Holds every awaiter, persists them as EDN (temp file, fsync, then
   ATOMIC_MOVE, so a crash mid-write never leaves a torn registry), and
   re-arms on init: a JVM restart while a ling is parked keeps its wait.

   Every decision is the pure hive-mcp.await.domain; this namespace only
   executes it. Two-phase park (decision 20261111151358-4b8e5a50): create!
   records a :pending awaiter and opens its source at once, but a matching
   event or a passed deadline while :pending is only RECORDED on the
   awaiter. `arm!` (on the :parked event) promotes it to :armed and, when a
   wake was recorded meanwhile, fires it. That is what stops a wake from
   resuming a run that is still turning, and what lets the run id be bound
   after the run parked instead of before.

   Wake delivery is at-least-once: :waking is persisted BEFORE the resumer
   is called, :woken only after it answered success, and a deferred or
   failed answer schedules a re-drive with backoff. A restart re-drives what
   was left :waking, so a wake is never silently lost.

   A registry is a map {:path :state :now-fn :executor :resume-executor},
   so a test builds its own over a temp file and a fake clock;
   `default-registry` is the process one, and `boot!` is what initialises it
   at server start."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-mcp.await.adapters.ednl-journal :as ednl-journal]
            [hive-mcp.await.domain :as domain]
            [hive-mcp.await.ports :as ports]
            [malli.core :as m]
            [taoensso.timbre :as log]
            [hive-mcp.resilience.swallow :as swallow])
  (:import (java.io File RandomAccessFile)
           (java.nio.channels FileChannel FileLock OverlappingFileLockException)
           (java.nio.file Files StandardCopyOption CopyOption)
           (java.util.concurrent Executors ExecutorService ScheduledExecutorService
                                 ScheduledFuture TimeUnit ThreadFactory)))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Configuration
;; =============================================================================

(def default-pending-ttl-ms
  "How long a :pending awaiter may sit before it is cancelled: a ling that
   created an awaiter and then ended some other way never parks, and its
   wait must not leak."
  600000)

(def default-max-attempts
  "How many times a wake is re-driven before the registry stops retrying and
   leaves the awaiter :waking with the failure recorded for a human."
  10)

(def backoff-ms
  "Re-drive backoff, indexed by attempt (the last value repeats)."
  [250 1000 5000 15000 60000])

(defn- backoff-for
  "The delay before re-drive attempt `n` (1-based). Pure."
  [n]
  (nth backoff-ms (min (dec (max 1 (long (or n 1)))) (dec (count backoff-ms)))))

(m/=> backoff-for [:=> [:cat :int] :int])

;; =============================================================================
;; Location
;; =============================================================================

(defn default-path
  "The registry file: the hive.await.registry system property, else
   registry.edn under the hive-mcp data dir ($XDG_DATA_HOME/hive-mcp/await,
   falling back to user.home/.local/share/hive-mcp/await)."
  []
  (or (System/getProperty "hive.await.registry")
      (let [xdg (System/getenv "XDG_DATA_HOME")
            base (if (seq xdg)
                   (io/file xdg "hive-mcp")
                   (io/file (System/getProperty "user.home") ".local" "share" "hive-mcp"))]
        (str (io/file base "await" "registry.edn")))))

(m/=> default-path [:=> [:cat] :string])

(def Registry
  "A registry value as make-registry builds it: its EDN :path, the :now-fn
   clock (epoch ms), the deadline :executor, the :resume-executor that runs
   a resume off the deadline thread, the single-writer :lock and the
   :state atom."
  [:map
   [:path :string]
   [:journal {:optional true} [:maybe [:fn #(satisfies? ports/IWakeJournal %)]]]
   [:now-fn [:fn ifn?]]
   [:executor [:fn some?]]
   [:resume-executor [:fn some?]]
   [:lock {:optional true} [:maybe [:fn some?]]]
   [:state [:fn #(instance? clojure.lang.IAtom %)]]])

(def ResumeResult
  "What an owner's resumer answered, as resume! records it: {:ok? true
   :value v} or {:ok? false :error message :data ex-data :attempt n}. A
   deferred answer (heap pressure) carries :deferred true, and the recorded
   failure carries :give-up? once :max-attempts is reached."
  [:map {:closed true}
   [:ok? :boolean]
   [:value {:optional true} :any]
   [:error {:optional true} [:maybe :string]]
   [:data {:optional true} [:maybe :map]]
   [:attempt {:optional true} [:maybe :int]]
   [:deferred {:optional true} :boolean]
   [:give-up? {:optional true} :boolean]])

(def InitReport
  "What init! answers: how many awaiters were loaded, the ids armed, the ids
   whose wake was put back in flight, the ids cancelled as stale :pending,
   the ids that could not be restored with the reason, and the path a
   corrupt registry file was moved aside to (nil when it read fine)."
  [:map {:closed true}
   [:loaded :int]
   [:armed [:vector :string]]
   [:redriven [:vector :string]]
   [:cancelled [:vector :string]]
   [:failed [:vector [:map {:closed true} [:id :string] [:error [:maybe :string]]]]]
   [:corrupt-aside {:optional true} [:maybe :string]]])

(def CreateRequest
  "A create! request: the awaiter's :owner and :subscription, plus an
   optional :id, absolute :deadline (epoch ms), relative :timeout-ms and
   continuation :prompt."
  [:map {:closed true}
   [:id {:optional true} [:maybe domain/AwaiterId]]
   [:owner domain/Owner]
   [:subscription domain/Subscription]
   [:deadline {:optional true} [:maybe :int]]
   [:timeout-ms {:optional true} [:maybe :int]]
   [:prompt {:optional true} [:maybe :string]]])

;; =============================================================================
;; Persistence: EDN, fsync, ATOMIC_MOVE
;; =============================================================================

(defn- corrupt-aside!
  "Move the unreadable registry at `f` aside to <name>.corrupt-<ts>, so a
   later persist! writes a fresh file instead of overwriting the evidence,
   and answer the new path."
  [^File f]
  (let [aside (io/file (.getParentFile (.getAbsoluteFile f))
                       (str (.getName f) ".corrupt-" (System/currentTimeMillis)))]
    (Files/move (.toPath f) (.toPath aside)
                (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))
    (str aside)))

(declare read-awaiters*)

(defn read-awaiters
  "The awaiters persisted at `path` as {id awaiter}; {} when there is no
   file yet.

   An unreadable (torn, empty, truncated) file is moved aside to
   <path>.corrupt-<ts> and read as {}, so boot is never blocked and the
   damaged file is preserved for a human instead of being silently
   overwritten by the next write."
  [path]
  (read-awaiters* path nil))

(defn read-awaiters*
  "read-awaiters, with the aside path of a corrupt file delivered to
   `on-corrupt` (a one-arg fn) when one is given."
  ([path] (read-awaiters* path nil))
  ([path on-corrupt]
   (let [f (io/file path)]
     (if-not (.exists f)
       {}
       (try
         (let [v (edn/read-string (slurp f))]
           (if (map? v) (or (:awaiters v) {}) {}))
         (catch Exception e
           (log/warn e "await registry: unreadable file, moving it aside" {:path path})
           (let [aside (try (corrupt-aside! f)
                            (catch Exception e2
                              (log/error e2 "await registry: could not move the corrupt file aside"
                                         {:path path})
                              nil))]
             (when (and aside on-corrupt) (on-corrupt aside))
             {})))))))

(m/=> read-awaiters* [:function
                      [:=> [:cat :string] [:map-of :string domain/Awaiter]]
                      [:=> [:cat :string [:maybe [:fn ifn?]]] [:map-of :string domain/Awaiter]]])

(m/=> read-awaiters [:=> [:cat :string] [:map-of :string domain/Awaiter]])

(defn- fsync!
  "Force `f`'s bytes to the device, so the temp file is durable before it is
   moved over the registry."
  [^File f]
  (with-open [raf (RandomAccessFile. f "rw")
              ch  (.getChannel raf)]
    (.force ch true)))

(defn write-awaiters!
  "Persist `awaiters` ({id awaiter}) at `path`: write a sibling temp file,
   fsync it, then move it over the target with ATOMIC_MOVE, so a reader sees
   the old registry or the new one, never a torn write, and a crash after
   the write still leaves the bytes on disk."
  [path awaiters]
  (let [target (io/file path)
        dir    (.getParentFile (.getAbsoluteFile target))
        _      (.mkdirs dir)
        tmp    (File/createTempFile "registry" ".edn.tmp" dir)]
    (try
      (spit tmp (pr-str {:version 2 :awaiters awaiters}))
      (fsync! tmp)
      (Files/move (.toPath tmp) (.toPath target)
                  (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                          StandardCopyOption/REPLACE_EXISTING]))
      (finally
        (when (.exists tmp) (.delete tmp))))
    path))

(m/=> write-awaiters! [:=> [:cat :string [:map-of :string domain/Awaiter]] :string])

;; =============================================================================
;; One registry per file
;; =============================================================================

(defn- lock-path [path] (str path ".lock"))

(defn acquire-lock!
  "Take the single-writer lock for `path`, so two registries over one file
   cannot erase each other's awaiters. Throws :await/registry-locked when
   another registry (in this JVM or another) holds it. Answers a
   {:channel :lock} pair to release later."
  [path]
  (let [f   (io/file (lock-path path))
        _   (.mkdirs (.getParentFile (.getAbsoluteFile f)))
        raf (RandomAccessFile. f "rw")
        ch  (.getChannel raf)]
    (try
      (let [lock (.tryLock ch)]
        (if lock
          {:channel ch :lock lock}
          (do (.close raf)
              (throw (ex-info (str "await registry: another registry holds " (str f))
                              {:error :await/registry-locked :path path :lock (str f)})))))
      (catch OverlappingFileLockException _
        (.close raf)
        (throw (ex-info (str "await registry: this JVM already holds " (str f))
                        {:error :await/registry-locked :path path :lock (str f)})))
      (catch Throwable t
        (when (.isOpen ch) (.close ch))
        (throw t)))))

(m/=> acquire-lock! [:=> [:cat :string] [:map [:channel [:fn some?]] [:lock [:fn some?]]]])

(defn release-lock!
  "Release the lock `held` (an acquire-lock! answer). Never throws."
  [held]
  (when-let [{:keys [^FileChannel channel ^FileLock lock]} held]
    (swallow/attempt (when (.isValid lock) (.release lock)))
    (swallow/attempt (when (.isOpen channel) (.close channel))))
  :released)

(m/=> release-lock! [:=> [:cat Registry] [:= :released]])

;; =============================================================================
;; Registry value
;; =============================================================================

(defn- daemon-factory
  "A ThreadFactory naming and daemonising every thread it makes, so no
   await thread can hold the JVM open."
  [name]
  (reify ThreadFactory
    (newThread [_ r]
      (doto (Thread. ^Runnable r ^String name)
        (.setDaemon true)))))

(defn make-registry
  "A registry over `path`. opts: :now-fn (epoch-ms clock, default
   System/currentTimeMillis), :pending-ttl-ms, :max-attempts. Nothing is
   armed and no lock is taken until `init!`."
  ([path] (make-registry path {}))
  ([path {:keys [now-fn pending-ttl-ms max-attempts journal]}]
   {:path            path
    :journal         (or journal (ednl-journal/journal (ednl-journal/path-beside path)))
    :now-fn          (or now-fn #(System/currentTimeMillis))
    :pending-ttl-ms  (long (or pending-ttl-ms default-pending-ttl-ms))
    :max-attempts    (long (or max-attempts default-max-attempts))
    :executor        (Executors/newSingleThreadScheduledExecutor
                      (daemon-factory "hive-await-deadline"))
    :resume-executor (Executors/newSingleThreadExecutor
                      (daemon-factory "hive-await-resume"))
    :lock            nil
    :state           (atom {:awaiters {} :closers {} :timers {} :resumes {} :resumers {} :lock nil}
                           :validator map?)}))

(m/=> make-registry [:function
                     [:=> [:cat :string] Registry]
                     [:=> [:cat :string [:map {:closed true}
                                         [:now-fn {:optional true} [:maybe [:fn ifn?]]]
                                         [:pending-ttl-ms {:optional true} [:maybe :int]]
                                         [:max-attempts {:optional true} [:maybe :int]]
                                         [:journal {:optional true} [:maybe [:fn #(satisfies? ports/IWakeJournal %)]]]]]
                      Registry]])

(defn- now [reg] ((:now-fn reg)))

(defn- persist!
  "Write the current awaiters of `reg` to its file. Serialized on the
   registry's state atom and the state is read inside the lock, so of two
   racing persists the later one always writes the newer snapshot."
  [reg]
  (let [state (:state reg)]
    (locking state
      (write-awaiters! (:path reg) (:awaiters @state)))))

(declare cancel! arm! default-registry)

(defn awaiters
  "Every awaiter the registry holds, as a vector sorted by :created."
  [reg]
  (vec (sort-by (juxt :created :id) (vals (:awaiters @(:state reg))))))

(m/=> awaiters [:=> [:cat Registry] [:vector domain/Awaiter]])

(defn lookup
  "The awaiter `id`, or nil."
  [reg id]
  (get-in @(:state reg) [:awaiters (str id)]))

(m/=> lookup [:=> [:cat Registry domain/AwaiterId] [:maybe domain/Awaiter]])

(defn resume-result
  "What the owner's resumer answered for awaiter `id`, or nil."
  [reg id]
  (get-in @(:state reg) [:resumes (str id)]))

(m/=> resume-result [:=> [:cat Registry domain/AwaiterId] [:maybe ResumeResult]])

(defonce ^:private process-registry (atom nil))

(defn lookup-awaiter
  "The awaiter `id` in the process registry, or nil.

   This is the read the park handshake needs from the other side of the
   boundary (hive-agent calls it, softly, before it ends a run :parked), so
   it takes the id alone: no registry argument, no side effect beyond the
   first-use init that default-registry already does."
  [id]
  (lookup (default-registry) id))

(m/=> lookup-awaiter [:=> [:cat domain/AwaiterId] [:maybe domain/Awaiter]])

;; =============================================================================
;; Executing a step
;; =============================================================================

(defn- release!
  "Close the source and cancel the deadline timer held for `id`."
  [reg id]
  (let [[old _] (swap-vals! (:state reg)
                            #(-> % (update :closers dissoc id)
                                 (update :timers dissoc id)))]
    (when-let [close (get-in old [:closers id])]
      (try (close) (catch Exception e
                     (log/warn e "await: source close failed" {:id id}))))
    (when-let [^ScheduledFuture t (get-in old [:timers id])]
      (.cancel t false))))

(defn- observe!
  "Hand one resume attempt of `a` with `result` to the registry's wake
   journal (ports/IWakeJournal), before its outcome is persisted. A journal
   that fails is logged and NEVER changes the outcome: a wake already
   delivered must not be re-driven because its record could not be written
   (review B1)."
  [reg a result]
  (when-let [j (:journal reg)]
    (try (ports/append-wake! j (domain/wake-entry a result (now reg)))
         (catch Throwable t
           (log/error t "await: wake journal append failed; the outcome stands"
                      {:id (:id a) :attempt (:attempt a) :ok? (:ok? result)})))))

(defn- record-result!
  [reg id result]
  (swap! (:state reg) assoc-in [:resumes id] result)
  result)

(declare resume!)

(defn- redrive!
  "Schedule one more resume attempt for `id`, on the deadline executor,
   after the backoff for its attempt count."
  [reg id]
  (let [a     (lookup reg id)
        delay (backoff-for (or (:attempt a) 1))
        ^ScheduledExecutorService ex (:executor reg)]
    (when a
      (log/warn "await: re-driving a wake" {:id id :attempt (:attempt a) :in-ms delay})
      (.schedule ex ^Runnable (fn []
                               (try (resume! reg id)
                                    (catch Throwable t
                                      (log/warn t "await: re-drive failed" {:id id}))))
                 (long delay) TimeUnit/MILLISECONDS))))

(defn- claim-attempt!
  "Claim the next attempt number of the :waking awaiter `id` in one swap and
   persist the claim BEFORE the attempt runs (review N1). Answers the
   awaiter as claimed, or nil when it is not :waking or its attempts are
   spent."
  [reg id]
  (let [max-n (long (:max-attempts reg default-max-attempts))
        [old new] (swap-vals! (:state reg)
                              (fn [st]
                                (if-let [a' (some-> (get-in st [:awaiters id])
                                                    (domain/begin-attempt max-n))]
                                  (assoc-in st [:awaiters id] a')
                                  st)))
        a (get-in new [:awaiters id])]
    (when-not (identical? (get-in old [:awaiters id]) a)
      (persist! reg)
      a)))

(defn- call-resumer
  "Run `a`'s resumer once; answer {:ok? ...} (never throws)."
  [reg id a]
  (let [payload (:wake a)
        prompt  (domain/resume-prompt a payload)]
    (try
      (let [f (or (get-in @(:state reg) [:resumers id])
                  (ports/open-resumer (:owner a)))
            v (f {:awaiter a :payload payload :prompt prompt})]
        (cond
          (domain/deferred? v)
          {:ok? false :deferred true :error "resumer deferred (heap pressure)" :data {:answer v}}

          (and (map? v) (false? (:success v)))
          {:ok? false :error "resumer reported failure" :data {:answer v}}

          :else {:ok? true :value v}))
      (catch Throwable e
        (log/warn e "await: resumer failed" {:id id :owner (:owner a)})
        {:ok? false :error (.getMessage e) :data (ex-data e)}))))

(defn resume!
  "Run ONE attempt of the waking awaiter's resumer, off the deadline thread.

   The attempt number is claimed and persisted first (claim-attempt!), so a
   crash anywhere after makes the next run a new attempt: no [id attempt]
   pair is ever run, or journaled, twice (review N1). Then the resumer
   runs, the wake journal observes the outcome, and the outcome is applied
   only while the row is still :waking, so a cancel that landed during the
   resume stands.

   A success marks the awaiter :woken. A deferred answer (heap pressure:
   {:success false :deferred true}) or any non-success leaves it :waking and
   schedules a re-drive while attempts remain; once :max-attempts attempts
   are claimed the failure stays recorded for a human, and since the spent
   count is on disk a restart does not run it again. Never throws. Answers
   the ResumeResult."
  [reg id]
  (let [id (str id)]
    (when-let [a0 (lookup reg id)]
      (if-let [a (claim-attempt! reg id)]
        (let [attempt (:attempt a)
              result  (assoc (call-resumer reg id a) :attempt attempt)
              _       (observe! reg a result)
              still?  (fn [st] (domain/waking? (get-in st [:awaiters id])))]
          (if (:ok? result)
            (let [[old _] (swap-vals! (:state reg)
                                      #(if (still? %)
                                         (-> % (update-in [:awaiters id] domain/mark-woken)
                                             (update :resumers dissoc id))
                                         %))]
              (when (still? old) (persist! reg))
              (record-result! reg id result))
            (let [give-up? (>= (long attempt) (long (:max-attempts reg default-max-attempts)))]
              (record-result! reg id (assoc result :give-up? give-up?))
              (cond
                (not (still? @(:state reg)))
                (log/info "await: awaiter left :waking during its resume; no re-drive" {:id id})

                give-up?
                (log/error "await: wake could not be delivered; leaving it :waking for a human"
                           {:id id :attempt attempt :error (:error result)})

                :else (redrive! reg id))
              result)))
        (if (domain/waking? a0)
          (or (resume-result reg id)
              (do (log/error "await: attempts were spent before this restart; leaving it :waking for a human"
                             {:id id :attempt (:attempt a0)})
                  (record-result! reg id {:ok? false :give-up? true :attempt (:attempt a0)
                                          :error "attempts spent before this restart; left :waking for a human"})))
          (or (resume-result reg id) {:ok? false :error "not waking"}))))))

(m/=> resume! [:=> [:cat Registry domain/AwaiterId] [:maybe ResumeResult]])

(defn- launch-wake!
  "The effects of a wake THIS thread started (its swap moved the row to
   :waking): persist it, drop the source and the deadline timer, then
   resume on the resume executor. Runs once per wake, because only one swap
   can make that move."
  [reg id]
  (persist! reg)
  (release! reg id)
  (.execute ^ExecutorService (:resume-executor reg)
            ^Runnable (fn [] (try (resume! reg id)
                                  (catch Throwable t
                                    (log/error t "await: resume crashed" {:id id})))))
  id)

(def ^:private wakeable-statuses
  "The statuses a wake may start from: an awaiter already :waking, :woken or
   :cancelled is never handed over a second time (review B3)."
  #{:pending :armed})

(defn- begin-wake-in
  "Pure over the state map: `st` with awaiter `id` moved to :waking on
   `payload` (default its recorded wake), when its status allows a wake;
   `st` unchanged otherwise."
  [st id payload]
  (let [a (get-in st [:awaiters id])
        p (or payload (:wake a) (:pending-wake a))]
    (if (and a p (contains? wakeable-statuses (:status a)))
      (assoc-in st [:awaiters id] (domain/begin-wake a p))
      st)))

(defn- started-wake?
  "True when the swap old -> new moved awaiter `id` into :waking."
  [old new id]
  (let [o (get-in old [:awaiters id]) n (get-in new [:awaiters id])]
    (and (not (identical? o n)) (= :waking (:status n)) (not= :waking (:status o)))))

(defn deliver!
  "Hand the awaiter `id` to its resumer with `payload` (default: the wake it
   recorded). The move to :waking happens inside ONE swap and only from
   :pending or :armed, so of two racing deliveries exactly one starts the
   wake; the other answers nil. The starter persists :waking, drops the
   source and the deadline timer, then resumes on the resume executor. This
   is the one place a wake leaves the registry. Answers `id` when this call
   started the wake, else nil."
  ([reg id] (deliver! reg id nil))
  ([reg id payload]
   (let [id (str id)
         [old new] (swap-vals! (:state reg) begin-wake-in id payload)]
     (when (started-wake? old new id)
       (launch-wake! reg id)))))

(m/=> deliver! [:function
                [:=> [:cat Registry domain/AwaiterId] [:maybe [:string {:min 1}]]]
                [:=> [:cat Registry domain/AwaiterId [:maybe :map]] [:maybe [:string {:min 1}]]]])

(defn offer!
  "Offer `event` (nil for a clock tick) to awaiter `id`: run the pure step
   and persist the result. A :pending awaiter records the wake and is NOT
   resumed; an :armed one that woke is moved to :waking IN THE SAME SWAP
   and handed to its resumer.

   Because the step and the move to :waking are one swap, two racing events
   (or an event racing arm!) start a wake at most once. Answers the step
   result, or nil when no such awaiter exists."
  [reg id event]
  (let [id      (str id)
        t       (now reg)
        result* (volatile! nil)
        [old new] (swap-vals! (:state reg)
                              (fn [st]
                                (if-let [a (get-in st [:awaiters id])]
                                  (let [r  (domain/step a event t)
                                        a' (if (:wake? r)
                                             (domain/begin-wake (:awaiter r) (:payload r))
                                             (:awaiter r))]
                                    (vreset! result* r)
                                    (assoc-in st [:awaiters id] a'))
                                  (do (vreset! result* nil) st))))
        r (when (get-in old [:awaiters id]) @result*)]
    (cond
      (started-wake? old new id) (launch-wake! reg id)
      (not= (get-in old [:awaiters id]) (get-in new [:awaiters id])) (persist! reg))
    r))

(m/=> offer! [:=> [:cat Registry domain/AwaiterId [:maybe domain/Event]] [:maybe domain/StepResult]])

(defn tick!
  "Offer a clock tick to every live awaiter, so every reached deadline is
   recorded (:pending) or wakes (:armed). Also cancels a :pending awaiter
   older than :pending-ttl-ms: its ling never parked, so nothing will ever
   arm it. Answers the ids that woke."
  [reg]
  (let [t    (now reg)
        ttl  (long (:pending-ttl-ms reg default-pending-ttl-ms))
        all  (filter domain/live? (awaiters reg))
        stale (filter (fn [a] (and (domain/pending? a)
                                   (some? (:created a))
                                   (<= (+ (long (:created a)) ttl) t)))
                      all)]
    (doseq [a stale]
      (log/warn "await: cancelling a :pending awaiter past its TTL (its ling never parked)"
                {:id (:id a) :created (:created a)})
      (cancel! reg (:id a)))
    (into []
          (keep (fn [a] (when (:wake? (offer! reg (:id a) nil)) (:id a))))
          (remove (set (map :id stale)) all))))

(m/=> tick! [:=> [:cat Registry] [:vector :string]])

;; =============================================================================
;; Arming
;; =============================================================================

(declare listening? store-while-listening!)

(defn- schedule-deadline!
  "Schedule one clock tick for awaiter `a` at its deadline, replacing any
   timer already held for it. The timer is stored with the same conditional
   swap as a source closer, and cancelled when the awaiter stopped
   listening meanwhile (review N3)."
  [reg a]
  (when-let [d (:deadline a)]
    (let [id    (:id a)
          delay (max 0 (- (long d) (now reg)))
          ^ScheduledExecutorService ex (:executor reg)
          ^ScheduledFuture fut
          (.schedule ex ^Runnable (fn [] (try (offer! reg id nil)
                                              (catch Throwable e
                                                (log/warn e "await: deadline tick failed" {:id id}))))
                     (long delay) TimeUnit/MILLISECONDS)
          ^ScheduledFuture prev (get-in @(:state reg) [:timers id])]
      (if (store-while-listening! reg :timers id fut)
        (when (and prev (not (identical? prev fut))) (.cancel prev false))
        (.cancel fut false)))))

(defn- listening?
  "True when awaiter `id` in state map `st` still holds its source."
  [st id]
  (contains? #{:pending :armed} (get-in st [:awaiters id :status])))

(defn- store-while-listening!
  "Store `handle` at [k id] ONLY while awaiter `id` is still listening, in
   one swap (review N3: no check-then-act). Answers true when stored. When
   it answers false a wake already ran release! for `id`, so the caller
   must release `handle` itself. Both orders are safe: a store that lands
   first is found by the waker's release!, which runs after the waker's own
   status swap; a wake that lands first refuses the store."
  [reg k id handle]
  (let [[_ new] (swap-vals! (:state reg)
                            (fn [st] (if (listening? st id) (assoc-in st [k id] handle) st)))]
    (identical? handle (get-in new [k id]))))

(defn- open-source!
  "Open the source for `a`, so its events reach offer! for `id`.

   A source may emit before its closer is stored: on the calling thread
   inside open-source (a burst at subscribe), or from a thread of its own
   (a poller, a tail, a concurrent `await fire`). The closer is stored only
   while the awaiter still listens, atomically, and is called at once when
   the store is refused, so no subscription outlives its awaiter."
  [reg a]
  (let [id    (:id a)
        close (ports/open-source (:subscription a) a #(offer! reg id %))]
    (when (and close (not (store-while-listening! reg :closers id close)))
      (try (close) (catch Exception e
                     (log/warn e "await: source close failed" {:id id}))))
    a))

(defn- resolve-resumer
  "Open `a`'s resumer now when its owner kind can be resolved at this point,
   so an unknown kind or an owner that is already complete is refused before
   anything is recorded against it. A deferred kind (`:hive-agent-run`) is
   resolved at delivery instead, because its run id does not exist yet."
  [reg a]
  (when-not (or (contains? ports/deferred-owner-kinds (get-in a [:owner :kind]))
                (get-in @(:state reg) [:resumers (:id a)]))
    (swap! (:state reg) assoc-in [:resumers (:id a)] (ports/open-resumer (:owner a))))
  a)

(defn- subscribe!
  "Open the source and schedule the deadline of `a`. Neither is kept for
   an awaiter that already woke while its source opened."
  [reg a]
  (resolve-resumer reg a)
  (open-source! reg a)
  ;; schedule-deadline! stores its timer only while the awaiter listens.
  (schedule-deadline! reg a)
  a)

(defn create!
  "Register a new awaiter from `request` {:id :owner :subscription :deadline
   :prompt}; :id defaults to a random uuid, and :timeout-ms sets :deadline
   relative to now. Validated against domain/Awaiter.

   An owner that parks (:hive-agent-run) is inserted :pending: its source
   opens at once, so an event that arrives before the ling parks is
   recorded rather than lost, but nothing is resumed until `arm!`. An owner
   that never parks (ports/arm-on-create?: :session, :journal) is inserted
   ALREADY :armed (review B3): there is no pending phase to race, so a
   source that emits from inside open-source (a burst at subscribe, the
   RUN-LEDGER-4 case) wakes it exactly once.

   The duplicate check and the insert are one swap, so two concurrent
   creates of one id cannot both succeed. Persisted before the source opens,
   so a crash between the two re-subscribes it on boot. Answers the awaiter
   as it stands after the subscribe (it may already be :waking)."
  [reg request]
  (let [t   (now reg)
        req (cond-> request
              (nil? (:id request)) (assoc :id (str (random-uuid)))
              (and (nil? (:deadline request)) (number? (:timeout-ms request)))
              (assoc :deadline (+ t (long (:timeout-ms request)))))
        a   (cond-> (domain/new-awaiter req t)
              (ports/arm-on-create? (:owner req)) (assoc :status :armed))]
    (when-let [err (m/explain domain/Awaiter a)]
      (throw (ex-info "await: invalid awaiter"
                      {:error :await/invalid :explain (pr-str (:errors err))})))
    ;; Resolve both ports before anything is recorded: a refused owner kind
    ;; or source type leaves no awaiter behind.
    (ports/require-ports! a)
    ;; A resumer that resolves now is OPENED now, before the insert: an
    ;; owner its kind refuses (a :session with no or a bad :inbox) leaves no
    ;; row.
    (let [resumer (when-not (contains? ports/deferred-owner-kinds (get-in a [:owner :kind]))
                    (ports/open-resumer (:owner a)))
          ins (volatile! nil)]
      (swap! (:state reg)
             (fn [st]
               (if (get-in st [:awaiters (:id a)])
                 (do (vreset! ins :duplicate) st)
                 (do (vreset! ins :ok)
                     (cond-> (assoc-in st [:awaiters (:id a)] a)
                       resumer (assoc-in [:resumers (:id a)] resumer))))))
      (when (= :duplicate @ins)
        (throw (ex-info (str "await: awaiter " (:id a) " already exists")
                        {:error :await/duplicate :id (:id a)}))))
    (persist! reg)
    (subscribe! reg a)
    (lookup reg (:id a))))

(m/=> create! [:=> [:cat Registry CreateRequest] domain/Awaiter])

(defn arm!
  "Promote a :pending awaiter to :armed, binding `run-id` as the parked
   run's chat-point id, and fire the wake it recorded while pending.

   Arming and starting the recorded wake are ONE swap (review B3): an event
   recorded between a read and a write can no longer be lost, and an event
   that wakes the freshly armed row cannot race arm! into a second resume.

   Called on the :parked event (via hive-agent-run's owner kind) or by
   `swarm await arm`. Idempotent: arming an already armed (or woken)
   awaiter changes nothing and delivers nothing twice. Answers
   {:awaiter a :pending-wake w :fired? bool}, or nil when `id` is unknown."
  [reg id run-id]
  (let [id     (str id)
        armed* (volatile! nil)
        [old new] (swap-vals! (:state reg)
                              (fn [st]
                                (if-let [a (get-in st [:awaiters id])]
                                  (let [{a' :awaiter w :pending-wake :as res} (domain/arm a run-id)]
                                    (vreset! armed* res)
                                    (assoc-in st [:awaiters id]
                                              (if w (domain/begin-wake a' w) a')))
                                  (do (vreset! armed* nil) st))))]
    (when (get-in old [:awaiters id])
      (let [{a' :awaiter w :pending-wake} @armed*
            fired? (started-wake? old new id)]
        (cond
          fired? (launch-wake! reg id)
          (not= (get-in old [:awaiters id]) (get-in new [:awaiters id])) (persist! reg))
        {:awaiter a' :pending-wake w :fired? fired?}))))

(m/=> arm! [:=> [:cat Registry domain/AwaiterId [:maybe :any]]
            [:maybe [:map [:awaiter domain/Awaiter] [:pending-wake [:maybe :map]] [:fired? :boolean]]]])

(defn cancel!
  "Cancel awaiter `id` when still live: persist, release its source and
   timer. Answers the awaiter, or nil when unknown."
  [reg id]
  (let [id (str id)]
    (when-let [a (lookup reg id)]
      (when (domain/live? a)
        (swap! (:state reg) update-in [:awaiters id] domain/cancel)
        (persist! reg)
        (release! reg id)
        (swap! (:state reg) update :resumers dissoc id))
      (lookup reg id))))

(m/=> cancel! [:=> [:cat Registry domain/AwaiterId] [:maybe domain/Awaiter]])

(defn init!
  "Take the registry's lock, load the persisted awaiters and restore every
   live one:

   - :armed     re-subscribed (source opened, deadline scheduled; a deadline
                that passed while the JVM was down wakes at once);
   - :pending   re-subscribed, and cancelled when older than the TTL;
   - :waking    its resume put back in flight: a wake was persisted but its
                outcome never was, so it is at-least-once, not lost.

   An awaiter whose port is no longer registered is kept on disk and
   reported under :failed, so a later init with the addon loaded still
   restores it. Answers an InitReport."
  [reg]
  (when-not (get-in @(:state reg) [:lock])
    (swap! (:state reg) assoc :lock (acquire-lock! (:path reg))))
  (let [aside* (volatile! nil)
        loaded (read-awaiters* (:path reg) (fn [p] (vreset! aside* p)))
        t      (now reg)
        ttl    (long (:pending-ttl-ms reg default-pending-ttl-ms))
        stale? (fn [a] (and (domain/pending? a)
                            (some? (:created a))
                            (<= (+ (long (:created a)) ttl) t)))]
    ;; Cancels are applied and persisted BEFORE any subscription opens.
    (swap! (:state reg) assoc :awaiters
           (reduce-kv (fn [acc id a]
                        (if (stale? a)
                          (do (log/warn "await: dropping a stale :pending awaiter at boot"
                                        {:id id :created (:created a)})
                              (assoc acc id (domain/cancel a)))
                          (assoc acc id a)))
                      {} loaded))
    (persist! reg)
    (let [all (vals (:awaiters @(:state reg)))]
      (reduce (fn [acc a]
                (try
                  (cond
                    (stale? a) (update acc :cancelled conj (:id a))
                    ;; The resumer is opened HERE, on the caller's thread, as
                    ;; create! opens it, so a port bound by the caller (an
                    ;; inbox adapter) is the one the re-drive uses.
                    (domain/waking? a) (do (resolve-resumer reg a)
                                           (redrive! reg (:id a))
                                           (update acc :redriven conj (:id a)))
                    (domain/live? a) (do (subscribe! reg a)
                                         (update acc :armed conj (:id a)))
                    :else acc)
                  (catch Throwable e
                    (log/warn e "await: restore failed" {:id (:id a)})
                    (update acc :failed conj {:id (:id a) :error (.getMessage e)}))))
              {:loaded (count loaded) :armed [] :redriven [] :cancelled [] :failed []
               :corrupt-aside @aside*}
              all))))

(m/=> init! [:=> [:cat Registry] InitReport])

(defn shutdown!
  "Close every source and timer, stop both executors and release the lock.
   Persisted state is left as it is, so the next init restores what was
   live."
  [reg]
  (doseq [id (keys (:closers @(:state reg)))] (release! reg id))
  (doseq [id (keys (:timers @(:state reg)))] (release! reg id))
  (swallow/attempt (.shutdownNow ^ScheduledExecutorService (:executor reg)))
  (swallow/attempt (.shutdownNow ^ExecutorService (:resume-executor reg)))
  (release-lock! (:lock @(:state reg)))
  (swap! (:state reg) assoc :lock nil)
  :shutdown)

(m/=> shutdown! [:=> [:cat Registry] [:= :shutdown]])

;; =============================================================================
;; Process registry
;; =============================================================================

(defn default-registry
  "The process registry over `default-path`, created and init!ed on first
   use. `boot!` initialises it at server start; this lazy path exists so a
   REPL or a test that never booted still gets a working registry."
  []
  (or @process-registry
      (locking process-registry
        (or @process-registry
            (let [reg (make-registry (default-path))]
              (init! reg)
              (reset! process-registry reg))))))

(m/=> default-registry [:=> [:cat] Registry])

(defn boot!
  "Initialise the process registry from its file, at server start.

   This is the boot hook B4 asked for: without it a restart left every
   parked ling asleep until somebody happened to call `swarm await list`.
   Idempotent: a process registry already up in this JVM (an earlier boot!,
   or the lazy default-registry) is shut down first, releasing its file lock,
   and the file is read again, so a second boot restores the same awaiters
   instead of refusing on its own lock. Non-fatal: a registry that cannot be
   initialised is logged and left unset, so a broken registry file cannot
   stop the server from starting. Answers the InitReport, or nil on failure."
  []
  (try
    (locking process-registry
      (when-let [old @process-registry]
        (shutdown! old)
        (reset! process-registry nil))
      (let [reg    (make-registry (default-path))
            report (init! reg)]
        (reset! process-registry reg)
        (log/info "await registry initialised at boot" (dissoc report :corrupt-aside))
        report))
    (catch Throwable e
      (log/error e "await registry: boot init failed; awaiters stay unloaded")
      nil)))

(m/=> boot! [:=> [:cat] [:maybe InitReport]])

(defn shutdown-process!
  "Stop the process registry, if one was created. Never throws."
  []
  (when-let [reg @process-registry]
    (try (shutdown! reg) (catch Throwable e (log/warn e "await: shutdown failed")))
    (reset! process-registry nil))
  :shutdown)

(m/=> shutdown-process! [:=> [:cat] [:= :shutdown]])

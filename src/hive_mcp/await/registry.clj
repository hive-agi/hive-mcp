(ns hive-mcp.await.registry
  "Wake bounded context, L2 boundary: the awaiter registry.

   Holds every awaiter, persists them as EDN (temp file + ATOMIC_MOVE, so a
   crash mid-write never leaves a torn registry), and re-arms the :armed
   ones on init: a JVM restart while a ling is parked keeps its wait.

   Every decision is the pure hive-mcp.await.domain/step; this namespace
   only executes it: it opens the source (ports/open-source), schedules the
   deadline tick, and on a wake persists, closes the source and calls the
   owner's resumer (ports/open-resumer) exactly once.

   A registry is a map {:path :state :now-fn :executor}, so a test builds
   its own over a temp file and a fake clock; `default-registry` is the
   process one the `swarm await` verb uses."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [hive-mcp.await.domain :as domain]
            [hive-mcp.await.ports :as ports]
            [malli.core :as m]
            [taoensso.timbre :as log])
  (:import (java.nio.file Files StandardCopyOption CopyOption)
           (java.util.concurrent Executors ScheduledExecutorService
                                 ScheduledFuture TimeUnit ThreadFactory)))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

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

;; =============================================================================
;; Persistence: EDN, temp file + ATOMIC_MOVE
;; =============================================================================

(defn read-awaiters
  "The awaiters persisted at `path` as {id awaiter}; {} when there is no
   file yet. An unreadable file is logged and read as {} rather than
   blocking boot, and is left in place for a human to inspect."
  [path]
  (let [f (io/file path)]
    (if-not (.exists f)
      {}
      (try
        (let [v (edn/read-string (slurp f))]
          (if (map? v) (or (:awaiters v) {}) {}))
        (catch Exception e
          (log/warn e "await registry: unreadable file, starting empty" {:path path})
          {})))))

(defn write-awaiters!
  "Persist `awaiters` ({id awaiter}) at `path`: write a sibling temp file,
   then move it over the target with ATOMIC_MOVE, so a reader sees the old
   registry or the new one, never a torn write."
  [path awaiters]
  (let [target (io/file path)
        dir    (.getParentFile (.getAbsoluteFile target))
        _      (.mkdirs dir)
        tmp    (java.io.File/createTempFile "registry" ".edn.tmp" dir)]
    (spit tmp (pr-str {:version 1 :awaiters awaiters}))
    (Files/move (.toPath tmp) (.toPath target)
                (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                        StandardCopyOption/REPLACE_EXISTING]))
    path))

;; =============================================================================
;; Registry value
;; =============================================================================

(defn- daemon-factory []
  (reify ThreadFactory
    (newThread [_ r]
      (doto (Thread. ^Runnable r "hive-await-deadline")
        (.setDaemon true)))))

(defn make-registry
  "A registry over `path`. opts: :now-fn (epoch-ms clock, default
   System/currentTimeMillis). Nothing is armed until `init!`."
  ([path] (make-registry path {}))
  ([path {:keys [now-fn]}]
   {:path     path
    :now-fn   (or now-fn #(System/currentTimeMillis))
    :executor (Executors/newSingleThreadScheduledExecutor (daemon-factory))
    :state    (atom {:awaiters {} :closers {} :timers {} :resumes {}})}))

(defn- now [reg] ((:now-fn reg)))

(defn- persist!
  [reg]
  (write-awaiters! (:path reg) (:awaiters @(:state reg))))

(defn awaiters
  "Every awaiter the registry holds, as a vector sorted by :created."
  [reg]
  (vec (sort-by (juxt :created :id) (vals (:awaiters @(:state reg))))))

(defn lookup
  "The awaiter `id`, or nil."
  [reg id]
  (get-in @(:state reg) [:awaiters (str id)]))

(defn resume-result
  "What the owner's resumer answered for awaiter `id`, or nil."
  [reg id]
  (get-in @(:state reg) [:resumes (str id)]))

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

(defn- resume!
  "Call the owner's resumer once for the woken `awaiter` and record what it
   answered (or the failure) under :resumes."
  [reg awaiter payload]
  (let [id     (:id awaiter)
        prompt (domain/resume-prompt awaiter payload)
        result (try
                 (let [f (get-in @(:state reg) [:resumers id])
                       f (or f (ports/open-resumer (:owner awaiter)))]
                   {:ok? true :value (f {:awaiter awaiter :payload payload :prompt prompt})})
                 (catch Throwable e
                   (log/warn e "await: resumer failed" {:id id :owner (:owner awaiter)})
                   {:ok? false :error (.getMessage e) :data (ex-data e)}))]
    (swap! (:state reg) #(-> % (assoc-in [:resumes id] result)
                             (update :resumers dissoc id)))
    result))

(defn offer!
  "Offer `event` (nil for a clock tick) to awaiter `id`: run the pure step
   and, on a wake, persist the woken awaiter, release its source and timer,
   then resume its owner. The wake is decided inside one swap, so two
   racing events wake an awaiter at most once. Answers the step result,
   or nil when no such awaiter exists."
  [reg id event]
  (let [id      (str id)
        t       (now reg)
        result* (volatile! nil)]
    (swap! (:state reg)
           (fn [st]
             (if-let [a (get-in st [:awaiters id])]
               (let [r (domain/step a event t)]
                 (vreset! result* (assoc r ::changed? (not= a (:awaiter r))))
                 (assoc-in st [:awaiters id] (:awaiter r)))
               (do (vreset! result* nil) st))))
    (let [r (some-> @result* (dissoc ::changed?))]
      (when (::changed? @result*)
        (persist! reg))
      (when (:wake? r)
        (release! reg id)
        (resume! reg (:awaiter r) (:payload r)))
      r)))

(defn tick!
  "Offer a clock tick to every :armed awaiter, so every reached deadline
   wakes with :reason :timeout. Answers the ids that woke."
  [reg]
  (into []
        (keep (fn [a] (when (:wake? (offer! reg (:id a) nil)) (:id a))))
        (filter domain/armed? (awaiters reg))))

;; =============================================================================
;; Arming
;; =============================================================================

(defn- schedule-deadline!
  "Schedule one clock tick for awaiter `a` at its deadline."
  [reg a]
  (when-let [d (:deadline a)]
    (let [id    (:id a)
          delay (max 0 (- d (now reg)))
          ^ScheduledExecutorService ex (:executor reg)
          fut   (.schedule ex ^Runnable (fn [] (try (offer! reg id nil)
                                                     (catch Throwable e
                                                       (log/warn e "await: deadline tick failed" {:id id}))))
                           (long delay) TimeUnit/MILLISECONDS)]
      (swap! (:state reg) assoc-in [:timers id] fut))))

(defn- arm!
  "Open the source and schedule the deadline of the :armed awaiter `a`.
   The resumer is resolved now, so an unknown owner kind is refused at arm
   time, not discovered at wake time. A deadline already reached wakes on
   the next tick of the executor."
  [reg a]
  (let [id      (:id a)
        resumer (ports/open-resumer (:owner a))
        close   (ports/open-source (:subscription a) a #(offer! reg id %))]
    (swap! (:state reg) #(cond-> (assoc-in % [:resumers id] resumer)
                           close (assoc-in [:closers id] close)))
    (schedule-deadline! reg a)
    a))

(defn create!
  "Register and arm a new awaiter from `request` {:id :owner :subscription
   :deadline :prompt}; :id defaults to a random uuid, and :timeout-ms sets
   :deadline relative to now. Validated against domain/Awaiter. Persisted
   before it is armed, so a crash between the two re-arms it on boot.
   Answers the awaiter."
  [reg request]
  (let [t   (now reg)
        req (cond-> request
              (nil? (:id request)) (assoc :id (str (random-uuid)))
              (and (nil? (:deadline request)) (number? (:timeout-ms request)))
              (assoc :deadline (+ t (long (:timeout-ms request)))))
        a   (domain/new-awaiter req t)]
    (when-let [err (m/explain domain/Awaiter a)]
      (throw (ex-info "await: invalid awaiter"
                      {:error :await/invalid :explain (pr-str (:errors err))})))
    (when (lookup reg (:id a))
      (throw (ex-info (str "await: awaiter " (:id a) " already exists")
                      {:error :await/duplicate :id (:id a)})))
    ;; Resolve both ports before anything is recorded: a refused port leaves
    ;; no awaiter behind.
    (ports/require-ports! a)
    (swap! (:state reg) assoc-in [:awaiters (:id a)] a)
    (persist! reg)
    (arm! reg a)))

(defn cancel!
  "Cancel awaiter `id` when still :armed: persist, release its source and
   timer. Answers the awaiter, or nil when unknown."
  [reg id]
  (let [id (str id)]
    (when (lookup reg id)
      (swap! (:state reg) update-in [:awaiters id] domain/cancel)
      (persist! reg)
      (release! reg id)
      (swap! (:state reg) update :resumers dissoc id)
      (lookup reg id))))

(defn init!
  "Load the persisted awaiters and re-arm every :armed one (sources opened,
   deadlines scheduled; a deadline that passed while the JVM was down wakes
   at once with :timeout). An awaiter whose port is no longer registered is
   kept :armed on disk and reported under :failed, so a later init with the
   addon loaded still re-arms it. Answers {:loaded n :armed [ids] :failed
   [{:id :error}]}."
  [reg]
  (let [loaded (read-awaiters (:path reg))]
    (swap! (:state reg) assoc :awaiters loaded)
    (reduce (fn [acc a]
              (try (arm! reg a)
                   (update acc :armed conj (:id a))
                   (catch Throwable e
                     (log/warn e "await: re-arm failed" {:id (:id a)})
                     (update acc :failed conj {:id (:id a) :error (.getMessage e)}))))
            {:loaded (count loaded) :armed [] :failed []}
            (filter domain/armed? (vals loaded)))))

(defn shutdown!
  "Close every source and timer and stop the executor. Persisted state is
   left as it is, so the next init re-arms what was armed."
  [reg]
  (doseq [id (keys (:closers @(:state reg)))] (release! reg id))
  (doseq [id (keys (:timers @(:state reg)))] (release! reg id))
  (.shutdownNow ^ScheduledExecutorService (:executor reg))
  :shutdown)

;; =============================================================================
;; Process registry
;; =============================================================================

(defonce ^:private process-registry (atom nil))

(defn default-registry
  "The process registry over `default-path`, created and init!ed on first
   use (so the first `swarm await` call after a restart re-arms)."
  []
  (or @process-registry
      (locking process-registry
        (or @process-registry
            (let [reg (make-registry (default-path))]
              (init! reg)
              (reset! process-registry reg))))))

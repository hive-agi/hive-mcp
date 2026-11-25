(ns hive-mcp.nats.sense-replay
  "The sense replay loop: sixth-sense senses reach parent inboxes even when
   they arrived while NATS was down, while the parent had not armed its
   inbox yet, or while the republish queue was full.

   Push (hive-agent.sixth-sense.api/listen!) is only a WAKE. The read path is
   the host sense log's consume-once drain under consumer
   `hive-mcp.nats.sense-adapters/drain-consumer`, whose cursor hive-agent
   persists. One step:

     1. drain every sense after the cursor (republishable classes only),
     2. append them to the PENDING outbox and persist it (EDN, fsync,
        ATOMIC_MOVE) BEFORE publishing, so a crash between the drain (which
        advanced the cursor) and the publish loses nothing,
     3. publish pending oldest first; stop at the first bus failure so order
        holds, keep senses whose parent is still unbound, drop what was sent,
     4. persist what is left.

   Delivery is at-least-once; `sense-<id>` msg ids (local memory plus the
   broker's 2 min duplicate window) make it exactly-once in practice. A
   replay older than that window relies on the outbox, not the broker.

   Hold policy (decision recorded on card 20261116182711-549cf262): a sense
   is held at most `default-ttl-ms` (24 h) and the outbox at most
   `default-cap` (1024) entries, oldest evicted first; both are counted.

   Baseline: a replayer with no persisted state has never run, and the log
   may hold days of old senses. Its first successful drain advances the
   cursor to the head and republishes nothing, so a first deploy does not
   flood inboxes with history.

   Wakes: a new sense (feed listener), a binding change (`inbox arm`), and
   every NATS (re)connect (`hive-mcp.nats.sense-adapters/start!`). Wakes are
   coalesced: at most one step is queued at a time.

   Strata (CPPB):
     Value objects  Held, ReplayState, StepResult
     Pure           hold, expired?, trim, keep-outcome?, bus-down?, edn-safe
     Ports          Store {:load :save!}; drain (fn [opts] -> DrainResult)
     Boundary       mem-store, file-store, replayer, step!, wake!,
                    wake-listener, install!, uninstall!, wake-current!,
                    status"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [hive-mcp.nats.sense-republish :as sr]
            [hive-mcp.resilience.swallow :as swallow]
            [malli.core :as m]
            [taoensso.timbre :as log])
  (:import [java.io File RandomAccessFile]
           [java.nio.file CopyOption Files StandardCopyOption]
           [java.util.concurrent.atomic AtomicBoolean]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Value objects
;; =============================================================================

(def default-ttl-ms
  "How long a sense is held for an unbound parent or a down bus: 24 h."
  (* 24 60 60 1000))

(def default-cap
  "How many senses the outbox holds; the oldest is evicted past it."
  1024)

(def default-batch
  "Senses per drain call."
  256)

(def ^:private max-drains-per-step 64)

(def Held
  "One pending sense and when it entered the outbox (epoch ms)."
  [:map [:sense :map] [:held-at :int]])

(def ReplayState
  [:map
   [:baselined? :boolean]
   [:pending [:vector Held]]])

(def empty-state {:baselined? false :pending []})

(def StepResult
  [:map
   [:drained :int]
   [:baseline? :boolean]
   [:sent :int]
   [:pending :int]
   [:expired :int]
   [:evicted :int]
   [:bus-down? :boolean]
   [:error {:optional true} [:maybe :string]]])

;; =============================================================================
;; Pure
;; =============================================================================

(defn edn-safe
  "`v` with every value EDN cannot read back printed as its string, so a
   persisted outbox is always readable."
  [v]
  (walk/postwalk
   (fn [x]
     (if (or (nil? x) (string? x) (number? x) (boolean? x) (keyword? x)
             (symbol? x) (char? x) (uuid? x) (inst? x)
             (map? x) (coll? x) (map-entry? x))
       x
       (str x)))
   v))

(m/=> edn-safe [:=> [:cat :any] :any])

(defn hold
  "`pending` with `senses` appended, oldest first, held at `now`: only
   republishable senses, and none already held (by sense id)."
  [pending senses now]
  (let [ids (into #{} (map (comp :sense/id :sense)) pending)]
    (first
     (reduce (fn [[acc ids] s]
               (if (or (not (sr/republishable? s)) (contains? ids (:sense/id s)))
                 [acc ids]
                 [(conj acc {:sense (edn-safe s) :held-at now}) (conj ids (:sense/id s))]))
             [(vec pending) ids]
             senses))))

(m/=> hold [:=> [:cat [:sequential Held] [:sequential :any] :int] [:vector Held]])

(defn expired?
  "Has `held` been in the outbox longer than `ttl-ms` at `now`?"
  [{:keys [held-at]} now ttl-ms]
  (> (- now held-at) ttl-ms))

(m/=> expired? [:=> [:cat Held :int :int] :boolean])

(defn trim
  "`pending` without expired entries and cut to `cap` (oldest evicted).
   -> {:pending [...] :expired n :evicted n}"
  [pending now {:keys [ttl-ms cap]}]
  (let [live (filterv #(not (expired? % now ttl-ms)) pending)
        over (max 0 (- (count live) cap))]
    {:pending (subvec live over)
     :expired (- (count pending) (count live))
     :evicted over}))

(m/=> trim [:=> [:cat [:vector Held] :int [:map [:ttl-ms :int] [:cap :int]]]
            [:map [:pending [:vector Held]] [:expired :int] [:evicted :int]]])

(defn bus-down?
  "Did a republish Outcome fail for want of a bus?"
  [{:keys [outcome]}]
  (contains? #{:degraded :failed} outcome))

(m/=> bus-down? [:=> [:cat :map] :boolean])

(defn keep-outcome?
  "Must a sense with this republish Outcome stay pending? Yes when the bus
   was down, or its parent has no inbox yet."
  [{:keys [outcome reason] :as o}]
  (or (bus-down? o) (and (= :skipped outcome) (= :no-inbox reason))))

(m/=> keep-outcome? [:=> [:cat :map] :boolean])

;; =============================================================================
;; Store port and adapters
;; =============================================================================

(defn- normalize [st]
  (if (m/validate ReplayState st) st nil))

(defn mem-store
  "An in-memory Store ({:load :save!}), optionally seeded with `state`."
  ([] (mem-store nil))
  ([state]
   (let [a (atom state)]
     {:load  (fn [] @a)
      :save! (fn [st] (reset! a st))
      :peek  (fn [] @a)})))

(m/=> mem-store [:function [:=> [:cat] :map] [:=> [:cat [:maybe :map]] :map]])

(defn default-path
  "$XDG_DATA_HOME/hive-mcp/sense-replay/state.edn (falling back to
   ~/.local/share)."
  []
  (let [xdg  (System/getenv "XDG_DATA_HOME")
        base (if (seq xdg)
               (io/file xdg "hive-mcp")
               (io/file (System/getProperty "user.home") ".local" "share" "hive-mcp"))]
    (str (io/file base "sense-replay" "state.edn"))))

(m/=> default-path [:=> [:cat] :string])

(defn- fsync! [^File f]
  (with-open [raf (RandomAccessFile. f "rw")
              ch  (.getChannel raf)]
    (.force ch true)))

(defn read-state
  "The replay state persisted at `path`, or nil when there is none. An
   unreadable file is moved aside to <path>.corrupt-<ts> and read as nil
   (the replayer then re-baselines)."
  [path]
  (let [f (io/file path)]
    (when (.exists f)
      (try
        (or (normalize (edn/read-string (slurp f)))
            (throw (ex-info "not a replay state" {:path path})))
        (catch Exception e
          (log/warn "[sense-replay] unreadable state, moving it aside:" (.getMessage e))
          (swallow/attempt
           (Files/move (.toPath f)
                       (.toPath (io/file (str path ".corrupt-" (System/currentTimeMillis))))
                       (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING])))
          nil)))))

(m/=> read-state [:=> [:cat :string] [:maybe ReplayState]])

(defn write-state!
  "Persist `state` at `path`: sibling temp file, fsync, ATOMIC_MOVE."
  [path state]
  (let [target (io/file path)
        dir    (.getParentFile (.getAbsoluteFile target))
        _      (.mkdirs dir)
        tmp    (File/createTempFile "state" ".edn.tmp" dir)]
    (try
      (spit tmp (pr-str state))
      (fsync! tmp)
      (Files/move (.toPath tmp) (.toPath target)
                  (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                          StandardCopyOption/REPLACE_EXISTING]))
      (finally
        (when (.exists tmp) (.delete tmp))))
    path))

(m/=> write-state! [:=> [:cat :string ReplayState] :string])

(defn file-store
  "A Store persisted at `path` (default `default-path`)."
  ([] (file-store (default-path)))
  ([path]
   {:load  (fn [] (read-state path))
    :save! (fn [st] (write-state! path st))
    :path  path}))

(m/=> file-store [:function [:=> [:cat] :map] [:=> [:cat :string] :map]])

;; =============================================================================
;; Boundary: the replayer
;; =============================================================================

(def ^:private zero-stats
  {:steps 0 :drained 0 :sent 0 :expired 0 :evicted 0 :drain-failed 0
   :save-failed 0 :wake-dropped 0 :last-error nil})

(def receptor
  "What the drain asks the host log for: only the republishable classes.
   The rest are consumed unseen (counted as :hidden by hive-agent)."
  {:classes (mapv name sr/republish-classes)})

(defn replayer
  "A replayer over republisher `r`. Options:
     :drain    (fn [opts]) -> DrainResult (hive-mcp.nats.sense-adapters/
               drain-senses over the host port). Required.
     :store    Store {:load :save!} (default `mem-store`)
     :execute  (fn [thunk]) -> truthy when accepted; default inline
     :now-fn   epoch-ms clock
     :ttl-ms :cap :batch  the hold policy (defaults above)"
  [r {:keys [drain store execute now-fn ttl-ms cap batch]}]
  {:republisher r
   :drain       drain
   :store       (or store (mem-store))
   :execute     (or execute (fn [thunk] (thunk) true))
   :now-fn      (or now-fn #(System/currentTimeMillis))
   :opts        {:ttl-ms (or ttl-ms default-ttl-ms)
                 :cap    (or cap default-cap)
                 :batch  (or batch default-batch)}
   :state*      (atom nil)
   :stats*      (atom zero-stats)
   :lock        (Object.)
   :scheduled   (AtomicBoolean. false)})

(m/=> replayer [:=> [:cat :map [:map [:drain ifn?]]] :map])

(defn- drain-all
  "Drain until the log answers an empty batch. -> {:senses [...] :error s?}"
  [drain batch]
  (loop [n 0 acc []]
    (let [d (drain {:limit batch :receptor receptor})]
      (cond
        (not (:ok? d))
        {:senses acc :error (or (:error d) (some-> (:reason d) name) "drain failed")}

        (or (empty? (:senses d)) (>= (inc n) max-drains-per-step))
        {:senses (into acc (:senses d))}

        :else (recur (inc n) (into acc (:senses d)))))))

(defn- flush-pending
  "Publish `pending` oldest first through `r`. Stops at the first bus
   failure (keeping it and everything after, so order holds)."
  [r pending]
  (loop [todo (seq pending) kept [] sent 0]
    (if-not todo
      {:pending kept :sent sent :bus-down? false}
      (let [h (first todo)
            o (sr/republish! r (:sense h))]
        (cond
          (bus-down? o)      {:pending (into kept todo) :sent sent :bus-down? true}
          (keep-outcome? o)  (recur (next todo) (conj kept h) sent)
          :else              (recur (next todo) kept
                                    (if (= :sent (:outcome o)) (inc sent) sent)))))))

(defn- save! [{:keys [store stats*]} st]
  (when-not (swallow/attempt ((:save! store) st) true)
    (swap! stats* update :save-failed inc)))

(defn step!
  "One replay step (see the ns doc). Serialised per replayer; never throws.
   -> StepResult"
  [{:keys [republisher drain store now-fn opts state* stats* lock] :as rp}]
  (locking lock
    (try
      (let [st0        (or @state* (normalize (swallow/attempt ((:load store)))) empty-state)
            now        (long (now-fn))
            {:keys [senses error]} (drain-all drain (:batch opts))
            baseline?  (not (:baselined? st0))
            held       (if baseline? (:pending st0) (hold (:pending st0) senses now))
            {:keys [pending expired evicted]} (trim held now opts)
            st1        {:baselined? (or (:baselined? st0) (nil? error)) :pending pending}
            _          (save! rp st1)
            fl         (flush-pending republisher pending)
            st2        (assoc st1 :pending (:pending fl))]
        (reset! state* st2)
        (when (not= st2 st1) (save! rp st2))
        (when (and baseline? (nil? error) (seq senses))
          (log/info "[sense-replay] baseline: skipped" (count senses) "senses already in the log"))
        (swap! stats* #(-> %
                           (update :steps inc)
                           (update :drained + (count senses))
                           (update :sent + (:sent fl))
                           (update :expired + expired)
                           (update :evicted + evicted)
                           (cond-> error (update :drain-failed inc))
                           (assoc :last-error error)))
        (cond-> {:drained   (count senses)
                 :baseline? baseline?
                 :sent      (:sent fl)
                 :pending   (count (:pending st2))
                 :expired   expired
                 :evicted   evicted
                 :bus-down? (:bus-down? fl)}
          error (assoc :error error)))
      (catch Throwable t
        (log/warn "[sense-replay] step failed:" (.getMessage t))
        (swap! stats* assoc :last-error (or (.getMessage t) (str (class t))))
        {:drained 0 :baseline? false :sent 0
         :pending (count (:pending @state*)) :expired 0 :evicted 0
         :bus-down? false :error (or (.getMessage t) (str (class t)))}))))

(m/=> step! [:=> [:cat :map] StepResult])

(defn wake!
  "Ask `rp` for a step on its executor. Coalesced: while one is queued a
   second wake is a no-op, and a wake during a step queues the next one.
   A refused hand-off is counted, never lost: the next wake drains it.
   Returns true when a step is queued or already pending."
  [{:keys [execute ^AtomicBoolean scheduled stats*] :as rp}]
  (if (.compareAndSet scheduled false true)
    (let [ok (swallow/attempt (execute (fn [] (.set scheduled false) (step! rp))))]
      (when-not ok
        (.set scheduled false)
        (swap! stats* update :wake-dropped inc))
      (boolean ok))
    true))

(m/=> wake! [:=> [:cat :map] :boolean])

(defn wake-listener
  "The feed listener in replay mode: every pushed sense only wakes `rp`."
  [rp]
  (fn [_sense] (wake! rp) nil))

(m/=> wake-listener [:=> [:cat :map] ifn?])

;; =============================================================================
;; Boundary: the process replayer
;; =============================================================================

(defonce ^:private current* (atom nil))

(def ^:private binding-watch-key ::replay)

(defn install!
  "Make `rp` THE process replayer: binding changes wake it, and it is woken
   once now to catch up. Returns {:replay? true}."
  [rp]
  (reset! current* rp)
  (sr/add-binding-watch! binding-watch-key (fn [] (wake! rp)))
  (wake! rp)
  {:replay? true})

(m/=> install! [:=> [:cat :map] [:map [:replay? :boolean]]])

(defn uninstall!
  "Drop the process replayer. Idempotent. Returns nil."
  []
  (sr/remove-binding-watch! binding-watch-key)
  (reset! current* nil)
  nil)

(m/=> uninstall! [:=> [:cat] :nil])

(defn installed?
  "Is a process replayer installed?"
  []
  (some? @current*))

(m/=> installed? [:=> [:cat] :boolean])

(defn wake-current!
  "Wake the process replayer (NATS reconnect). False when there is none."
  []
  (if-let [rp @current*] (wake! rp) false))

(m/=> wake-current! [:=> [:cat] :boolean])

(defn status
  "The process replayer: installed?, pending count, baselined?, stats."
  []
  (if-let [{:keys [state* stats* store]} @current*]
    (cond-> {:installed? true
             :pending    (count (:pending @state*))
             :baselined? (boolean (:baselined? @state*))
             :stats      @stats*}
      (:path store) (assoc :path (:path store)))
    {:installed? false}))

(m/=> status [:=> [:cat] [:map [:installed? :boolean]]])

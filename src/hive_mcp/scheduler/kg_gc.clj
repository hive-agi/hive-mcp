(ns hive-mcp.scheduler.kg-gc
  "The KG garbage-collection lane: periodic synthetic cleanup.

   Called from `housekeeping-sweep!` every 5 minutes through its guarded
   resolve-and-call pattern, but gated by its OWN interval (default 60 min):
   the registered-synthetic liveness scan is memory-store bound (~24 s per 50
   synthetics after batching), and a cycle that retracts edges is a sustained
   write load. A tick inside the interval returns {:skipped true} at once.

   What it runs: `hive-mcp.tools.kg.synthetics/cleanup-synthetics!` over the
   EDGE-derived synthetic universe, which has an orphaned-record class
   (edges survive, record gone). The record-derived GCs cannot see that
   class, and it was 43% of the graph when measured
   (memory 20260830173421-2baa1fb0). Wiring those instead would have
   reported zero collected every hour, and that zero would have been believed.

   Every result says what the lane could NOT see, not only what it
   collected:
     :ok?       false whenever any part of the universe was not decidable
     :blind     [{:class :reason}], one entry per invisible class
     :vacuous?  the derived universe was empty, so \"0 collected\" means
                nothing. An empty universe is reported as a failure, never as
                a clean graph (axiom 20260813235938-4a45db9c).
     :coverage  scanned vs universe for this cycle, plus the resume cursor

   Bounds per cycle: :limit synthetics, :edge-budget retracted edges in
   :chunk-size transactions, :deadline-ms of wall clock. Before every chunk
   the lane re-checks heap occupancy and stops with :heap-pressure above
   :max-heap-ratio. It also refuses to start above that ratio. A stopped
   cycle resumes from its cursor on the next one.

   Config (all optional), under [:services :kg-gc]:
     {:enabled true :interval-minutes 60 :limit 50 :edge-budget 20000
      :chunk-size 500 :deadline-ms 300000 :max-heap-ratio 0.85
      :dry-run? false}"
  (:require [hive-mcp.config.core :as config]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def defaults
  {:enabled          true
   :interval-minutes 60
   :limit            50
   :edge-budget      20000
   :chunk-size       500
   :deadline-ms      (* 5 60 1000)
   :max-heap-ratio   0.85
   :dry-run?         false})

(defonce ^:private lane-state
  (atom {:running? false :last-run-ms nil :cursor nil :cycles 0 :last-result nil}))

(defn lane-config
  "Lane config with defaults applied."
  []
  (merge defaults (config/get-service-config :kg-gc)))

;; =============================================================================
;; Guards
;; =============================================================================

(defn heap-ratio
  "Used heap / max heap, 0.0-1.0."
  []
  (let [rt (Runtime/getRuntime)
        used (- (.totalMemory rt) (.freeMemory rt))]
    (double (/ used (.maxMemory rt)))))

(defn make-continue?
  "0-arg fn asked before every retraction chunk. nil = go on; otherwise the
   reason to stop. heap-fn and now-fn are injectable for tests."
  [{:keys [max-heap-ratio deadline-ms]} start-ms
   & [{:keys [heap-fn now-fn] :or {heap-fn heap-ratio now-fn #(System/currentTimeMillis)}}]]
  (fn []
    (cond
      (> (heap-fn) max-heap-ratio)                   :heap-pressure
      (> (- (now-fn) start-ms) deadline-ms)          :deadline
      :else                                          nil)))

;; =============================================================================
;; Verdict
;; =============================================================================

(defn verdict
  "Fold a cleanup-synthetics! result into the lane report. Pure."
  [{:keys [universe blind scanned orphaned pruned demoted errors partial
           next-cursor error] :as r}]
  (let [sources   (:sources universe 0)
        vacuous?  (and (zero? sources) (not (:error r)))
        blind     (cond-> (vec blind)
                    vacuous?
                    (conj {:class  :universe
                           :reason (str "derived synthetic universe is empty; an "
                                        "empty graph and a broken read look the "
                                        "same, so 0 collected proves nothing")})
                    error
                    (conj {:class :lane :reason error}))
        done      (filter :complete? (:details r))
        n-done    (fn [o] (count (filter #(= o (:outcome %)) done)))
        collected (+ (n-done :orphaned) (n-done :pruned) (n-done :demoted))]
    {:ok?       (and (empty? blind) (zero? (or errors 0)))
     :vacuous?  vacuous?
     :blind     blind
     ;; :selected is what the classifier chose; :collected only what was
     ;; finished. The gap is :coverage :partial, resumed next cycle.
     :selected  {:orphaned (or orphaned 0) :pruned (or pruned 0)
                 :demoted (or demoted 0)}
     :collected {:orphaned (n-done :orphaned) :pruned (n-done :pruned)
                 :demoted (n-done :demoted) :total collected
                 :edges-removed (reduce + 0 (keep :effect-count
                                                  (remove #(= :demoted (:outcome %))
                                                          (:details r))))}
     :coverage  {:scanned (or scanned 0) :universe universe
                 :cursor next-cursor
                 :partial (count partial)}
     :errors    (or errors 0)
     :stopped   (some :stopped partial)}))

;; =============================================================================
;; Lane
;; =============================================================================

(defn- due? [{:keys [interval-minutes]} {:keys [last-run-ms]} now-ms]
  (or (nil? last-run-ms)
      (>= (- now-ms last-run-ms) (* interval-minutes 60 1000))))

(defn- claim!
  "Atomically take the lane if it is due and not running. true when taken."
  [cfg now-ms force?]
  (let [[old _] (swap-vals! lane-state
                            (fn [s]
                              (if (and (not (:running? s))
                                       (or force? (due? cfg s now-ms)))
                                (assoc s :running? true :last-run-ms now-ms)
                                s)))]
    (and (not (:running? old)) (or force? (due? cfg old now-ms)))))

(defn run-lane!
  "One KG GC lane tick. Returns {:skipped true :reason ..} when disabled, not
   due, already running, or under heap pressure; otherwise the verdict map.

   opts override lane-config; :force? ignores the interval; :cleanup-fn and
   :heap-fn are injectable for tests."
  ([] (run-lane! {}))
  ([{:keys [force? cleanup-fn heap-fn] :as opts}]
   (let [cfg    (merge (lane-config) (dissoc opts :force? :cleanup-fn :heap-fn))
         now-ms (System/currentTimeMillis)
         heap-fn (or heap-fn heap-ratio)]
     (cond
       (not (:enabled cfg))
       {:skipped true :reason "kg-gc disabled via config"}

       (> (heap-fn) (:max-heap-ratio cfg))
       (do (log/warn "KG GC lane: heap above" (:max-heap-ratio cfg) "- not starting")
           {:skipped true :reason "heap-pressure" :heap-ratio (heap-fn)})

       (not (claim! cfg now-ms force?))
       {:skipped true :reason "not due or already running"}

       :else
       (try
         (let [cleanup (or cleanup-fn
                           (requiring-resolve 'hive-mcp.tools.kg.synthetics/cleanup-synthetics!))
               raw     (cleanup {:limit       (:limit cfg)
                                 :edge-budget (:edge-budget cfg)
                                 :chunk-size  (:chunk-size cfg)
                                 :dry-run?    (:dry-run? cfg)
                                 :after       (:cursor @lane-state)
                                 :continue?   (make-continue? cfg now-ms {:heap-fn heap-fn})})
               v       (assoc (verdict raw)
                              :dry-run? (boolean (:dry-run? cfg))
                              :duration-ms (- (System/currentTimeMillis) now-ms))]
           (swap! lane-state #(-> %
                                  (assoc :cursor (get-in v [:coverage :cursor])
                                         :last-result v)
                                  (update :cycles inc)))
           (if (:ok? v)
             (log/info "KG GC lane:" (:collected v) (:coverage v))
             (log/error "KG GC lane: incomplete view -" (:blind v)
                        "collected" (:collected v)))
           v)
         (finally
           (swap! lane-state assoc :running? false)))))))

(defn status [] (assoc @lane-state :config (lane-config)))

(defn reset-state!
  "Test/ops hook: forget the cursor and the last run."
  []
  (reset! lane-state {:running? false :last-run-ms nil :cursor nil :cycles 0 :last-result nil}))

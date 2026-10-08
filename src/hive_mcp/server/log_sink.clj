(ns hive-mcp.server.log-sink
  "Bounded, dropping console sink for the server's Timbre appender.

   Why: the println appender used to write synchronously through *err*. Under
   CIDER that stream is the cider out-middleware proxy, which fans every line
   out to every nREPL session. One editor that stops draining its socket held
   the proxy lock, and every thread that logged waited on it (measured
   2026-10-02, card 20261002221707-04f355cd).

   Shape:
     LineWriter      — the port: where a formatted line finally goes.
     make-sink       — a bounded queue plus one daemon drain thread.
     offer!          — the caller's only contact: never blocks. A full queue
                       drops the line and counts it.
     appender-fn     — Timbre :fn that forces the output and offers it.

   A slow writer stalls only the drain thread; the worst case for callers is
   lost console lines, visible as (dropped sink)."
  (:import [java.util.concurrent ArrayBlockingQueue BlockingQueue TimeUnit]
           [java.util.concurrent.atomic AtomicLong]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Port
;; =============================================================================

(defprotocol LineWriter
  (write-line! [w line] "Write one formatted log line. May block; only the drain thread calls it."))

(defrecord ErrStreamWriter []
  LineWriter
  (write-line! [_ line]
    ;; Root *err*, read at write time: the drain thread carries no bindings,
    ;; so this is the same console stream the old synchronous appender used.
    (let [^java.io.Writer w *err*]
      (.write w (str line \newline))
      (.flush w))))

(defn err-stream-writer
  "The production writer: the root *err* stream."
  []
  (->ErrStreamWriter))

;; =============================================================================
;; Sink
;; =============================================================================

(def default-capacity
  "Lines buffered before new ones are dropped."
  4096)

(defn- drain-loop
  [^BlockingQueue q writer running ^AtomicLong failed]
  (while @running
    (when-let [line (.poll q 200 TimeUnit/MILLISECONDS)]
      (try
        (write-line! writer line)
        (catch Throwable _
          ;; A broken console must not kill the drainer; count and go on.
          (.incrementAndGet failed))))))

(defn make-sink
  "A bounded dropping sink over `writer` (a LineWriter).
   opts: :capacity (default 4096), :start? (default true; false builds the
   queue without a drain thread, for deterministic admission tests)."
  ([writer] (make-sink writer {}))
  ([writer {:keys [capacity start?] :or {capacity default-capacity start? true}}]
   (let [q       (ArrayBlockingQueue. (int capacity))
         dropped (AtomicLong. 0)
         failed  (AtomicLong. 0)
         running (atom true)
         thread  (when start?
                   (doto (Thread. ^Runnable #(drain-loop q writer running failed)
                                  "hive-log-sink-drain")
                     (.setDaemon true)
                     (.start)))]
     {:queue q :dropped dropped :failed failed :running running :thread thread
      :capacity capacity})))

(defn offer!
  "Hand `line` to the sink without blocking. Returns :queued or :dropped."
  [{:keys [^BlockingQueue queue ^AtomicLong dropped]} line]
  (if (.offer queue line)
    :queued
    (do (.incrementAndGet dropped) :dropped)))

(defn dropped
  "Lines dropped because the queue was full."
  [sink]
  (.get ^AtomicLong (:dropped sink)))

(defn depth
  "Lines currently waiting for the writer."
  [sink]
  (.size ^BlockingQueue (:queue sink)))

(defn stop!
  "Stop the drain thread after its current write. Queued lines are abandoned."
  [{:keys [running]}]
  (reset! running false))

(defn appender-fn
  "Timbre appender :fn that offers the formatted line to `sink`."
  [sink]
  (fn [data]
    (let [{:keys [output_]} data]
      (offer! sink (force output_)))))

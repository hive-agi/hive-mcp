(ns hive-mcp.server.log-guard
  "Close-safety for the log sinks, and the embedding-warning file appender.

   The private file writer (`log-sink/file-line-writer`) calls libc through JNA
   on the sink's daemon drain thread. Daemon threads keep running while the
   JVM's shutdown hooks run and while it exits, so a native call can still be
   in flight when the process tears down; the 2026-10-21 restart died with
   SIGSEGV in libc right after a shutdown hook (kanban 20261020231003-2a41a962).

   `guarded-writer` puts one closed flag and one lock in front of a writer:
   after `close-writer!` returns, no call reaches the delegate, and a call in
   flight when it began has finished. `close-sink!` stops the drain thread,
   closes the writer and joins the thread; it is idempotent, so a shutdown hook
   built by `close-sinks-fn` may run any number of times."
  (:require [clojure.string :as str]
            [hive-mcp.server.log-sink :as sink]
            [malli.core :as m])
  (:import [java.util.concurrent TimeUnit]
           [java.util.concurrent.atomic AtomicBoolean]
           [java.util.concurrent.locks ReentrantLock]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord GuardedWriter [delegate ^ReentrantLock lock ^AtomicBoolean closed]
  sink/LineWriter
  (write-line! [_ line]
    (.lock lock)
    (try
      (when-not (.get closed)
        (sink/write-line! delegate line))
      (finally (.unlock lock)))))

(defn guarded-writer
  "Wrap `delegate` (a LineWriter) so it can be closed once and never reached
   again."
  [delegate]
  (->GuardedWriter delegate (ReentrantLock.) (AtomicBoolean. false)))

(m/=> guarded-writer [:=> [:cat sink/Writer] sink/Writer])

(defn closed?
  "True once `close-writer!` ran on `w`. A writer that is not guarded is never
   closed."
  [w]
  (and (instance? GuardedWriter w) (.get ^AtomicBoolean (:closed w))))

(m/=> closed? [:=> [:cat :any] :boolean])

(defn close-writer!
  "Close `w`: later writes are no-ops. Waits up to `timeout-ms` for a write in
   flight; true when none is left, false when the wait ran out. Idempotent;
   anything that is not a GuardedWriter is left alone and answers true."
  [w timeout-ms]
  (if-not (instance? GuardedWriter w)
    true
    (let [^ReentrantLock lock (:lock w)]
      (.set ^AtomicBoolean (:closed w) true)
      (if (.tryLock lock (long timeout-ms) TimeUnit/MILLISECONDS)
        (do (.unlock lock) true)
        false))))

(m/=> close-writer! [:=> [:cat :any :int] :boolean])

(defn close-sink!
  "Stop `s`'s drain thread, close its writer and join the thread for up to
   `timeout-ms`. Lines offered afterwards are dropped (`log-sink/offer!`).
   Idempotent."
  [s timeout-ms]
  (sink/stop! s)
  (close-writer! (:writer s) timeout-ms)
  (when-let [^Thread t (:thread s)]
    (when-not (= t (Thread/currentThread))
      (.join t (long timeout-ms))))
  nil)

(m/=> close-sink! [:=> [:cat sink/Sink :int] :nil])

(defn close-sinks-fn
  "A shutdown-hook body closing every sink in `sinks`. Never throws, so one bad
   sink cannot stop the others or the hook thread."
  [sinks timeout-ms]
  (fn []
    (doseq [s sinks]
      (try (close-sink! s timeout-ms)
           ;; Logging is closing: there is nowhere left to report to.
           (catch Throwable _shutdown-hook-must-not-throw nil)))))

(m/=> close-sinks-fn [:=> [:cat [:sequential sink/Sink] :int] ifn?])

(defn register-close-on-shutdown!
  "Register one JVM shutdown hook that closes `sinks`. Returns the hook thread."
  [sinks timeout-ms]
  (let [hook (Thread. ^Runnable (close-sinks-fn sinks timeout-ms) "hive-log-sink-close")]
    (.addShutdownHook (Runtime/getRuntime) hook)
    hook))

(def HookThread
  "A shutdown-hook thread."
  [:fn {:gen/schema :nil :gen/fmap (fn [_] (Thread. ^Runnable (fn [])))}
   #(instance? Thread %)])

(m/=> register-close-on-shutdown! [:=> [:cat [:sequential sink/Sink] :int] HookThread])

;; =============================================================================
;; Embedding warnings -> file
;; =============================================================================

(def max-line-chars
  "Longest message the warning appender writes."
  2000)

(def ^:private secret-markers
  "Lower-case fragments of a word that assigns a credential."
  ["api_key" "api-key" "apikey" "token" "secret" "password" "passwd"])

(defn- key-word?
  "True when `word` is a bare credential key whose value follows in the NEXT
   word: it names a credential and ends in ':' or '=' ('token:', 'password=',
   '\"api_key\":')."
  [word]
  (let [lw (str/lower-case word)]
    (boolean
     (and (some #(str/includes? lw %) secret-markers)
          (or (str/ends-with? lw "=") (str/ends-with? lw ":"))))))

(defn- redacts-next?
  "True when `prev` announces that the word after it is a credential: a
   bearer/authorization prefix, or a bare credential key ('token:')."
  [prev]
  (boolean
   (and prev
        (or (contains? #{"bearer" "authorization:" "authorization="} (str/lower-case prev))
            (key-word? prev)))))

(defn- secret-word?
  "True when `word`, or the word before it, looks like a credential."
  [prev word]
  (let [lw (str/lower-case word)]
    (or (redacts-next? prev)
        (and (str/starts-with? lw "sk-") (> (count lw) 8))
        (and (some #(str/includes? lw %) secret-markers)
             (or (str/includes? lw "=") (str/includes? lw ":"))
             (not (key-word? word))))))

(defn- open-quote
  "The quote char a credential `word` opens and does not close, else nil. The
   value is what follows the word's first ':' or '=' for an inline assignment
   ('password=\"a'), the whole word when `prev` announced it ('\"a')."
  [prev word]
  (let [i (if (redacts-next? prev)
            0
            (inc (reduce min (count word)
                         (remove nil? [(str/index-of word "=") (str/index-of word ":")]))))
        value (subs word (min i (count word)))
        q (first value)]
    (when (and (contains? #{\" \'} q)
               (not (str/includes? (subs value 1) (str q))))
      q)))

(defn redact
  "Replace credential-looking words in `s` with [redacted]; whitespace is
   collapsed to single spaces. A key word ending in ':' or '=' redacts the
   word after it, and a credential value that opens a quote is redacted word
   by word through the word that closes it."
  [s]
  (let [words (->> (partition-by #(Character/isWhitespace ^char %) (str s))
                   (map #(apply str %))
                   (remove str/blank?))]
    (loop [prev nil, ws words, open nil, out []]
      (if-let [[word & more] (seq ws)]
        (cond
          open
          (recur word more (when-not (str/includes? word (str open)) open)
                 (conj out "[redacted]"))

          (secret-word? prev word)
          (recur word more (open-quote prev word) (conj out "[redacted]"))

          :else
          (recur word more nil (conj out word)))
        (str/join " " out)))))

(m/=> redact [:=> [:cat :any] :string])

(defn- render-warning
  "One line: ISO instant, level, namespace, then the logged values joined by
   spaces, credentials redacted and the message capped at `max-line-chars`."
  [{:keys [level ?ns-str vargs instant]}]
  (let [msg (redact (str/join " " (map str vargs)))
        msg (cond-> msg (> (count msg) max-line-chars) (subs 0 max-line-chars))
        ts (if (instance? java.util.Date instant)
             (.toInstant ^java.util.Date instant)
             (java.time.Instant/now))]
    (str ts " " (str/upper-case (name level)) " " ?ns-str " " msg)))

(defn ns-warn-appender-fn
  "Timbre appender :fn that writes :warn and worse from namespaces under any
   of `ns-prefixes` to `s`. It renders the logged values itself, never Timbre's
   formatted output, and redacts credential-looking words."
  [s ns-prefixes]
  (fn [{:keys [level ?ns-str] :as data}]
    (when (and (#{:warn :error :fatal} level)
               (string? ?ns-str)
               (some #(str/starts-with? ?ns-str %) ns-prefixes))
      (sink/offer! s (render-warning data)))))

(m/=> ns-warn-appender-fn [:=> [:cat sink/Sink [:sequential :string]] sink/AppenderFn])

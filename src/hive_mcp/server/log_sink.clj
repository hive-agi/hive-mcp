;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.server.log-sink
  "Bounded, dropping console sink for the server's Timbre appender.

   Shape:
     LineWriter      — the port: where a formatted line finally goes.
     make-sink       — a bounded queue plus one daemon drain thread.
     offer!          — the caller's only contact: never blocks. A full queue
                       drops the line and counts it.
     appender-fn     — Timbre :fn that forces the output and offers it.

   A slow writer stalls only the drain thread; the worst case for callers is
   lost console lines, visible as (dropped sink)."
  (:require [clojure.java.io :as io])
  (:import [java.nio.file Files LinkOption StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute PosixFilePermissions]
           [java.util.concurrent ArrayBlockingQueue BlockingQueue TimeUnit]
           [java.util.concurrent.atomic AtomicLong]))


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

(defn file-path
  "Coordinator log under the user-private state directory. Honors XDG_STATE_HOME."
  []
  (let [state-home (System/getenv "XDG_STATE_HOME")]
    (io/file (if (and state-home (not (.isBlank ^String state-home)))
               state-home
               (str (System/getProperty "user.home") "/.local/state"))
             "hive-mcp" "coordinator.log")))

(defn- private-file!
  "Create a private log directory/file, refusing symlinks before writes."
  [^java.io.File file]
  (let [dir (.toPath (.getParentFile file))
        path (.toPath file)
        no-links (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])
        private-dir (PosixFilePermissions/fromString "rwx------")]
    (doseq [p (take-while some? (iterate #(.getParent %) (.toAbsolutePath dir)))]
      (when (Files/isSymbolicLink p)
        (throw (ex-info "Log directory contains symlink" {:directory (str p)}))))
    (Files/createDirectories dir (make-array java.nio.file.attribute.FileAttribute 0))
    (Files/setPosixFilePermissions dir private-dir)
    (when (Files/exists path no-links)
      (when-not (Files/isRegularFile path no-links)
        (throw (ex-info "Log file must be regular" {:path (str path)})))
      (when (not= 1 (long (Files/getAttribute path "unix:nlink" no-links)))
        (throw (ex-info "Log file must not be hardlinked" {:path (str path)})))
      (when-not (= (PosixFilePermissions/fromString "rw-------")
                   (Files/getPosixFilePermissions path no-links))
        (throw (ex-info "Log file must be private" {:path (str path)}))))
    path))

(def max-file-bytes
  "Maximum bytes in the active log and each retained generation."
  1048576)

(def retained-files
  "Number of retained older generations."
  3)

(defrecord FileLineWriter [file]
  LineWriter
  (write-line! [_ line]
    (let [path (private-file! file)
          bytes (.getBytes (str line \newline) "UTF-8")
          bytes (if (> (alength bytes) max-file-bytes)
                  (.getBytes (str (String. bytes 0 (- max-file-bytes 32) "UTF-8") " [truncated]" \newline) "UTF-8")
                  bytes)
          no-links (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])
          attrs (into-array java.nio.file.attribute.FileAttribute
                            [(PosixFilePermissions/asFileAttribute (PosixFilePermissions/fromString "rw-------"))])]
      (when (and (Files/exists path no-links)
                 (> (+ (Files/size path) (alength bytes)) max-file-bytes))
        (doseq [n (range retained-files 0 -1)]
          (let [source (if (= n 1) path (.resolveSibling path (str (.getFileName path) "." (dec n))))
                target (.resolveSibling path (str (.getFileName path) "." n))]
            (when (Files/exists source no-links)
              (private-file! (.toFile source))
              (when (Files/exists target no-links) (private-file! (.toFile target)))
              (Files/move source target (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING]))))))
      (let [before (when (Files/exists path no-links)
                     (Files/readAttributes path java.nio.file.attribute.BasicFileAttributes no-links))
            options #{StandardOpenOption/CREATE StandardOpenOption/WRITE
                      StandardOpenOption/APPEND LinkOption/NOFOLLOW_LINKS}]
        (with-open [channel (Files/newByteChannel path options attrs)]
          (let [after (Files/readAttributes path java.nio.file.attribute.BasicFileAttributes no-links)]
            (when (or (not (.isRegularFile after))
                      (not= 1 (long (Files/getAttribute path "unix:nlink" no-links)))
                      (not= (PosixFilePermissions/fromString "rw-------")
                            (Files/getPosixFilePermissions path no-links))
                      (and before (not= (.fileKey before) (.fileKey after))))
              (throw (ex-info "Log file changed during open" {:path (str path)}))))
          (.write channel (java.nio.ByteBuffer/wrap bytes)))))))

(defn file-line-writer
  "A private append-only coordinator log writer. The sink's daemon thread owns IO."
  ([] (file-line-writer (file-path)))
  ([file] (->FileLineWriter (io/file file))))

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

(defn boot-line?
  "Admit only audited, single-line boot timing records, never arbitrary log payloads."
  [data]
  (let [args (:vargs data)
        ns-name (:?ns-str data)
        line (first args)]
    (and (= :info (:level data))
         (or (= "hive-mcp.server.core" ns-name)
             (= "hive-mcp.extensions.loader" ns-name))
         (= 1 (count args))
         (string? line)
         (or (and (= "hive-mcp.server.core" ns-name)
                  (or (.startsWith ^String line "[boot-timing] init ")
                      (.startsWith ^String line "[boot-timing] ===== ig/init complete:")
                      (.startsWith ^String line "[boot-timing]   slowest ")))
             (and (= "hive-mcp.extensions.loader" ns-name)
                  (or (.startsWith ^String line "[boot-timing] addon ")
                      (.startsWith ^String line "[boot-timing] classpath manifest scan "))))
         (not-any? #(Character/isISOControl ^char %) line))))

(defn boot-appender-fn
  "Timbre file appender that filters before forcing formatted output."
  [sink]
  (fn [data]
    (when (boot-line? data)
      (offer! sink (force (:output_ data))))))

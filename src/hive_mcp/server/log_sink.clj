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
  "Create a private log directory, refusing symlinks before writes; file inode is verified after open."
  [^java.io.File file]
  (let [dir (.toPath (.getParentFile file))
        path (.toPath file)
        private-dir (PosixFilePermissions/fromString "rwx------")]
    (doseq [p (take-while some? (iterate #(.getParent %) (.toAbsolutePath dir)))]
      (when (Files/isSymbolicLink p)
        (throw (ex-info "Log directory contains symlink" {:directory (str p)}))))
    (Files/createDirectories dir (make-array java.nio.file.attribute.FileAttribute 0))
    (Files/setPosixFilePermissions dir private-dir)
    path))

(def max-file-bytes
  "Maximum bytes in the active log and each retained generation."
  1048576)

(def retained-files
  "Number of retained older generations."
  3)

(defn- open-verified-fd!
  "Open with O_NOFOLLOW and inspect the OPEN Linux inode with fstat before writing.
   The portable NIO attribute APIs address a pathname, not a channel, and are
   insufficient against a rename/hardlink race. Fail closed off Linux."
  [path]
  (when-not (= "Linux" (System/getProperty "os.name"))
    (throw (ex-info "Secure log sink requires Linux fstat" {})))
  (let [libc (com.sun.jna.NativeLibrary/getInstance "c")
        call (fn [name ret args]
               (.invoke (.getFunction libc name) ret (object-array args)))
        fd (int (call "open" Integer/TYPE [(str path) (int (bit-or 1 64 1024 131072 524288)) (int 384)]))]
    (when (neg? fd)
      (throw (ex-info "Secure log open failed" {:path (str path)})))
    (try
      (let [stat (com.sun.jna.Memory. 256)
            uid (int (call "getuid" Integer/TYPE []))]
        (when (neg? (int (call "fstat" Integer/TYPE [fd stat])))
          (throw (ex-info "fstat failed" {:path (str path)})))
        ;; Linux x86_64 stat: nlink +16, mode +24, uid +28, size +48.
        (when-not (and (= "amd64" (System/getProperty "os.arch"))
                       (= 1 (.getLong stat 16))
                       (= 32768 (bit-and 61440 (.getInt stat 24)))
                       (= 384 (bit-and 511 (.getInt stat 24)))
                       (= uid (.getInt stat 28)))
          (throw (ex-info "Opened log inode is not private, owned, singly-linked regular file"
                          {:path (str path)})))
        {:fd fd :size (.getLong stat 48) :libc libc})
      (catch Throwable t
        (.invoke (.getFunction libc "close") Integer/TYPE (object-array [fd]))
        (throw t)))))

(defn- close-fd! [{:keys [fd libc]}]
  (.invoke (.getFunction libc "close") Integer/TYPE (object-array [fd])))

(defn- write-fd! [{:keys [fd libc]} bytes]
  (let [buf (com.sun.jna.Memory. (alength bytes))]
    (.write buf 0 bytes 0 (alength bytes))
    (when-not (= (long (alength bytes))
                 (long (.invoke (.getFunction libc "write") Long/TYPE
                                (object-array [fd buf (long (alength bytes))]))))
      (throw (ex-info "Log write failed or incomplete" {})))))

(defrecord FileLineWriter [file]
  LineWriter
  (write-line! [_ line]
    (let [path (private-file! file)
          bytes (.getBytes (str line \newline) "UTF-8")
          bytes (if (> (alength bytes) max-file-bytes)
                  (.getBytes (str (String. bytes 0 (- max-file-bytes 32) "UTF-8") " [truncated]" \newline) "UTF-8")
                  bytes)
          no-links (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])
          opened (open-verified-fd! path)
          rotate? (> (+ (:size opened) (alength bytes)) max-file-bytes)]
      (try
        (if rotate?
          (do
            (close-fd! opened)
            (doseq [n (range retained-files 0 -1)]
              (let [source (if (= n 1) path (.resolveSibling path (str (.getFileName path) "." (dec n))))
                    target (.resolveSibling path (str (.getFileName path) "." n))]
                (when (Files/exists source no-links)
                  (private-file! (.toFile source))
                  (when (Files/exists target no-links) (private-file! (.toFile target)))
                  (Files/move source target (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING])))))
            (let [rotated (open-verified-fd! path)]
              (try (write-fd! rotated bytes)
                   (finally (close-fd! rotated)))))
          (write-fd! opened bytes))
        (finally
          (when-not rotate? (close-fd! opened)))))))

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

(defrecord BootEvent [addon-id phase elapsed-ms status])

(defn boot-event
  "Build a typed timing record; appender hashes the identifier rather than writing untrusted text."
  [addon-id phase elapsed-ms status]
  (->BootEvent addon-id phase elapsed-ms status))

(defn- valid-boot-event?
  [event]
  (and (instance? BootEvent event)
       (some? (:addon-id event))
       (#{:addon :init :slowest :complete :manifest-scan} (:phase event))
       (#{:ok} (:status event))
       (number? (:elapsed-ms event))
       (let [ms (double (:elapsed-ms event))]
         (and (Double/isFinite ms) (<= 0.0 ms)))))

(defn- render-boot-event
  "Only fixed literals, numeric duration and a digest of the identifier reach disk."
  [event]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")
        id-bytes (.digest digest (.getBytes (str (:addon-id event)) java.nio.charset.StandardCharsets/UTF_8))
        id (apply str (map #(format "%02x" (bit-and 255 %)) (take 8 id-bytes)))]
    (format "[boot-timing] addon=%s phase=%s elapsed-ms=%.1f status=ok"
            id (name (:phase event)) (double (:elapsed-ms event)))))

(defn boot-line?
  "Admit only structured, typed boot events; never inspect or force raw log output."
  [data]
  (and (= :info (:level data))
       (#{"hive-mcp.server.core" "hive-mcp.extensions.loader"} (:?ns-str data))
       (= 1 (count (:vargs data)))
       (instance? BootEvent (first (:vargs data)))
       (valid-boot-event? (first (:vargs data)))))

(defn boot-appender-fn
  "Timbre file appender: serialize only typed boot events, never formatted output."
  [sink]
  (fn [data]
    (when (boot-line? data)
      (offer! sink (render-boot-event (first (:vargs data)))))))

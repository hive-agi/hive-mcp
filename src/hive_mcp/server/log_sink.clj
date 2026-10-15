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
  "Open every ancestor with O_DIRECTORY|O_NOFOLLOW, creating only the final
   private directory relative to its opened parent. Never chmod a pathname.
   Returns a pinned directory descriptor; caller must close it."
  [^java.io.File file]
  (when-not (and (= "Linux" (System/getProperty "os.name"))
                 (= "amd64" (System/getProperty "os.arch")))
    (throw (ex-info "Secure log sink requires Linux amd64" {})))
  (let [libc (com.sun.jna.NativeLibrary/getInstance "c")
        call (fn [name ret args] (.invoke (.getFunction libc name) ret (object-array args)))
        parts (map str (iterator-seq (.iterator (.normalize (.toAbsolutePath (.toPath file))))))
        root (int (call "open" Integer/TYPE ["/" (int (bit-or 65536 131072 524288))]))
        close! (fn [fd] (call "close" Integer/TYPE [fd]))]
    (when (neg? root) (throw (ex-info "Cannot open log root" {})))
    (loop [fd root dirs (butlast parts)]
      (if-let [part (first dirs)]
        (let [opened (int (call "openat" Integer/TYPE [fd part (int (bit-or 65536 131072 524288))]))
              next-fd (if (and (neg? opened) (= 1 (count dirs)))
                        (do
                          (when (neg? (int (call "mkdirat" Integer/TYPE [fd part (int 448)])))
                            (close! fd)
                            (throw (ex-info "Cannot create private log directory" {})))
                          (int (call "openat" Integer/TYPE [fd part (int (bit-or 65536 131072 524288))])))
                        opened)]
          (close! fd)
          (when (neg? next-fd) (throw (ex-info "Unsafe log ancestor" {:component part})))
          (when (= 1 (count dirs))
            (try
              (let [stat (com.sun.jna.Memory. 256)
                    uid (int (call "getuid" Integer/TYPE []))]
                (when (or (neg? (int (call "fstat" Integer/TYPE [next-fd stat])))
                          (not= 16384 (bit-and 61440 (.getInt stat 24)))
                          (not= 448 (bit-and 511 (.getInt stat 24)))
                          (not= uid (.getInt stat 28)))
                  (throw (ex-info "Log directory is not owned and private" {}))))
              (catch Throwable t (close! next-fd) (throw t))))
          (recur next-fd (next dirs)))
        {:fd fd :libc libc :name (last parts)}))))

(def max-file-bytes
  "Maximum bytes in the active log and each retained generation."
  1048576)

(def retained-files
  "Number of retained older generations."
  3)

(defn- open-verified-fd!
  "Open relative to a pinned private directory and inspect the OPEN Linux
   inode with fstat before writing. Fail closed off Linux amd64."
  [{:keys [fd libc]} name]
  (let [call (fn [sym ret args] (.invoke (.getFunction libc sym) ret (object-array args)))
        opened (int (call "openat" Integer/TYPE
                          [fd name (int (bit-or 1 64 1024 131072 524288)) (int 384)]))]
    (when (neg? opened)
      (throw (ex-info "Secure log open failed" {:name name})))
    (try
      (let [stat (com.sun.jna.Memory. 256)
            uid (int (call "getuid" Integer/TYPE []))]
        (when (neg? (int (call "fstat" Integer/TYPE [opened stat])))
          (throw (ex-info "fstat failed" {:name name})))
        ;; Linux x86_64 stat: nlink +16, mode +24, uid +28, size +48.
        (when-not (and (= 1 (.getLong stat 16))
                       (= 32768 (bit-and 61440 (.getInt stat 24)))
                       (= 384 (bit-and 511 (.getInt stat 24)))
                       (= uid (.getInt stat 28)))
          (throw (ex-info "Opened log inode is not private, owned, singly-linked regular file"
                          {:name name})))
        {:fd opened :size (.getLong stat 48) :libc libc})
      (catch Throwable t
        (call "close" Integer/TYPE [opened])
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

(declare rotate-generations!)

(defrecord FileLineWriter [file]
  LineWriter
  (write-line! [_ line]
    (let [parent (private-file! file)]
      (try
        (let [bytes (.getBytes (str line \newline) "UTF-8")
              bytes (if (> (alength bytes) max-file-bytes)
                      (.getBytes (str (String. bytes 0 (- max-file-bytes 32) "UTF-8") " [truncated]" \newline) "UTF-8")
                      bytes)
              opened (open-verified-fd! parent (:name parent))
              rotate? (> (+ (:size opened) (alength bytes)) max-file-bytes)]
          (if rotate?
            (do
              (close-fd! opened)
              (rotate-generations! parent)
              (let [rotated (open-verified-fd! parent (:name parent))]
                (try (write-fd! rotated bytes)
                     (finally (close-fd! rotated)))))
            (try (write-fd! opened bytes)
                 (finally (close-fd! opened)))))
        (finally (close-fd! parent))))))

(defn- verified-generation!
  "Inspect an opened generation inode, including symlinks, before renameat.
   Only ENOENT is absent; other open failures refuse rotation."
  [{:keys [fd libc]} name]
  (let [call (fn [sym ret args] (.invoke (.getFunction libc sym) ret (object-array args)))
        opened (int (call "openat" Integer/TYPE [fd name (int (bit-or 2097152 131072 524288))]))]
    (if (neg? opened)
      (if (= 2 (com.sun.jna.Native/getLastError))
        false
        (throw (ex-info "Cannot inspect log generation" {:name name})))
      (try
        (let [stat (com.sun.jna.Memory. 256)]
          (when (or (neg? (int (call "fstat" Integer/TYPE [opened stat])))
                    (not= 32768 (bit-and 61440 (.getInt stat 24)))
                    (not= 384 (bit-and 511 (.getInt stat 24)))
                    (not= 1 (.getLong stat 16))
                    (not= (int (call "getuid" Integer/TYPE [])) (.getInt stat 28)))
            (throw (ex-info "Unsafe log generation" {:name name})))
          true)
        (finally (call "close" Integer/TYPE [opened]))))))

(defn- rotate-generations!
  "Rename only within the opened private directory; no path lookup can redirect
   rotation to a swapped ancestor. Generation entries must be private files."
  [{:keys [fd libc name] :as parent}]
  (let [call (fn [sym ret args] (.invoke (.getFunction libc sym) ret (object-array args)))]
    (doseq [n (range retained-files 0 -1)]
      (let [source (str name (when (> n 1) (str "." (dec n))))
            target (str name "." n)]
        (when (verified-generation! parent source)
          (verified-generation! parent target)
          (when (neg? (int (call "renameat" Integer/TYPE [fd source fd target])))
            (throw (ex-info "Log rotation failed" {:generation n}))))))))

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

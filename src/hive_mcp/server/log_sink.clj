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
  (:require [clojure.java.io :as io]
            [malli.core :as m])
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

(def Writer
  "A LineWriter: the port a sink drains into."
  [:fn {:error/message "must satisfy LineWriter"
        :gen/schema :nil :gen/fmap (fn [_] (->ErrStreamWriter))}
   #(satisfies? LineWriter %)])

(defn err-stream-writer
  "The production writer: the root *err* stream."
  []
  (->ErrStreamWriter))

(m/=> err-stream-writer [:=> [:cat] Writer])

(defn file-path
  "Coordinator log under the user-private state directory. Honors XDG_STATE_HOME."
  []
  (let [state-home (System/getenv "XDG_STATE_HOME")]
    (io/file (if (and state-home (not (.isBlank ^String state-home)))
               state-home
               (str (System/getProperty "user.home") "/.local/state"))
             "hive-mcp" "coordinator.log")))

(def FileValue
  "A java.io.File."
  [:fn {:gen/schema [:string {:min 1}] :gen/fmap #(java.io.File. (str "/tmp/" %))}
   #(instance? java.io.File %)])

(m/=> file-path [:=> [:cat] FileValue])

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

(def log-levels
  "Timbre levels HIVE_MCP_LOG_LEVEL may name."
  #{:trace :debug :info :warn :error :fatal :report})

(defn parse-log-level
  "Timbre :min-level for the HIVE_MCP_LOG_LEVEL value `s` (\"debug\",
   \":warn\", \"INFO\" ...). Blank, nil or unknown values give :info, so the
   server never runs at :debug by accident: at the live request rate every
   request's DEBUG lines cost real CPU."
  [s]
  (let [k (some-> s str .trim .toLowerCase (.replaceFirst "^:" "") not-empty keyword)]
    (if (contains? log-levels k) k :info)))

(m/=> parse-log-level [:=> [:cat :any] (into [:enum] log-levels)])

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
      (throw (ex-info "Log write failed or incomplete" {})))
    (java.lang.ref.Reference/reachabilityFence buf))) ; Memory frees its peer once collected

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

(m/=> file-line-writer [:function
                        [:=> [:cat] Writer]
                        [:=> [:cat [:or [:string {:min 1}] FileValue]] Writer]])

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
      :capacity capacity :writer writer})))

(def SinkOpts
  "make-sink options."
  [:map
   [:capacity {:optional true} [:int {:min 1}]]
   [:start? {:optional true} :boolean]])

(defn- of-instance
  "A schema for an instance of a host class, generating (make)."
  [pred make]
  [:fn {:gen/schema :nil :gen/fmap (fn [_] (make))} pred])

(def Sink
  "A bounded dropping sink: its queue, the dropped and failed counters, the
   running flag, the drain thread (nil when built with :start? false) and the
   capacity it was built with."
  [:map
   [:queue (of-instance #(instance? BlockingQueue %) #(ArrayBlockingQueue. 4))]
   [:dropped (of-instance #(instance? AtomicLong %) #(AtomicLong. 0))]
   [:failed (of-instance #(instance? AtomicLong %) #(AtomicLong. 0))]
   [:running (of-instance #(instance? clojure.lang.IAtom %) #(atom false))]
   [:thread [:maybe (of-instance #(instance? Thread %) #(Thread. ^Runnable (fn [])))]]
   [:capacity [:int {:min 1}]]])

(m/=> make-sink [:function
                 [:=> [:cat Writer] Sink]
                 [:=> [:cat Writer SinkOpts] Sink]])

(defn offer!
  "Hand `line` to the sink without blocking: :queued, or :dropped when full or stopped."
  [{:keys [^BlockingQueue queue ^AtomicLong dropped running]} line]
  (if (and @running (.offer queue line))
    :queued
    (do (.incrementAndGet dropped) :dropped)))

(m/=> offer! [:=> [:cat Sink :string] [:enum :queued :dropped]])

(defn dropped
  "Lines dropped because the queue was full."
  [sink]
  (.get ^AtomicLong (:dropped sink)))

(m/=> dropped [:=> [:cat Sink] [:int {:min 0}]])

(defn depth
  "Lines currently waiting for the writer."
  [sink]
  (.size ^BlockingQueue (:queue sink)))

(m/=> depth [:=> [:cat Sink] [:int {:min 0}]])

(defn stop!
  "Stop the drain thread after its current write. Queued lines are abandoned."
  [{:keys [running]}]
  (reset! running false))

(m/=> stop! [:=> [:cat [:map [:running (of-instance #(instance? clojure.lang.IAtom %) #(atom true))]]]
             [:= false]])

(defn appender-fn
  "Timbre appender :fn that offers the formatted line to `sink`."
  [sink]
  (fn [data]
    (let [{:keys [output_]} data]
      (offer! sink (force output_)))))

(def AppenderFn
  "A Timbre appender :fn: one arity over Timbre's data map."
  [:=> [:cat :map] :any])

(m/=> appender-fn [:=> [:cat Sink] AppenderFn])

(defrecord BootEvent [addon-id phase elapsed-ms status])

(defn boot-event
  "Build a typed timing record; appender writes only validated addon identifiers."
  [addon-id phase elapsed-ms status]
  (->BootEvent addon-id phase elapsed-ms status))

(def BootEventValue
  "A BootEvent record. boot-event builds one from any four values; the file
   appender validates it before rendering (valid-boot-event?)."
  [:fn {:gen/schema [:tuple :keyword [:enum :addon :init :slowest :complete :manifest-scan]
                     [:double {:min 0.0 :max 1e6}] [:= :ok]]
        :gen/fmap #(apply ->BootEvent %)}
   #(instance? BootEvent %)])

(m/=> boot-event [:=> [:cat :any :any :any :any] BootEventValue])

(defn- valid-boot-event?
  [event]
  (and (instance? BootEvent event)
       (let [id (:addon-id event)
             text (cond (keyword? id) (if-let [ns (namespace id)] (str ns "/" (name id)) (name id))
                        (symbol? id) (str id)
                        :else nil)]
         (and text
              (<= 1 (count text) 160)
              (boolean (every? #(or (<= (int \a) (int %) (int \z))
                                    (<= (int \A) (int %) (int \Z))
                                    (<= (int \0) (int %) (int \9))
                                    (#{\. \- \_ \/} %)) text))))
       (#{:addon :init :slowest :complete :manifest-scan} (:phase event))
       (#{:ok} (:status event))
       (number? (:elapsed-ms event))
       (let [ms (double (:elapsed-ms event))]
         (and (Double/isFinite ms) (<= 0.0 ms)))))

(defn- render-boot-event
  "Render only a validated addon identifier, its digest, fixed literals and
   numeric duration. Caller must validate before rendering."
  [event]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")
        addon (:addon-id event)
        id (if (keyword? addon)
             (if-let [ns (namespace addon)] (str ns "/" (name addon)) (name addon))
             (str addon))
        id-bytes (.digest digest (.getBytes id java.nio.charset.StandardCharsets/UTF_8))
        hash (apply str (map #(format "%02x" (bit-and 255 %)) (take 8 id-bytes)))]
    (format "[boot-timing] addon=%s addon-hash=%s phase=%s elapsed-ms=%.1f status=ok"
            id hash (name (:phase event)) (double (:elapsed-ms event)))))

(defn boot-line?
  "Admit only structured, typed boot events; never inspect or force raw log output."
  [data]
  (and (= :info (:level data))
       (#{"hive-mcp.server.core" "hive-mcp.extensions.loader"} (:?ns-str data))
       (= 1 (count (:vargs data)))
       (instance? BootEvent (first (:vargs data)))
       (valid-boot-event? (first (:vargs data)))))

;; A falsy answer is nil when an (and ...) clause short-circuits on a set
;; lookup, false otherwise; a truthy answer is true.
(m/=> boot-line? [:=> [:cat [:maybe :map]] [:maybe :boolean]])

(defn boot-appender-fn
  "Timbre file appender: serialize only typed boot events, never formatted output."
  [sink]
  (fn [data]
    (when (boot-line? data)
      (offer! sink (render-boot-event (first (:vargs data)))))))

(m/=> boot-appender-fn [:=> [:cat Sink] AppenderFn])

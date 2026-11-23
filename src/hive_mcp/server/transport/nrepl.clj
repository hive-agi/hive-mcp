(ns hive-mcp.server.transport.nrepl
  "Embedded nREPL server for bb-mcp tool forwarding.

   Manages nREPL startup with CIDER / refactor-nrepl middleware detection,
   classloader GC, port-binding retry logic and the `.nrepl-port` file.

   STD-04 T1, OFF by default: with config [:services :nrepl :socket] true
   (or a path), or env HIVE_MCP_NREPL_SOCKET naming a path, the same
   handler is ALSO served on a Unix-domain socket (T2 path
   $XDG_RUNTIME_DIR/hive/mcp-nrepl.sock), bound through the hive-system
   adapter, its path written to `.nrepl-socket`. The TCP listener is the
   same in every case."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [nrepl.server :as nrepl-server]
            [nrepl.transport :as nrepl-transport]
            [hive-system.network.path :as npath]
            [hive-system.network.unix :as unix]
            [hive-mcp.nrepl.classloader-gc :as classloader-gc]
            [hive-mcp.config.core :as config]
            [hive-mcp.dns.result :as result]
            [taoensso.timbre :as log]
            [malli.core :as m]
            [hive-mcp.server.transport.schema :as ts])
  (:import [java.nio.channels ServerSocketChannel SocketChannel]
           [java.nio.file FileSystems Files LinkOption Path]
           [java.nio.file.attribute PosixFilePermissions]
           [java.util.concurrent.atomic AtomicLong]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; nREPL Server
;; =============================================================================

(defn- port-available?
  "Check if a port is available for binding. Returns Result."
  [port]
  (result/try-effect* :nrepl/port-probe-failed
    (let [ss (java.net.ServerSocket. (int port) 1 (java.net.InetAddress/getByName "127.0.0.1"))]
      (.close ss)
      true)))

(defn- resolve-cider-middleware
  "Collect the CIDER middleware that actually resolves in THIS image.

   cider.nrepl/cider-middleware names its middleware by symbol. An entry whose
   namespace is absent from the running classpath resolves to nil, and a single
   nil makes nrepl.server/default-handler throw NullPointerException on
   with-meta, which loses the entire nREPL server. Unresolvable entries are
   dropped and counted in a warning rather than allowed to do that.

   Returns a vector of vars/fns, empty when CIDER is not present."
  []
  (result/rescue []
    (require 'cider.nrepl)
    (if-let [mw-var (resolve 'cider.nrepl/cider-middleware)]
      (let [declared (or @mw-var [])
            resolved (into []
                           (keep (fn [mw]
                                   (cond
                                     (var? mw)    mw
                                     (symbol? mw) (result/rescue nil (requiring-resolve mw))
                                     (ifn? mw)    mw
                                     :else        nil)))
                           declared)
            dropped  (- (count declared) (count resolved))]
        (when (pos? dropped)
          (log/warn "CIDER middleware did not resolve in this image and was dropped"
                    {:declared (count declared)
                     :resolved (count resolved)
                     :dropped  dropped}))
        resolved)
      [])))

(defn- resolve-refactor-middleware
  "Collect refactor-nrepl's middleware when it is on the classpath.

   Returns a vector holding `refactor-nrepl.middleware/wrap-refactor`, empty
   when refactor-nrepl is not present."
  []
  (or (result/rescue nil
        (when-let [mw (requiring-resolve 'refactor-nrepl.middleware/wrap-refactor)]
          [mw]))
      []))

(defn- build-nrepl-handler
  "Build nREPL handler with classloader-gc + optional CIDER and refactor-nrepl middleware."
  []
  (let [cider-mw    (resolve-cider-middleware)
        refactor-mw (resolve-refactor-middleware)
        ;; GC-fix-6: wrap-shared-classloader pins session-less evals (bb-mcp pattern)
        ;; to a shared session, preventing DynamicClassLoader proliferation.
        all-mw      (-> [#'classloader-gc/wrap-shared-classloader]
                        (into cider-mw)
                        (into refactor-mw))]
    {:handler       (apply nrepl-server/default-handler all-mw)
     :has-cider?    (seq cider-mw)
     :has-refactor? (seq refactor-mw)}))

(def ^:private port-file ".nrepl-port")

(defn write-port-file!
  "Write PORT to `.nrepl-port` in the working directory — the file CIDER's
   connect and `clojure_discover` read. Returns Result."
  [port]
  (result/try-effect* :nrepl/port-file-write-failed
    (spit port-file (str port))
    true))

(m/=> write-port-file! [:=> [:cat :any] ts/EffectResult])

;; =============================================================================
;; Unix-domain socket listener (STD-04 T1) — OFF unless configured
;; =============================================================================

(def socket-service
  "The T2 service name of this listener: $XDG_RUNTIME_DIR/hive/mcp-nrepl.sock."
  "mcp-nrepl")

(def socket-env
  "Env var naming an explicit socket path; it also turns the listener on."
  "HIVE_MCP_NREPL_SOCKET")

(def max-socket-connections
  "Connections served at once; one accepted past the cap is closed unanswered."
  unix/default-max-connections)

(def bind-deadline-ms
  "Longest the boot waits for the socket bind (the adapter's bind lock has no
   deadline of its own). Past it the socket is not started; TCP is unaffected."
  3000)

(def ^:private accept-backoff-ms 50)

(def ^:private socket-file ".nrepl-socket")

(def ^:private nofollow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defonce ^{:doc "The running Unix-socket listener, or nil. A defonce so a
   namespace reload neither forgets nor rebinds it: the bind!-answer of
   hive-system.network.unix plus :open-channels (atom of a set) and
   :stopped? (atom). On a re-init the idempotent start answers this
   listener, which keeps the handler it was started with."}
  socket-server
  (atom nil))

(defonce ^{:doc "The outcome of the last socket start: {:status :running :path}
   or {:status :not-started :reason kw}; nil when none was asked for."}
  socket-last-start
  (atom nil))

(defonce ^:private socket-lock (Object.))

(defonce ^:private conn-counter (AtomicLong.))

(defn socket-path-for
  "Where the socket listener binds, or nil when it is off.

   `setting` is config [:services :nrepl :socket]: true takes the T2 canon
   path (hive-system.network.path/socket-path for `socket-service`), a
   non-blank string is an explicit path, anything else is off. A non-blank
   `env-path` (HIVE_MCP_NREPL_SOCKET) wins over both and turns it on. An
   explicit path is marked :explicit? and must pass admit-socket-path
   before it is bound. `sys-env`, `tmpdir` and `uid` feed the T2 rule. Pure.
   => nil | Result<{:path ...}>"
  [setting env-path sys-env tmpdir uid]
  (cond
    (not (str/blank? env-path))                        (result/ok {:path env-path :explicit? true})
    (and (string? setting) (not (str/blank? setting))) (result/ok {:path setting :explicit? true})
    (true? setting)                                    (npath/socket-path sys-env tmpdir uid socket-service)
    :else                                              nil))

(m/=> socket-path-for [:=> [:cat :any [:maybe :string] [:maybe :map] [:maybe :string] [:maybe :int]]
                       [:maybe ts/EffectResult]])

(defn- process-user
  "This process's UserPrincipal (by user.name), or nil when it cannot be learned."
  []
  (try
    (-> (FileSystems/getDefault)
        .getUserPrincipalLookupService
        (.lookupPrincipalByName (System/getProperty "user.name")))
    (catch Throwable _ nil)))

(defn admit-socket-path
  "Refuse an explicit socket `path` the adapter must not touch. The adapter
   chmods the socket's parent to 0700, so an explicit path is admitted only
   when it is absolute and its parent is either `t2-dir` (the canon socket
   directory) or an existing, non-symlinked directory this process owns
   that is ALREADY 0700, so no directory the code did not create changes
   mode. => Result<{:path}>"
  [path t2-dir]
  (let [p      (.normalize (Path/of (str path) (make-array String 0)))
        parent (.getParent p)
        t2     (some-> t2-dir str not-empty (Path/of (make-array String 0)) .normalize)
        refuse (fn [k msg] (result/err k {:path (str path) :message msg}))]
    (cond
      (not (.isAbsolute p))
      (refuse :nrepl/socket-path-relative "an explicit socket path must be absolute")

      (and t2 (= t2 parent))
      (result/ok {:path (str p)})

      (not (Files/isDirectory parent (make-array LinkOption 0)))
      (refuse :nrepl/socket-dir-missing "the socket's directory must exist (or be the T2 directory)")

      (not= parent (result/rescue nil (.toRealPath parent (make-array LinkOption 0))))
      (refuse :nrepl/socket-dir-not-canonical "the socket's directory path goes through a symlink")

      (let [me (process-user)]
        (not (and me (.equals me (Files/getOwner parent nofollow)))))
      (refuse :nrepl/socket-dir-not-owned "the socket's directory is not owned by this process")

      (not= "rwx------" (PosixFilePermissions/toString (Files/getPosixFilePermissions parent nofollow)))
      (refuse :nrepl/socket-dir-not-private "the socket's directory is not already 0700; it is not changed")

      :else (result/ok {:path (str p)}))))

(m/=> admit-socket-path [:=> [:cat :string [:maybe :string]] ts/EffectResult])

(defn- t2-dir []
  (:ok (npath/socket-dir (into {} (System/getenv))
                         (System/getProperty "java.io.tmpdir") (unix/current-uid))))

(defn- configured-socket-path
  "socket-path-for over the live config and process env. Reads nothing else
   (no uid, no tmpdir) while the listener is off."
  []
  (let [setting  (config/get-service-value :nrepl :socket)
        env-path (System/getenv socket-env)]
    (when (or setting (not (str/blank? env-path)))
      (socket-path-for setting env-path (into {} (System/getenv))
                       (System/getProperty "java.io.tmpdir") (unix/current-uid)))))

(defn- quiet-close [^java.io.Closeable c]
  (try (.close c) (catch Throwable _ nil)))

(defn- write-socket-file!
  "Write the bound path to `.nrepl-socket` beside `.nrepl-port`."
  [path]
  (result/try-effect* :nrepl/socket-file-write-failed
    (spit socket-file path)
    true))

(defn- delete-socket-file!
  "Delete `.nrepl-socket` only while it still names `path`."
  [path]
  (let [f (io/file socket-file)]
    (when (and (.exists f) (= path (result/rescue nil (slurp f))))
      (io/delete-file f true))))

(defn- stop-listener!
  "Stop `srv`: mark it stopped (a connection registering later is closed by
   its accept thread), unlink the socket while it is still ours and close
   the channel (unix/close-server!), close every open connection. Clears
   socket-server and `.nrepl-socket` only while `srv` is the current one.
   => the close-server! Result"
  [{:keys [stopped? open-channels path] :as srv}]
  (locking socket-lock
    (reset! stopped? true)
    (let [res (unix/close-server! srv)]
      (doseq [ch @open-channels] (quiet-close ch))
      (when (identical? srv @socket-server)
        (reset! socket-server nil)
        (delete-socket-file! path)
        (log/info "nREPL Unix socket stopped" path))
      res)))

(defn- serve-socket-connection!
  "Answer one accepted connection with the nREPL `handler` over bencode,
   until the peer leaves or the listener stops."
  [handler open ^SocketChannel ch]
  (try
    (let [t (nrepl-transport/bencode ch)]
      (try (nrepl-server/handle handler t)
           (finally (quiet-close t))))
    (catch Throwable _ nil)
    (finally
      (swap! open disj ch)
      (quiet-close ch))))

(defn- dispatch-connection!
  "Hand one accepted channel to its own daemon thread, or close it: a peer
   that fails the T4 check (SO_PEERCRED principal = owner of the socket
   directory) or one past the connection cap is closed unanswered, and so
   is one registering after the listener stopped."
  [{:keys [open-channels stopped? max-connections peer-allowed?]} handler dir ^SocketChannel ch]
  (cond
    (not (peer-allowed? ch dir))                  (quiet-close ch)
    (>= (count @open-channels) max-connections)   (quiet-close ch)
    :else
    (do (swap! open-channels conj ch)
        (if @stopped?
          (do (swap! open-channels disj ch) (quiet-close ch))
          (try
            (doto (Thread. ^Runnable #(serve-socket-connection! handler open-channels ch)
                           (str "hive-nrepl-unix-conn-" (.incrementAndGet ^AtomicLong conn-counter)))
              (.setDaemon true)
              (.start))
            (catch Throwable t
              (swap! open-channels disj ch)
              (quiet-close ch)
              (throw t)))))))

(defn- start-accept-loop!
  "Accept on the bound channel until it is closed or the listener stops. A
   failure to serve one connection is logged and the loop goes on after a
   backoff; anything that ends the loop while the listener is still live
   (an interrupt, an error outside one connection) stops the listener, so
   it is never left bound and recorded as running with nobody accepting."
  [{:keys [server-socket path stopped?] :as srv} handler]
  (let [^ServerSocketChannel ssc server-socket
        dir (.getParent (io/file path))]
    (doto (Thread.
           ^Runnable
           (fn []
             (try
               (loop []
                 (when (and (.isOpen ssc) (not @stopped?))
                   (let [ch (try (.accept ssc) (catch Throwable _ nil))]
                     (if ch
                       (try (dispatch-connection! srv handler dir ch)
                            (catch Throwable t
                              (quiet-close ch)
                              (log/warn t "nREPL Unix socket: a connection could not be served")
                              (Thread/sleep (long accept-backoff-ms))))
                       (when (.isOpen ssc) (Thread/sleep (long accept-backoff-ms)))))
                   (recur)))
               (catch Throwable t
                 (when-not @stopped?
                   (log/warn t "nREPL Unix socket accept loop ended; closing the listener"))))
             (when-not @stopped?
               (stop-listener! srv)))
           "hive-nrepl-unix-accept")
      (.setDaemon true)
      (.start))))

(defn- bind-within!
  "(bind-fn path) on a daemon thread, waited for at most `deadline-ms`, so
   a starter elsewhere holding the bind lock cannot hang the boot. A bind
   that answers after the deadline is closed at once: it was never recorded.
   => Result"
  [bind-fn path deadline-ms]
  (let [state (atom :pending)
        p     (promise)]
    (doto (Thread.
           ^Runnable
           (fn []
             (let [r (try (bind-fn path)
                          (catch Throwable e
                            (result/err :nrepl/socket-bind-threw {:path path :message (ex-message e)})))]
               (if (compare-and-set! state :pending :done)
                 (deliver p r)
                 (when (result/ok? r) (unix/close-server! (:ok r))))))
           "hive-nrepl-unix-bind")
      (.setDaemon true)
      (.start))
    (let [r (deref p deadline-ms ::timeout)]
      (if (and (= ::timeout r) (compare-and-set! state :pending :abandoned))
        (result/err :nrepl/socket-bind-timeout {:path path :deadline-ms deadline-ms})
        @p))))

(defn- not-started!
  "Log why the socket did not start and record it. TCP is unaffected. => nil"
  [why r]
  (log/warn "nREPL Unix socket not started (non-fatal, TCP unaffected):" why r)
  (reset! socket-last-start {:status :not-started :reason (:error r)})
  nil)

(defn start-socket-nrepl!
  "ALSO serve nREPL `handler` on a Unix-domain socket, when configured
   (see socket-path-for; default off: nothing bound, no directory made, no
   file written). An explicit path must pass admit-socket-path. The
   hive-system adapter does the T2 directory (0700), the T3 stale probe and
   bind lock and the 0600 mode (unix/bind!), run under `bind-deadline-ms`
   so the boot never blocks on it. Idempotent: while a listener runs it is
   answered as is (with the handler it was started with), never bound twice.
   Non-fatal: a refusal or a timeout is logged, recorded in
   socket-last-start and answers nil.
   opts (tests): :bind-fn, :deadline-ms, :peer-allowed?, :max-connections.
   => the listener map, or nil"
  ([handler] (start-socket-nrepl! handler (configured-socket-path) {}))
  ([handler path-result] (start-socket-nrepl! handler path-result {}))
  ([handler path-result {:keys [bind-fn deadline-ms peer-allowed? max-connections]}]
   (when path-result
     (locking socket-lock
       (or @socket-server
           (let [admitted (cond
                            (result/err? path-result)               path-result
                            (get-in path-result [:ok :explicit?])   (admit-socket-path
                                                                     (get-in path-result [:ok :path])
                                                                     (t2-dir))
                            :else                                   path-result)]
             (if (result/err? admitted)
               (not-started! "path refused" admitted)
               (let [path (get-in admitted [:ok :path])
                     r    (bind-within! (or bind-fn unix/bind!) path (or deadline-ms bind-deadline-ms))]
                 (if (result/err? r)
                   (not-started! "not bound" r)
                   (let [srv (assoc (:ok r)
                                    :open-channels   (atom #{})
                                    :stopped?        (atom false)
                                    :max-connections (or max-connections max-socket-connections)
                                    :peer-allowed?   (or peer-allowed? unix/peer-allowed?))]
                     (reset! socket-server srv)
                     (if-let [t (try (start-accept-loop! srv handler) nil
                                     (catch Throwable t t))]
                       (do (stop-listener! srv)
                           (not-started! "accept thread did not start"
                                         {:error :nrepl/socket-accept-failed :message (ex-message t)}))
                       (do
                         (when (result/err? (write-socket-file! (:path srv)))
                           (log/warn "nREPL Unix socket: could not write" socket-file))
                         (reset! socket-last-start {:status :running :path (:path srv)})
                         (log/info "Embedded nREPL also on Unix socket" (:path srv))
                         srv))))))))))))

(m/=> start-socket-nrepl! [:function
                           [:=> [:cat :any] [:maybe :map]]
                           [:=> [:cat :any [:maybe ts/EffectResult]] [:maybe :map]]
                           [:=> [:cat :any [:maybe ts/EffectResult] [:maybe :map]] [:maybe :map]]])

(defn stop-socket-nrepl!
  "Stop the Unix-socket listener, if one runs: mark it stopped, unlink the
   socket (only while it is still ours, unix/close-server!), close the
   channel and every open connection, remove `.nrepl-socket`. A no-op when
   none runs. => the close-server! Result, or nil"
  []
  (locking socket-lock
    (when-let [srv @socket-server]
      (stop-listener! srv))))

(m/=> stop-socket-nrepl! [:=> [:cat] [:maybe ts/EffectResult]])


(defn delete-port-file!
  "Delete the `.nrepl-port` file written by `write-port-file!`, if present.
   This is the :hive/nrepl halt's cleanup, so it also stops the Unix-socket
   listener when one runs (a no-op when it is off)."
  []
  (result/rescue nil (stop-socket-nrepl!))
  (io/delete-file port-file true))

;; io/delete-file answers the `silently` flag (true) when there is nothing to delete
(m/=> delete-port-file! [:=> [:cat] :boolean])

(defn- try-start-nrepl!
  "Attempt to start nREPL server. Returns Result ok with server or err."
  [nrepl-server-atom server-opts]
  (result/try-effect* :nrepl/start-failed
    (let [server (nrepl-server/start-server server-opts)]
      (reset! nrepl-server-atom server)
      server)))

(defn start-embedded-nrepl!
  "Start an embedded nREPL server for bb-mcp tool forwarding.

   CRITICAL: This runs in the SAME JVM as the MCP server and channel,
   allowing bb-mcp to forward tool calls that access the live channel.

   Railway: probe-port → build-handler → start-server (with retry) → write
   `.nrepl-port`. Returns server or nil (non-fatal)."
  [nrepl-server-atom]
  (let [nrepl-port  (config/get-service-value :nrepl :port
                                              :env "HIVE_MCP_NREPL_PORT"
                                              :parse parse-long
                                              :default 7910)
        probe       (port-available? nrepl-port)]
    (if (result/err? probe)
      ;; Port already bound (e.g. nREPL started during extension loading) — skip
      (do (log/info "nREPL already running on port" nrepl-port "— skipping embedded start")
          nil)
      ;; Port free — build handler and start with retry
      (let [{:keys [handler has-cider? has-refactor?]} (build-nrepl-handler)
            server-opts  {:port nrepl-port :bind "127.0.0.1" :handler handler}
            max-retries  5
            retry-delay  2000]
        (loop [attempt 1]
          (let [r (try-start-nrepl! nrepl-server-atom server-opts)]
            (cond
              (result/ok? r)
              (do (log/info "Embedded nREPL started on port" nrepl-port
                            (str "(with classloader-gc"
                                 (when has-cider? " + CIDER")
                                 (when has-refactor? " + refactor-nrepl")
                                 ")")
                            (when (> attempt 1) (str "(attempt " attempt ")")))
                  (when (result/err? (write-port-file! nrepl-port))
                    (log/warn "Embedded nREPL could not write" port-file "— CIDER connect must be given the port"))
                  ;; STD-04 T1: off by default (nil path → nothing bound)
                  (result/rescue nil (start-socket-nrepl! handler))
                  (:ok r))

              (and (= "class java.net.BindException" (:class r))
                   (< attempt max-retries))
              (do (log/warn "nREPL port" nrepl-port "busy (attempt" (str attempt "/" max-retries "),")
                            "retrying in" (str retry-delay "ms..."))
                  (Thread/sleep retry-delay)
                  (recur (inc attempt)))

              :else
              (do (log/warn "Embedded nREPL failed (non-fatal):" (:message r))
                  nil))))))))

(m/=> start-embedded-nrepl! [:=> [:cat ts/AtomRef] [:maybe ts/NreplServer]])

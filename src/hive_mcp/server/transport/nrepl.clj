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
            [hive-mcp.server.transport.schema :as ts]))
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

(def ^:private socket-file ".nrepl-socket")

(defonce ^{:doc "The running Unix-socket listener, or nil. A defonce so a
   namespace reload neither forgets nor rebinds it: the bind!-answer of
   hive-system.network.unix plus :open-transports (atom of a set)."}
  socket-server
  (atom nil))

(defonce ^:private socket-lock (Object.))

(defn socket-path-for
  "Where the socket listener binds, or nil when it is off.

   `setting` is config [:services :nrepl :socket]: true takes the T2 canon
   path (hive-system.network.path/socket-path for `socket-service`), a
   non-blank string is an explicit path, anything else is off. A non-blank
   `env-path` (HIVE_MCP_NREPL_SOCKET) wins over both and turns it on.
   `sys-env`, `tmpdir` and `uid` feed the T2 rule. Pure.
   => nil | Result<{:path ...}>"
  [setting env-path sys-env tmpdir uid]
  (cond
    (not (str/blank? env-path))                   (result/ok {:path env-path})
    (and (string? setting) (not (str/blank? setting))) (result/ok {:path setting})
    (true? setting)                               (npath/socket-path sys-env tmpdir uid socket-service)
    :else                                         nil))

(m/=> socket-path-for [:=> [:cat :any [:maybe :string] [:maybe :map] [:maybe :string] [:maybe :int]]
                       [:maybe ts/EffectResult]])

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

(defn- serve-socket-connection!
  "Answer one accepted connection with the nREPL `handler` over bencode,
   until the peer leaves or the listener stops."
  [handler open ch]
  (let [t (nrepl-transport/bencode ch)]
    (try
      (swap! open conj t)
      (nrepl-server/handle handler t)
      (catch Throwable _ nil)
      (finally
        (swap! open disj t)
        (quiet-close t)))))

(defn- start-accept-loop!
  "Accept on the bound channel until it is closed. Each peer passes the T4
   check (unix/peer-allowed?: SO_PEERCRED principal = owner of the socket
   directory) or is closed unanswered."
  [{:keys [server-socket path open-transports]} handler]
  (let [^java.nio.channels.ServerSocketChannel ssc server-socket
        dir (.getParent (io/file path))]
    (doto (Thread.
           ^Runnable
           (fn []
             (loop []
               (when (.isOpen ssc)
                 (let [ch (try (.accept ssc) (catch Throwable _ nil))]
                   (cond
                     (nil? ch)
                     (when (.isOpen ssc) (Thread/sleep 50))

                     (unix/peer-allowed? ch dir)
                     (doto (Thread. ^Runnable #(serve-socket-connection! handler open-transports ch)
                                    "hive-nrepl-unix-conn")
                       (.setDaemon true)
                       (.start))

                     :else (quiet-close ch)))
                 (recur))))
           "hive-nrepl-unix-accept")
      (.setDaemon true)
      (.start))))

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

(defn start-socket-nrepl!
  "ALSO serve nREPL `handler` on a Unix-domain socket, when configured
   (see socket-path-for; default off: nothing bound, no directory made, no
   file written). The hive-system adapter does the T2 directory (0700), the
   T3 stale probe and bind lock and the 0600 mode (unix/bind!). Idempotent:
   while a listener runs it is answered as is, never bound twice.
   Non-fatal: a refusal is logged and answers nil.
   => the listener map, or nil"
  ([handler] (start-socket-nrepl! handler (configured-socket-path)))
  ([handler path-result]
   (when path-result
     (locking socket-lock
       (or @socket-server
           (if (result/err? path-result)
             (do (log/warn "nREPL Unix socket path refused:" path-result) nil)
             (let [path (:path (:ok path-result))
                   r    (unix/bind! path)]
               (if (result/err? r)
                 (do (log/warn "nREPL Unix socket not bound (non-fatal):" r) nil)
                 (let [srv (assoc (:ok r) :open-transports (atom #{}))]
                   (reset! socket-server srv)
                   (start-accept-loop! srv handler)
                   (when (result/err? (write-socket-file! (:path srv)))
                     (log/warn "nREPL Unix socket: could not write" socket-file))
                   (log/info "Embedded nREPL also on Unix socket" (:path srv))
                   srv)))))))))

(m/=> start-socket-nrepl! [:function
                           [:=> [:cat :any] [:maybe :map]]
                           [:=> [:cat :any [:maybe ts/EffectResult]] [:maybe :map]]])

(defn stop-socket-nrepl!
  "Stop the Unix-socket listener, if one runs: unlink the socket (only while
   it is still ours, unix/close-server!), close the channel and every open
   connection, remove `.nrepl-socket`. A no-op when none runs.
   => the close-server! Result, or nil"
  []
  (locking socket-lock
    (when-let [srv @socket-server]
      (reset! socket-server nil)
      (let [res (unix/close-server! srv)]
        (doseq [t @(:open-transports srv)] (quiet-close t))
        (delete-socket-file! (:path srv))
        (log/info "nREPL Unix socket stopped" (:path srv))
        res))))

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

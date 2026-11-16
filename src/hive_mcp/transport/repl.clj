(ns hive-mcp.transport.repl
  "The :repl TransportChain: ling REPLs without Emacs.

   Chain (config.edn [:transports :repl :chain], shipped
   [:headless-nrepl :emacs-cider]):
     :headless-nrepl  starts an nREPL JVM in the project dir as a child
                      process, reads the port it announces and records the
                      session in a JVM-side registry (no Emacs)
     :emacs-cider     the Emacs jack-in, through the :vessel dispatch seam

   Pure half:
     command-argv     the argv that starts the nREPL
     parse-port       the port in an nREPL banner line
   Effectful half:
     start!           spawn, wait for the port, register
     sessions, session, stop!   the JVM-side registry
     spawn-repl!      run a request through the chain

   The default alias is dev-repl, which binds port 0: an alias that binds a
   fixed port could land inside a serving image."
  (:require [clojure.string :as str]
            [hive-dsl.result :as r]
            [hive-mcp.transport.registry :as reg]
            [malli.core :as m]
            [taoensso.timbre :as log])
  (:import (java.io BufferedReader InputStreamReader File)))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-aliases ["dev-repl"])

(def default-timeout-ms 120000)

(def banner-prefix "nREPL server started on port ")

(def Session
  [:map
   [:name :string]
   [:port :int]
   [:dir :string]
   [:pid [:maybe :int]]
   [:started-at :int]
   [:via :keyword]])

(defonce ^:private registry (atom {}))

(defn command-argv
  "The argv that starts a REPL with `aliases` (default `default-aliases`)."
  [aliases]
  ["clojure" (str "-M:" (str/join ":" (or (seq aliases) default-aliases)))])

(m/=> command-argv [:=> [:cat [:maybe [:sequential :string]]] [:vector :string]])

(defn parse-port
  "The port a banner `line` announces, or nil."
  [line]
  (when (and (string? line) (str/includes? line banner-prefix))
    (let [tail (subs line (+ (str/index-of line banner-prefix) (count banner-prefix)))
          digits (apply str (take-while #(Character/isDigit ^char %) tail))]
      (when (seq digits) (parse-long digits)))))

(m/=> parse-port [:=> [:cat :any] [:maybe :int]])

(defn sessions
  "The registered sessions, by name."
  []
  @registry)

(m/=> sessions [:=> [:cat] [:map-of :string Session]])

(defn session
  "The session `name`, or nil."
  [name]
  (get @registry name))

(m/=> session [:=> [:cat :string] [:maybe Session]])

(defn forget!
  "Drop the session `name` from the registry. Answers nil."
  [name]
  (swap! registry dissoc name)
  nil)

(m/=> forget! [:=> [:cat :string] :nil])

(defn- await-port
  "Read `process` output until the banner names a port, or `timeout-ms`
   passes or the process exits. Answers the port or nil. The reader keeps
   draining afterwards on a daemon thread so the child never blocks on a full
   pipe."
  [^Process process timeout-ms]
  (let [p        (promise)
        reader   (BufferedReader. (InputStreamReader. (.getInputStream process)))
        t        (Thread. ^Runnable
                          (fn []
                            (try
                              (loop []
                                (when-let [line (.readLine reader)]
                                  (when-let [port (parse-port line)]
                                    (deliver p port))
                                  (recur)))
                              (catch Exception _ nil)
                              (finally (deliver p nil))))
                          "repl-chain-reader")]
    (.setDaemon t true)
    (.start t)
    (deref p timeout-ms nil)))

(defn start!
  "Start an nREPL in `dir` and register it as `name`. opts: :aliases,
   :timeout-ms, :argv (tests). Answers a Result: ok with the Session, or an
   :unavailable error when no port was announced (the child is then killed)."
  [{:keys [name dir aliases timeout-ms argv]}]
  (let [argv (or argv (command-argv aliases))
        pb   (doto (ProcessBuilder. ^java.util.List argv)
               (.directory (File. ^String dir))
               (.redirectErrorStream true))
        proc (.start pb)
        port (await-port proc (or timeout-ms default-timeout-ms))]
    (if port
      (let [s {:name name :port (int port) :dir dir :pid (int (.pid proc))
               :started-at (int (quot (System/currentTimeMillis) 1000))
               :via :headless-nrepl}]
        (swap! registry assoc name s)
        (log/info "headless nREPL registered" s)
        (r/ok s))
      (do (.destroyForcibly proc)
          (r/err :repl/no-port {:error/kind :unavailable
                                :message (str "nREPL in " dir " announced no port")})))))

(m/=> start! [:=> [:cat [:map [:name :string] [:dir :string]]] :map])

(defn stop!
  "Kill the session `name`'s process and forget it. Answers {:stopped? bool}."
  [name]
  (let [s (session name)]
    (when-let [pid (:pid s)]
      (when-let [h (.orElse (java.lang.ProcessHandle/of (long pid)) nil)]
        (.destroy ^java.lang.ProcessHandle h)))
    (forget! name)
    {:stopped? (some? s)}))

(m/=> stop! [:=> [:cat :string] [:map [:stopped? :boolean]]])

(defn- headless-nrepl-call
  [req]
  (start! req))

(defn- headless-nrepl-probe
  "The adapter needs a `clojure` launcher on PATH."
  []
  (let [p (.start (ProcessBuilder. ^java.util.List ["clojure" "--help"]))]
    (if (zero? (.waitFor p))
      (r/ok :clojure)
      (r/err :repl/no-launcher {:error/kind :unavailable :message "clojure --help failed"}))))

(defn- emacs-cider-call
  "The Emacs jack-in. Its failures are availability failures, so the chain
   reports them rather than hiding them."
  [{:keys [name]}]
  (let [invoke (requiring-resolve 'hive-spi.editor.services/invoke)
        {:keys [success error]} (invoke :vessel :dispatch {:op :cider/spawn-session :name name} 5000)]
    (if success
      (r/ok {:name name :via :emacs-cider})
      (r/err :repl/emacs-unavailable {:error/kind :unavailable :message (str error)}))))

(defmethod reg/open-adapter [:repl :headless-nrepl] [_ _]
  {:call #'headless-nrepl-call :probe #'headless-nrepl-probe})

(defmethod reg/open-adapter [:repl :emacs-cider] [_ _]
  {:call #'emacs-cider-call})

(defn spawn-repl!
  "Start a ling REPL through the :repl chain. `req` {:name :dir :aliases?}.
   The Result's :ok is {:value session-or-ack :via :resolved-by :skipped}."
  [req]
  (reg/call! :repl req {:sleep-fn (fn [_] nil)}))

(m/=> spawn-repl! [:=> [:cat [:map [:name :string] [:dir :string]]] :map])

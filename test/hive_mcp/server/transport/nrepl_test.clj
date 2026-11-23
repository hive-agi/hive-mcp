(ns hive-mcp.server.transport.nrepl-test
  "Facets for the embedded nREPL starter and its `.nrepl-port` file.

   The port file lives in the working directory, which is also where a
   developer's own REPL keeps its port, so every test snapshots the file and
   puts it back. The starter is only driven with a port the probe refuses,
   which is the `already running, skip` arm: no server is ever started."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.dns.result :as result]
            [hive-system.network.unix :as unix]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.source :as src]
            [hive-mcp.server.transport.nrepl :as tn]
            [hive-mcp.server.transport.schema :as ts]
            [hive-mcp.system.layer2]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [integrant.core :as ig]
            [malli.core :as m]
            [nrepl.core :as nrepl]
            [nrepl.server])
  (:import [java.nio.channels SocketChannel]
           [java.nio.file Files LinkOption Path]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private port-file (io/file ".nrepl-port"))

(defn- own-the-port-file [f]
  (let [prior (when (.exists port-file) (slurp port-file))]
    (try (f)
         (finally
           (if prior
             (spit port-file prior)
             (io/delete-file port-file true))))))

(deftrifecta-from-schema write-port-file-round-trips
  #'tn/write-port-file!
  {:in [:cat [:or :int :string :keyword]]
   :out ts/EffectResult
   :rel (fn [[port] out]
          (and (= {:ok true} out)
               (= (str port) (slurp port-file))))
   :num-tests 50})

(golden/deftest-golden port-file-lifecycle-golden
  "test/golden/hive-mcp/server/transport/nrepl-port-file.edn"
  (let [written (tn/write-port-file! 7910)
        content (slurp port-file)
        deleted (tn/delete-port-file!)
        gone?   (not (.exists port-file))
        again   (tn/delete-port-file!)]
    {:written written :content content :deleted deleted
     :gone? gone? :delete-when-absent again}))

(deftest a-refused-port-skips-the-embedded-start
  (let [server-atom (atom nil)]
    (binding [config/*config-source*
              (src/atom-source (atom {:services {:nrepl {:port -1}}}) {})]
      (is (nil? (tn/start-embedded-nrepl! server-atom))))
    (is (nil? @server-atom) "nothing was started, so nothing was stored")))

(deftest the-server-schema-generates-a-server-record
  (is (m/validate ts/NreplServer (nrepl.server/map->Server {})))
  (is (not (m/validate ts/NreplServer {:port 1}))))

;; =============================================================================
;; STD-04 T1: the Unix-socket listener. Every socket lives under a fresh temp
;; directory (a temp XDG_RUNTIME_DIR or an explicit temp path), never at the
;; live service's canon path, and is removed by the test.
;; =============================================================================

(def ^:private socket-file (io/file ".nrepl-socket"))

(defn- own-the-socket-file [f]
  (let [prior (when (.exists socket-file) (slurp socket-file))]
    (try (f)
         (finally
           (tn/stop-socket-nrepl!)
           (if prior
             (spit socket-file prior)
             (io/delete-file socket-file true))))))

(use-fixtures :each own-the-port-file own-the-socket-file)

(defn- temp-dir ^java.io.File []
  (.toFile (Files/createTempDirectory (Path/of "/tmp" (make-array String 0)) "std04-"
                                      (make-array FileAttribute 0))))

(defn- rm-rf [^java.io.File f]
  (when (.isDirectory f) (run! rm-rf (.listFiles f)))
  (io/delete-file f true))

(defn- mode [p]
  (PosixFilePermissions/toString
   (Files/getPosixFilePermissions (Path/of (str p) (make-array String 0))
                                  (make-array LinkOption 0))))

(defn- eval-over-socket [path code]
  (with-open [conn (nrepl/connect :socket path)]
    (-> (nrepl/client conn 10000)
        (nrepl/message {:op "eval" :code code})
        nrepl/combine-responses
        :value)))

(defn- set-mode! [p m]
  (Files/setPosixFilePermissions (Path/of (str p) (make-array String 0))
                                 (PosixFilePermissions/fromString m)))

(defn- our-threads []
  (->> (Thread/getAllStackTraces) keys
       (filter #(.startsWith (.getName ^Thread %) "hive-nrepl-unix-"))
       (remove #(= "hive-nrepl-unix-bind" (.getName ^Thread %)))))

(defn- wait-until [pred ms]
  (loop [left ms]
    (cond (pred) true
          (neg? left) false
          :else (do (Thread/sleep 20) (recur (- left 20))))))

(defn- closed-by-server?
  "True when the server closes `ch` within 2 s without answering: EOF, or a
   reset (Linux resets a Unix peer that closes with unread data)."
  [ch]
  (contains? #{:frame/eof :frame/read-failed}
             (:error (unix/read-line! (unix/line-reader ch) 2000))))

(defn- start! [path & [opts]]
  (tn/start-socket-nrepl! (nrepl.server/default-handler) (result/ok {:path path}) (or opts {})))

(def ran (atom nil))

(deftest the-socket-path-is-off-unless-asked-for
  (let [env {"XDG_RUNTIME_DIR" "/run/user/4242"}]
    (is (nil? (tn/socket-path-for nil nil env "/tmp" 4242)) "default config: off")
    (is (nil? (tn/socket-path-for false nil env "/tmp" 4242)))
    (is (nil? (tn/socket-path-for "" "" env "/tmp" 4242)) "blank strings are off")
    (is (= {:ok {:path "/run/user/4242/hive/mcp-nrepl.sock" :dir "/run/user/4242/hive"}}
           (tn/socket-path-for true nil env "/tmp" 4242))
        "true takes the T2 canon path of mcp-nrepl")
    (is (= {:ok {:path "/tmp/hive-4242/mcp-nrepl.sock" :dir "/tmp/hive-4242"}}
           (tn/socket-path-for true nil {} "/tmp" 4242))
        "without XDG_RUNTIME_DIR the T2 fallback")
    (is (= {:ok {:path "/x/y.sock" :explicit? true}} (tn/socket-path-for nil "/x/y.sock" env "/tmp" 4242))
        "HIVE_MCP_NREPL_SOCKET turns it on, as an explicit path")
    (is (= {:ok {:path "/x/y.sock" :explicit? true}} (tn/socket-path-for true "/x/y.sock" env "/tmp" 4242))
        "and wins over the canon path")))

(deftest the-default-config-opens-no-socket
  (is (nil? (System/getenv tn/socket-env)) "precondition: the test JVM has no socket env")
  (binding [config/*config-source*
            (src/atom-source (atom {:services {:nrepl {:port 7910}}}) {})]
    (is (nil? (tn/start-socket-nrepl! (nrepl.server/default-handler)))))
  (is (nil? @tn/socket-server) "nothing bound, nothing held")
  (is (not (.exists socket-file)) "no .nrepl-socket written")
  (is (nil? (tn/stop-socket-nrepl!)) "stop with nothing running is a no-op"))

(deftest an-explicit-path-is-admitted-only-where-no-directory-changes-mode
  (let [own (temp-dir)                      ; createTempDirectory: ours, 0700
        wide (doto (io/file own "wide") .mkdir)
        link (io/file own "link")]
    (try
      (set-mode! wide "rwxr-xr-x")
      (Files/createSymbolicLink (.toPath link) (.toPath own) (make-array FileAttribute 0))
      (is (= :nrepl/socket-path-relative (:error (tn/admit-socket-path "x.sock" nil))))
      (is (= {:ok {:path (str own "/a.sock")}} (tn/admit-socket-path (str own "/a.sock") nil))
          "an owned, existing 0700 directory")
      (is (= {:ok {:path "/run/user/4242/hive/a.sock"}}
             (tn/admit-socket-path "/run/user/4242/hive/a.sock" "/run/user/4242/hive"))
          "the T2 directory, even before it exists")
      (is (= :nrepl/socket-dir-missing (:error (tn/admit-socket-path (str own "/nope/a.sock") nil))))
      (is (= :nrepl/socket-dir-not-private (:error (tn/admit-socket-path (str wide "/a.sock") nil))))
      (is (= :nrepl/socket-dir-not-canonical (:error (tn/admit-socket-path (str link "/a.sock") nil))))
      (is (= :nrepl/socket-dir-not-owned (:error (tn/admit-socket-path "/usr/a.sock" nil))))
      (testing "a refused path binds nothing and changes no mode"
        (is (nil? (tn/start-socket-nrepl! (nrepl.server/default-handler)
                                          (result/ok {:path (str wide "/a.sock") :explicit? true}))))
        (is (= "rwxr-xr-x" (mode wide)) "never chmod a directory the code did not create")
        (is (not (.exists (io/file wide "a.sock"))))
        (is (= :not-started (:status @tn/socket-last-start))))
      (finally (rm-rf own)))))

(deftest the-canon-socket-answers-an-nrepl-eval
  (let [xdg  (temp-dir)
        want (tn/socket-path-for true nil {"XDG_RUNTIME_DIR" (str xdg)} nil nil)
        path (get-in want [:ok :path])]
    (try
      (let [srv (tn/start-socket-nrepl! (nrepl.server/default-handler) want)]
        (is (= path (:path srv)))
        (is (= (str xdg "/hive/mcp-nrepl.sock") path))
        (is (= "rwx------" (mode (str xdg "/hive"))) "directory 0700")
        (is (= "rw-------" (mode path)) "socket 0600")
        (is (= path (slurp socket-file)) ".nrepl-socket names the bound path")
        (is (identical? srv (tn/start-socket-nrepl! (nrepl.server/default-handler) want))
            "a second start answers the running listener: no second bind")
        (is (= ["3"] (eval-over-socket path "(+ 1 2)")))
        (is (:ok (tn/stop-socket-nrepl!)))
        (is (not (.exists (io/file path))) "socket unlinked on stop")
        (is (not (.exists socket-file)) ".nrepl-socket removed on stop")
        (is (nil? @tn/socket-server)))
      (finally
        (tn/stop-socket-nrepl!)
        (rm-rf xdg)))))

(deftest a-refused-peer-runs-no-op
  (let [dir (temp-dir) path (str dir "/r.sock")]
    (try
      (reset! ran nil)
      (start! path {:peer-allowed? (constantly false)})
      (let [ch (:channel (:ok (unix/connect! path)))]
        (try
          (unix/write-line! ch "d4:code54:(reset! hive-mcp.server.transport.nrepl-test/ran :ran)2:op4:evale")
          (is (closed-by-server? ch) "closed unanswered")
          (Thread/sleep 200)
          (is (nil? @ran) "the eval never ran")
          (finally (.close ^SocketChannel ch))))
      (finally (tn/stop-socket-nrepl!) (rm-rf dir)))))

(deftest a-connection-past-the-cap-is-closed
  (let [dir (temp-dir) path (str dir "/c.sock")]
    (try
      (let [srv (start! path {:max-connections 1})]
        (with-open [conn (nrepl/connect :socket path)]
          (is (= ["3"] (-> (nrepl/client conn 10000)
                           (nrepl/message {:op "eval" :code "(+ 1 2)"})
                           nrepl/combine-responses :value)))
          (is (= 1 (count @(:open-channels srv))))
          (let [ch (:channel (:ok (unix/connect! path)))]
            (try (is (closed-by-server? ch) "the second connection is refused")
                 (finally (.close ^SocketChannel ch))))
          (is (= 1 (count @(:open-channels srv))) "the first one still served")))
      (finally (tn/stop-socket-nrepl!) (rm-rf dir)))))

(deftest a-connection-registering-after-halt-is-closed
  (let [open (atom #{})
        ch   (SocketChannel/open)
        before (count (our-threads))]
    (#'tn/dispatch-connection! {:open-channels open :stopped? (atom true) :max-connections 256
                                :peer-allowed? (constantly true)}
                               (nrepl.server/default-handler) "/tmp" ch)
    (is (not (.isOpen ch)) "closed at once")
    (is (empty? @open) "never left registered")
    (is (= before (count (our-threads))) "no connection thread started")))

(deftest no-thread-of-ours-survives-halt
  (let [dir (temp-dir) path (str dir "/t.sock")]
    (try
      (start! path)
      (let [ch (:channel (:ok (unix/connect! path)))]
        (try
          (is (wait-until #(= 1 (count @(:open-channels @tn/socket-server))) 2000))
          (is (wait-until #(some (fn [^Thread t] (.startsWith (.getName t) "hive-nrepl-unix-conn-"))
                                 (our-threads))
                          2000)
              "connection threads carry a numbered name")
          (tn/stop-socket-nrepl!)
          (is (closed-by-server? ch) "an open client is disconnected by halt")
          (is (wait-until #(empty? (our-threads)) 3000) "accept and connection threads end")
          (finally (.close ^SocketChannel ch))))
      (finally (tn/stop-socket-nrepl!) (rm-rf dir)))))

(deftest a-hung-bind-times-out-and-returns
  (let [release (promise)
        t0      (System/nanoTime)
        r       (tn/start-socket-nrepl! (nrepl.server/default-handler)
                                        (result/ok {:path "/tmp/never.sock"})
                                        {:deadline-ms 200
                                         :bind-fn (fn [_] @release (result/err :test/late {}))})]
    (deliver release true)
    (is (nil? r))
    (is (< (/ (- (System/nanoTime) t0) 1e6) 2000) "the start returned within the deadline")
    (is (= {:status :not-started :reason :nrepl/socket-bind-timeout} @tn/socket-last-start))
    (is (nil? @tn/socket-server))))

(deftest a-socket-that-cannot-start-leaves-tcp-running
  (let [dir (temp-dir) wide (doto (io/file dir "wide") .mkdir)]
    (set-mode! wide "rwxr-xr-x")
    (try
      (doseq [[why redefs cfg]
              [["path refused" {} {:port 0 :socket (str wide "/x.sock")}]
               ["bind failed" {#'unix/bind! (fn [_] (result/err :test/boom {}))}
                {:port 0 :socket (str dir "/x.sock")}]
               ["bind hung" {#'unix/bind! (fn [_] (Thread/sleep 1500) (result/err :test/late {}))
                             #'tn/bind-deadline-ms 200}
                {:port 0 :socket (str dir "/x.sock")}]]]
        (testing why
          (with-redefs-fn redefs
            #(binding [config/*config-source* (src/atom-source (atom {:services {:nrepl cfg}}) {})]
               (let [a   (atom nil)
                     srv (tn/start-embedded-nrepl! a)]
                 (try
                   (is (instance? nrepl.server.Server srv) "start returns the TCP server")
                   (is (pos? (:port srv)))
                   (is (nil? @tn/socket-server))
                   (is (= :not-started (:status @tn/socket-last-start)))
                   (finally (nrepl.server/stop-server srv))))))))
      (finally (rm-rf dir)))))

(deftest the-integrant-key-serves-tcp-and-the-socket-and-halts-both
  (let [dir  (temp-dir)
        path (str dir "/mcp-nrepl.sock")]
    (try
      (binding [config/*config-source*
                (src/atom-source (atom {:services {:nrepl {:port 0 :socket path}}}) {})]
        (let [state (ig/init-key :hive/nrepl {})]
          (try
            (is (= :running (:status state)) "TCP listener still starts")
            (is (pos? (:port (:server state))) "on a real TCP port")
            (is (= path (:path @tn/socket-server)))
            (is (= "rw-------" (mode path)))
            (is (= ["3"] (eval-over-socket path "(+ 1 2)")))
            (finally
              (ig/halt-key! :hive/nrepl state)))
          (is (nil? @tn/socket-server) "halt stops the socket listener")
          (is (not (.exists (io/file path))) "halt unlinks the socket")
          (is (not (.exists socket-file)) "halt removes .nrepl-socket")))
      (finally
        (tn/stop-socket-nrepl!)
        (rm-rf dir)))))

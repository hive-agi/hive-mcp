(ns hive-mcp.server.transport.nrepl-test
  "Facets for the embedded nREPL starter and its `.nrepl-port` file.

   The port file lives in the working directory, which is also where a
   developer's own REPL keeps its port, so every test snapshots the file and
   puts it back. The starter is only driven with a port the probe refuses,
   which is the `already running, skip` arm: no server is ever started."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is use-fixtures]]
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
  (:import [java.nio.file Files LinkOption Path]
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
    (is (= {:ok {:path "/x/y.sock"}} (tn/socket-path-for nil "/x/y.sock" env "/tmp" 4242))
        "HIVE_MCP_NREPL_SOCKET turns it on")
    (is (= {:ok {:path "/x/y.sock"}} (tn/socket-path-for true "/x/y.sock" env "/tmp" 4242))
        "and wins over the canon path")))

(deftest the-default-config-opens-no-socket
  (is (nil? (System/getenv tn/socket-env)) "precondition: the test JVM has no socket env")
  (binding [config/*config-source*
            (src/atom-source (atom {:services {:nrepl {:port 7910}}}) {})]
    (is (nil? (tn/start-socket-nrepl! (nrepl.server/default-handler)))))
  (is (nil? @tn/socket-server) "nothing bound, nothing held")
  (is (not (.exists socket-file)) "no .nrepl-socket written")
  (is (nil? (tn/stop-socket-nrepl!)) "stop with nothing running is a no-op"))

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

(deftest the-integrant-key-serves-tcp-and-the-socket-and-halts-both
  (let [dir  (temp-dir)
        path (str dir "/hive/mcp-nrepl.sock")]
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

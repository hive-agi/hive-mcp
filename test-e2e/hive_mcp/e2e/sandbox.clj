(ns hive-mcp.e2e.sandbox
  "Explicit sandboxes for the e2e tier: a throwaway docker container on a
   random loopback port, started and removed by the test itself. An e2e test
   never points at the operator's live instance, even by default
   (convention 20261111105238-0ceaa524)."
  (:require [clojure.java.shell :as sh])
  (:import [java.net ServerSocket InetAddress]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn free-port
  "A loopback port nothing listens on right now."
  []
  (with-open [s (ServerSocket. 0 0 (InetAddress/getLoopbackAddress))]
    (.getLocalPort s)))

(defn docker?
  "True when a docker daemon answers."
  []
  (try (zero? (:exit (sh/sh "docker" "info" "--format" "{{.ID}}")))
       (catch Exception _ false)))

(defn wait-until
  "Poll (ready?) every 200 ms until it is truthy or ms elapse; answer it."
  [ready? ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (or (try (ready?) (catch Exception _ nil))
          (when (< (System/currentTimeMillis) deadline)
            (Thread/sleep 200)
            (recur))))))

(defn with-container
  "Run IMAGE (args ARGS) as a throwaway container publishing CONTAINER-PORT
   on a random 127.0.0.1 port, call (f host-port) once (ready? host-port)
   holds, and remove the container afterwards. Throws when docker is absent
   or the container never becomes ready: an e2e run that cannot build its
   sandbox is a failure, never a silent skip."
  [{:keys [image args container-port ready? ready-ms name-prefix]
    :or {ready-ms 60000 name-prefix "hive-e2e"}} f]
  (when-not (docker?)
    (throw (ex-info "e2e sandbox needs docker, and no docker daemon answered" {:image image})))
  (let [port  (free-port)
        cname (str name-prefix "-" port)]
    (try
      (let [{:keys [exit err]} (apply sh/sh "docker" "run" "-d" "--rm" "--name" cname
                                      "-p" (str "127.0.0.1:" port ":" container-port)
                                      image args)]
        (when-not (zero? exit)
          (throw (ex-info "e2e sandbox: docker run failed" {:image image :err err}))))
      (when-not (wait-until #(ready? port) ready-ms)
        (throw (ex-info "e2e sandbox: container never became ready" {:image image :port port})))
      (f port)
      (finally (sh/sh "docker" "rm" "-f" cname)))))

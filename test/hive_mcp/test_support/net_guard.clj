(ns hive-mcp.test-support.net-guard
  "Runtime tripwire for the default test tier: the default tier runs on stubs
   and fakes only (convention 20261111105238-0ceaa524), so an outbound
   connection to anything but a loopback port the test itself opened is a
   defect in the suite.

   Mechanism: a JVM-wide ProxySelector. java.net.Socket, java.net.http and
   HttpURLConnection consult it before every connect (clj-http and the Chroma
   vendor client sit on those). It answers NO_PROXY, so behaviour is unchanged,
   and records each refused target. A target is refused when its host is not
   loopback, or its port is one of the live-service ports in `live-ports`
   (Chroma, Milvus, Qdrant, NATS, Ollama, the serving hive-mcp). The record is
   read at the end of the run by hive-mcp.test-runner, which fails the run.

   NIO SocketChannel and Netty bypass the ProxySelector; the static gate
   (hive-mcp.default-tier-isolation-test) covers what this cannot see.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def live-ports
  "Ports where a developer host runs a live backend or the serving hive-mcp."
  {8000  :chroma
   19530 :milvus
   6333  :qdrant
   6334  :qdrant-grpc
   4222  :nats
   11434 :ollama
   7910  :hive-mcp-nrepl
   7911  :hive-mcp-olympus})

(defonce ^:private violations (atom []))

(defn loopback-host?
  "True for localhost and loopback literals."
  [host]
  (boolean
   (and host
        (or (#{"localhost" "127.0.0.1" "::1" "[::1]" "0:0:0:0:0:0:0:1" "0.0.0.0"} host)
            (re-matches #"127(\.\d{1,3}){3}" host)))))

(defn violation
  "The refusal for a connect to URI, or nil when it is allowed."
  [^java.net.URI uri]
  (let [host (.getHost uri)
        port (.getPort uri)
        port (if (neg? port) (case (.getScheme uri) "https" 443 "http" 80 -1) port)]
    (cond
      (nil? host) nil
      (not (loopback-host? host)) {:uri (str uri) :reason :non-loopback-host}
      (contains? live-ports port) {:uri (str uri) :reason :live-service-port
                                   :service (live-ports port)}
      :else nil)))

(defn- caller-frames []
  (->> (.getStackTrace (Thread/currentThread))
       (map str)
       (filter #(re-find #"^hive_mcp\." %))
       (take 6)
       vec))

(defn install!
  "Install the tripwire selector (idempotent). Answers the prior selector."
  []
  (let [prior (java.net.ProxySelector/getDefault)]
    (java.net.ProxySelector/setDefault
     (proxy [java.net.ProxySelector] []
       (select [^java.net.URI uri]
         (when-let [v (violation uri)]
           (swap! violations conj (assoc v :thread (.getName (Thread/currentThread))
                                         :frames (caller-frames))))
         [java.net.Proxy/NO_PROXY])
       (connectFailed [_ _ _] nil)))
    prior))

(defn violations-seen
  "Every refused connect recorded since install! (or the last reset!)."
  []
  @violations)

(defn reset!*
  "Forget recorded violations."
  []
  (reset! violations []))

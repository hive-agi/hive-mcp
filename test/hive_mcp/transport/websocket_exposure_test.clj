(ns hive-mcp.transport.websocket-exposure-test
  "The WebSocket MCP server listens on loopback unless told otherwise, and
   answers 403 to an upgrade from a non-loopback Origin."
  (:require [clojure.core.async :as async]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.transport.websocket :as ws])
  (:import [java.io BufferedReader InputStreamReader]
           [java.net InetAddress InetSocketAddress NetworkInterface Socket]))

(defn- site-address
  "A non-loopback IPv4 address of this host, or nil."
  []
  (->> (enumeration-seq (NetworkInterface/getNetworkInterfaces))
       (mapcat #(enumeration-seq (.getInetAddresses ^NetworkInterface %)))
       (filter #(and (not (.isLoopbackAddress ^InetAddress %))
                     (instance? java.net.Inet4Address %)))
       first))

(defn- connects? [^InetAddress addr port]
  (try
    (with-open [s (Socket.)]
      (.connect s (InetSocketAddress. addr (int port)) 1000)
      true)
    (catch java.io.IOException _refused false)))

(defn- upgrade-status
  "Status code answered to a WebSocket upgrade on 127.0.0.1:`port`, sending
   `origin` as the Origin header when non-nil."
  [port origin]
  (with-open [s (Socket.)]
    (.connect s (InetSocketAddress. "127.0.0.1" (int port)) 2000)
    (.setSoTimeout s 5000)
    (let [req (str "GET / HTTP/1.1\r\n"
                   "Host: 127.0.0.1:" port "\r\n"
                   "Upgrade: websocket\r\n"
                   "Connection: Upgrade\r\n"
                   "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
                   "Sec-WebSocket-Version: 13\r\n"
                   (when origin (str "Origin: " origin "\r\n"))
                   "\r\n")
          out (.getOutputStream s)]
      (.write out (.getBytes req "US-ASCII"))
      (.flush out)
      (let [line (.readLine (BufferedReader. (InputStreamReader. (.getInputStream s) "US-ASCII")))]
        (parse-long (second (str/split line #" ")))))))

(defn- with-server
  "Run `(f port)` against a real server started with `opts`, without
   writing the IDE lockfile or registering a shutdown hook."
  [opts f]
  (with-redefs [ws/create-lockfile!       (fn [_ _] true)
                ws/register-shutdown-hook! (fn [_] nil)
                ws/remove-lockfile!       (fn [_] nil)]
    (ws/stop-server!)
    (let [port (ws/start-server! (merge {:port 0
                                         :input-ch (async/chan (async/sliding-buffer 8))
                                         :output-ch (async/chan)}
                                        opts))]
      (try (f port) (finally (ws/stop-server!))))))

(deftest the-websocket-server-binds-loopback-by-default
  (with-server {}
    (fn [port]
      (is (pos-int? port))
      (is (connects? (InetAddress/getByName "127.0.0.1") port))
      (when-let [addr (site-address)]
        (is (not (connects? addr port)) "not reachable on a site address")))))

(deftest an-explicit-host-is-honoured
  (when-let [addr (site-address)]
    (with-server {:host "0.0.0.0"}
      (fn [port]
        (is (connects? addr port))))))

(deftest a-foreign-origin-upgrade-is-refused
  (with-server {}
    (fn [port]
      (testing "no Origin and a loopback Origin upgrade"
        (is (= 101 (upgrade-status port nil)))
        (is (= 101 (upgrade-status port "http://localhost:8280")))
        (is (= 101 (upgrade-status port "http://127.0.0.1"))))
      (testing "a foreign Origin is refused"
        (is (= 403 (upgrade-status port "http://evil.example")))
        (is (= 403 (upgrade-status port "http://localhost.evil.example")))))))

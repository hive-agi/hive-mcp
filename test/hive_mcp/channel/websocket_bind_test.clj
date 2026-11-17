(ns hive-mcp.channel.websocket-bind-test
  "The WebSocket channel listens on loopback unless told otherwise."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.channel.websocket :as ws])
  (:import [java.net InetAddress InetSocketAddress NetworkInterface Socket]))

(defn- bind-address [host port]
  (#'ws/bind-address host port))

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

(deftest the-default-bind-address-is-loopback
  (is (.isLoopbackAddress (.getAddress ^InetSocketAddress (bind-address nil 9999))))
  (testing "an explicit host is honoured"
    (is (.isAnyLocalAddress (.getAddress ^InetSocketAddress (bind-address "0.0.0.0" 9999))))))

(deftest a-started-channel-answers-only-on-loopback
  (ws/stop!)
  (let [port (ws/start! {:port 0})]
    (try
      (is (connects? (InetAddress/getByName "127.0.0.1") port))
      (when-let [addr (site-address)]
        (is (not (connects? addr port))))
      (finally (ws/stop!)))))

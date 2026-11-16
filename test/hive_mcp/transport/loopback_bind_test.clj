(ns hive-mcp.transport.loopback-bind-test
  "The legacy TCP channel and the Olympus server listen on loopback unless
   told otherwise."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.transport.core :as t]
            [hive-mcp.transport.olympus.http :as olympus])
  (:import [java.net InetAddress InetSocketAddress NetworkInterface Socket]
           [java.nio.channels ServerSocketChannel]))

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

(defn- bound-address ^InetSocketAddress [server]
  (.getLocalAddress ^ServerSocketChannel (:server-channel server)))

(deftest the-tcp-channel-binds-loopback-by-default
  (let [server (t/start-tcp-server! {:port 0})]
    (try
      (is (.isLoopbackAddress (.getAddress (bound-address server))))
      (finally (t/stop-server! server))))
  (testing "an explicit host is honoured"
    (let [server (t/start-tcp-server! {:host "0.0.0.0" :port 0})]
      (try
        (is (.isAnyLocalAddress (.getAddress (bound-address server))))
        (finally (t/stop-server! server))))))

(deftest olympus-answers-only-on-loopback
  (olympus/stop!)
  (let [port (olympus/start! {:port 0})]
    (try
      (is (pos-int? port))
      (is (connects? (InetAddress/getByName "127.0.0.1") port))
      (when-let [addr (site-address)]
        (is (not (connects? addr port))))
      (finally (olympus/stop!)))))
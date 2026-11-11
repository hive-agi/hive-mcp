(ns hive-mcp.channel.websocket-origin-test
  "A WebSocket upgrade from a non-loopback Origin is refused, so a web page in
   a local browser cannot read the channel."
  (:require [aleph.http :as http]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.channel.websocket :as ws]
            [manifold.stream :as s]))

(deftest origin-allowed-is-loopback-or-absent
  (testing "no Origin: a non-browser client"
    (is (true? (ws/origin-allowed? nil))))
  (testing "loopback origins, any port and scheme http(s)"
    (doseq [o ["http://localhost" "http://localhost:7911" "https://127.0.0.1:8443"
               "http://[::1]:9999"]]
      (is (true? (ws/origin-allowed? o)) o)))
  (testing "anything else"
    (doseq [o ["https://evil.example" "http://localhost.evil.example" "null"
               "file://" "http://192.168.100.137:9999" "" "not a uri"
               "chrome-extension://abc"]]
      (is (false? (ws/origin-allowed? o)) o))))

(defn- upgrades? [port headers]
  (try
    (let [c (deref (http/websocket-client (str "ws://127.0.0.1:" port "/")
                                          {:headers headers})
                   5000 ::timeout)]
      (when (s/stream? c) (s/close! c))
      (s/stream? c))
    (catch Exception _refused-upgrade false)))

(deftest a-started-channel-refuses-a-foreign-origin
  (ws/stop!)
  (let [port (ws/start! {:port 0})]
    (try
      (is (upgrades? port {}) "no Origin connects")
      (is (upgrades? port {"Origin" "http://localhost:7911"}) "loopback Origin connects")
      (is (not (upgrades? port {"Origin" "https://evil.example"})) "foreign Origin is refused")
      (finally (ws/stop!)))))
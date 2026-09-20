(ns hive-mcp.transport.a2a-bind-test
  "The A2A gateway fails closed: no api-key means loopback only, and a key
   gates every route that yields agent data, the SSE stream included."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.transport.a2a :as a2a])
  (:import [java.net InetSocketAddress Socket NetworkInterface Inet4Address]))

(use-fixtures :each (fn [t] (a2a/stop!) (try (t) (finally (a2a/stop!)))))

(deftest bind-host-is-loopback-without-a-key
  (testing "no key: loopback, whatever was requested"
    (is (= "127.0.0.1" (a2a/bind-host nil nil)))
    (is (= "127.0.0.1" (a2a/bind-host nil "0.0.0.0")))
    (is (= "127.0.0.1" (a2a/bind-host "" "0.0.0.0")))
    (is (= "127.0.0.1" (a2a/bind-host "   " "10.0.0.5"))))
  (testing "with a key the requested host wins, default all interfaces"
    (is (= "0.0.0.0" (a2a/bind-host "k" nil)))
    (is (= "0.0.0.0" (a2a/bind-host "k" "")))
    (is (= "10.0.0.5" (a2a/bind-host "k" "10.0.0.5")))))

(def ^:private handler-for #'a2a/make-http-handler)

(defn- req [method uri & [auth]]
  {:request-method method :uri uri
   :headers (cond-> {} auth (assoc "authorization" auth))})

(deftest a-key-gates-rpc-and-sse
  (let [h (handler-for "s3cret")]
    (testing "JSON-RPC"
      (is (= 401 (:status (h (req :post "/")))))
      (is (= 401 (:status (h (req :post "/" "Bearer wrong")))))
      (is (= 401 (:status (h (req :post "/" "Bearer s3cre")))))
      (is (= 401 (:status (h (req :post "/" "s3cret"))))))
    (testing "SSE stream is gated too"
      (is (= 401 (:status (h (req :get "/sse/task-1")))))
      (is (= 401 (:status (h (req :get "/sse/task-1" "Bearer nope"))))))
    (testing "health stays open for probes"
      (is (= 200 (:status (h (req :get "/health"))))))))

(defn- non-loopback-ipv4 []
  (->> (enumeration-seq (NetworkInterface/getNetworkInterfaces))
       (filter #(and (.isUp ^NetworkInterface %) (not (.isLoopback ^NetworkInterface %))))
       (mapcat #(enumeration-seq (.getInetAddresses ^NetworkInterface %)))
       (filter #(instance? Inet4Address %))
       first))

(defn- connects? [^java.net.InetAddress addr port]
  (try (with-open [s (Socket.)]
         (.connect s (InetSocketAddress. addr (int port)) 1000)
         true)
       (catch java.io.IOException _ false)))

(deftest a-keyless-gateway-is-unreachable-off-loopback
  (let [port (a2a/start! {:port 0})]
    (is (pos-int? port))
    (is (= "127.0.0.1" (:bind (a2a/status))))
    (is (false? (:auth? (a2a/status))))
    (is (connects? (java.net.InetAddress/getByName "127.0.0.1") port))
    (when-let [addr (non-loopback-ipv4)]
      (is (not (connects? addr port))
          (str "keyless gateway answered on " addr)))))

(deftest a-keyed-gateway-listens-where-asked
  (let [port (a2a/start! {:port 0 :api-key "k"})]
    (is (= "0.0.0.0" (:bind (a2a/status))))
    (is (true? (:auth? (a2a/status))))
    (when-let [addr (non-loopback-ipv4)]
      (is (connects? addr port)))))

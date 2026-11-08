(ns hive-mcp.transport.olympus-origin-test
  "Olympus answers 403 to any request from a non-loopback Origin, so a web
   page in a local browser cannot read /api/* or open /ws."
  (:require [aleph.http :as http]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.transport.olympus.http :as olympus]
            [hive-mcp.transport.origin :as origin]))

(deftest allowed-is-loopback-or-absent
  (is (true? (origin/allowed? nil)))
  (doseq [o ["http://localhost:8280" "https://127.0.0.1" "http://[::1]:7911"]]
    (is (true? (origin/allowed? o)) o))
  (doseq [o ["https://evil.example" "http://localhost.evil.example" "null" "" "file://"]]
    (is (false? (origin/allowed? o)) o)))

(deftest the-middleware-refuses-before-the-handler-runs
  (let [ran (atom 0)
        h   (origin/wrap-loopback-origin (fn [_] (swap! ran inc) {:status 200}))]
    (is (= 403 (:status (h {:headers {"origin" "https://evil.example"}}))))
    (is (zero? @ran))
    (is (= 200 (:status (h {:headers {}}))))
    (is (= 200 (:status (h {:headers {"origin" "http://localhost:7911"}}))))))

(defn- status-of [port headers]
  (:status @(http/get (str "http://127.0.0.1:" port "/health")
                      {:headers headers :throw-exceptions? false})))

(deftest a-started-olympus-refuses-a-foreign-origin
  (olympus/stop!)
  (let [port (olympus/start! {:port 0})]
    (try
      (testing "no Origin and a loopback Origin read /health"
        (is (= 200 (status-of port {})))
        (is (= 200 (status-of port {"Origin" "http://localhost:8280"}))))
      (testing "a foreign Origin is refused"
        (is (= 403 (status-of port {"Origin" "https://evil.example"}))))
      (finally (olympus/stop!)))))
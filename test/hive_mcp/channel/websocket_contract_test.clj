(ns hive-mcp.channel.websocket-contract-test
  "Contracts of the WebSocket channel, exercised under instrumentation against
   a real server on an ephemeral port and a manifold stream standing in for a
   client socket."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.websocket :as ws]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.instrument :as mi]
            [manifold.stream :as s]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private contracted
  #{#'ws/add-on-connect-hook! #'ws/clear-on-connect-hooks! #'ws/emit-to!
    #'ws/start! #'ws/stop! #'ws/broadcast! #'ws/connected? #'ws/client-count
    #'ws/status #'ws/emit!})

(use-fixtures :once
  (fn [f]
    (mi/instrument! {:filters [(mi/-filter-var contracted)]})
    (try (f) (finally (mi/unstrument! {:filters [(mi/-filter-var contracted)]})))))

(deftest schemas-generate
  (doseq [sch [ws/Socket ws/Deferred ws/EventType ws/Status]]
    (is (seq (mg/sample sch {:size 3})))))

(deftest server-lifecycle-honours-contracts
  (ws/clear-on-connect-hooks!)
  (try
    (let [port (ws/start! {:port 0})]
      (is (pos-int? port))
      (testing "a second start! answers the running port"
        (is (= port (ws/start! {:port 0}))))
      (let [st (ws/status)]
        (is (m/validate ws/Status st))
        (is (true? (:running? st)))
        (is (= port (:port st)))
        (is (zero? (ws/client-count)))
        (is (false? (ws/connected?))))
      (is (nil? (ws/emit! :ping {:x 1})) "no clients: nothing to send"))
    (finally
      (is (true? (ws/stop!)))))
  (is (nil? (ws/stop!)) "stopping a stopped channel is a no-op")
  (is (= {:running? false :port nil :clients 0 :connected? false :endpoints nil}
         (ws/status))))

(deftest emit-to-writes-one-json-frame
  (let [sock (s/stream 4)]
    (ws/emit-to! sock :hello {:who "x"})
    (let [frame @(s/try-take! sock ::none 100 ::timeout)
          decoded (json/read-str frame :key-fn keyword)]
      (is (= "hello" (:type decoded)))
      (is (= "x" (:who decoded)))
      (is (int? (:timestamp decoded))))
    (s/close! sock)
    (is (nil? (ws/emit-to! sock :hello {})) "a closed socket is skipped")))

(deftest on-connect-hooks-count
  (ws/clear-on-connect-hooks!)
  (is (= 1 (ws/add-on-connect-hook! (fn [_emit] nil))))
  (is (= 2 (ws/add-on-connect-hook! (fn [_emit] nil))))
  (is (nil? (ws/clear-on-connect-hooks!))))

(deftest-golden status-when-stopped
  "test/golden/channel/websocket-status-stopped.edn"
  (do (ws/stop!) (ws/status)))

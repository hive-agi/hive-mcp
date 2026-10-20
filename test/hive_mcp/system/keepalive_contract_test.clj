(ns hive-mcp.system.keepalive-contract-test
  "Contract facets for hive-mcp.system.keepalive. No JVM shutdown hook is
   registered; every channel and promise is local to the test."
  (:require [clojure.core.async :as async]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.system.keepalive :as keepalive]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- delivered-ch [v]
  (doto (async/promise-chan) (async/put! v)))

(golden/deftest-golden await-arms-golden
  "test/golden/hive-mcp/system/keepalive-await.edn"
  {:await-shutdown     (keepalive/await-shutdown! {:mode :stdio :shutdown-ch (delivered-ch :shutdown)})
   :await-done         (keepalive/await-shutdown! {:mode :promise :shutdown-ch (delivered-ch :done)})
   :await-closed       (keepalive/await-shutdown! {:shutdown-ch (doto (async/chan) async/close!)})
   :stdio-wins         (keepalive/await-shutdown-or-stdio!
                        {:shutdown-ch (async/promise-chan)} (doto (promise) (deliver :done)))
   :shutdown-wins      (keepalive/await-shutdown-or-stdio!
                        {:shutdown-ch (delivered-ch :shutdown)} (promise))})

(deftest keepalive-awaits-return-what-was-delivered
  (let [ch (async/promise-chan)]
    (async/put! ch :shutdown)
    (is (= :shutdown (keepalive/await-shutdown! {:mode :stdio :shutdown-ch ch}))))
  (testing "stdio join wins when shutdown never fires"
    (let [p (doto (promise) (deliver :done))]
      (is (= [:stdio :done]
             (keepalive/await-shutdown-or-stdio! {:shutdown-ch (async/promise-chan)} p)))))
  (testing "shutdown wins when the join never delivers"
    (let [ch (async/promise-chan)]
      (async/put! ch :shutdown)
      (is (= [:shutdown :shutdown]
             (keepalive/await-shutdown-or-stdio! {:shutdown-ch ch} (promise)))))))

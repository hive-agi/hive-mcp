(ns hive-mcp.vectordb.facade-index-test
  "Contract tests for ID-only facade writes against an IMemoryStore stub."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.protocols.memory :as proto]
            [hive-mcp.vectordb.facade :as facade]
            [hive-test.trifecta :refer [deftrifecta]]))

(defn- store-returning [response]
  (reify proto/IMemoryStore
    (connect! [_ _] {:success? true})
    (disconnect! [_] nil)
    (connected? [_] true)
    (health-check [_] {:healthy? true})
    (add-entry! [_ _] response)
    (get-entry [_ _] nil)
    (update-entry! [_ _ _] nil)
    (delete-entry! [_ _] nil)
    (query-entries [_ _] [])
    (search-similar [_ _ _] [])
    (supports-semantic-search? [_] false)
    (cleanup-expired! [_] nil)
    (entries-expiring-soon [_ _ _] [])
    (find-duplicate [_ _ _ _] nil)
    (store-status [_] {})
    (reset-store! [_] nil)))

(defn- index-result [scenario]
  (let [response (case scenario
                   :id "memory-1"
                   :envelope {:success? false :id "memory-1"}
                   :nil nil
                   :blank "")]
    (with-redefs [proto/get-store (fn [& _] (store-returning response))]
      (try
        (facade/index-memory-entry! {:type "note"})
        (catch clojure.lang.ExceptionInfo e
          (:error (ex-data e)))))))

(deftrifecta facade-write-id-contract
  index-result
  {:cases {:id :id
           :envelope :envelope
           :nil :nil
           :blank :blank}
   :xf (fn [result] (if (= "memory-1" result) :id
                       (if (= :vectordb/invalid-write-result result)
                         :invalid :unknown)))
   :gen (gen/elements [:id :envelope :nil :blank])
   :pred #(or (= "memory-1" %) (= :vectordb/invalid-write-result %))
   :num-tests 40
   :mutations [["accept-envelope" (fn [scenario]
                                     (if (= :envelope scenario)
                                       {:success? false :id "memory-1"}
                                       :vectordb/invalid-write-result))]
               ["accept-nil" (fn [scenario]
                               (if (= :nil scenario) nil
                                   :vectordb/invalid-write-result))]]
   :golden-path "test/golden/facade-write-id-contract.edn"})

(deftest batch-invalid-results-preserve-positions
  (let [responses (atom ["one" {:success? false :id "ghost"} nil "two"])
        store (reify proto/IMemoryStore
                (connect! [_ _] {:success? true})
                (disconnect! [_] nil)
                (connected? [_] true)
                (health-check [_] {:healthy? true})
                (add-entry! [_ _] (let [r (first @responses)]
                                    (swap! responses rest)
                                    r))
                (get-entry [_ _] nil)
                (update-entry! [_ _ _] nil)
                (delete-entry! [_ _] nil)
                (query-entries [_ _] [])
                (search-similar [_ _ _] [])
                (supports-semantic-search? [_] false)
                (cleanup-expired! [_] nil)
                (entries-expiring-soon [_ _ _] [])
                (find-duplicate [_ _ _ _] nil)
                (store-status [_] {})
                (reset-store! [_] nil))]
    (with-redefs [proto/get-store (fn [& _] store)]
      (is (= ["one" nil nil "two"]
             (facade/index-memory-entries! (repeat 4 {:type "note"})))))))

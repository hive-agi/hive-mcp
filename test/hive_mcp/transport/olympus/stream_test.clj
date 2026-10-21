(ns hive-mcp.transport.olympus.stream-test
  "Facets for hive-mcp.transport.olympus.stream (cov-facets f07). No socket:
   the namespace's public `clients` set is the port, and an in-memory manifold
   stream is a faithful client (the WebSocket socket aleph hands the handler
   is itself a manifold duplex stream). Each emitter is driven against it and
   the JSON it received is read back. :timestamp is wall-clock and is dropped
   from the snapshot."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [hive-mcp.transport.olympus.stream :as stream]
            [hive-test.golden :refer [deftest-golden]]
            [manifold.stream :as s]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- drain [client]
  (loop [acc []]
    (let [m @(s/try-take! client nil 100 ::none)]
      (if (or (nil? m) (= ::none m))
        acc
        (recur (conj acc (-> (json/read-str m :key-fn keyword) (dissoc :timestamp))))))))

(defn- with-client
  "Run f with one in-memory client installed in `stream/clients`, restoring
   the previous set afterwards; answer {:returns (f) :received [...]}."
  [f]
  (let [prev   @stream/clients
        client (s/stream 64)]
    (reset! stream/clients #{client})
    (try (let [ret (f)] {:returns ret :received (drain client)})
         (finally (s/close! client) (reset! stream/clients prev)))))

(deftest-golden olympus-stream-emitters-golden
  "test/golden/hive-mcp/f07-olympus-stream-emitters.edn"
  {:connected
   (with-client
     #(vector (stream/broadcast! {:type :agents :data [{:id "ling-1"}]})
              (stream/emit! :custom {:k 1})
              (stream/emit! "string-type" nil)
              (stream/emit-agent-event! :agent-spawned {:id "ling-2" :status "idle"})
              (stream/emit-hivemind-shout! {:agent-id "ling-2" :event-type :progress
                                            :message "half" :task "t"})
              (stream/emit-kg-event! :kg-entry-added {:entry {:id "e1"}})))
   :no-clients
   (let [prev @stream/clients]
     (reset! stream/clients #{})
     (try (stream/broadcast! {:type :agents})
          (finally (reset! stream/clients prev))))})

(deftest a-closed-client-is-skipped
  (let [prev   @stream/clients
        closed (s/stream)]
    (s/close! closed)
    (reset! stream/clients #{closed})
    (try (is (nil? (stream/broadcast! {:type :x})))
         (finally (reset! stream/clients prev)))))

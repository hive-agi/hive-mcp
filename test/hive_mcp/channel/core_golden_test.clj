(ns hive-mcp.channel.core-golden-test
  "Golden over `hive-mcp.channel.core`: the internal event bus (publish,
   subscribe, unsubscribe, emit-event!'s envelope) with no server, and the
   server lifecycle on a throwaway unix socket under java.io.tmpdir. The
   transport is the real one; nothing is redefined."
  (:require [clojure.core.async :as async]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.channel.core :as ch]
            [hive-test.golden :refer [deftest-golden-fn]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (ch/force-stop-server!) (try (f) (finally (ch/force-stop-server!)))))

(defn- take! [c] (async/alt!! c ([v] v) (async/timeout 1000) ::timeout))

(deftest-golden-fn event-bus-golden
  "test/golden/channel/core-event-bus.edn"
  (fn []
    (let [sub-a (ch/subscribe! :f02/alpha)
          sub-b (ch/subscribe! :f02/beta)]
      (try
        [[:publish      (ch/publish! {:type :f02/alpha :n 1})]
         [:got-alpha    (take! sub-a)]
         [:emit         (ch/emit-event! :f02/beta {:agent-id "l1" "already" "str"})]
         [:got-beta     (-> (take! sub-b) (update "timestamp" int?))]
         [:no-type      (try (ch/publish! {:n 2}) (catch clojure.lang.ExceptionInfo e (ex-message e)))]
         [:no-server    [(ch/broadcast! {:x 1}) (ch/client-count) (ch/server-connected?) (ch/stop-server!)]]
         [:unsubscribe  (ch/unsubscribe! :f02/alpha sub-a)]
         [:closed-take  (take! sub-a)]]
        (finally (ch/unsubscribe! :f02/beta sub-b))))))

(deftest-golden-fn server-lifecycle-golden
  "test/golden/channel/core-server-lifecycle.edn"
  (fn []
    (let [path (str (io/file (System/getProperty "java.io.tmpdir") (str "f02-channel-" (random-uuid) ".sock")))
          started (ch/start-server! {:type :unix :path path})
          shape   (fn [s] (-> s (select-keys [:type :port]) (assoc :path? (= path (:path s)))))]
      [[:started       (shape started)]
       [:again-same?   (= (dissoc started :running) (dissoc (ch/start-server! {:type :unix :path path}) :running))]
       [:clients       (ch/client-count)]
       [:connected?    (ch/server-connected?)]
       [:mark          (ch/mark-coordinator-running!)]
       [:guarded-stop  [(ch/stop-server!) (ch/client-count)]]
       [:force-stop    [(ch/force-stop-server!) (ch/client-count)]]])))

(deftest envelope-keys-are-pinned-test
  (is (= {:type "type" :timestamp "timestamp"} ch/envelope-keys)))

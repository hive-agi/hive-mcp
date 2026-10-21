(ns hive-mcp.addons.stdio-bridge-test
  "Golden over the stdio IMcpBridge record in its unit-safe arms: a fresh
   bridge (disconnected status, health, empty tool list, call refused), a
   start whose command does not exist (failure reported as data, state left
   empty), a start against a process that is not an MCP server (handshake
   refused), and stop on a never-started bridge. The connected lifecycle
   against a real MCP server is the ^:integration suite in mcp-bridge-test."
  (:require [clojure.test :refer [deftest is]]
            [hive-addon.protocol :as proto]
            [hive-mcp.addons.mcp-bridge :as bridge]
            [hive-mcp.addons.stdio-bridge :as stdio]
            [hive-test.golden :refer [deftest-golden-fn]]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- start-view [res]
  (-> res
      (update :errors #(mapv string? %))
      (select-keys [:success? :errors])))

(defn- sorted-vec [s] (vec (sort s)))

(defn- scenario []
  (let [fresh   (stdio/->stdio-bridge :bridge/f08-fresh)
        missing (stdio/->stdio-bridge :bridge/f08-missing)
        not-mcp (stdio/->stdio-bridge :bridge/f08-not-mcp)]
    {:identity   {:addon-id   (proto/addon-id fresh)
                  :addon-type (proto/addon-type fresh)
                  :caps       (sorted-vec (proto/capabilities fresh))
                  :transport  (bridge/transport-type fresh)
                  :schema     (proto/schema-extensions fresh)
                  :excluded   (sorted-vec (proto/excluded-tools fresh))
                  :hooks      (proto/hooks fresh)}
     :fresh      {:status (bridge/bridge-status fresh)
                  :health (proto/health fresh)
                  :tools  (bridge/list-remote-tools fresh)
                  :call   (try (bridge/call-tool fresh "echo" {}) :no-throw
                               (catch clojure.lang.ExceptionInfo e
                                 {:msg (ex-message e) :data (ex-data e)}))
                  :stop   (bridge/stop-bridge! fresh)}
     :missing    {:start (start-view (bridge/start-bridge! missing {:command ["/no/such/f08-binary"]}))
                  :state @(:state missing)}
     :not-mcp    {:start (start-view (proto/initialize! not-mcp {:command ["true"]}))
                  :state @(:state not-mcp)}}))

(deftest-golden-fn stdio-bridge-arms-golden
  "test/golden/addons/stdio-bridge-arms.edn"
  scenario)

(deftest constructor-answers-a-bridge-addon
  (let [b (stdio/->stdio-bridge :bridge/f08)]
    (is (m/validate bridge/BridgeAddon b))
    (is (nil? @(:state b)))))

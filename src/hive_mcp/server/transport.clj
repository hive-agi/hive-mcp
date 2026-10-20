(ns hive-mcp.server.transport
  "Transport facade — re-exports start functions from SRP sub-namespaces.

   Each transport concern lives in its own namespace:
   - transport.nrepl          → start-embedded-nrepl!
   - transport.websocket-mcp  → start-websocket-server!
   - transport.ws-channel     → start-ws-channel-with-healing!
   - transport.olympus-ws     → start-olympus-ws!
   - transport.a2a-gateway    → start-a2a-gateway!
   - transport.legacy         → start-legacy-channel!

   Callers should migrate to direct sub-namespace requires over time."
  (:require [hive-mcp.server.transport.nrepl :as transport-nrepl]
            [hive-mcp.server.transport.websocket-mcp :as ws-mcp]
            [hive-mcp.server.transport.ws-channel :as ws-ch]
            [hive-mcp.server.transport.olympus-ws :as oly-ws]
            [hive-mcp.server.transport.a2a-gateway :as a2a-gw]
            [hive-mcp.server.transport.legacy :as legacy-ch]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; nREPL Server (delegated to hive-mcp.server.transport.nrepl)
;; =============================================================================

(def AtomRef
  "An atom the transport stores its server or monitor in."
  [:fn {:error/message "must be an atom" :gen/schema :nil :gen/fmap (fn [_] (atom nil))}
   #(instance? clojure.lang.IAtom %)])

(def StartStatus
  "What a gated transport answers: nil when not enabled, else its status map
   (:running with the server where one is held, or :failed)."
  [:maybe [:map [:status [:enum :running :failed]] [:port {:optional true} [:maybe :int]]]])

(def ComponentConfig
  "An Integrant component config for a gated transport."
  [:maybe [:map-of :keyword :any]])

(defn start-embedded-nrepl!
  "Start an embedded nREPL server for bb-mcp tool forwarding.
   Delegates to hive-mcp.server.transport.nrepl."
  [nrepl-server-atom]
  (transport-nrepl/start-embedded-nrepl! nrepl-server-atom))

(m/=> start-embedded-nrepl! [:=> [:cat AtomRef] :any])

;; =============================================================================
;; WebSocket MCP Server (delegated to hive-mcp.server.transport.websocket-mcp)
;; =============================================================================

(defn start-websocket-server!
  "Start WebSocket MCP server if enabled via component or runtime config.
   Delegates to hive-mcp.server.transport.websocket-mcp."
  ([] (ws-mcp/start-websocket-server!))
  ([component-config] (ws-mcp/start-websocket-server! component-config)))

(m/=> start-websocket-server! [:function [:=> [:cat] StartStatus] [:=> [:cat ComponentConfig] StartStatus]])

;; =============================================================================
;; WebSocket Channel (delegated to hive-mcp.server.transport.ws-channel)
;; =============================================================================

(defn start-ws-channel-with-healing!
  "Start WebSocket channel server with auto-healing.
   Delegates to hive-mcp.server.transport.ws-channel."
  [ws-channel-monitor]
  (ws-ch/start-ws-channel-with-healing! ws-channel-monitor))

(m/=> start-ws-channel-with-healing! [:=> [:cat AtomRef] :nil])

;; =============================================================================
;; Olympus WebSocket (delegated to hive-mcp.server.transport.olympus-ws)
;; =============================================================================

(defn start-olympus-ws!
  "Start Olympus WebSocket server for Web UI.
   Delegates to hive-mcp.server.transport.olympus-ws."
  []
  (oly-ws/start-olympus-ws!))

(m/=> start-olympus-ws! [:=> [:cat] :nil])

;; =============================================================================
;; A2A Gateway (delegated to hive-mcp.server.transport.a2a-gateway)
;; =============================================================================

(defn start-a2a-gateway!
  "Start A2A JSON-RPC gateway for external agent interoperability.
   Delegates to hive-mcp.server.transport.a2a-gateway."
  ([] (a2a-gw/start-a2a-gateway!))
  ([component-config] (a2a-gw/start-a2a-gateway! component-config)))

(m/=> start-a2a-gateway! [:function [:=> [:cat] StartStatus] [:=> [:cat ComponentConfig] StartStatus]])

;; =============================================================================
;; Legacy Channel (delegated to hive-mcp.server.transport.legacy)
;; =============================================================================

(defn start-legacy-channel!
  "Start legacy TCP channel server (deprecated).
   Delegates to hive-mcp.server.transport.legacy."
  []
  (legacy-ch/start-legacy-channel!))

(m/=> start-legacy-channel! [:=> [:cat] :nil])

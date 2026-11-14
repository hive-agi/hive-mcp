(ns hive-mcp.server.transport.ws-channel
  "WebSocket channel with auto-healing monitor.

   Single responsibility: start WebSocket channel server and monitor loop
   that restarts the server if it dies."
  (:require [hive-mcp.config.core :as config]
            [hive-mcp.channel.websocket :as ws-channel]
            [clojure.core.async :as async]
            [taoensso.timbre :as log]
            [hive-mcp.channel.inbound :as inbound]
            [malli.core :as m]
            [hive-mcp.server.transport.schema :as ts]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn channel-opts
  "Start options for the WebSocket channel on `port`. :on-message routes what
   a client sends (an answer to a hivemind ask); without it every client
   message was read and dropped."
  [port]
  {:port port
   :on-message inbound/handle-client-message!})

(defn- bind-exception?
  "True when `cause` (or any nested cause) is a java.net.BindException —
   i.e. the configured host:port is already held, almost always by another
   process. Drives the once-with-backoff path so a second hive-mcp JVM on
   the same box does not spam 'restart failed' every 30s."
  [^Throwable cause]
  (loop [t cause]
    (cond
      (nil? t) false
      (instance? java.net.BindException t) true
      :else (recur (.getCause t)))))

(def ^:private bind-backoff-ms
  "How long to suppress further restart attempts after a BindException.
   Long enough that the other process has a chance to release the port;
   short enough that we still recover on the same launch window."
  (* 5 60 1000))

(def ^:private bind-conflict-until
  "Epoch ms until which restart attempts are suppressed because the port is
   held by another process. Reset to 0 on a successful restart."
  (atom 0))

(defn- check-channel!
  "Run one healing pass. Close the stale handle before one restart attempt,
   and retain any stop/bind exception as the warning's cause.

   When the restart fails with a BindException (another process already
   holds the port — the typical two-hive-mcp-JVMs case), log ONE clear
   'port in use' warning and back off for `bind-backoff-ms` instead of
   warning every 30s. Other failures are logged each pass with their
   cause."
  [opts host port]
  (let [now (System/currentTimeMillis)]
    (when (and (not (:running? (ws-channel/status)))
               (>= now @bind-conflict-until))
      (try
        (ws-channel/stop!)
        (ws-channel/start! opts)
        (reset! bind-conflict-until 0)
        (log/info "WebSocket channel server restarted on" host "port" port)
        (catch Exception cause
          (if (bind-exception? cause)
            (do
              (reset! bind-conflict-until (+ now bind-backoff-ms))
              (log/warn cause
                        "WebSocket channel port" port "on" host
                        "is already in use by another process;"
                        "suppressing restart attempts for"
                        (quot bind-backoff-ms 1000) "s"))
            (log/warn cause
                      "WebSocket channel server restart failed on"
                      host "port" port)))))))

(def ChannelOpts
  "Start options `channel-opts` builds for `hive-mcp.channel.websocket/start!`."
  [:map {:closed true}
   [:port ts/Port]
   [:on-message [:fn {:gen/schema :nil
                      :gen/fmap (fn [_] inbound/handle-client-message!)}
                 ifn?]]])

(m/=> channel-opts [:=> [:cat ts/Port] ChannelOpts])

(defn start-ws-channel-with-healing!
  "Start WebSocket channel server with auto-healing.

   Runs a background async loop that monitors and restarts if needed.

   Parameters:
     ws-channel-monitor - atom to store the monitoring go-loop channel"
  [ws-channel-monitor]
  (let [port (config/get-service-value :ws-channel :port
                                       :env "HIVE_MCP_WS_CHANNEL_PORT"
                                       :parse parse-long
                                       :default 9999)
        host (config/get-service-value :ws-channel :host
                                       :env "HIVE_MCP_WS_CHANNEL_HOST"
                                       :default "127.0.0.1")
        opts (assoc (channel-opts port) :host host)
        check-interval-ms 30000] ; Check every 30 seconds
    ;; Start initial server
    (try
      (ws-channel/start! opts)
      (log/info "WebSocket channel server started on" host "port" port)
      (catch Exception cause
        (log/warn cause "WebSocket channel server start failed on" host "port" port)))
    ;; Start monitoring loop
    (when-not @ws-channel-monitor
      (reset! ws-channel-monitor
              (async/go-loop []
                (async/<! (async/timeout check-interval-ms))
                (when-not (ws-channel/connected?)
                  (log/debug "WebSocket channel: no clients, server healthy"))
                ;; Server running but no clients is fine

                (check-channel! opts host port)
                (recur)))
      (log/info "WebSocket channel auto-heal monitor started"))))

;; both branches end in nil: a monitor already present short-circuits the
;; when-not, and a fresh one ends on log/info
(m/=> start-ws-channel-with-healing! [:=> [:cat ts/AtomRef] :nil])

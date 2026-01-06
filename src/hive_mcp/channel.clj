(ns hive-mcp.channel
  "Bidirectional communication channel between Clojure and Emacs.

   Built on transport abstraction for robust async networking.

   Architecture:
   - Transport layer (Unix sockets or TCP) accepts Emacs connections
   - Bencode message format (compatible with nREPL/Emacs)
   - core.async pub/sub for internal event routing

   Usage:
     (start-server! {:type :unix})  ; default - Unix domain socket
     (start-server! {:type :tcp :port 9998})  ; TCP alternative
     (broadcast! {:type :hivemind-progress :data {...}})
     (subscribe! :hivemind-progress) ; => core.async channel
   "
  (:require [hive-mcp.transport :as t]
            [clojure.core.async :as async :refer [chan pub sub unsub close!]]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Event Bus (core.async pub/sub for internal routing)
;; =============================================================================

(defonce ^:private event-chan (chan 1024))
(defonce ^:private event-pub (pub event-chan :type))

(defn publish!
  "Publish event to the internal event bus.
   Event must have :type key for routing."
  [event]
  (when-not (:type event)
    (throw (ex-info "Event must have :type" {:event event})))
  (async/put! event-chan event))

(defn subscribe!
  "Subscribe to events of given type.
   Returns a core.async channel that receives matching events."
  [event-type]
  (let [ch (chan 256)]
    (sub event-pub event-type ch)
    ch))

(defn unsubscribe!
  "Unsubscribe channel from event type."
  [event-type ch]
  (unsub event-pub event-type ch)
  (close! ch))

;; =============================================================================
;; Server State
;; =============================================================================

(defonce ^:private server-state (atom nil))

;; =============================================================================
;; Public API
;; =============================================================================

(defn start-server!
  "Start the channel server.
   Emacs will connect to this server.

   Options:
     :type - :unix (default) or :tcp
     :path - Unix socket path (default: /tmp/hive-mcp-channel.sock)
     :port - TCP port (default: 9998) - only used for :tcp type

   Returns server state map."
  [{:keys [type path port] :or {type :unix} :as opts}]
  (if @server-state
    (do
      (log/warn "Server already running")
      @server-state)
    (let [on-message (fn [msg client-id]
                       (log/debug "Received from" client-id ":" msg)
                       ;; Route to internal pub/sub
                       (when-let [type-str (get msg "type")]
                         (publish! (assoc msg :type (keyword type-str) :client-id client-id))))
          on-connect (fn [client-id]
                       (log/info "Client connected:" client-id))
          server (t/start-server! (merge opts
                                         {:type type
                                          :on-message on-message
                                          :on-connect on-connect}))]
      (log/info "Channel server started" (if (= type :unix)
                                           (str "on socket " (or path "/tmp/hive-mcp-channel.sock"))
                                           (str "on TCP port " (or port 9998))))
      (reset! server-state {:server server :type type})
      @server-state)))

(defn stop-server!
  "Stop the channel server."
  []
  (when-let [{:keys [server]} @server-state]
    (t/stop-server! server)
    (reset! server-state nil)
    (log/info "Server stopped")))

(defn broadcast!
  "Send message to all connected clients."
  [msg]
  (when-let [{:keys [server]} @server-state]
    (t/broadcast! server msg)))

(defn server-connected?
  "Check if the channel server is running and has connected clients."
  []
  (when-let [{:keys [server]} @server-state]
    (and (t/server-running? server)
         (pos? (or (t/client-count server) 0)))))

(defn client-count
  "Return number of connected clients."
  []
  (when-let [{:keys [server]} @server-state]
    (t/client-count server)))

;; =============================================================================
;; Convenience Functions
;; =============================================================================

(defn emit-event!
  "Emit an event to all connected clients and local subscribers.

   Example:
     (emit-event! :task-completed {:task-id \"123\" :result \"done\"})"
  [event-type data]
  (let [string-data (into {} (map (fn [[k v]] [(name k) v]) data))
        event (assoc string-data
                     "type" (name event-type)
                     "timestamp" (System/currentTimeMillis)
                     :type event-type)]
    ;; Local pub/sub
    (publish! event)
    ;; Broadcast to Emacs clients
    (broadcast! event)))

(comment
  ;; Development REPL examples

  ;; Start server (Unix socket - default)
  (start-server! {:type :unix})
  ;; or with custom path
  (start-server! {:type :unix :path "/tmp/my-channel.sock"})
  ;; or TCP
  (start-server! {:type :tcp :port 9998})

  ;; Check status
  (server-connected?)
  (client-count)

  ;; Test broadcast
  (broadcast! {:type :hivemind-progress
               :data {:agent-id "test" :message "hello"}})

  ;; Stop server
  (stop-server!))

;; =============================================================================
;; MCP Tool Definitions
;; =============================================================================

(def channel-tools
  "Channel-related MCP tools - currently empty as channel operations
   are handled internally and not exposed as user-facing tools."
  [])

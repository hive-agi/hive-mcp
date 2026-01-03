# Bidirectional Channel: Real-Time Communication Between Emacs and Clojure

## Overview

The bidirectional channel provides **push-based event communication** between the Clojure MCP server and Emacs. Unlike the existing CIDER/nREPL connection (which is request-response), this channel allows either side to initiate communication.

```
┌─────────────────┐                      ┌─────────────────┐
│     Emacs       │◄────── Push ────────►│    Clojure      │
│                 │                      │   MCP Server    │
│ emacs-mcp-      │   TCP Port 9999      │                 │
│ channel.el      │   Bencode Format     │ channel.clj     │
└─────────────────┘                      └─────────────────┘
```

## Why Not Reuse CIDER?

| Aspect | CIDER/nREPL | Bidirectional Channel |
|--------|-------------|----------------------|
| Direction | Request → Response | Both ways |
| Initiator | Always Emacs | Either side |
| Use Case | Code evaluation, completion | Real-time events |
| Latency | Per-request | Sub-100ms push |

**Key insight**: nREPL is fundamentally request-response. The Clojure side cannot send unsolicited messages to Emacs. For swarm coordination (e.g., "slave finished task!"), we need push notifications.

## Architecture

### Message Format: Bencode

We use **bencode** (the same format as nREPL) for message encoding:

```
d7:message12:Hello World!4:type10:test-evente
```

Decodes to:
```elisp
(("message" . "Hello World!") ("type" . "test-event"))
```

### Clojure Side (`src/emacs_mcp/channel.clj`)

**Key Functions:**

```clojure
;; Start the server (automatically done on MCP server start)
(channel/start-server! {:type :tcp :port 9999})

;; Send message to ALL connected Emacs clients
(channel/broadcast! {:type "task-completed"
                     :task-id "abc123"
                     :result "success"})

;; Subscribe to events FROM Emacs
(let [ch (channel/subscribe! :emacs-ping)]
  (go-loop []
    (when-let [msg (<! ch)]
      (println "Received from Emacs:" msg)
      (recur))))

;; Stop server (closes all connections)
(channel/stop-server!)
```

**Server State:**
```clojure
@server-state
;; => {:type :tcp
;;     :port 9999
;;     :server #<ServerSocket>
;;     :clients #<Atom {client-123 <TcpChannel>}>
;;     :running #<Atom true>}
```

### Emacs Side (`elisp/emacs-mcp-channel.el`)

**Key Functions:**

```elisp
;; Connect to Clojure server
(setq emacs-mcp-channel-type 'tcp)
(setq emacs-mcp-channel-port 9999)
(emacs-mcp-channel-connect)

;; Check connection
(emacs-mcp-channel-connected-p) ;; => t

;; Send message TO Clojure
(emacs-mcp-channel-send '(("type" . "emacs-ping")
                          ("message" . "Hello from Emacs!")))

;; Register handler for events FROM Clojure
(emacs-mcp-channel-on :task-completed
  (lambda (msg)
    (message "Task %s completed!" (cdr (assoc "task-id" msg)))))

;; Get recent events (automatically stored)
(emacs-mcp-channel-get-recent-events 10)

;; Disconnect
(emacs-mcp-channel-disconnect)
```

## Event Flow

### Clojure → Emacs (Push Notifications)

```
1. Clojure: (broadcast! {:type "hivemind-hello" :message "Hi!"})
2. Server encodes to bencode, sends to all clients
3. Emacs process-filter receives data
4. emacs-mcp-channel--decode parses bencode
5. emacs-mcp-channel--dispatch routes to handlers
6. Registered handlers receive the message
7. Default handler stores in emacs-mcp-channel--recent-events
```

### Emacs → Clojure (Client Messages)

```
1. Emacs: (emacs-mcp-channel-send '(("type" . "emacs-ping")))
2. emacs-mcp-channel--encode converts to bencode
3. Sent via process-send-string
4. Clojure handle-client go-loop receives via recv!
5. Message published to event-bus with (publish!)
6. Subscribers via (subscribe! :emacs-ping) receive the message
```

## Swarm Integration

The swarm system uses the channel for real-time coordination:

```elisp
;; In emacs-mcp-swarm.el
(defun emacs-mcp-swarm--emit-event (event-type data)
  "Emit EVENT-TYPE with DATA through the channel if connected."
  (when (emacs-mcp-swarm--channel-available-p)
    (emacs-mcp-channel-send
     `(("type" . ,event-type)
       ("timestamp" . ,(float-time))
       ,@data))))

;; Events emitted:
;; - task-completed: When a slave finishes its work
;; - task-failed: When a slave encounters an error
;; - prompt-shown: When a slave needs human input
;; - slave-spawned: When a new slave is created
;; - slave-killed: When a slave is terminated
```

## Configuration

### Emacs Customization

```elisp
;; Channel transport (unix or tcp)
(setq emacs-mcp-channel-type 'tcp)

;; TCP settings
(setq emacs-mcp-channel-host "localhost")
(setq emacs-mcp-channel-port 9999)

;; Unix socket settings
(setq emacs-mcp-channel-socket-path "/tmp/emacs-mcp-channel.sock")

;; Auto-reconnect settings
(setq emacs-mcp-channel-reconnect-interval 5.0)
(setq emacs-mcp-channel-max-reconnects 10)

;; Auto-connect on load (optional)
(setq emacs-mcp-channel-auto-connect t)
(emacs-mcp-channel-setup-auto-connect)

;; Event history size
(setq emacs-mcp-channel-event-history-size 100)
```

### Clojure Configuration

The server starts automatically when the MCP server starts (in `server.clj`):

```clojure
(defn start!
  [& _args]
  ;; ... other initialization ...
  
  ;; Start bidirectional channel server
  (try
    (channel/start-server! {:type :tcp :port 9999})
    (log/info "Channel server started on TCP port 9999")
    (catch Exception e
      (log/warn "Channel server failed to start:" (.getMessage e))))
  
  ;; ... rest of startup ...
  )
```

## Debugging

### Check Server Status (Clojure)

```clojure
(let [state @(var-get #'emacs-mcp.channel/server-state)]
  {:running @(:running state)
   :client-count (count @(:clients state))
   :port (:port state)})
```

### Check Connection (Emacs)

```elisp
(list :connected (emacs-mcp-channel-connected-p)
      :process emacs-mcp-channel--process
      :recent-events (length emacs-mcp-channel--recent-events))
```

### Check Port Binding (Shell)

```bash
ss -tlnp | grep 9999
```

### View Recent Events (Emacs)

```elisp
(emacs-mcp-channel-get-recent-events 5)
;; Returns list of (TIMESTAMP . EVENT-ALIST)
```

## Common Issues

### "Address already in use"

The previous server socket wasn't properly closed. Either:
1. Wait for OS to release the port (TIME_WAIT)
2. Kill the Java process
3. Use a different port

### Messages Not Received

1. Check connection: `(emacs-mcp-channel-connected-p)`
2. Check handlers: `emacs-mcp-channel--handlers`
3. Check buffer: `emacs-mcp-channel--buffer`
4. Check decoder: Test with `(emacs-mcp-channel--decode "d4:type4:teste")`

### Broken Pipe Errors

Stale client connections. The server continues working; broken clients are logged but don't affect healthy connections.

## Protocol Reference

### Standard Event Types

| Type | Direction | Purpose |
|------|-----------|---------|
| `task-completed` | Clj→Emacs | Swarm task finished |
| `task-failed` | Clj→Emacs | Swarm task error |
| `prompt-shown` | Clj→Emacs | Slave needs input |
| `slave-spawned` | Clj→Emacs | New slave created |
| `slave-killed` | Clj→Emacs | Slave terminated |
| `hivemind-hello` | Clj→Emacs | Server greeting |
| `emacs-ping` | Emacs→Clj | Client ping |

### Message Structure

All messages MUST have a `type` field:

```clojure
{:type "event-name"    ;; Required: string
 :timestamp 123456     ;; Optional: epoch millis
 :message "..."        ;; Optional: human-readable
 ;; ... additional fields as needed
 }
```

## Future Enhancements

1. **Encryption**: TLS support for production deployments
2. **Authentication**: Token-based client authentication
3. **Compression**: For large message payloads
4. **Clustering**: Multiple server instances with message routing
5. **Persistence**: Message queue for offline clients

## Files

| File | Purpose |
|------|---------|
| `src/emacs_mcp/channel.clj` | Clojure server implementation |
| `elisp/emacs-mcp-channel.el` | Emacs client implementation |
| `elisp/addons/emacs-mcp-swarm.el` | Swarm integration (uses channel) |
| `test/emacs_mcp/channel_test.clj` | Clojure tests |

## Version History

- **v0.3.0** (2026-01-02): Initial implementation
  - IChannel protocol
  - Unix socket and TCP transports
  - Bencode message format
  - core.async event bus

- **v0.3.1** (2026-01-03): Integration fixes
  - Fixed decoder availability check
  - Added auto-start in server.clj
  - Added event history storage
  - Fixed stop-server! socket cleanup

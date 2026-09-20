(ns hive-mcp.server.transport.olympus-ws
  "Olympus WebSocket server startup (Olympus Web UI).

   Single responsibility: start Olympus WS server and wire hivemind events."
  (:require [hive-mcp.dns.result :as result]
            [hive-mcp.swarm.adapters.soft :as soft]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn start-olympus-ws!
  "Start the Olympus WebSocket server for the Olympus Web UI (port 7911).
   Sends a full snapshot on connect and supports the typed event protocol.

   The transport itself is a hive-emacs extraction target, so it is resolved
   BY SYMBOL: with it present this starts exactly what it always did, and
   without it there is no Olympus UI to serve and boot says so instead of
   failing."
  []
  (result/rescue nil
                 (if-let [start! (soft/resolve-soft 'hive-mcp.transport.olympus/start!)]
                   (do (start!)
                       (when-let [wire! (soft/resolve-soft 'hive-mcp.transport.olympus/wire-hivemind-events!)]
                         (wire!))
                       (log/info "Olympus WebSocket server started on port 7911"))
                   (log/debug "no Olympus transport in this build; skipping WS startup"))))

(ns hive-mcp.agent.context
  "DEPRECATED alias: moved to hive-mcp.context.request. Kept only for
   hive-knowledge, which soft-resolves these two symbols by name."
  (:require [hive-mcp.context.request :as request]))

(def ^:deprecated current-caller-id request/current-caller-id)
(def ^:deprecated current-agent-id request/current-agent-id)

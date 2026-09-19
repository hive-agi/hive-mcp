(ns hive-mcp.tools.swarm.prompt
  "Swarm prompt handlers for human-in-the-loop prompt management and lazy preset headers."
  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-spi.editor.services :as svc]
            [hive-mcp.channel.core :as channel]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- emit-prompt-pending-events!
  "Emit :ling/prompt-pending events for each pending prompt."
  [prompts-data]
  (try
    (let [parsed (if (string? prompts-data)
                   (json/read-str prompts-data :key-fn keyword)
                   prompts-data)
          prompts (get parsed :prompts [])]
      (doseq [prompt prompts]
        (let [slave-id (get prompt :slave-id)
              prompt-text (get prompt :prompt "")
              timestamp (get prompt :timestamp)]
          (when slave-id
            (log/debug "Emitting :ling/prompt-pending for" slave-id)
            (channel/emit-event! :ling/prompt-pending
                                 {:slave-id slave-id
                                  :prompt-preview (subs prompt-text 0 (min 100 (count prompt-text)))
                                  :pending-since timestamp})))))
    (catch Exception e
      (log/warn "Failed to emit prompt-pending events:" (ex-message e)))))

(defn handle-swarm-pending-prompts
  "Get list of pending prompts awaiting human decision."
  [_]
  (core/with-swarm
    (let [{:keys [success result error timed-out]}
          (svc/invoke :vessel :dispatch {:op :swarm/pending-prompts} 5000)]
      (cond
        timed-out
        (core/mcp-timeout-error "Pending prompts check")

        success
        (do
          (emit-prompt-pending-events! result)
          (core/mcp-success result))

        :else
        (core/mcp-error (str "Error: " error))))))

(defn handle-swarm-respond-prompt
  "Send a response to a pending prompt from a specific slave."
  [{:keys [slave_id response]}]
  (core/with-swarm
    (let [{:keys [success result error timed-out]}
          (svc/invoke :vessel :dispatch {:op :swarm/respond-prompt, :slave-id slave_id, :response response} 5000)]
      (cond
        timed-out
        (core/mcp-timeout-error "Respond prompt" :extra-data {:slave_id slave_id})

        success
        (core/mcp-success result)

        :else
        (core/mcp-error (str "Error: " error))))))

(def lazy-instructions
  "Static instruction template for lazy preset loading headers."
  (str
   "## Assigned Presets\n\n"
   "You have access to: **%s**\n\n"

   "### IMMEDIATE: Fetch at Session Start\n\n"
   "**Before starting work**, fetch your assigned presets:\n"
   "```\n"
   "%s\n"
   "```\n\n"

   "### Quick Summary (Lower Tokens)\n\n"
   "For orientation without full content (~200 tokens vs ~1500):\n"
   "```\n"
   "preset(command: \"core\", name: \"<preset-name>\")\n"
   "```\n\n"

   "### Discovery\n\n"
   "Find presets by topic:\n"
   "```\n"
   "preset(command: \"search\", query: \"testing patterns\")\n"
   "preset(command: \"list_slim\")  ; Names + categories only\n"
   "```\n\n"

   "### When to Fetch\n\n"
   "- Session start: fetch assigned presets\n"
   "- Unfamiliar task: search for relevant presets\n"
   "- Need guidance: use `core` for quick summary\n"))

(defn build-lazy-preset-header
  "Generate lightweight system prompt header with preset names and fetch instructions."
  [preset-names]
  (when (seq preset-names)
    (let [names-str (str/join ", " preset-names)
          fetch-cmds (str/join "\n" (map #(str "preset(command: \"get\", name: \"" % "\")") preset-names))]
      (format lazy-instructions names-str fetch-cmds))))

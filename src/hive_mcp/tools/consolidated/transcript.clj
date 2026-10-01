(ns hive-mcp.tools.consolidated.transcript
  "MCP transcript supertool: query agent conversation transcripts.

   Commands: help, list, query, tail, since, stats, replay, report.

   Entries carry a 120-char `preview` by default; `full true` returns each
   entry's whole `content`, `max_chars N` returns up to N chars of it. Every
   entry names its tool calls (`tool_calls`) and is marked `empty true` when
   it has neither text nor tool calls. `report` is what a coordinator reads
   when a ling finishes: its last assistant text in full, turn count, how the
   transcript ends, and the last tool result.

   Reads go through the `TranscriptSource` port
   (hive-mcp.agent.transcript-source), which covers legacy JSONL files and
   the Datalevin stores headless hive-agent lings write. Params are coerced
   at this boundary (strings and numbers alike), and every failure, thrown
   or returned, leaves as an MCP error envelope."
  (:require [hive-mcp.agent.transcript-query :as tq]
            [hive-mcp.agent.transcript-source :as src]
            [hive-dsl.adt :refer [adt-case]]
            [hive-dsl.result :as r]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive-mcp.tools.core :as tcore]))

;; =============================================================================
;; Query Execution
;; =============================================================================

(declare final-report)

(defn- execute-query
  "Run a TranscriptQuery against `source`. Returns Result<vector<entry>>."
  [source query]
  (adt-case tq/TranscriptQuery query
    :query/by-agent (src/read-entries source (:agent-id query))
    :query/by-time  (r/err :transcript/not-implemented
                           {:message "time-range query is not implemented"})
    :query/since    (r/map-ok (src/read-entries source (:agent-id query))
                              (fn [es] (filterv #(> (src/entry-turn %) (:turn query)) es)))
    :query/tail     (r/map-ok (src/read-entries source (:agent-id query))
                              (fn [es] (vec (take-last (:n query) es))))
    :query/report   (r/map-ok (src/read-entries source (:agent-id query))
                              #(final-report (:agent-id query) %))))

(defn- err-message
  "Human text for an err Result."
  [res]
  (or (:message res) (pr-str res)))

;; =============================================================================
;; Response Formatting
;; =============================================================================

(defn- entry-role [e]
  (or (:role e) (some-> (:transcript/role e) name)))

(defn- entry-content [e]
  (str (or (:content e) (:transcript/content e) "")))

(def ^:private preview-chars 120)

(defn- clip [s n]
  (subs s 0 (min (long n) (count s))))

(defn- entry-turn [e]
  (or (:turn e) (:transcript/turn e)))

(defn- tool-call-name
  "Name of one tool call in either shape: hive-agent's stored
   {:tool-call/name}, or an OpenAI-style {:function {:name}} / {:name}."
  [tc]
  (or (:tool-call/name tc) (get-in tc [:function :name]) (:name tc) "unknown"))

(defn- entry-tool-calls
  "Raw tool-call maps of an entry (Datalevin or JSONL shape)."
  [e]
  (or (seq (:transcript/tool-calls e)) (seq (:tool_calls e)) (seq (:tool-calls e))))

(defn- entry-tool-names [e]
  (mapv tool-call-name (entry-tool-calls e)))

(defn- entry-empty?
  "True when an entry carries neither text nor tool calls."
  [e]
  (and (str/blank? (entry-content e)) (empty? (entry-tool-calls e))))

(defn- format-entry-compact
  "One entry for list responses. `mode` is {:full? bool :max-chars int?}:
   the default gives a 120-char `preview`; `:full?` gives the whole
   `content`; `:max-chars` gives up to that many chars of it (with
   `truncated true` when it was cut). Tool-call names and an `empty`
   flag are added whenever they apply."
  ([entry] (format-entry-compact entry {}))
  ([entry {:keys [full? max-chars]}]
   (let [content (entry-content entry)
         names   (entry-tool-names entry)
         base    {:role (entry-role entry) :turn (entry-turn entry)}]
     (cond-> (cond
               full?     (assoc base :content content)
               max-chars (cond-> (assoc base :content (clip content max-chars))
                           (> (count content) max-chars) (assoc :truncated true))
               :else     (assoc base :preview (clip content preview-chars)))
       (seq names)          (assoc :tool_calls names)
       (entry-empty? entry) (assoc :empty true)))))

(defn- assistant? [e] (= "assistant" (entry-role e)))

(defn- llm-error? [e]
  (and (= "system" (entry-role e))
       (str/starts-with? (entry-content e) "LLM error")))

(defn- last-tool-result
  "{:turn :tool :preview} of the newest tool output: a `tool` role entry,
   or a stored tool-call result, whichever comes last."
  [entries]
  (some (fn [e]
          (or (when (= "tool" (entry-role e))
                {:entry e :tool nil :content (entry-content e)})
              (when-let [tc (some #(when-not (str/blank? (str (:tool-call/result %))) %)
                                  (reverse (entry-tool-calls e)))]
                {:entry e :tool (tool-call-name tc) :content (str (:tool-call/result tc))})))
        (rseq (vec entries))))

(defn- ending
  "How the transcript ends, read from its last entries: `error` (an LLM
   error was recorded last), `text` (the last assistant turn wrote text and
   called no tool: a finished report), `tool-calls` (the last assistant turn
   only called tools: the run was cut off mid-work), or `unknown`."
  [entries]
  (let [lst  (peek (vec entries))
        asst (some #(when (assistant? %) %) (rseq (vec entries)))]
    (cond
      (nil? lst)                               "unknown"
      (llm-error? lst)                         "error"
      (nil? asst)                              "unknown"
      (seq (entry-tool-calls asst))            "tool-calls"
      (not (str/blank? (entry-content asst)))  "text"
      :else                                    "unknown")))

(defn final-report
  "Pure: the final report of a ling read from its transcript entries.

   :final-text is the newest NON-BLANK assistant text in full (a closing
   turn that wrote nothing does not hide the report before it). The
   transcript store records no exit variant, so :outcome is absent here;
   :ending is what the entries themselves show."
  [agent-id entries]
  (let [entries (vec entries)
        final   (some #(when (and (assistant? %) (not (str/blank? (entry-content %)))) %)
                      (rseq entries))
        tool    (last-tool-result entries)
        cost    (keep #(or (:cost_usd %) (:transcript/cost-usd %)) entries)]
    (cond-> {:agent-id   agent-id
             :final-text (some-> final entry-content)
             :final-turn (some-> final entry-turn)
             :turns      (apply max 0 (keep entry-turn entries))
             :entries    (count entries)
             :ending     (ending entries)}
      tool (assoc :last-tool-result
                  (cond-> {:turn    (entry-turn (:entry tool))
                           :preview (clip (:content tool) preview-chars)}
                    (:tool tool) (assoc :tool (:tool tool))))
      (seq cost) (assoc :total-cost (reduce + 0.0 cost)))))

(defn- format-replay
  "Format entries as markdown conversation."
  [entries agent-id]
  (let [header (format "## Transcript: %s (%d entries)\n\n" agent-id (count entries))]
    (->> entries
         (map #(format "**[%s]** %s\n" (or (entry-role %) "?") (entry-content %)))
         (str/join "\n")
         (str header))))

(def ^:private cost-note
  (str "total-cost sums the cost-usd stored on each entry. hive-agent's bb loop "
       "(hive-agent.loop.bb-agentic/record-entry!) writes 0.0 there and never meters "
       "usage, so headless lings read 0.0 here whatever they spent."))

(defn- compute-stats
  "Compute transcript statistics from entries."
  [entries agent-id]
  {:agent-id   agent-id
   :total      (count entries)
   :by-role    (frequencies (map entry-role entries))
   :turns      (apply max 0 (keep #(or (:turn %) (:transcript/turn %)) entries))
   ;; hive-agent's bb loop records :cost-usd 0.0 on every entry, so this sum
   ;; is 0.0 for headless lings whatever their model; see cost-note.
   :total-cost (reduce + 0.0 (keep #(or (:cost_usd %) (:transcript/cost-usd %)) entries))
   :cost-note  cost-note})

(defn- entries-response
  ([agent-id entries] (entries-response agent-id entries {}))
  ([agent-id entries mode]
   (tcore/mcp-json {:entries  (mapv #(format-entry-compact % mode) entries)
                    :count    (count entries)
                    :agent-id agent-id})))

;; =============================================================================
;; Param coercion
;; =============================================================================

(defn- with-int-param
  "Coerce an integer MCP param, then call `f` with it.
   A number or numeric string becomes an int, nil takes `default`, anything
   else is an MCP error naming the param."
  [value param-name default f]
  (let [{:keys [ok error]} (tcore/coerce-int value param-name default)]
    (if error
      (tcore/mcp-error error)
      (f (int ok)))))

(defn- truthy? [v]
  (or (true? v) (and (string? v) (contains? #{"true" "1" "yes"} (str/lower-case (str/trim v))))))

(defn- with-content-mode
  "Coerce `full` and `max_chars` into {:full? :max-chars}, then call `f`.
   A bad `max_chars` is an MCP error naming it."
  [full max-chars f]
  (if (nil? max-chars)
    (f {:full? (truthy? full)})
    (let [{:keys [ok error]} (tcore/coerce-int max-chars :max_chars nil)]
      (cond
        error         (tcore/mcp-error error)
        (not (pos? ok)) (tcore/mcp-error "transcript: `max_chars` must be a positive integer")
        :else         (f {:full? (truthy? full) :max-chars (long ok)})))))

(defn- with-agent-id
  "Call `f` with a non-blank agent id string, else an MCP error naming agent_id."
  [agent-id f]
  (let [s (some-> agent-id str str/trim)]
    (if (str/blank? s)
      (tcore/mcp-error "transcript: `agent_id` is required")
      (f s))))

(defn- run-query
  "Execute `query` and render the ok value with `render`, or an MCP error."
  [source query render]
  (let [res (execute-query source query)]
    (if (r/ok? res)
      (render (:ok res))
      (tcore/mcp-error (err-message res)))))

;; =============================================================================
;; MCP Command Router
;; =============================================================================

(def ^:private content-params
  "Params that choose how much of each entry's text comes back."
  ["full" "max_chars"])

(def ^:private help-response
  {:tool     "transcript"
   :entry-shape (str "Each entry: role, turn, preview (first 120 chars) or content (with full/max_chars), "
                     "tool_calls [names] when it called tools, empty true when it has neither text nor tool calls.")
   :options  {"full"      "true: return each entry's whole content instead of the 120-char preview"
              "max_chars" "N: return up to N chars of content (truncated true when cut)"}
   :commands [{:command "list"   :params []                  :description "Available transcripts (JSONL + Datalevin)"}
              {:command "query"  :params (into ["agent_id"] content-params)        :description "Full conversation entries"}
              {:command "tail"   :params (into ["agent_id" "n"] content-params)    :description "Last N entries (default 10)"}
              {:command "since"  :params (into ["agent_id" "turn"] content-params) :description "Entries after turn"}
              {:command "report" :params ["agent_id"]        :description "A finished ling's final report: last assistant text in full, turns, ending (text|tool-calls|error|unknown), last tool result preview"}
              {:command "stats"  :params ["agent_id"]        :description "Turn count, cost, role breakdown"}
              {:command "replay" :params ["agent_id"]        :description "Formatted markdown conversation"}]})

(defn- list-response [source]
  (let [res (src/list-transcripts source)]
    (if-not (r/ok? res)
      (tcore/mcp-error (err-message res))
      (let [all (->> (:ok res)
                     (group-by :agent-id)
                     (map (fn [[_ rows]]
                            (assoc (apply max-key #(or (:modified %) 0) rows)
                                   :sources (vec (distinct (map :source rows))))))
                     (sort-by #(or (:modified %) 0) >)
                     vec)]
        (tcore/mcp-json {:transcripts all :count (count all)})))))

(defn- dispatch [source {:keys [command agent-id agent_id id n turn full max_chars max-chars]}]
  (let [agent-id  (or agent-id agent_id id)
        max-chars (or max_chars max-chars)
        by-agent  (fn [render]
                    (with-agent-id agent-id
                      (fn [aid]
                        (run-query source (tq/transcript-query :query/by-agent {:agent-id aid})
                                   #(render aid %)))))
        listing   (fn [f]
                    (with-content-mode full max-chars
                      (fn [mode] (f #(entries-response %1 %2 mode)))))]
    (case command
      "help"   (tcore/mcp-json help-response)
      "list"   (list-response source)
      "query"  (listing by-agent)
      "stats"  (by-agent #(tcore/mcp-json (compute-stats %2 %1)))
      "replay" (by-agent #(tcore/mcp-json {:markdown (format-replay %2 %1) :agent-id %1}))
      ("report" "final")
      (with-agent-id agent-id
        (fn [aid]
          (run-query source (tq/transcript-query :query/report {:agent-id aid})
                     tcore/mcp-json)))
      "tail"   (listing
                (fn [render]
                  (with-agent-id agent-id
                    (fn [aid]
                      (with-int-param n :n 10
                        (fn [n]
                          (run-query source (tq/transcript-query :query/tail {:agent-id aid :n n})
                                     #(render aid %))))))))
      "since"  (listing
                (fn [render]
                  (with-agent-id agent-id
                    (fn [aid]
                      (with-int-param turn :turn 0
                        (fn [turn]
                          (run-query source (tq/transcript-query :query/since {:agent-id aid :turn turn})
                                     #(render aid %))))))))
      (tcore/mcp-error (str "Unknown transcript command: " command)))))

(defn handle-transcript
  "Route transcript MCP commands. Returns an MCP-envelope result, never throws.

   ([params])        reads `src/default-source`.
   ([source params]) reads the given TranscriptSource.

   Commands:
     help                     This command list
     list                     Available transcripts (JSONL + Datalevin)
     query  {:agent-id}       Full conversation entries
     tail   {:agent-id :n?}   Last N entries (default 10)
     since  {:agent-id :turn} Entries after turn
            query/tail/since also take :full (whole content) and
            :max_chars N (content up to N chars); default is a preview.
     report {:agent-id}       Final assistant text in full, turns, ending,
                              last tool result (alias: final)
     stats  {:agent-id}       Turn count, cost, role breakdown
     replay {:agent-id}       Formatted markdown conversation"
  ([params] (handle-transcript (src/default-source) params))
  ([source params]
   (try
     (dispatch source params)
     (catch Throwable e
       (log/warn e "[transcript] command failed" (:command params))
       (tcore/mcp-error (str "transcript " (:command params) " failed: " (ex-message e)))))))

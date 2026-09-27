(ns hive-mcp.tools.consolidated.transcript
  "MCP transcript supertool: query agent conversation transcripts.

   Commands: help, list, query, tail, since, stats, replay.

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
                              (fn [es] (vec (take-last (:n query) es))))))

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

(defn- format-entry-compact
  "Compact entry for list responses."
  [entry]
  (let [content (entry-content entry)]
    {:role    (entry-role entry)
     :turn    (or (:turn entry) (:transcript/turn entry))
     :preview (subs content 0 (min 120 (count content)))}))

(defn- format-replay
  "Format entries as markdown conversation."
  [entries agent-id]
  (let [header (format "## Transcript: %s (%d entries)\n\n" agent-id (count entries))]
    (->> entries
         (map #(format "**[%s]** %s\n" (or (entry-role %) "?") (entry-content %)))
         (str/join "\n")
         (str header))))

(defn- compute-stats
  "Compute transcript statistics from entries."
  [entries agent-id]
  {:agent-id   agent-id
   :total      (count entries)
   :by-role    (frequencies (map entry-role entries))
   :turns      (apply max 0 (keep #(or (:turn %) (:transcript/turn %)) entries))
   :total-cost (reduce + 0.0 (keep #(or (:cost_usd %) (:transcript/cost-usd %)) entries))})

(defn- entries-response [agent-id entries]
  (tcore/mcp-json {:entries  (mapv format-entry-compact entries)
                   :count    (count entries)
                   :agent-id agent-id}))

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

(def ^:private help-response
  {:tool     "transcript"
   :commands [{:command "list"   :params []                  :description "Available transcripts (JSONL + Datalevin)"}
              {:command "query"  :params ["agent_id"]        :description "Full conversation entries"}
              {:command "tail"   :params ["agent_id" "n"]    :description "Last N entries (default 10)"}
              {:command "since"  :params ["agent_id" "turn"] :description "Entries after turn"}
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

(defn- dispatch [source {:keys [command agent-id agent_id id n turn]}]
  (let [agent-id (or agent-id agent_id id)
        by-agent (fn [render]
                   (with-agent-id agent-id
                     (fn [aid]
                       (run-query source (tq/transcript-query :query/by-agent {:agent-id aid})
                                  #(render aid %)))))]
    (case command
      "help"   (tcore/mcp-json help-response)
      "list"   (list-response source)
      "query"  (by-agent entries-response)
      "stats"  (by-agent #(tcore/mcp-json (compute-stats %2 %1)))
      "replay" (by-agent #(tcore/mcp-json {:markdown (format-replay %2 %1) :agent-id %1}))
      "tail"   (with-agent-id agent-id
                 (fn [aid]
                   (with-int-param n :n 10
                     (fn [n]
                       (run-query source (tq/transcript-query :query/tail {:agent-id aid :n n})
                                  #(entries-response aid %))))))
      "since"  (with-agent-id agent-id
                 (fn [aid]
                   (with-int-param turn :turn 0
                     (fn [turn]
                       (run-query source (tq/transcript-query :query/since {:agent-id aid :turn turn})
                                  #(entries-response aid %))))))
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
     stats  {:agent-id}       Turn count, cost, role breakdown
     replay {:agent-id}       Formatted markdown conversation"
  ([params] (handle-transcript (src/default-source) params))
  ([source params]
   (try
     (dispatch source params)
     (catch Throwable e
       (log/warn e "[transcript] command failed" (:command params))
       (tcore/mcp-error (str "transcript " (:command params) " failed: " (ex-message e)))))))

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
            [hive-mcp.agent.transcript-insight :as ti]
            [hive-mcp.agent.transcript-source :as src]
            [hive-dsl.adt :refer [adt-case]]
            [hive-dsl.result :as r]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive-mcp.tools.core :as tcore]))

;; =============================================================================
;; Query Execution
;; =============================================================================

(declare run-find)

(defn- execute-query
  "Run a TranscriptQuery against `source`. Returns a Result. `ctx` carries
   what the cross-run variants need: {:selection [listing rows] :opts {...}}."
  ([source query] (execute-query source query {}))
  ([source query ctx]
  (adt-case tq/TranscriptQuery query
    :query/by-agent (src/read-entries source (:agent-id query))
    :query/by-time  (r/err :transcript/not-implemented
                           {:message "time-range query is not implemented"})
    :query/since    (r/map-ok (src/read-entries source (:agent-id query))
                              (fn [es] (filterv #(> (src/entry-turn %) (:turn query)) es)))
    :query/tail     (r/map-ok (src/read-entries source (:agent-id query))
                              (fn [es] (vec (take-last (:n query) es))))
    :query/report   (r/map-ok (src/read-entries source (:agent-id query))
                              #(ti/final-report (:agent-id query) %))
    :query/digest   (r/map-ok (src/read-entries source (:agent-id query))
                              #(ti/digest (:agent-id query) %))
    :query/find     (run-find source (:query query) ctx))))

(defn- err-message
  "Human text for an err Result."
  [res]
  (or (:message res) (pr-str res)))

;; =============================================================================
;; Response Formatting
;; =============================================================================

(def ^:private entry-role ti/entry-role)
(def ^:private entry-content ti/entry-content)
(def ^:private preview-chars ti/preview-chars)
(def ^:private clip ti/clip)
(def ^:private entry-turn ti/entry-turn)
(def ^:private entry-tool-names ti/entry-tool-names)
(def ^:private entry-empty? ti/entry-empty?)

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

(def final-report
  "Pure: the final report of a ling read from its transcript entries.
   See hive-mcp.agent.transcript-insight/final-report."
  ti/final-report)

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
  ([source query render] (run-query source query {} render))
  ([source query ctx render]
  (let [res (execute-query source query ctx)]
    (if (r/ok? res)
      (render (:ok res))
      (tcore/mcp-error (err-message res))))))

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
   :filters  {"agent"   "agent id prefix, or a glob with * / ?"
              "project" "project id (exact)"
              "parent"  "spawning coordinator/ling id (exact)"
              "since"   "30m, 2h, 1d, or an ISO-8601 instant"
              "limit"   "max rows/hits (default 25)"}
   :commands [{:command "list"   :params ["agent" "project" "parent" "since" "limit"]
               :description "Runs, newest first: agent project parent turns modified ending (matched = total before limit)"}
              {:command "find"   :params ["query" "role" "tool" "agent" "project" "parent" "since" "limit" "runs"]
               :description "Cross-run search. query: substring (case-insensitive) or /regex/[i]; role assistant|tool|user; tool: tool name; runs: newest runs scanned (default 50). Hits: agent run turn role tool snippet"}
              {:command "digest" :params ["agent" "parent" "project" "since" "limit"]
               :description "What a ling did: tools, files written, shell commands by class, commits, test runs, errors, idle gap, final report. With parent or an agent glob: one compact row per ling"}
              {:command "query"  :params (into ["agent_id"] content-params)        :description "Full conversation entries"}
              {:command "tail"   :params (into ["agent_id" "n"] content-params)    :description "Last N entries (default 10)"}
              {:command "since"  :params (into ["agent_id" "turn"] content-params) :description "Entries after turn"}
              {:command "report" :params ["agent_id"]        :description "A finished ling's final report: last assistant text in full, turns, ending (text|tool-calls|error|unknown), last tool result preview"}
              {:command "stats"  :params ["agent_id"]        :description "Turn count, cost, role breakdown"}
              {:command "replay" :params ["agent_id"]        :description "Formatted markdown conversation"}]})

;; =============================================================================
;; Selection: list filters (boundary: one listing read + parent lookups)
;; =============================================================================

(def ^:private default-list-limit 25)
(def ^:private default-find-runs 50)

(defn- with-limit
  "Coerce `limit` (default `default`) to a positive int, then call `f`."
  [value param default f]
  (with-int-param value param default
    (fn [n] (if (pos? n) (f n) (tcore/mcp-error (str "transcript: `" (name param) "` must be positive"))))))

(defn- with-filters
  "Coerce the listing filters into ti/select-listing opts, then call `f`.
   A bad `since` is an MCP error naming it."
  [{:keys [agent agent_id id project project_id parent since]} now-ms f]
  (let [since-ms (ti/parse-since since now-ms)]
    (if (:error since-ms)
      (tcore/mcp-error (:error since-ms))
      (f {:agent    (or agent agent_id id)
          :project  (some-> (or project project_id) str str/trim not-empty)
          :parent   (some-> parent str str/trim not-empty)
          :since-ms since-ms}))))

(defn- select-runs
  "Result<[row]>: the collapsed listing, parent-annotated when a parent
   filter asks for it, filtered and capped by `opts`."
  [source parents opts]
  (r/map-ok (src/list-transcripts source)
            (fn [rows]
              (let [rows (ti/collapse-listing rows)
                    rows (if (:parent opts)
                           (map #(assoc % :parent (src/parent-of parents (:agent-id %))) rows)
                           rows)]
                (ti/select-listing rows opts)))))

(defn- run-summary
  "{:turns :ending} of one run, read through `source`; {} when unreadable."
  [source agent-id]
  (let [res (src/read-entries source agent-id)]
    (if (r/ok? res)
      {:turns (ti/max-turn (:ok res)) :ending (ti/ending (:ok res))}
      {})))

(defn- list-row
  "Wire projection of one listing row: strings and numbers only."
  [source parents row]
  (let [{:keys [turns ending]} (run-summary source (:agent-id row))]
    {:agent    (:agent-id row)
     :project  (:project-id row)
     :parent   (or (:parent row) (src/parent-of parents (:agent-id row)))
     :turns    turns
     :modified (ti/iso (:modified row))
     :ending   ending}))

(defn- list-response [source parents params now-ms]
  (with-filters params now-ms
    (fn [opts]
      (with-limit (:limit params) :limit default-list-limit
        (fn [limit]
          (let [res (select-runs source parents opts)]
            (if-not (r/ok? res)
              (tcore/mcp-error (err-message res))
              (let [matched (:ok res)
                    page    (take limit matched)]
                (tcore/mcp-json {:transcripts (mapv #(list-row source parents %) page)
                                 :count       (count page)
                                 :matched     (count matched)})))))))))

;; =============================================================================
;; find: cross-run full-text search
;; =============================================================================

(defn- run-find
  "Result<{:hits :runs-scanned}>: `needle-text` searched across the runs of
   `(:selection ctx)`, newest first, stopping at `(:limit ctx)` hits."
  [source needle-text {:keys [selection limit role tool]}]
  (let [needle (ti/compile-needle needle-text)]
    (if (:error needle)
      (r/err :transcript/bad-query {:message (:error needle)})
      (let [[hits scanned]
            (reduce (fn [[hits n] row]
                      (if (>= (count hits) limit)
                        (reduced [hits n])
                        (let [res (src/read-entries source (:agent-id row))
                              es  (if (r/ok? res) (:ok res) [])
                              found (ti/find-hits {:agent (:agent-id row) :run (:project-id row)}
                                                  es needle {:role role :tool tool})]
                          [(into hits (take (- limit (count hits))) found) (inc n)])))
                    [[] 0]
                    selection)]
        (r/ok {:hits hits :count (count hits) :runs-scanned scanned})))))

(defn- find-response [source parents params now-ms]
  (with-filters params now-ms
    (fn [opts]
      (with-limit (:limit params) :limit default-list-limit
        (fn [limit]
          (with-limit (:runs params) :runs default-find-runs
            (fn [runs]
              (let [sel (select-runs source parents (assoc opts :limit runs))]
                (if-not (r/ok? sel)
                  (tcore/mcp-error (err-message sel))
                  (run-query source
                             (tq/transcript-query :query/find {:query (str (:query params))})
                             {:selection (:ok sel) :limit limit
                              :role (some-> (:role params) str str/trim not-empty)
                              :tool (some-> (:tool params) str str/trim not-empty)}
                             tcore/mcp-json))))))))))

;; =============================================================================
;; digest: what a ling did
;; =============================================================================

(def ^:private shell-cap 40)

(defn- digest-projection
  "Wire projection of one full digest: timestamps as ISO, shell list capped."
  [d]
  (let [shell (:shell d)]
    (cond-> (-> d
                (dissoc :started :ended)
                (assoc :shell (mapv #(update % :command ti/clip 200) (take-last shell-cap shell))))
      (> (count shell) shell-cap) (assoc :shell-omitted (- (count shell) shell-cap))
      (:started d) (assoc :started (ti/iso (:started d)) :ended (ti/iso (:ended d))))))

(defn- multi-digest?
  "A digest over many lings: a parent is given, or the agent is a glob."
  [{:keys [parent agent agent_id id]}]
  (or (not (str/blank? (str parent)))
      (boolean (re-find #"[*?]" (str (or agent agent_id id))))))

(defn- digest-response [source parents params now-ms]
  (if-not (multi-digest? params)
    (with-agent-id (or (:agent params) (:agent_id params) (:id params))
      (fn [aid]
        (run-query source (tq/transcript-query :query/digest {:agent-id aid})
                   (comp tcore/mcp-json digest-projection))))
    (with-filters params now-ms
      (fn [opts]
        (with-limit (:limit params) :limit default-list-limit
          (fn [limit]
            (let [sel (select-runs source parents (assoc opts :limit limit))]
              (if-not (r/ok? sel)
                (tcore/mcp-error (err-message sel))
                (let [rows (keep (fn [row]
                                   (let [res (execute-query source
                                                            (tq/transcript-query :query/digest
                                                                                 {:agent-id (:agent-id row)}))]
                                     (when (r/ok? res)
                                       (assoc (ti/digest-row (:ok res)) :project (:project-id row)))))
                                 (:ok sel))]
                  (tcore/mcp-json {:lings (vec rows) :count (count rows)}))))))))))

(defn- dispatch [{:keys [source parents now-ms]}
                 {:keys [command agent-id agent_id id n turn full max_chars max-chars] :as params}]
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
      "list"   (list-response source parents params (now-ms))
      ("find" "search") (find-response source parents params (now-ms))
      "digest" (digest-response source parents params (now-ms))
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
   ([ports params])  `ports` is a TranscriptSource, or a map
                     {:source TranscriptSource :parents ParentIndex
                      :now-ms (fn [] epoch-ms)}; absent ports take defaults.

   Commands:
     help                     This command list
     list   {:agent :project :parent :since :limit}
                              Runs newest first (default limit 25), compact
                              rows: agent project parent turns modified ending
     find   {:query :role :tool :agent :project :parent :since :limit :runs}
                              Cross-run search; query is a substring or /re/
     digest {:agent | :parent | agent glob}
                              What a ling did; one compact row per ling when
                              many are selected
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
  ([ports params]
   ;; ports: {:source TranscriptSource :parents ParentIndex :now-ms (fn [])}
   (try
     (dispatch (merge {:parents (src/default-parent-index)
                       :now-ms  #(System/currentTimeMillis)}
                      (if (satisfies? src/TranscriptSource ports) {:source ports} ports))
               params)
     (catch Throwable e
       (log/warn e "[transcript] command failed" (:command params))
       (tcore/mcp-error (str "transcript " (:command params) " failed: " (ex-message e)))))))

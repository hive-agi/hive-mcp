(ns hive-mcp.agent.transcript-insight
  "Pure folds over agent transcripts: what a ling did, read fast.

   Nothing here performs I/O. Every fn takes data (listing rows, entries,
   an injected `now`) and returns data; the transcript tool reads stores at
   its boundary and projects these values onto the wire.

   Layers, bottom up:
     entry accessors   role/content/turn/timestamp/tool calls in either the
                       JSONL or the hive-agent Datalevin shape
     listing filters   agent glob, project, parent, since, newest-first, limit
     matcher           substring or /regex/ needle, role and tool filters,
                       snippets around each hit
     final report      the ling's closing text, ending, last tool result
     digest            one fold over a ling's entries: tools, files, shell
                       commands, commits, test outcomes, errors, idle gap"
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Entry accessors (JSONL and hive-agent Datalevin shapes)
;; =============================================================================

(defn entry-role [e]
  (or (:role e) (some-> (:transcript/role e) name)))

(defn entry-content [e]
  (str (or (:content e) (:transcript/content e) "")))

(defn entry-turn [e]
  (or (:turn e) (:transcript/turn e)))

(defn entry-timestamp
  "Epoch ms of an entry, or nil when it carries none (or not as a number)."
  [e]
  (let [t (or (:transcript/timestamp e) (:timestamp e))]
    (when (number? t) (long t))))

(defn tool-call-name
  "Name of one tool call in either shape: hive-agent's stored
   {:tool-call/name}, or an OpenAI-style {:function {:name}} / {:name}."
  [tc]
  (or (:tool-call/name tc) (get-in tc [:function :name]) (:name tc) "unknown"))

(defn tool-call-arguments
  "Raw argument string of a tool call (JSON text), or \"\"."
  [tc]
  (let [a (or (:tool-call/arguments tc) (get-in tc [:function :arguments]) (:arguments tc))]
    (cond (string? a) a
          (nil? a)    ""
          :else       (json/write-str a))))

(defn tool-call-result [tc]
  (str (or (:tool-call/result tc) (:result tc) "")))

(defn entry-tool-calls
  "Raw tool-call maps of an entry (Datalevin or JSONL shape)."
  [e]
  (or (seq (:transcript/tool-calls e)) (seq (:tool_calls e)) (seq (:tool-calls e))))

(defn entry-tool-names [e]
  (mapv tool-call-name (entry-tool-calls e)))

(defn entry-empty?
  "True when an entry carries neither text nor tool calls."
  [e]
  (and (str/blank? (entry-content e)) (empty? (entry-tool-calls e))))

(defn assistant? [e] (= "assistant" (entry-role e)))

(defn llm-error? [e]
  (and (= "system" (entry-role e))
       (str/starts-with? (entry-content e) "LLM error")))

(defn clip [s n]
  (subs s 0 (min (long n) (count s))))

(def preview-chars 120)

(defn parse-args
  "Tool-call arguments as a keyword map; {} when they are not a JSON object."
  [tc]
  (let [a (tool-call-arguments tc)]
    (try (let [v (json/read-str a :key-fn keyword)] (if (map? v) v {}))
         (catch Exception _ {}))))

;; =============================================================================
;; Time
;; =============================================================================

(def ^:private unit-ms {"s" 1000 "m" 60000 "h" 3600000 "d" 86400000 "w" 604800000})

(defn parse-since
  "Epoch ms for `since`: a relative span back from `now-ms` (\"90s\", \"30m\",
   \"2h\", \"1d\", \"1w\"), an ISO-8601 instant or date-time with offset, a
   local date (\"2026-09-30\", midnight UTC), or epoch ms. nil for nil/blank;
   {:error msg} when it parses as none of these."
  [since now-ms]
  (let [s (some-> since str str/trim)]
    (cond
      (str/blank? s) nil
      (re-matches #"\d{10,}" s) (Long/parseLong s)
      :else
      (if-let [[_ n u] (re-matches #"(\d+)\s*([smhdw])" s)]
        (- (long now-ms) (* (Long/parseLong n) (long (unit-ms u))))
        (or (some (fn [parse] (try (parse s) (catch Exception _ nil)))
                  [#(.toEpochMilli (java.time.Instant/parse %))
                   #(.toEpochMilli (.toInstant (java.time.OffsetDateTime/parse %)))
                   #(.toEpochMilli (.toInstant (.atStartOfDay (java.time.LocalDate/parse %)
                                                              java.time.ZoneOffset/UTC)))])
            {:error (str "since: cannot read " (pr-str s)
                         " (use 30m, 2h, 1d or an ISO-8601 instant)")})))))

(defn iso
  "ISO-8601 UTC text of epoch ms, nil for nil."
  [ms]
  (when ms (str (java.time.Instant/ofEpochMilli (long ms)))))

;; =============================================================================
;; Listing filters
;; =============================================================================

(defn glob->pred
  "Predicate on an agent id. A pattern with `*` or `?` is a glob over the
   whole id; any other non-blank pattern is a prefix. nil/blank matches all."
  [pattern]
  (let [p (some-> pattern str str/trim)]
    (cond
      (str/blank? p) (constantly true)
      (re-find #"[*?]" p)
      (let [re (re-pattern (str "^"
                                (str/join (map (fn [c]
                                                 (case c
                                                   \* ".*"
                                                   \? "."
                                                   (java.util.regex.Pattern/quote (str c))))
                                               p))
                                "$"))]
        #(boolean (re-matches re (str %))))
      :else #(str/starts-with? (str %) p))))

(defn collapse-listing
  "One row per agent id from the raw listing (an agent may live in several
   sources): the newest row, with every source it was found in."
  [rows]
  (->> rows
       (group-by :agent-id)
       (map (fn [[_ rs]]
              (assoc (apply max-key #(or (:modified %) 0) rs)
                     :sources (vec (distinct (map :source rs))))))))

(defn select-listing
  "Filter, order (newest first) and cap listing rows. Pure.

   rows    [{:agent-id :project-id :modified :parent?}] (already collapsed)
   opts    {:agent glob/prefix, :project exact, :parent exact,
            :since-ms epoch ms, :limit n (nil = no cap)}
   A row's :parent is compared as given; resolve parents before calling."
  [rows {:keys [agent project parent since-ms limit]}]
  (let [agent? (glob->pred agent)]
    (cond->> (->> rows
                  (filter #(agent? (:agent-id %)))
                  (filter #(or (str/blank? project) (= project (:project-id %))))
                  (filter #(or (str/blank? parent) (= parent (:parent %))))
                  (filter #(or (nil? since-ms) (>= (or (:modified %) 0) since-ms)))
                  (sort-by #(or (:modified %) 0) >))
      limit (take limit)
      true  vec)))

;; =============================================================================
;; Final report and ending
;; =============================================================================

(defn last-tool-result
  "{:entry :tool :content} of the newest tool output: a `tool` role entry,
   or a stored tool-call result, whichever comes last."
  [entries]
  (some (fn [e]
          (or (when (= "tool" (entry-role e))
                {:entry e :tool nil :content (entry-content e)})
              (when-let [tc (some #(when-not (str/blank? (tool-call-result %)) %)
                                  (reverse (entry-tool-calls e)))]
                {:entry e :tool (tool-call-name tc) :content (tool-call-result tc)})))
        (rseq (vec entries))))

(defn ending
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

(defn max-turn [entries]
  (apply max 0 (keep entry-turn entries)))

(defn final-report
  "The final report of a ling read from its transcript entries.

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
             :turns      (max-turn entries)
             :entries    (count entries)
             :ending     (ending entries)}
      tool (assoc :last-tool-result
                  (cond-> {:turn    (entry-turn (:entry tool))
                           :preview (clip (:content tool) preview-chars)}
                    (:tool tool) (assoc :tool (:tool tool))))
      (seq cost) (assoc :total-cost (reduce + 0.0 cost)))))

;; =============================================================================
;; Matcher (transcript find)
;; =============================================================================

(defn compile-needle
  "A needle from `query`: `/re/` or `/re/i` is a regex, anything else a
   case-insensitive substring. {:error msg} for a blank query or bad regex."
  [query]
  (let [q (str query)]
    (if (str/blank? q)
      {:error "find: `query` is required"}
      (if-let [[_ body flags] (re-matches #"(?s)/(.+)/([i]*)" q)]
        (try {:kind :regex
              :pattern (re-pattern (str (when (seq flags) "(?i)") body))}
             (catch Exception e {:error (str "find: bad regex: " (ex-message e))}))
        {:kind :substring :pattern (re-pattern (str "(?i)" (java.util.regex.Pattern/quote q)))}))))

(def snippet-radius 160)

(defn snippet
  "Text around [start end) with `radius` chars each side, ellipsised."
  ([text start end] (snippet text start end snippet-radius))
  ([text start end radius]
   (let [from (max 0 (- start radius))
         to   (min (count text) (+ end radius))]
     (str (when (pos? from) "…") (subs text from to) (when (< to (count text)) "…")))))

(defn entry-segments
  "Searchable text of one entry as [{:role :tool :text}]: the entry's own
   content under its role, each tool call's arguments under role
   `assistant`, and each stored tool-call result under role `tool`."
  [e]
  (let [role (entry-role e)
        tool-name (when (= "tool" role) (or (:name e) (:tool_name e)))]
    (into (if (str/blank? (entry-content e))
            []
            [{:role role :tool tool-name :text (entry-content e)}])
          (mapcat (fn [tc]
                    (let [n (tool-call-name tc) a (tool-call-arguments tc) r (tool-call-result tc)]
                      (cond-> []
                        (not (str/blank? a)) (conj {:role "assistant" :tool n :text a})
                        (not (str/blank? r)) (conj {:role "tool" :tool n :text r})))))
          (entry-tool-calls e))))

(defn find-hits
  "Hits of `needle` in one ling's entries, in turn order, at most one per
   segment. opts {:role r :tool name}. Pure.
   -> [{:agent :run :turn :role :tool :snippet}]"
  [{:keys [agent run]} entries needle {:keys [role tool]}]
  (for [e entries
        seg (entry-segments e)
        :when (and (or (str/blank? role) (= role (:role seg)))
                   (or (str/blank? tool) (= tool (:tool seg))))
        :let [m (re-matcher (:pattern needle) (:text seg))]
        :when (.find m)]
    (cond-> {:agent agent :run run :turn (entry-turn e) :role (:role seg)
             :snippet (snippet (:text seg) (.start m) (.end m))}
      (:tool seg) (assoc :tool (:tool seg)))))

;; =============================================================================
;; Digest
;; =============================================================================

(def file-write-tools
  "Tool names whose call writes or edits the file named in its arguments."
  #{"file_write" "write_file" "edit" "edit_file" "Edit" "Write" "MultiEdit"
    "str_replace" "str_replace_editor" "apply_patch" "patch"})

(def shell-tools #{"bash" "Bash" "shell" "sh" "exec"})

(defn written-path
  "Path a write/edit tool call touches, or nil."
  [tc]
  (let [n (tool-call-name tc) a (parse-args tc)]
    (cond
      (file-write-tools n) (or (:file_path a) (:path a) (:file a))
      (and (= "fs" n) (= "write" (:command a))) (or (:file_path a) (:path a))
      :else nil)))

(defn shell-command [tc]
  (when (shell-tools (tool-call-name tc))
    (let [c (:command (parse-args tc))] (when (string? c) c))))

(defn classify-command
  "test | git | build | other for one shell command line."
  [cmd]
  (let [c (str cmd)]
    (cond
      (re-find #"run-tests|clojure\.test|-M:test|cognitect\.test-runner|kaocha|lein test|bb test|pytest|npm (run )?test|cargo test|go test|mvn test|\bmake test" c) "test"
      (re-find #"(^|[;&|(]\s*|\s)git\s" c) "git"
      (re-find #"-T:build|\bmake\b|npm run build|cargo build|mvn (package|install|compile)|lein (uberjar|jar|compile)|gradle|\bbuild\b" c) "build"
      :else "other")))

(def ^:private commit-re
  #"(?m)^\[([^\]\s]+)(?: \(root-commit\))? ([0-9a-f]{7,40})\] (.+)$")

(defn parse-commits
  "[{:branch :sha :subject}] from git commit output."
  [text]
  (mapv (fn [[_ b sha subj]] {:branch b :sha sha :subject (str/trim subj)})
        (re-seq commit-re (str text))))

(def ^:private test-re
  #"Ran (\d+) tests containing (\d+) assertions\.\s+(\d+) failures, (\d+) errors\.")

(defn parse-test-runs
  "[{:tests :assertions :failures :errors}] from clojure.test summaries."
  [text]
  (mapv (fn [[_ t a f e]]
          {:tests (Long/parseLong t) :assertions (Long/parseLong a)
           :failures (Long/parseLong f) :errors (Long/parseLong e)})
        (re-seq test-re (str text))))

(def ^:private error-re
  #"(?i)^\s*(error|exception|fatal)\b|\"error\"\s*:\s*\"|Syntax error|Execution error|Caused by:")

(defn error-text? [text]
  (boolean (re-find error-re (str text))))

(defn- tool-outputs
  "[{:turn :tool :text}] of every tool output in `entries`."
  [entries]
  (vec (for [e entries
             out (concat (when (= "tool" (entry-role e))
                           [{:tool (:name e) :text (entry-content e)}])
                         (keep (fn [tc] (let [r (tool-call-result tc)]
                                          (when-not (str/blank? r)
                                            {:tool (tool-call-name tc) :text r})))
                               (entry-tool-calls e)))]
         (assoc out :turn (entry-turn e)))))

(defn idle-gap
  "{:ms :after-turn} of the longest pause between consecutive timestamped
   entries, nil with fewer than two timestamps."
  [entries]
  (let [ts (filter entry-timestamp entries)]
    (when (next ts)
      (apply max-key :ms
             (map (fn [a b] {:ms (- (entry-timestamp b) (entry-timestamp a))
                             :after-turn (entry-turn a)})
                  ts (rest ts))))))

(defn digest
  "One pure fold over a ling's entries: what it did.

   -> {:agent-id :turns :entries :wall-ms :started :ended :ending
       :tools {name n} :files [path] :commands {:test n :git n ...}
       :shell [{:turn :class :command}] :commits [{:sha :subject :branch :turn}]
       :tests [{:tests :assertions :failures :errors :turn}]
       :errors {:count n :samples [{:turn :tool :text}]} :idle-gap {:ms :after-turn}
       :report final-report}"
  [agent-id entries]
  (let [entries (vec entries)
        calls   (for [e entries tc (entry-tool-calls e)] [e tc])
        shell   (vec (for [[e tc] calls :let [c (shell-command tc)] :when c]
                       {:turn (entry-turn e) :class (classify-command c) :command c}))
        outs    (tool-outputs entries)
        commits (->> outs
                     (mapcat (fn [o] (map #(assoc % :turn (:turn o)) (parse-commits (:text o)))))
                     (reduce (fn [acc c] (if (some #(= (:sha %) (:sha c)) acc) acc (conj acc c))) []))
        tests   (vec (mapcat (fn [o] (map #(assoc % :turn (:turn o)) (parse-test-runs (:text o)))) outs))
        errs    (vec (concat (for [e entries :when (llm-error? e)]
                               {:turn (entry-turn e) :tool nil :text (entry-content e)})
                             (filter #(error-text? (:text %)) outs)))
        stamps  (keep entry-timestamp entries)]
    (cond-> {:agent-id agent-id
             :turns    (max-turn entries)
             :entries  (count entries)
             :ending   (ending entries)
             :tools    (into (sorted-map) (frequencies (map (comp tool-call-name second) calls)))
             :files    (vec (distinct (keep (comp written-path second) calls)))
             :commands (into (sorted-map) (frequencies (map :class shell)))
             :shell    shell
             :commits  commits
             :tests    tests
             :errors   {:count   (count errs)
                        :samples (mapv #(update % :text clip preview-chars) (take 5 errs))}
             :report   (final-report agent-id entries)}
      (seq stamps) (assoc :started (apply min stamps) :ended (apply max stamps)
                          :wall-ms (- (apply max stamps) (apply min stamps)))
      (idle-gap entries) (assoc :idle-gap (idle-gap entries)))))

(defn digest-row
  "One compact row of a digest, for multi-agent views."
  [d]
  (let [last-test (peek (:tests d))]
    (cond-> {:agent   (:agent-id d)
             :turns   (:turns d)
             :ending  (:ending d)
             :tools   (->> (:tools d) (sort-by val >) (take 4)
                           (map (fn [[k v]] (str k ":" v))) (str/join " "))
             :files   (count (:files d))
             :commits (count (:commits d))
             :errors  (get-in d [:errors :count])}
      (:wall-ms d) (assoc :wall_s (quot (:wall-ms d) 1000))
      last-test    (assoc :tests (format "%d tests, %d fail, %d err"
                                         (:tests last-test) (:failures last-test) (:errors last-test)))
      (get-in d [:report :final-text])
      (assoc :final (clip (get-in d [:report :final-text]) preview-chars)))))

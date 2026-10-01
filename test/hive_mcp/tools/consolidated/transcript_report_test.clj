(ns hive-mcp.tools.consolidated.transcript-report-test
  "A coordinator can read what its lings said through `transcript`.

   - `full` / `max_chars` return entry content beyond the 120-char preview;
     the preview stays the default.
   - Every entry names its tool calls, and is marked `empty` when it has
     neither text nor tool calls.
   - `report` returns a ling's final assistant text in full, its turn count,
     how the transcript ends and the last tool result.

   Entries are in hive-agent's Datalevin shape, served by a stub source."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-mcp.agent.transcript-source :as src]
            [hive-mcp.tools.consolidated.transcript :as transcript]))

(defrecord MapSource [entries-by-agent]
  src/TranscriptSource
  (list-transcripts [_] (r/ok []))
  (read-entries [_ agent-id]
    (if-let [es (get entries-by-agent agent-id)]
      (r/ok es)
      (r/err :transcript/not-found {:agent-id agent-id :message (str "No transcript for agent " agent-id)}))))

(defn- e [turn role content & [calls]]
  (cond-> {:transcript/agent-id "ling" :transcript/turn turn
           :transcript/role role :transcript/content content
           :transcript/cost-usd 0.0}
    calls (assoc :transcript/tool-calls calls)))

(defn- tc [name result]
  {:tool-call/name name :tool-call/arguments "{}" :tool-call/result result})

(def ^:private long-report
  (str "## Final report\n" (str/join "\n" (map #(str "line " % " of the findings") (range 60)))))

(def ^:private finished
  [(e 1 :user "do the task")
   (e 1 :assistant "" [(tc "bash" "ok") (tc "read_file" "(ns foo)")])
   (e 2 :tool "file contents here")
   (e 2 :assistant "")
   (e 3 :assistant long-report)])

(def ^:private cut-off
  [(e 1 :user "go")
   (e 1 :assistant "looking" [(tc "grep" "3 matches")])
   (e 2 :assistant "" [(tc "bash" (apply str (repeat 300 "x")))])])

(def ^:private errored
  [(e 1 :user "go")
   (e 1 :assistant "a partial note")
   (e 2 :system "LLM error: 529 overloaded")])

(def ^:private source
  (->MapSource {"done" finished "cut" cut-off "err" errored "blank" [(e 1 :user "go")]}))

(defn- call [params]
  (transcript/handle-transcript source params))

(defn- body [response]
  (is (not (:isError response)) (:text response))
  (json/read-str (:text response) :key-fn keyword))

(deftest preview-stays-the-default
  (let [es (:entries (body (call {:command "tail" :agent_id "done" :n 1})))]
    (is (= 120 (count (:preview (first es)))))
    (is (not (contains? (first es) :content)))))

(deftest full-returns-whole-content
  (doseq [cmd [{:command "tail" :n 1} {:command "query"} {:command "since" :turn 2}]
          full [true "true"]]
    (let [es (:entries (body (call (assoc cmd :agent_id "done" :full full))))]
      (is (= long-report (:content (last es))) (pr-str cmd full))
      (is (not (contains? (last es) :preview))))))

(deftest max-chars-bounds-content
  (testing "cut entries say so"
    (let [x (last (:entries (body (call {:command "tail" :agent_id "done" :n 1 :max_chars "500"}))))]
      (is (= (subs long-report 0 500) (:content x)))
      (is (true? (:truncated x)))))
  (testing "short entries are whole and not marked"
    (let [x (first (:entries (body (call {:command "query" :agent_id "done" :max_chars 500}))))]
      (is (= "do the task" (:content x)))
      (is (not (contains? x :truncated)))))
  (testing "a bad max_chars is an MCP error naming it"
    (doseq [v ["lots" 0 -3]]
      (let [res (call {:command "tail" :agent_id "done" :max_chars v})]
        (is (:isError res) (pr-str v))
        (is (re-find #"max_chars" (:text res)))))))

(deftest entries-name-tool-calls-and-mark-empty
  (let [es (:entries (body (call {:command "query" :agent_id "done"})))
        by (fn [turn role] (first (filter #(and (= turn (:turn %)) (= role (:role %))) es)))]
    (testing "a tools-only turn names its calls and is not empty"
      (is (= ["bash" "read_file"] (:tool_calls (by 1 "assistant"))))
      (is (not (contains? (by 1 "assistant") :empty))))
    (testing "a turn with no text and no tool calls is empty"
      (is (true? (:empty (by 2 "assistant"))))
      (is (not (contains? (by 2 "assistant") :tool_calls))))
    (testing "a text turn is neither"
      (is (not (contains? (by 3 "assistant") :empty)))
      (is (not (contains? (by 3 "assistant") :tool_calls))))))

(deftest jsonl-tool-calls-are-named-too
  (let [s (->MapSource {"j" [{:turn 1 :role "assistant" :content ""
                              :tool_calls [{:id "1" :function {:name "bash" :arguments "{}"}}]}]})
        x (first (:entries (json/read-str (:text (transcript/handle-transcript
                                                  s {:command "query" :agent_id "j"}))
                                          :key-fn keyword)))]
    (is (= ["bash"] (:tool_calls x)))
    (is (not (contains? x :empty)))))

(deftest report-of-a-finished-ling
  (doseq [command ["report" "final"]]
    (let [b (body (call {:command command :agent_id "done"}))]
      (is (= long-report (:final-text b)) command)
      (is (= 3 (:final-turn b)))
      (is (= 3 (:turns b)))
      (is (= 5 (:entries b)))
      (is (= "text" (:ending b)))
      (is (= {:turn 2 :preview "file contents here"} (:last-tool-result b))))))

(deftest report-skips-a-blank-closing-turn
  (let [b (body (call {:command "report" :agent_id "done"}))]
    (is (str/starts-with? (:final-text b) "## Final report"))))

(deftest report-of-a-cut-off-ling
  (let [b (body (call {:command "report" :agent_id "cut"}))]
    (is (= "looking" (:final-text b)))
    (is (= 1 (:final-turn b)))
    (is (= "tool-calls" (:ending b)))
    (is (= "bash" (get-in b [:last-tool-result :tool])))
    (is (= 120 (count (get-in b [:last-tool-result :preview]))))))

(deftest report-of-an-errored-ling
  (let [b (body (call {:command "report" :agent_id "err"}))]
    (is (= "error" (:ending b)))
    (is (= "a partial note" (:final-text b)))))

(deftest report-with-no-assistant-text
  (let [b (body (call {:command "report" :agent_id "blank"}))]
    (is (nil? (:final-text b)))
    (is (= "unknown" (:ending b)))
    (is (not (contains? b :last-tool-result)))))

(deftest report-needs-a-known-agent-id
  (is (re-find #"agent_id" (:text (call {:command "report"}))))
  (let [res (call {:command "report" :agent_id "nobody"})]
    (is (:isError res))
    (is (re-find #"nobody" (:text res)))))

(deftest help-names-the-new-params
  (let [b (body (call {:command "help"}))
        cmds (into {} (map (juxt :command :params)) (:commands b))]
    (is (contains? cmds "report"))
    (doseq [c ["query" "tail" "since"]]
      (is (some #{"full"} (cmds c)) c)
      (is (some #{"max_chars"} (cmds c)) c))))

(deftest stats-explains-a-zero-cost
  (let [b (body (call {:command "stats" :agent_id "done"}))]
    (is (= 0.0 (:total-cost b)))
    (is (re-find #"bb-agentic" (:cost-note b)))))

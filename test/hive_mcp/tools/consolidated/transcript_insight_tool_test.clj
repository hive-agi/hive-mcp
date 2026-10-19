(ns hive-mcp.tools.consolidated.transcript-insight-tool-test
  "transcript list filters, find and digest, through the MCP handler.

   Ports are reify'd stubs: a TranscriptSource over a map, a ParentIndex
   over a map, and a fixed clock."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-mcp.agent.transcript-source :as src]
            [hive-mcp.tools.consolidated.transcript :as transcript]
            [hive-mcp.schema.tools :as schema]))

(def ^:private now 100000000)

(defn- e [agent turn role content & [calls]]
  (cond-> {:transcript/agent-id agent :transcript/turn turn :transcript/timestamp (* 1000 turn)
           :transcript/role role :transcript/content content}
    calls (assoc :transcript/tool-calls calls)))

(defn- tc [name args result]
  {:tool-call/name name :tool-call/arguments args :tool-call/result result})

(def ^:private runs
  {"ss-a" {:project "hive-mcp" :modified (- now 1000) :parent "coordinator:s1"
           :entries [(e "ss-a" 0 :user "find the bug")
                     (e "ss-a" 1 :assistant "" [(tc "bash" "{\"command\":\"git commit -m x\"}"
                                                    "[feat/a abcdef1] fix: the bug")])
                     (e "ss-a" 2 :assistant "Fixed the bug.")]}
   "ss-b" {:project "hive-mcp" :modified (- now 7200000) :parent "coordinator:s1"
           :entries [(e "ss-b" 0 :user "write docs")
                     (e "ss-b" 1 :assistant "" [(tc "file_write" "{\"file_path\":\"README.md\"}" "ok")])]}
   "e3-c" {:project "dirge" :modified (- now 500) :parent "coordinator:s2"
           :entries [(e "e3-c" 0 :user "nothing about bugs here")
                     (e "e3-c" 1 :system "LLM error: 529")]}})

(def ^:private chat-runs
  "run-id -> {:row run-summary :tip tip-summary}, in the shapes
   hive-agent.loop.chat-point run-summaries / tip-summary return."
  {"r-a1" {:row {:run-id "r-a1" :agent-id "ss-a" :project-id "hive-mcp" :model "m1"
                 :created (- now 5000) :parent-point nil}
           :tip {:seq 4 :turn 2 :ended? true}}
   "r-a2" {:row {:run-id "r-a2" :agent-id "ss-a" :project-id "hive-mcp" :model "m1"
                 :created (- now 1000) :parent-point {:run-id "r-a1" :seq 4 :turn 2}}
           :tip {:seq 6 :turn 3 :ended? false}}
   "r-c"  {:row {:run-id "r-c" :agent-id "e3-c" :project-id "dirge" :model "m2"
                 :created (- now 300) :parent-point nil}
           :tip nil}})

(def ^:private run-index
  {:list-runs (fn [] (r/ok (mapv :row (vals chat-runs))))
   :run-tip   (fn [id] (r/ok (get-in chat-runs [id :tip])))})

(def ^:private ports
  {:source (reify src/TranscriptSource
             (list-transcripts [_]
               (r/ok (mapv (fn [[id {:keys [project modified]}]]
                             {:agent-id id :project-id project :source :datalevin :modified modified})
                           runs)))
             (read-entries [_ id]
               (if-let [run (get runs id)]
                 (r/ok (:entries run))
                 (r/err :transcript/not-found {:message (str "No transcript for agent " id)}))))
   :parents (reify src/ParentIndex
              (parent-of [_ id] (get-in runs [id :parent])))
   :runs    run-index
   :now-ms (constantly now)})

(defn- call [params] (transcript/handle-transcript ports params))

(defn- body [response]
  (is (not (:isError response)) (:text response))
  (json/read-str (:text response) :key-fn keyword))

(deftest list-is-compact-newest-first-and-filtered
  (let [b (body (call {:command "list"}))]
    (is (= ["e3-c" "ss-a" "ss-b"] (map :agent (:transcripts b))))
    (is (= #{:agent :project :parent :turns :modified :ending :run_id}
           (set (keys (first (:transcripts b))))))
    (is (= {:agent "ss-a" :project "hive-mcp" :parent "coordinator:s1" :turns 2
            :ending "text" :modified "1970-01-02T03:46:39Z" :run_id "r-a2"}
           (second (:transcripts b)))))
  (testing "run_id is each agent's newest chat-point run, null when it has none"
    (is (= {"e3-c" "r-c" "ss-a" "r-a2" "ss-b" nil}
           (into {} (map (juxt :agent :run_id)) (:transcripts (body (call {:command "list"})))))))
  (testing "filters"
    (is (= ["ss-a" "ss-b"] (map :agent (:transcripts (body (call {:command "list" :agent "ss-"}))))))
    (is (= ["e3-c"] (map :agent (:transcripts (body (call {:command "list" :agent "*3-?"}))))))
    (is (= ["e3-c"] (map :agent (:transcripts (body (call {:command "list" :project "dirge"}))))))
    (is (= ["ss-a" "ss-b"] (map :agent (:transcripts (body (call {:command "list" :parent "coordinator:s1"}))))))
    (is (= ["e3-c" "ss-a"] (map :agent (:transcripts (body (call {:command "list" :since "1h"}))))))
    (let [b (body (call {:command "list" :limit "1"}))]
      (is (= 1 (:count b)))
      (is (= 3 (:matched b)))))
  (testing "bad params are MCP errors"
    (is (:isError (call {:command "list" :since "soonish"})))
    (is (:isError (call {:command "list" :limit 0})))))

(def ^:private failing-run-index
  {:list-runs (fn [] (r/err :chat-point/unreadable {:message "store down"}))
   :run-tip   (fn [_] (r/err :chat-point/unreadable {:message "store down"}))})

(deftest list-survives-an-unreadable-run-store
  (let [b (body (transcript/handle-transcript (assoc ports :runs failing-run-index)
                                              {:command "list"}))]
    (is (= 3 (:count b)))
    (is (every? nil? (map :run_id (:transcripts b))))))

(deftest the-default-runs-port-is-empty-without-hive-agent
  (when-not (try (requiring-resolve 'hive-agent.loop.chat-point/run-summaries)
                 (catch Throwable _hive-agent-absent nil))
    (let [runs (transcript/hive-agent-runs)]
      (is (= {:ok []} (select-keys ((:list-runs runs)) [:ok])))
      (is (r/ok? ((:run-tip runs) "r1"))))))

(deftest runs-lists-chat-point-runs-newest-first
  (let [b (body (call {:command "runs"}))]
    (is (= ["r-c" "r-a2" "r-a1"] (map :run_id (:runs b))))
    (is (= 3 (:count b)))
    (is (= 3 (:matched b)))
    (is (= {:run_id "r-a2" :agent "ss-a" :project "hive-mcp" :parent "coordinator:s1"
            :parent_point {:run_id "r-a1" :seq 4 :turn 2} :model "m1"
            :created "1970-01-02T03:46:39Z" :tip {:seq 6 :turn 3} :ended false}
           (second (:runs b))))
    (is (= {:tip {:seq 4 :turn 2} :ended true :parent_point nil}
           (select-keys (nth (:runs b) 2) [:tip :ended :parent_point])))
    (is (= {:tip nil :ended false}
           (select-keys (first (:runs b)) [:tip :ended]))))
  (testing "filters"
    (is (= ["r-a2" "r-a1"] (map :run_id (:runs (body (call {:command "runs" :agent "ss-"}))))))
    (is (= ["r-c"] (map :run_id (:runs (body (call {:command "runs" :project "dirge"}))))))
    (is (= ["r-c"] (map :run_id (:runs (body (call {:command "runs" :parent "coordinator:s2"}))))))
    (is (= ["r-c" "r-a2"] (map :run_id (:runs (body (call {:command "runs" :since "1s"}))))))
    (let [b (body (call {:command "runs" :limit 1}))]
      (is (= 1 (:count b)))
      (is (= 3 (:matched b)))))
  (testing "an unreadable store and bad params are MCP errors"
    (is (:isError (transcript/handle-transcript (assoc ports :runs failing-run-index)
                                                {:command "runs"})))
    (is (:isError (call {:command "runs" :limit 0})))
    (is (:isError (call {:command "runs" :since "soonish"})))))

(deftest the-spawn-resume-schema-names-commands-this-tool-serves
  (let [help    (body (call {:command "help"}))
        by-name (into {} (map (juxt :command identity)) (:commands help))
        run-id  (some #(when (and (vector? %) (= :run_id (first %))) %) schema/ResumeParam)
        text    (:json-schema/description (second run-id))]
    (doseq [cmd ["runs" "list"]]
      (is (str/includes? text (str "`transcript " cmd "`")))
      (is (contains? by-name cmd)))
    (is (str/includes? (get-in by-name ["list" :description]) "run_id"))
    (is (str/includes? (get-in by-name ["runs" :description]) "run_id"))))

(deftest find-searches-across-runs
  (let [b (body (call {:command "find" :query "bug"}))]
    (is (= #{"ss-a" "e3-c"} (set (map :agent (:hits b)))))
    (is (every? #(str/includes? (str/lower-case (:snippet %)) "bug") (:hits b))))
  (testing "role, tool, regex, filters, limit"
    (is (= [{:agent "ss-a" :run "hive-mcp" :turn 1 :role "tool" :tool "bash"
             :snippet "[feat/a abcdef1] fix: the bug"}]
           (:hits (body (call {:command "find" :query "/[0-9a-f]{7}\\]/" :role "tool"})))))
    (is (= ["ss-b"] (map :agent (:hits (body (call {:command "find" :query "README" :tool "file_write"}))))))
    (is (empty? (:hits (body (call {:command "find" :query "bug" :project "nope"})))))
    (is (= 1 (:count (body (call {:command "find" :query "bug" :limit 1}))))))
  (testing "a missing or bad query is an MCP error"
    (is (:isError (call {:command "find"})))
    (is (:isError (call {:command "find" :query "/(/"})))))

(deftest digest-one-and-many
  (let [d (body (call {:command "digest" :agent "ss-a"}))]
    (is (= [{:branch "feat/a" :sha "abcdef1" :subject "fix: the bug" :turn 1}] (:commits d)))
    (is (= {:git 1} (:commands d)))
    (is (= "Fixed the bug." (get-in d [:report :final-text])))
    (is (= "1970-01-01T00:00:00Z" (:started d))))
  (let [b (body (call {:command "digest" :parent "coordinator:s1"}))]
    (is (= ["ss-a" "ss-b"] (map :agent (:lings b))))
    (is (= 1 (:files (second (:lings b)))))
    (is (= 1 (:commits (first (:lings b))))))
  (is (= ["e3-c"] (map :agent (:lings (body (call {:command "digest" :agent "e3-*"}))))))
  (is (= 1 (:errors (first (:lings (body (call {:command "digest" :agent "e3-*"})))))))
  (is (:isError (call {:command "digest" :agent "ghost"}))))

(ns hive-mcp.tools.consolidated.transcript-insight-tool-test
  "transcript list filters, find and digest, through the MCP handler.

   Ports are reify'd stubs: a TranscriptSource over a map, a ParentIndex
   over a map, and a fixed clock."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-mcp.agent.transcript-source :as src]
            [hive-mcp.tools.consolidated.transcript :as transcript]))

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
   :now-ms (constantly now)})

(defn- call [params] (transcript/handle-transcript ports params))

(defn- body [response]
  (is (not (:isError response)) (:text response))
  (json/read-str (:text response) :key-fn keyword))

(deftest list-is-compact-newest-first-and-filtered
  (let [b (body (call {:command "list"}))]
    (is (= ["e3-c" "ss-a" "ss-b"] (map :agent (:transcripts b))))
    (is (= #{:agent :project :parent :turns :modified :ending}
           (set (keys (first (:transcripts b))))))
    (is (= {:agent "ss-a" :project "hive-mcp" :parent "coordinator:s1" :turns 2
            :ending "text" :modified "1970-01-02T03:46:39Z"}
           (second (:transcripts b)))))
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

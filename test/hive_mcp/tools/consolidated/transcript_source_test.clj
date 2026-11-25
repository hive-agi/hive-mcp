(ns hive-mcp.tools.consolidated.transcript-source-test
  "`transcript` list/query/tail/since/stats read every TranscriptSource.

   A headless hive-agent ling persists to Datalevin only, so a handler that
   reads JSONL alone answers [] for it. The sources here are stub records of
   the port; the Datalevin layout test uses a temp dir and a stub reader."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-mcp.agent.transcript-source :as src]
            [hive-mcp.tools.consolidated.transcript :as transcript]))

(defrecord StubSource [kind listing entries-by-agent]
  src/TranscriptSource
  (list-transcripts [_] (r/ok listing))
  (read-entries [_ agent-id]
    (if-let [es (get entries-by-agent agent-id)]
      (r/ok es)
      (r/err :transcript/not-found {:agent-id agent-id :source kind}))))

(defrecord BrokenSource []
  src/TranscriptSource
  (list-transcripts [_] (throw (ex-info "disk gone" {})))
  (read-entries [_ _] (throw (ex-info "disk gone" {}))))

(defn- dl-entry [turn role content]
  {:transcript/agent-id "ling-1" :transcript/turn turn
   :transcript/role (keyword role) :transcript/content content})

(def ^:private datalevin-only
  (->StubSource :datalevin
                [{:agent-id "ling-1" :source :datalevin :modified 2}]
                {"ling-1" (mapv #(dl-entry % (if (odd? %) "user" "assistant") (str "m" %))
                                (range 1 7))}))

(def ^:private jsonl-only
  (->StubSource :jsonl
                [{:agent-id "old" :source :jsonl :modified 1}]
                {"old" [{:turn 1 :role "user" :content "hi"}]}))

(def ^:private sources (src/composite [jsonl-only datalevin-only]))

(defn- body [response]
  (json/read-str (:text response) :key-fn keyword))

(deftest list-sees-a-datalevin-only-ling
  (let [res (transcript/handle-transcript sources {:command "list"})
        ids (set (map :agent (:transcripts (body res))))]
    (is (not (:isError res)))
    (is (= #{"ling-1" "old"} ids))))

(deftest queries-read-a-datalevin-only-ling
  (testing "query"
    (is (= 6 (:count (body (transcript/handle-transcript sources {:command "query" :agent_id "ling-1"}))))))
  (testing "tail with n as number and string"
    (doseq [n [2 "2"]]
      (is (= [5 6] (map :turn (:entries (body (transcript/handle-transcript
                                               sources {:command "tail" :agent_id "ling-1" :n n}))))))))
  (testing "since with turn as number and string"
    (doseq [t [4 "4"]]
      (is (= [5 6] (map :turn (:entries (body (transcript/handle-transcript
                                               sources {:command "since" :agent_id "ling-1" :turn t}))))))))
  (testing "stats"
    (is (= 6 (:turns (body (transcript/handle-transcript sources {:command "stats" :agent_id "ling-1"}))))))
  (testing "jsonl still answers"
    (is (= 1 (:count (body (transcript/handle-transcript sources {:command "query" :agent_id "old"}))))))
  (testing "unknown agent is an MCP error naming it"
    (let [res (transcript/handle-transcript sources {:command "tail" :agent_id "nobody" :n "3"})]
      (is (:isError res))
      (is (re-find #"nobody" (:text res))))))

(deftest a-missing-agent-id-is-an-mcp-error
  (doseq [c ["query" "tail" "since" "stats" "replay"]]
    (let [res (transcript/handle-transcript sources {:command c})]
      (is (:isError res) c)
      (is (re-find #"agent_id" (:text res)) c))))

(deftest a-throwing-source-is-an-mcp-error-not-an-exception
  (doseq [params [{:command "list"} {:command "tail" :agent_id "x" :n 3}
                  {:command "since" :agent_id "x" :turn "1"}]]
    (let [res (transcript/handle-transcript (->BrokenSource) params)]
      (is (:isError res) (pr-str params)))))

(deftest datalevin-source-follows-the-hive-agent-layout
  (let [root (doto (io/file (System/getProperty "java.io.tmpdir")
                            (str "tsrc-" (System/nanoTime)))
               (.mkdirs))
        mk   (fn [& segs]
               (let [d (apply io/file root segs)]
                 (.mkdirs d)
                 (spit (io/file d "data.mdb") "x")
                 d))
        _    (mk "hive-mcp" "ling-1")
        _    (mk "dirge" "ling-2")
        _    (.mkdirs (io/file root "datahike" "legacy"))
        _    (.mkdirs (io/file root "global" "empty-dir"))
        read (fn [dir agent-id] (r/ok [{:dir (str dir) :agent agent-id}]))
        s    (src/->DatalevinSource (str root) read)]
    (testing "list finds <root>/<project>/<agent-id> stores with data"
      (is (= #{["ling-1" "hive-mcp"] ["ling-2" "dirge"]}
             (set (map (juxt :agent-id :project-id) (:ok (src/list-transcripts s)))))))
    (testing "read-entries opens the agent's store dir"
      (is (= [{:dir (str (io/file root "dirge" "ling-2")) :agent "ling-2"}]
             (:ok (src/read-entries s "ling-2")))))
    (testing "an agent with no store is not-found"
      (is (r/err? (src/read-entries s "nobody"))))))

;; -----------------------------------------------------------------------------
;; Stores that are not opened stay visible (busy / unknown), and a listing
;; reads each store at most once per call. Temp dirs and an injected reader.
;; -----------------------------------------------------------------------------

(defn- three-stores
  "A DatalevinSource over a temp root with three agent stores; `busy-id`'s
   reader answers :transcript/store-busy. Returns {:source :reads}."
  [busy-id]
  (let [root  (doto (io/file (System/getProperty "java.io.tmpdir") (str "tsrc-busy-" (System/nanoTime)))
                (.mkdirs))
        reads (atom {})]
    (doseq [a ["a1" "a2" "a3"]]
      (let [d (io/file root "proj" a)]
        (.mkdirs d)
        (spit (io/file d "data.mdb") "x")))
    {:reads  reads
     :source (src/->DatalevinSource
              (str root)
              (fn [dir agent-id]
                (swap! reads update (str dir) (fnil inc 0))
                (if (= agent-id busy-id)
                  (r/err :transcript/store-busy {:dir dir :agent-id agent-id})
                  (r/ok [(assoc (dl-entry 1 "user" "hi") :transcript/agent-id agent-id)
                         (assoc (dl-entry 2 "assistant" "done") :transcript/agent-id agent-id)]))))}))

(def ^:private no-runs
  {:list-runs (fn [] (r/ok [{:run-id "run-a2" :agent-id "a2" :created 1}]))
   :run-tip   (fn [_] (r/ok nil))})

(deftest list-shows-a-busy-run-and-opens-each-store-once
  (let [{:keys [source reads]} (three-stores "a2")
        res  (transcript/handle-transcript {:source source :runs no-runs} {:command "list"})
        rows (:transcripts (body res))
        busy (first (filter #(= "a2" (:agent %)) rows))]
    (is (not (:isError res)))
    (is (= #{"a1" "a2" "a3"} (set (map :agent rows))) "the busy run keeps its row")
    (is (= "transcript/store-busy" (:status busy)))
    (is (= "run-a2" (:run_id busy)))
    (is (every? #(= 1 %) (vals @reads)) "list never reads the same dir twice in one call")))

(deftest digest-skips-a-busy-run-and-answers-the-rest
  (let [{:keys [source]} (three-stores "a2")
        res (body (transcript/handle-transcript {:source source :runs no-runs}
                                                {:command "digest" :agent "a*"}))]
    (is (= 2 (:count res)))
    (is (= [{:agent "a2" :project "proj" :status "transcript/store-busy" :run_id "run-a2"}]
           (:skipped res)))))

(deftest find-lists-a-busy-run-as-skipped
  (let [{:keys [source]} (three-stores "a2")
        raw (transcript/handle-transcript {:source source :runs no-runs}
                                          {:command "find" :query "done"})
        _   (is (not (:isError raw)) (:text raw))
        res (when-not (:isError raw) (body raw))]
    (is (= 2 (:count res)))
    (is (= ["a2"] (map :agent (:skipped res))))))

(ns hive-mcp.tools.consolidated.transcript-test
  "Integer params of `transcript tail` / `since` survive the `multi` path.

   `multi` forwards every param as a string, and `clojure.core/int` on a
   String throws ClassCastException (String -> Character), so both commands
   crashed there while working when called directly. The JSONL readers are
   stubbed: nothing here reads a transcript on disk."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-mcp.tools.consolidated.transcript :as transcript]
            [hive-mcp.agent.transcript-source :as src]))

(defrecord RecordingSource [calls]
  src/TranscriptSource
  (list-transcripts [_] (r/ok []))
  (read-entries [_ agent-id]
    (swap! calls conj [:read agent-id])
    (r/ok (mapv (fn [t] {:turn t :role "user" :content (str t)}) (range 1 21)))))

(defn- run
  "Run `params` through the handler against a recording stub source.
   Returns {:response ... :calls [[:read agent-id] ...]}."
  [params]
  (let [calls  (atom [])
        source (->RecordingSource calls)]
    {:response (transcript/handle-transcript source params)
     :calls    @calls}))

(deftest tail-takes-n-as-a-number-or-a-numeric-string
  (doseq [n [8 "8"]]
    (testing (pr-str n)
      (let [{:keys [response calls]} (run {:command "tail" :agent_id "a" :n n})]
        (is (not (:isError response)))
        (is (= [[:read "a"]] calls))
        (is (re-find #"\"count\":8" (:text response))))))
  (testing "absent n falls back to 10"
    (is (re-find #"\"count\":10" (:text (:response (run {:command "tail" :agent_id "a"})))))))

(deftest since-takes-turn-as-a-number-or-a-numeric-string
  (doseq [turn [36 "36"]]
    (testing (pr-str turn)
      (let [{:keys [response calls]} (run {:command "since" :agent_id "a" :turn turn})]
        (is (not (:isError response)))
        (is (= 1 (count calls)) "since reads the transcript once")))))

(deftest a-non-numeric-param-is-an-mcp-error-not-an-exception
  (doseq [[command k] [["tail" :n] ["since" :turn]]]
    (testing command
      (let [{:keys [response calls]} (run {:command command :agent_id "a" k "eight"})]
        (is (:isError response))
        (is (re-find (re-pattern (str "`" (name k) "`")) (:text response)))
        (is (empty? calls) "nothing is read when the param is rejected")))))

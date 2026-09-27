(ns hive-mcp.tools.consolidated.transcript-test
  "Integer params of `transcript tail` / `since` survive the `multi` path.

   `multi` forwards every param as a string, and `clojure.core/int` on a
   String throws ClassCastException (String -> Character), so both commands
   crashed there while working when called directly. The JSONL readers are
   stubbed: nothing here reads a transcript on disk."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-mcp.tools.consolidated.transcript :as transcript]))

(defn- run
  "Run `params` through the handler with both JSONL readers stubbed.
   Returns {:response ... :calls [[reader & args] ...]}."
  [params]
  (let [calls (atom [])]
    {:response (with-redefs [transcript/query-jsonl
                             (fn [& args] (swap! calls conj (into [:query] args)) (r/ok []))
                             transcript/query-jsonl-tail
                             (fn [& args] (swap! calls conj (into [:tail] args)) (r/ok []))]
                 (transcript/handle-transcript params))
     :calls @calls}))

(deftest tail-takes-n-as-a-number-or-a-numeric-string
  (doseq [n [8 "8"]]
    (testing (pr-str n)
      (let [{:keys [response calls]} (run {:command "tail" :agent_id "a" :n n})]
        (is (not (:isError response)))
        (is (= [[:tail "a" 8]] calls))
        (is (int? (last (first calls)))))))
  (testing "absent n falls back to 10"
    (is (= [[:tail "a" 10]] (:calls (run {:command "tail" :agent_id "a"}))))))

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

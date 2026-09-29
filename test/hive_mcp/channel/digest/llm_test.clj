(ns hive-mcp.channel.digest.llm-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.channel.digest :as digest]
            [hive-mcp.channel.digest.llm :as llm]))

(use-fixtures :each (fn [f] (llm/clear-memo!) (f) (llm/clear-memo!)))

(def opts (assoc (digest/settings {}) :timeout-ms 500))

(defrecord StubDigester [answer]
  llm/ITerminalDigester
  (digest-terminal [_ _ _ _] answer))

(defrecord Recording [inner calls]
  llm/ITerminalDigester
  (digest-terminal [_ a texts n]
    (swap! calls conj [a texts n])
    (llm/digest-terminal inner a texts n)))

(defrecord Throwing []
  llm/ITerminalDigester
  (digest-terminal [_ _ _ _] (throw (ex-info "provider down" {:status 503}))))

(defrecord Slow [ms]
  llm/ITerminalDigester
  (digest-terminal [_ _ _ _] (Thread/sleep (long ms)) "late"))

(defn- long-terminal []
  (first (digest/coalesce-rows opts [{:a "l1" :e "completed"
                                      :m (str/join " " (repeat 90 "fact")) :ts 3}])))

(deftest digest-replaces-message-and-marks-row
  (let [row (llm/digest-row (->StubDigester "3 tests pass, commit abc123") opts (long-terminal))]
    (is (= "3 tests pass, commit abc123" (:m row)))
    (is (:dg row))
    (is (nil? (:clip row)))
    (is (= 3 (:ts (digest/finalize row))))))

(deftest short-rows-never-reach-the-model
  (let [calls (atom [])
        row (first (digest/coalesce-rows opts [{:a "l1" :e "completed" :m "done"}]))]
    (is (= row (llm/digest-row (->Recording (->StubDigester "x") calls) opts row)))
    (is (empty? @calls))))

(deftest failure-keeps-stage-one-row
  (let [row (long-terminal)]
    (is (= row (llm/digest-row (->Throwing) opts row)))
    (is (= row (llm/digest-row (->StubDigester nil) opts row)))
    (is (= row (llm/digest-row (->StubDigester "  ") opts row)))))

(deftest timeout-keeps-stage-one-row
  (let [row (long-terminal)
        t0 (System/currentTimeMillis)]
    (is (= row (llm/digest-row (->Slow 3000) (assoc opts :timeout-ms 100) row)))
    (is (< (- (System/currentTimeMillis) t0) 1500))))

(deftest memo-pays-once
  (let [calls (atom [])
        d (->Recording (->StubDigester "digest") calls)
        row (long-terminal)]
    (llm/digest-row d opts row)
    (llm/digest-row d opts row)
    (is (= 1 (count @calls)))
    (is (= "l1" (ffirst @calls)))))

(deftest overlong-model-answer-is-clipped
  (let [row (llm/digest-row (->StubDigester (str/join " " (repeat 200 "x"))) opts (long-terminal))]
    (is (<= (count (digest/words (:m row))) (inc (:max-words opts))))))

(defrecord StubBackend [response seen]
  proto/LLMBackend
  (chat [_ messages tools] (reset! seen [messages tools]) response)
  (model-name [_] "stub"))

(deftest llm-digester-adapts-backend
  (let [seen (atom nil)
        d (llm/->LLMDigester (->StubBackend {:type :text :content " ok \n"} seen))]
    (is (= "ok" (llm/digest-terminal d "l1" ["a" "b"] 40)))
    (is (nil? (second @seen)))
    (is (str/includes? (:content (first (first @seen))) "40 words"))
    (is (nil? (llm/digest-terminal (llm/->LLMDigester (->StubBackend {:type :tool_calls :calls []} seen))
                                   "l1" ["a"] 40)))))

(deftest unbuildable-spec-yields-no-digester
  (is (nil? (llm/configured-digester nil)))
  (is (nil? (llm/configured-digester {:provider :no-such-provider-xyz :model "m"}))))

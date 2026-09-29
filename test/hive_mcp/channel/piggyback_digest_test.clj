(ns hive-mcp.channel.piggyback-digest-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.channel.digest.llm :as llm]
            [hive-mcp.channel.piggyback :as pb]))

(def ^:private shouts (atom []))

(use-fixtures :each
  (fn [f]
    (let [src @pb/message-source-fn
          cursors @pb/agent-read-cursors
          buf @pb/backbone-buffer]
      (try
        (reset! shouts [])
        (reset! pb/message-source-fn (fn [] @shouts))
        (reset! pb/agent-read-cursors {})
        (reset! pb/backbone-buffer [])
        (llm/clear-memo!)
        (f)
        (finally
          (reset! pb/message-source-fn src)
          (reset! pb/agent-read-cursors cursors)
          (reset! pb/backbone-buffer buf))))))

(def ^:private trace
  (str "clojure.lang.ExceptionInfo: kanban update failed {:status 500}\n"
       "\tat hive_mcp.tools.kanban$update.invoke(kanban.clj:88)\n"
       "\tat clojure.lang.AFn.run(AFn.java:22)"))

(defn- seed! []
  (reset! shouts
          [{:agent-id "ling-a" :event-type :started :message "starting" :timestamp 10 :project-id "p"}
           {:agent-id "ling-a" :event-type :progress :message "turn 1" :timestamp 11 :project-id "p"}
           {:agent-id "ling-a" :event-type :completed :timestamp 12 :project-id "p"
            :message (str/join " " (repeat 90 "detail"))}
           {:agent-id "ling-b" :event-type :error :message trace :timestamp 13 :project-id "p"}]))

(defn- drain []
  (pb/get-messages "coordinator" :project-id "p"))

(defrecord StubDigester [answer]
  llm/ITerminalDigester
  (digest-terminal [_ _ _ _] answer))

(deftest disabled-is-todays-shape
  (seed!)
  (binding [pb/*digest-settings* {:enabled? false}]
    (let [rows (drain)]
      (is (= 4 (count rows)))
      (is (not-any? :ts rows))
      (is (= trace (:m (last rows)))))))

(deftest coalesce-without-model
  (seed!)
  (binding [pb/*digest-settings* {:enabled? true}]
    (let [[a b :as rows] (drain)]
      (is (= 2 (count rows)))
      (is (= {:a "ling-a" :e "completed" :n 3 :ts 10 :clip 30} (dissoc a :m)))
      (is (= "ExceptionInfo 500: kanban update failed {:status 500}" (:m b)))
      (is (= 13 (:ts b))))))

(deftest model-digest-on-terminal
  (seed!)
  (binding [pb/*digest-settings* {:enabled? true}
            pb/*terminal-digester* (->StubDigester "ling-a: did X, 3 tests green")]
    (let [[a] (drain)]
      (is (= "ling-a: did X, 3 tests green" (:m a)))
      (is (:dg a)))))

(deftest raw-is-redeemable-from-ts
  (seed!)
  (binding [pb/*digest-settings* {:enabled? true}]
    (let [[a] (drain)
          raw (pb/fetch-history :since (dec (:ts a)) :agent-id "ling-a")]
      (is (= ["started" "progress" "completed"] (mapv :e raw)))
      (is (= 90 (count (str/split (:m (last raw)) #" ")))))))

(defrecord Exploding []
  llm/ITerminalDigester
  (digest-terminal [_ _ _ _] (throw (Error. "boom"))))

(deftest any-failure-falls-back
  (seed!)
  (binding [pb/*digest-settings* {:enabled? true :max-words "not-a-number"}]
    (let [rows (drain)]
      (is (= 4 (count rows)))
      (is (not-any? :ts rows))))
  (reset! pb/agent-read-cursors {})
  (binding [pb/*digest-settings* {:enabled? true}
            pb/*terminal-digester* (->Exploding)]
    (is (= 2 (count (drain))))))

(ns hive-mcp.agent.cache-test
  "Prompt-cache breakpoints on the OpenAI-compatible wire: who is marked, how
   many markers land, and the invariant that a marker never edits a message."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.agent.cache :as cache]
            [hive-mcp.agent.provider.model :as model]))

(def ^:private openrouter (:openrouter model/seed-registry))
(def ^:private groq (:groq model/seed-registry))

(defn- convo
  "A ling history: system, then n user/assistant exchanges with a tool result."
  [n]
  (into [{:role "system" :content "preset + primed context"}]
        (mapcat (fn [i]
                  [{:role "user" :content (str "step " i)}
                   {:role "assistant" :content "" :tool_calls [{:id (str "t" i)}]}
                   {:role "tool" :tool_call_id (str "t" i) :content (str "output " i)}])
                (range n))))

(defn- texts
  "What each message says, whatever shape it says it in."
  [messages]
  (mapv (fn [m]
          [(:role m)
           (let [c (:content m)]
             (if (sequential? c) (mapv :text c) [c]))])
        messages))

;;; ---------------------------------------------------------------------------
;;; Who gets marked
;;; ---------------------------------------------------------------------------

(deftest only-a-declaring-provider-with-an-anthropic-model-is-marked
  (testing "OpenRouter declares the dialect, so an Anthropic model is marked"
    (is (= :anthropic-style (cache/cache-control-style openrouter "anthropic/claude-opus-4-7")))
    (is (= :anthropic-style (cache/cache-control-style openrouter "claude-sonnet-4"))))
  (testing "a non-Anthropic model on the same gateway is not"
    (is (nil? (cache/cache-control-style openrouter "deepseek/deepseek-chat"))))
  (testing "a provider that declares nothing is never marked, whatever the model"
    (is (nil? (cache/cache-control-style groq "anthropic/claude-opus-4-7"))))
  (testing "a missing entry answers nil rather than throwing"
    (is (nil? (cache/cache-control-style nil "claude-sonnet-4")))))

(deftest an-unmarked-provider-gets-a-byte-identical-array
  (let [msgs (convo 3)]
    (is (identical? msgs (cache/maybe-mark groq "claude-sonnet-4" msgs)))
    (is (identical? msgs (cache/maybe-mark openrouter "deepseek/deepseek-chat" msgs)))))

;;; ---------------------------------------------------------------------------
;;; Where the markers go
;;; ---------------------------------------------------------------------------

(deftest the-system-prompt-is-the-first-breakpoint
  (let [marked (cache/mark-messages (convo 2))]
    (is (= [{:type "text" :text "preset + primed context"
             :cache_control {:type "ephemeral"}}]
           (:content (first marked)))
        "string content is lifted to a block array so the marker has a home")))

(deftest a-tool-result-is-never-marked
  (testing "the gateway rewrites role=tool into a tool_result block and no
            contract says the marker survives, so it is left alone"
    (let [marked (cache/mark-messages (convo 3))
          tool-msgs (filter #(= "tool" (:role %)) marked)]
      (is (seq tool-msgs))
      (is (every? #(string? (:content %)) tool-msgs)))))

(deftest an-unmarked-message-keeps-its-string-content
  (let [marked (cache/mark-messages [{:role "system" :content "sys"}
                                     {:role "user" :content "u"}
                                     {:role "assistant" :content "a"}])]
    (is (= "a" (:content (last marked))))))

(deftest an-empty-array-is-returned-untouched
  (is (= [] (cache/mark-messages [])))
  (is (zero? (cache/marker-count (cache/mark-messages [])))))

(deftest a-history-with-no-system-message-still-marks-exchanges
  (let [marked (cache/mark-messages [{:role "user" :content "u1"}
                                     {:role "assistant" :content "a1"}])]
    (is (= 1 (cache/marker-count marked)))))

;;; ---------------------------------------------------------------------------
;;; Invariants
;;; ---------------------------------------------------------------------------

(defspec never-more-than-four-markers 60
  (prop/for-all [n (gen/choose 0 30)]
    (<= (cache/marker-count (cache/mark-messages (convo n))) cache/max-breakpoints)))

(defspec marking-never-changes-what-a-message-says 60
  (testing "a marker may lift string content into a block array, but it never
            edits, drops or reorders the text. A prefix that changed would be a
            cache MISS, which is the failure this exists to avoid."
    (prop/for-all [n (gen/choose 0 15)]
      (let [msgs (convo n)]
        (= (texts msgs) (texts (cache/mark-messages msgs)))))))

(defspec marking-preserves-roles-and-length 60
  (prop/for-all [n (gen/choose 0 15)]
    (let [msgs (convo n)
          marked (cache/mark-messages msgs)]
      (and (= (count msgs) (count marked))
           (= (mapv :role msgs) (mapv :role marked))
           (= (mapv :tool_calls msgs) (mapv :tool_calls marked))))))

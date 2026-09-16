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

(deftest the-tail-is-marked-even-when-it-is-a-tool-result
  (testing "measured 2026-09-15 on anthropic/claude-sonnet-5 through OpenRouter:
            the marker DOES survive the gateway's tool_result rewrite. Two arms
            identical but for this one marker read back 11024 against 6172
            cached tokens of an 11041-token prompt, so the old exclusion left
            the whole newest turn uncached on every turn."
    (let [marked (cache/mark-messages (convo 3))
          tail   (last marked)]
      (is (= "tool" (:role tail)))
      (is (= [{:type "text" :text "output 2" :cache_control {:type "ephemeral"}}]
             (:content tail)))))
  (testing "the TAIL tool result only, not every one of them: an interior tool
            message is already inside a cached prefix and a marker there buys
            nothing while spending budget"
    (let [marked (cache/mark-messages (convo 3))
          interior (filter #(and (= "tool" (:role %)) (string? (:content %))) marked)]
      (is (= 2 (count interior))))))

(deftest an-unmarked-message-keeps-its-string-content
  (testing "the system message, the tail and completed user turns are marked;
            an assistant turn in the middle is left byte-identical, string
            content and all"
    (let [marked   (cache/mark-messages (convo 3))
          interior (nth marked 5)]
      (is (= "assistant" (:role interior)))
      (is (string? (:content interior)) "never lifted to a block array"))))

(deftest an-empty-array-is-returned-untouched
  (is (= [] (cache/mark-messages [])))
  (is (zero? (cache/marker-count (cache/mark-messages [])))))

(deftest a-history-with-no-system-message-still-marks-exchanges
  (testing "index 0 is walked like any other when it is not a system message,
            so the task itself does not sit outside every cached prefix"
    (let [marked (cache/mark-messages [{:role "user" :content "u1"}
                                       {:role "assistant" :content "a1"}])]
      (is (= 2 (cache/marker-count marked))
          "the tail, plus the user turn behind it"))))

(deftest the-tail-is-marked-once-when-it-is-a-user-message
  (testing "the tail pass and the user-history walk must not both claim the
            same message, or a two-message request burns three of four markers"
    (let [marked (cache/mark-messages [{:role "system" :content "sys"}
                                       {:role "user" :content "u"}])]
      (is (= 2 (cache/marker-count marked))
          "the system message and the tail, not the tail twice"))))

(deftest the-default-lifetime-is-spelled-by-omitting-the-field
  (testing "Anthropic reads a missing ttl as 5m, so :5m must not send one"
    (let [marked (cache/mark-messages (convo 1))]
      (is (= {:type "ephemeral"} (:cache_control (first (:content (first marked)))))))))

(deftest an-hour-lifetime-reaches-the-marker
  (testing "measured 2026-09-15: OpenRouter forwards the ttl field and needs no
            beta header for it. The same 6166-token write billed 0.01528362 at
            5m and 0.02441934 at 1h, a ratio of 1.598, which is 1.25x against
            2x over one base."
    (let [marked (cache/mark-messages (convo 1) :1h)]
      (is (= {:type "ephemeral" :ttl "1h"}
             (:cache_control (first (:content (first marked))))))
      (is (= 3 (cache/marker-count marked)) "every marker placed, not just the first"))))

(deftest the-lifetime-is-declared-per-provider
  (testing "whether a gateway forwards ttl is a property of the gateway, so it
            is read off the registry entry like the dialect itself"
    (is (= :5m (cache/ttl-of openrouter)) "nothing declared means the cheap default")
    (is (= :1h (cache/ttl-of (assoc openrouter :cache-ttl :1h))))
    (is (= :5m (cache/ttl-of (assoc openrouter :cache-ttl :nonsense)))
        "an unknown lifetime falls back rather than reaching the wire")
    (is (= :5m (cache/ttl-of nil)))))

(deftest an-hour-lifetime-travels-through-maybe-mark
  (let [entry (assoc openrouter :cache-ttl :1h)
        marked (cache/maybe-mark entry "anthropic/claude-sonnet-5" (convo 1))]
    (is (= {:type "ephemeral" :ttl "1h"}
           (:cache_control (first (:content (first marked))))))))

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

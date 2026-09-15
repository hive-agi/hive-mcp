(ns hive-mcp.agent.cache
  "Prompt-cache breakpoints for the OpenAI-compatible wire.

   A ling re-sends its whole conversation on every turn. What that costs depends
   on how much of it the provider serves from cache, and that is decided by
   where `cache_control` markers sit. This host speaks ONE wire shape (OpenAI
   /v1/chat/completions), so this namespace owns one question: which messages of
   that array carry a marker, and for which provider/model pairs it is placed
   at all.

   WHO GETS MARKED is declared, never sniffed. A gateway that fronts an
   Anthropic model (OpenRouter) forwards a `cache_control` block marker through
   unchanged; the registry entry says so with `:cache-control :anthropic-style`.
   Every other provider is left byte-identical to what it was before: one caches
   implicitly (Gemini), one caches automatically (DeepSeek), and a strict
   OpenAI-shaped endpoint rejects the unknown field outright.

   WHAT GETS MARKED, within the 4-marker ceiling the Anthropic API enforces:

     1. the system message, which carries the preset and the primed context and
        is the largest span that does not change between turns;
     2. the most recent completed user exchanges, newest first.

   A `role: tool` message is NOT marked. The gateway translates it into a
   tool_result block and no published contract says the marker survives that
   translation, so the one span whose caching cannot be verified from here is
   left alone.

   Marking is an ANNOTATION, never an edit. String content is lifted into a
   one-block array so the marker has somewhere to sit, and the text itself is
   untouched: a prefix that changed is a cache MISS, which is the whole failure
   this namespace exists to avoid.

   Pure. No I/O, no state."
  (:require [clojure.string :as str]
            [malli.core :as m]))

;;; ---------------------------------------------------------------------------
;;; Value objects
;;; ---------------------------------------------------------------------------

(def CacheStyle
  "The cache dialect a provider entry speaks, or nil for none."
  [:maybe [:enum :anthropic-style]])

(def max-breakpoints
  "The Anthropic API rejects a request carrying more than four markers."
  4)

(def ^:private marker {:cache_control {:type "ephemeral"}})

;;; ---------------------------------------------------------------------------
;;; Who gets marked
;;; ---------------------------------------------------------------------------

(defn anthropic-family?
  "True when `model` names an Anthropic model, however the gateway spells it:
   `anthropic/claude-opus-4-7` on OpenRouter, `claude-sonnet-4` bare."
  [model]
  (boolean (and (string? model)
                (or (str/starts-with? model "anthropic/")
                    (str/includes? model "claude")))))

(defn cache-control-style
  "The cache dialect to use for this registry `entry` and `model`, or nil.

   Both halves must agree: the provider declares that it forwards the marker,
   and the model is one that honours it. Anything else answers nil, and the
   request body goes out exactly as it did before this namespace existed."
  [entry model]
  (when (and (map? entry)
             (= :anthropic-style (:cache-control entry))
             (anthropic-family? model))
    :anthropic-style))

;;; ---------------------------------------------------------------------------
;;; Where the markers go
;;; ---------------------------------------------------------------------------

(defn- ->blocks
  "Message content as a block array, or nil when there is nothing to mark."
  [content]
  (cond
    (string? content)     (when-not (str/blank? content) [{:type "text" :text content}])
    (sequential? content) (when (seq content) (vec content))
    :else                 nil))

(defn- mark-at
  "Mark the last content block of the message at `i`, lifting string content to
   a block array first. nil when that message has nothing markable, so the
   caller spends no budget on it."
  [messages i]
  (when-let [blocks (->blocks (:content (nth messages i)))]
    (assoc-in (vec messages) [i :content]
              (update blocks (dec (count blocks)) merge marker))))

(defn mark-messages
  "Place cache breakpoints in an OpenAI-shaped message array.

   The system message first (the stable prefix), then user messages from the end
   backwards, never exceeding `max-breakpoints` in total.

   Index 0 is walked like any other when it is NOT the system message: a history
   that opens on the user's task would otherwise leave its most stable span, the
   task itself, outside every cached prefix."
  [messages]
  (let [msgs (vec messages)
        n    (count msgs)
        system? (= "system" (:role (first msgs)))]
    (if (zero? n)
      messages
      (let [[acc used] (if system?
                         (if-let [m (mark-at msgs 0)] [m 1] [msgs 0])
                         [msgs 0])
            floor (if system? 1 0)]
        (loop [i (dec n), marked used, acc acc]
          (if (or (< i floor) (>= marked max-breakpoints))
            acc
            (if (= "user" (:role (nth acc i)))
              (if-let [m (mark-at acc i)]
                (recur (dec i) (inc marked) m)
                (recur (dec i) marked acc))
              (recur (dec i) marked acc))))))))

(defn maybe-mark
  "`mark-messages` when this provider/model pair speaks a cache dialect, and the
   untouched array otherwise. The one entry point a request builder needs."
  [entry model messages]
  (if (cache-control-style entry model)
    (mark-messages messages)
    messages))

(defn marker-count
  "How many markers an array carries. Request-built observability: the ceiling
   is a property of the built body, so it is checkable there."
  [messages]
  (reduce (fn [acc m]
            (+ acc (let [c (:content m)]
                     (if (sequential? c) (count (filter :cache_control c)) 0))))
          0
          messages))

;;; ---------------------------------------------------------------------------
;;; Contracts
;;; ---------------------------------------------------------------------------

(m/=> anthropic-family? [:=> [:cat :any] :boolean])
(m/=> cache-control-style [:=> [:cat :any :any] CacheStyle])
(m/=> mark-messages [:=> [:cat [:sequential :map]] [:sequential :map]])
(m/=> maybe-mark [:=> [:cat :any :any [:sequential :map]] [:sequential :map]])
(m/=> marker-count [:=> [:cat [:sequential :map]] :int])

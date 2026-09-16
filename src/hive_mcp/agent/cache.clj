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
     2. the TAIL, whatever its role, so the newest turn is written to cache
        instead of re-read at full price on the next call;
     3. the most recent completed user exchanges, newest first.

   A `role: tool` message used to be excluded, on the grounds that the gateway
   rewrites it into a tool_result block and no published contract says the
   marker survives. Measured 2026-09-15 on anthropic/claude-sonnet-5 through
   OpenRouter: it survives, and the exclusion was costing 4852 tokens per turn
   on an 11k prompt. See `mark-messages` for the two-arm numbers.


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

(def CacheTtl
  "How long a written cache entry lives. `:5m` is Anthropic's default and costs
   1.25x base to write; `:1h` costs 2x."
  [:enum :5m :1h])

(defn- marker
  "The block a marker merges into message content. The default lifetime is
   spelled by OMITTING the field, so `:5m` must not send `ttl` at all."
  [ttl]
  {:cache_control (cond-> {:type "ephemeral"}
                    (= :1h ttl) (assoc :ttl "1h"))})

(defn ttl-of
  "The cache lifetime this registry `entry` declares, `:5m` unless it says
   otherwise.

   Declared per PROVIDER, like the dialect itself, because whether a gateway
   forwards the `ttl` field is a property of the gateway and not of the model.
   Measured 2026-09-15: OpenRouter forwards it, and unlike the native Anthropic
   wire it needs no `extended-cache-ttl-2025-04-11` beta header to do so. The
   same 6166-token write billed 0.01528362 at `:5m` and 0.02441934 at `:1h`, a
   ratio of 1.598, which is 1.25x against 2x over one base.

   Reach for `:1h` only when the stable prefix would otherwise EXPIRE between
   turns, a loop whose tool calls routinely outlast five minutes. One 2x write
   beats two 1.25x rewrites and loses to one, so the break-even is the prefix
   being rewritten twice within the hour."
  [entry]
  (if (= :1h (:cache-ttl entry)) :1h :5m))

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
  "Mark the last content block of the message at `i` with `mk`, lifting string
   content to a block array first. nil when that message has nothing markable,
   so the caller spends no budget on it."
  [messages i mk]
  (when-let [blocks (->blocks (:content (nth messages i)))]
    (assoc-in (vec messages) [i :content]
              (update blocks (dec (count blocks)) merge mk))))

(defn mark-messages
  "Place cache breakpoints in an OpenAI-shaped message array.

   The system message first (the stable prefix), then the TAIL, then user
   messages from the end backwards, never exceeding `max-breakpoints`.

   The tail is marked WHATEVER its role, `tool` included. That exclusion used to
   live here, on the grounds that the gateway rewrites a tool message into a
   tool_result block and no published contract says the marker survives the
   rewrite. It does. Measured 2026-09-15 against anthropic/claude-sonnet-5
   through OpenRouter, two arms identical but for that one marker, reading
   `usage.prompt_tokens_details.cached_tokens` on the second call:

       tool message marked     cached 11024 of an 11041-token prompt
       tool message untouched  cached  6172   (the system span alone)

   4852 tokens, the whole newest turn, re-read at full price on every turn for
   want of one marker. In a ling loop the newest turn IS the tool output and is
   usually the largest single span, which is why the tail gets a breakpoint of
   its own instead of waiting for some later user message to cover it.

   Index 0 is walked like any other when it is NOT the system message: a history
   that opens on the user's task would otherwise leave its most stable span, the
   task itself, outside every cached prefix.

   `ttl` applies to EVERY marker placed here, which is deliberate and slightly
   wasteful: only the system span is stable enough to earn a 1h write, and the
   rolling ones are superseded next turn. Splitting them would buy back the
   difference between 2x and 1.25x on the rolling markers alone, and is not
   worth a second policy until someone measures that it is."
  ([messages] (mark-messages messages :5m))
  ([messages ttl]
   (let [msgs (vec messages)
         n    (count msgs)]
     (if (zero? n)
       messages
       (let [mk         (marker ttl)
             system?    (= "system" (:role (first msgs)))
             floor      (if system? 1 0)
             [acc used] (if system?
                          (if-let [m (mark-at msgs 0 mk)] [m 1] [msgs 0])
                          [msgs 0])
             [acc used] (if (>= (dec n) floor)
                          (if-let [m (mark-at acc (dec n) mk)] [m (inc used)] [acc used])
                          [acc used])]
         (loop [i (- n 2), marked used, acc acc]
           (if (or (< i floor) (>= marked max-breakpoints))
             acc
             (if (= "user" (:role (nth acc i)))
               (if-let [m (mark-at acc i mk)]
                 (recur (dec i) (inc marked) m)
                 (recur (dec i) marked acc))
               (recur (dec i) marked acc)))))))))

(defn maybe-mark
  "`mark-messages` when this provider/model pair speaks a cache dialect, and the
   untouched array otherwise. The one entry point a request builder needs.

   The lifetime comes from the same registry entry that declares the dialect,
   so an operator buys a 1h cache for one gateway without touching any other."
  [entry model messages]
  (if (cache-control-style entry model)
    (mark-messages messages (ttl-of entry))
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
(m/=> ttl-of [:=> [:cat :any] CacheTtl])
(m/=> mark-messages [:function
                     [:=> [:cat [:sequential :map]] [:sequential :map]]
                     [:=> [:cat [:sequential :map] CacheTtl] [:sequential :map]]])
(m/=> maybe-mark [:=> [:cat :any :any [:sequential :map]] [:sequential :map]])
(m/=> marker-count [:=> [:cat [:sequential :map]] :int])

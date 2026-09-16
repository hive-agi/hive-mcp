;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns openrouter-cache-probe
  "Does a `cache_control` marker survive an OpenAI-compat gateway's tool_result
   translation?

   `hive-mcp.agent.cache/mark-messages` deliberately never marks a `role: tool`
   message: OpenRouter rewrites it into an Anthropic tool_result block and no
   published contract says the marker survives that rewrite. In a ling loop the
   tool output is usually the largest single span of the newest turn, so if the
   marker DOES survive, the saving hive-agent measured on the native wire is
   sitting unclaimed on this wire.

   Two arms, identical in every byte except one marker:

     :mark-tool? false   what ships today
     :mark-tool? true    the same array with the tool message marked too

   Each arm sends the same prefix twice. Call 1 writes the cache, call 2 reads
   it, and `usage.prompt_tokens_details.cached_tokens` on call 2 says how much
   of the prefix came back cached. If the marked arm reads back the tool span
   and the unmarked arm does not, the marker survived.

   Run it from a REPL, never from a shell -e, which is
   :guard/dev-ns-not-shell-clojure-eval:

     cider spawn, project_dir this repo, aliases [\"dev\"]
     cider eval  (require 'openrouter-cache-probe) (openrouter-cache-probe/-main)

   It spends real tokens against a live key, so no test runner reaches it.

   The key is read from `pass` AT EXEC TIME, bound locally, and never printed,
   logged or returned. Axiom 20260425144849-06a420d6 allows exactly this shape
   and forbids the conversation from running `pass show` itself."
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [hive-mcp.agent.cache :as cache]))

(def ^:private pass-entry
  "The INFERENCE key. OpenRouter refuses a provisioning/management key on
   /chat/completions with a bare `401 User not found`, which reads like a bad
   key and is not: `GET /api/v1/auth/key` answers `is_management_key true` for
   one. Check that field before blaming the secret."
  "openrouter/keys/hive-mcp")
(def ^:private chat-endpoint "https://openrouter.ai/api/v1/chat/completions")
(def ^:private models-endpoint "https://openrouter.ai/api/v1/models")

(defn- api-key
  "The key, read at exec time. A failure answers nil so the caller can report a
   STATUS; the value itself never reaches a printing caller."
  []
  (let [{:keys [exit out]} (shell/sh "pass" "show" pass-entry)]
    (when (zero? exit)
      (some-> out str/split-lines first str/trim not-empty))))

(defn key-status
  "Presence only. Safe to call from anywhere."
  []
  (if (api-key) :present :absent))

;; ---------------------------------------------------------------------------
;; The spans
;; ---------------------------------------------------------------------------

(defn system-text
  "A stable system span past the 1024-token minimum a cacheable prefix needs.
   The nonce LEADS so the two arms can never share a cache entry."
  [nonce]
  (str "Probe run " nonce ".\n\n"
       "You are a measurement fixture. Answer with a single integer and nothing else.\n\n"
       (str/join "\n"
                 (for [i (range 130)]
                   (str "Rule " i ": a cached prefix is worth exactly what it shares with "
                        "the next request, so the bytes ahead of a breakpoint must not move.")))))

(defn tool-payload
  "Stands in for the tool output of the newest turn: the span this probe is
   about, sized like a real one."
  [nonce]
  (str "{\"run\":\"" nonce "\",\"rows\":["
       (str/join ","
                 (for [i (range 120)]
                   (str "{\"id\":" i ",\"qn\":\"hive.probe.row/field-" i
                        "\",\"note\":\"filler so this span is large enough to matter\"}")))
       "]}"))

(defn- conversation
  "A ling-shaped turn: system, task, an assistant tool call, and the tool
   result that answers it."
  [nonce]
  [{:role "system" :content (system-text nonce)}
   {:role "user" :content "How many rows did the tool return? Answer with the integer only."}
   {:role "assistant" :content nil
    :tool_calls [{:id "call_probe_1" :type "function"
                  :function {:name "dump_rows" :arguments "{}"}}]}
   {:role "tool" :tool_call_id "call_probe_1" :content (tool-payload nonce)}])

(defn mark-tool-message
  "Put a marker on the LAST `role: tool` message, lifting string content into a
   one-block array if it is not one already. This is the line
   `hive-mcp.agent.cache` refuses to write, and the whole subject of the probe."
  [messages]
  (let [v (vec messages)
        i (last (filter #(= "tool" (:role (nth v %))) (range (count v))))]
    (if (nil? i)
      v
      (let [c (:content (nth v i))
            blocks (cond
                     (string? c) [{:type "text" :text c}]
                     (sequential? c) (vec c)
                     :else nil)]
        (if (seq blocks)
          (assoc-in v [i :content]
                    (update blocks (dec (count blocks)) merge {:cache_control {:type "ephemeral"}}))
          v)))))

;; ---------------------------------------------------------------------------
;; The wire
;; ---------------------------------------------------------------------------

(defn- send!
  "One call. Returns the usage row, never the completion."
  [key model messages]
  (let [resp (http/post chat-endpoint
                        {:headers {"Authorization" (str "Bearer " key)
                                   "Content-Type" "application/json"}
                         :body (json/write-str {:model model
                                                :messages messages
                                                :max_tokens 16
                                                :usage {:include true}})
                         :throw-exceptions false
                         :socket-timeout 180000
                         :connection-timeout 30000})
        body (:body resp)]
    (if (= 200 (:status resp))
      (let [u (:usage (json/read-str body :key-fn keyword))]
        {:prompt (:prompt_tokens u)
         :cached (or (get-in u [:prompt_tokens_details :cached_tokens]) 0)
         :completion (:completion_tokens u)})
      {:error (:status resp)
       :message (when body (subs body 0 (min 400 (count body))))})))

(defn pick-model
  "The newest Anthropic Sonnet the account can reach, by the gateway's own
   spelling. Asking beats hardcoding a name that gets retired."
  [key]
  (let [resp (http/get models-endpoint
                       {:headers {"Authorization" (str "Bearer " key)}
                        :throw-exceptions false})]
    (when (= 200 (:status resp))
      (->> (:data (json/read-str (:body resp) :key-fn keyword))
           (map :id)
           (filter #(and (str/starts-with? % "anthropic/") (str/includes? % "sonnet")))
           (remove #(str/includes? % ":"))
           sort
           last))))

(defn arm
  "Two calls over one prefix. `mark-tool?` is the only difference between arms.

   Call 2 appends an exchange so the prefix under test is strictly interior,
   which is what a real loop looks like on its second turn."
  [key model nonce mark-tool?]
  (let [prepare (fn [msgs]
                  (cond-> (cache/mark-messages msgs) mark-tool? mark-tool-message))
        base   (conversation nonce)
        r1     (send! key model (prepare base))
        follow (conj (vec base)
                     {:role "assistant" :content "120"}
                     {:role "user" :content "Say it once more, integer only."})
        r2     (send! key model (prepare follow))]
    {:mark-tool? mark-tool? :call-1 r1 :call-2 r2}))

(defn -main
  "Run both arms and report. Prints usage rows only."
  [& _]
  (if-let [key (api-key)]
    (let [model (or (pick-model key) "anthropic/claude-sonnet-4.5")
          nonce (str (System/currentTimeMillis))
          off   (arm key model (str nonce "-off") false)
          on    (arm key model (str nonce "-on") true)]
      (println "model:" model)
      (doseq [a [off on]]
        (println (format "mark-tool? %-5s  call1 prompt=%s cached=%s  call2 prompt=%s cached=%s"
                         (str (:mark-tool? a))
                         (get-in a [:call-1 :prompt]) (get-in a [:call-1 :cached])
                         (get-in a [:call-2 :prompt]) (get-in a [:call-2 :cached])))
        (when-let [e (or (get-in a [:call-1 :error]) (get-in a [:call-2 :error]))]
          (println "  HTTP" e (or (get-in a [:call-1 :message]) (get-in a [:call-2 :message])))))
      {:model model
       :marked-off off
       :marked-on on
       :cached-delta (- (or (get-in on [:call-2 :cached]) 0)
                        (or (get-in off [:call-2 :cached]) 0))})
    {:key :absent
     :note (str "no key at " pass-entry)}))

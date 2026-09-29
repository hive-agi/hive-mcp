(ns hive-mcp.channel.digest.llm
  "Boundary for the cheap-model terminal digest: the ITerminalDigester port,
   an LLMBackend adapter, and the bounded, memoised call. Any failure keeps
   the input row."
  (:require [clojure.string :as str]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.channel.digest :as digest]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defprotocol ITerminalDigester
  (digest-terminal [this agent-id texts max-words]
    "Digest of one agent's texts (oldest first) in at most max-words, or nil."))

(defn prompt-messages [agent-id texts max-words]
  [{:role "system"
    :content (str "You condense a worker agent's messages for its coordinator. "
                  "Reply with at most " max-words " words of concrete facts: "
                  "what was done, results, files, commit ids, test counts, blockers. "
                  "For an error give the exception class, status and one line. "
                  "No preamble, no advice, no markdown headings.")}
   {:role "user"
    :content (str "Agent " agent-id " messages, oldest first:\n\n"
                  (str/join "\n---\n" texts))}])

(defrecord LLMDigester [backend]
  ITerminalDigester
  (digest-terminal [_ agent-id texts max-words]
    (let [{:keys [type content]} (proto/chat backend (prompt-messages agent-id texts max-words) nil)]
      (when (and (= :text type) (string? content) (not (str/blank? content)))
        (str/trim content)))))

(defn- normalize-spec [spec]
  (cond-> (select-keys spec [:provider :model :api-url :secret-key])
    (string? (:provider spec)) (update :provider keyword)
    (string? (:secret-key spec)) (update :secret-key keyword)))

(defonce ^:private built (atom nil))

(defn configured-digester
  "LLMDigester for a [:hivemind :digest :model] spec, cached per spec; nil when
   the spec is absent or the backend cannot be built."
  [spec]
  (when (map? spec)
    (let [[cached-spec cached] @built]
      (if (= cached-spec spec)
        cached
        (let [d (try
                  (let [f (requiring-resolve 'hive-mcp.agent.openrouter/openai-compat-backend)]
                    (->LLMDigester (f (normalize-spec spec))))
                  (catch Throwable t
                    (log/warn "hivemind digest: model backend unavailable:" (ex-message t))
                    nil))]
          (reset! built [spec d])
          d)))))

(defonce ^:private memo (atom {}))

(def ^:private memo-cap 256)

(defn clear-memo! [] (reset! memo {}))

(defn- remember! [k v]
  (swap! memo (fn [m] (assoc (if (>= (count m) memo-cap) {} m) k v)))
  v)

(defn- bounded
  "(f) within timeout-ms, else nil. A throw inside f propagates."
  [timeout-ms f]
  (let [fut (future (f))
        v (deref fut timeout-ms ::timeout)]
    (if (= ::timeout v)
      (do (future-cancel fut) nil)
      v)))

(defn digest-row
  "Row with its message replaced by a model digest when it needs one and the
   call succeeds in time; the row unchanged otherwise."
  [digester {:keys [max-words timeout-ms]} row]
  (if-not (digest/needs-model? max-words row)
    row
    (let [texts (digest/texts row)
          k [(:a row) (hash texts) max-words]
          d (or (get @memo k)
                (try
                  (some->> (bounded timeout-ms #(digest-terminal digester (:a row) texts max-words))
                           str/trim
                           not-empty
                           (digest/clip-words max-words)
                           :text
                           (remember! k))
                  (catch Throwable t
                    (log/warn "hivemind digest: model call failed for" (:a row) (ex-message t))
                    nil)))]
      (if d (digest/with-digest row d) row))))

(defn digest-rows
  "Stage 2 over coalesced rows; identity without a digester."
  [digester opts rows]
  (if digester
    (mapv #(digest-row digester opts %) rows)
    (vec rows)))

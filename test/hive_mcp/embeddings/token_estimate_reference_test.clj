(ns hive-mcp.embeddings.token-estimate-reference-test
  "estimate-tokens is bounded on both sides against a reference tokenizer:
   never below the qwen3-embedding:4b count, and at most ~1.5x it on English
   prose and code. The reference counts are the provider's
   prompt_eval_count for each sample, measured 2026-10-21."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.embeddings.service :as svc]))

(def reference
  "Sample key -> [qwen3-embedding:4b token count, text]."
  {:en     [49 "The quick brown fox jumps over the lazy dog. Knowledge graphs connect memories, decisions and conventions across a swarm of agents working in parallel on shared code. Each agent reads the context before acting, and the coordinator reviews every change before it lands."]
   :pt     [66 "A memória do enxame guarda decisões, convenções e notas; cada agente lê o contexto antes de agir, e a ingestão de livros em português exige atenção ao tamanho máximo da entrada. Não há garantia de que a estimativa esteja correta sem medição."]
   :zh     [31 "知识图谱连接了记忆、决策和约定。每个代理在行动前都会阅读上下文，协调者在合并之前审查每一个变更。"]
   :ja     [27 "日本語のテキストも含めて、埋め込みモデルの入力長を正しく見積もる必要があります。"]
   :ko     [30 "한국어 문장도 포함합니다. 임베딩 모델의 입력 길이를 정확하게 추정해야 합니다."]
   :ru     [28 "Граф знаний связывает воспоминания, решения и соглашения между агентами роя."]
   :code   [83 "(defn validate-content-size!\n  [doc-text {:keys [max-tokens provider-key chars-per-token] :as _resolved}]\n  (let [estimated-tokens (estimate-tokens doc-text chars-per-token)]\n    (when (> estimated-tokens max-tokens)\n      (throw (ex-info (str \"too large \" (name provider-key)) {:error :embedder/input-too-large})))))\n"]
   :json   [76 "{\n  \"a\": [1, 2, 3],\n  \"name\": \"hive-mcp\",\n  \"nested\": {\"k\": \"v\", \"n\": 12345},\n  \"created\": \"2026-10-21T14:00:47-03:00\"\n}"]
   :digits [70 "1234567890 9876543210 3.14159265358979 2026-10-21 0x7f3a9b 42 1000000"]
   :emoji  [21 "🚀🔥✨ deploy ok ✅ ❌ 🙃 — “quotes” … naïve café"]
   :hex    [66 "3f2a9c1e7b4d4e0f9a8b7c6d5e4f3a2b 0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f"]
   :b64    [58 "dGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZyBYeVo0NTZhYmNERUY+Pz8/"]
   :url    [37 "https://github.com/hive-agi/hive-mcp/pull/534?tab=files&diff=split#discussion_r1234567"]})

(def max-over-count
  "Ceiling on estimate / reference for English prose and code."
  1.5)

(def bounded-above #{:en :code})

(deftest never-under-counts-the-reference
  (doseq [[k [ref-count text]] reference]
    (testing k
      (is (>= (svc/estimate-tokens text) ref-count)))))

(deftest over-count-bounded-on-english-and-code
  (doseq [k bounded-above
          :let [[ref-count text] (reference k)]]
    (testing k
      (is (<= (/ (double (svc/estimate-tokens text)) ref-count) max-over-count)))))

(deftest old-chars-over-3-basis-under-counts-the-reference
  (doseq [k [:zh :ja :digits]
          :let [[ref-count text] (reference k)]]
    (is (< (long (Math/ceil (/ (count text) 3.0))) ref-count) (str k))))

(defn- sample-slice
  "The first `n` space-separated words of `text`."
  [text n]
  (str/join " " (take n (str/split text #" "))))

(defspec repeated-samples-never-under-count 100
  (prop/for-all [k    (gen/elements (vec (keys reference)))
                 reps (gen/choose 1 20)]
    (let [[ref-count text] (reference k)
          s (str/join (repeat reps (str text " ")))]
      (>= (svc/estimate-tokens s) (inc (* reps (dec ref-count)))))))

(defspec estimate-is-monotone-in-prefix 100
  (prop/for-all [k (gen/elements (vec (keys reference)))
                 n (gen/choose 0 40)]
    (let [[_ text] (reference k)]
      (<= (svc/estimate-tokens (sample-slice text n))
          (svc/estimate-tokens text)))))

(deftest empty-and-nil-are-zero
  (is (zero? (svc/estimate-tokens nil)))
  (is (zero? (svc/estimate-tokens ""))))

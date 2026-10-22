(ns hive-mcp.embeddings.chars-per-token-test
  "The size guard and the escalation resolver estimate tokens on ONE basis,
   ~3.0 chars/token (measured on non-English prose), so content between the
   real ceiling and the old chars/4 ceiling is refused locally instead of
   500ing at the provider."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.embeddings.service :as svc]))

(def ^:private cases
  {:empty        [""]
   :three-chars  ["abc"]
   :four-chars   ["abcd"]
   :explicit-4   [(apply str (repeat 32768 \a)) 4.0]
   :pt-ceiling   [(apply str (repeat 24500 \a))]
   :old-gap      [(apply str (repeat 30000 \a))]})

(defn mutant-chars-over-4
  "The pre-fix basis: integer chars/4, ignoring any declared ratio."
  [content & _]
  (quot (count (or content "")) 4))

(deftrifecta estimate-tokens-basis
  hive-mcp.embeddings.service/estimate-tokens
  {:golden-path "test/hive_mcp/embeddings/golden/chars_per_token.edn"
   :apply? true
   :cases cases
   :gen (gen/fmap vector gen/string-alphanumeric)
   :pred (fn [n] (and (integer? n) (not (neg? n))))
   :num-tests 100
   :mutations [["chars-over-4" mutant-chars-over-4]]})

(def ^:private pt-prose
  "A memória do enxame guarda decisões, convenções e notas; cada agente lê o contexto antes de agir, e a ingestão de livros em português exige atenção ao tamanho máximo da entrada. Não há garantia de que a estimativa esteja correta sem medição. ")

(defn- pt-of-length [n]
  (subs (apply str (repeat (inc (quot n (count pt-prose))) pt-prose)) 0 n))

(deftest guard-refuses-content-the-old-basis-let-through
  ;; Portuguese prose measured at ~3.0 chars/token on qwen3-embedding:4b:
  ;; 30,000 chars passed chars/4 (7,500 <= 8,192) and then 500ed remotely.
  (let [resolved {:max-tokens 8192 :provider-key :ollama-qwen3}]
    (is (= :embedder/input-too-large
           (try (svc/validate-content-size! (pt-of-length 30000) resolved)
                (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
    (is (nil? (svc/validate-content-size! (pt-of-length 20000) resolved)))))

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

(deftest guard-refuses-content-the-old-basis-let-through
  ;; 30,000 chars: chars/4 = 7,500 <= 8,192 (passed, then 500ed remotely);
  ;; chars/3 = 10,000 > 8,192 (refused locally with the size in the message).
  (let [resolved {:max-tokens 8192 :provider-key :ollama-qwen3}]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"10000 est\. tokens"
                          (svc/validate-content-size! (apply str (repeat 30000 \a)) resolved)))
    (is (nil? (svc/validate-content-size! (apply str (repeat 24000 \a)) resolved)))
    (is (nil? (svc/validate-content-size! (apply str (repeat 30000 \a))
                                          (assoc resolved :chars-per-token 4.0))))))

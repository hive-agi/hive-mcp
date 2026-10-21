(ns hive-mcp.tools.result-bridge-contract-test
  "Contract facets for the Result -> MCP bridge: schema-derived trifectas
   over result->mcp and result->mcp-text (ok -> success envelope, err ->
   error envelope naming the message and the exception class) and a golden
   over try-result and keywordize-map."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [hive-mcp.tools.result-bridge :as rb]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private SomeResult
  "Ok results with scalar payloads, and err results with and without a
   message and class, so every branch of the error text is reached."
  [:or
   [:map [:ok [:or :string :int [:map [:a :int]]]]]
   [:map
    [:error [:enum :x/failed :y/broken]]
    [:message {:optional true} [:enum "went wrong" "nope"]]
    [:class {:optional true} [:enum "java.lang.NullPointerException" "clojure.lang.ExceptionInfo"]]]])

(defn- error-text-ok?
  "The error envelope names the message (else the error keyword) and,
   when present, the class in parentheses."
  [r out]
  (and (true? (:isError out))
       (str/includes? (:text out) (or (:message r) (str (:error r))))
       (or (nil? (:class r)) (str/includes? (:text out) (str "(" (:class r) ")")))))

(deftrifecta-from-schema result->mcp-renders-both-arms
  #'rb/result->mcp
  {:in [:cat SomeResult]
   :out rb/McpResponse
   :rel (fn [[r] out]
          (if (contains? r :ok)
            (and (not (:isError out))
                 (= (json/read-str (json/write-str (:ok r)) :key-fn keyword)
                    (json/read-str (:text out) :key-fn keyword)))
            (error-text-ok? r out)))
   :classify (fn [[r] _] (if (contains? r :ok) :ok :err))
   :classify-domain #{:ok :err}
   :golden-path "test/golden/hive-mcp/tools/result-bridge-result-mcp.edn"
   :num-tests 200})

(deftrifecta-from-schema result->mcp-text-renders-both-arms
  #'rb/result->mcp-text
  {:in [:cat SomeResult]
   :out rb/McpResponse
   :rel (fn [[r] out]
          (if (contains? r :ok)
            (and (not (:isError out))
                 (= (:text out) (let [v (:ok r)] (if (string? v) v (pr-str v)))))
            (error-text-ok? r out)))
   :classify (fn [[r] _] (if (contains? r :ok) :ok :err))
   :classify-domain #{:ok :err}
   :golden-path "test/golden/hive-mcp/tools/result-bridge-result-mcp-text.edn"
   :num-tests 200})

(golden/deftest-golden try-result-and-keywordize-golden
  "test/golden/hive-mcp/tools/result-bridge-try-result.edn"
  {:ok        (rb/try-result :t/x (fn [] {:ok 1}))
   :ex-info   (rb/try-result :t/x (fn [] (throw (ex-info "bad" {:k 1}))))
   :npe       (rb/try-result :t/x (fn [] (throw (NullPointerException.))))
   :keywords  (rb/keywordize-map {"a" 1 :b 2})
   :nil-map   (rb/keywordize-map nil)})

(deftest result-arms-are-disjoint
  (is (m/validate rb/OkResult {:ok nil}))
  (is (not (m/validate rb/ErrResult {:ok nil})))
  (is (m/validate rb/ErrResult (rb/try-result :t/x (fn [] (throw (Exception. "e"))))))
  (is (not (m/validate rb/OkResult (rb/try-result :t/x (fn [] (throw (Exception. "e"))))))))

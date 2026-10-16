(ns hive-mcp.embeddings.role-test
  "Contract tests for revisioned embedding-role shaping; no provider or store."
  (:require [clojure.test.check.generators :as gen]
            [hive-mcp.embeddings.role :as role]
            [hive-test.trifecta :refer [deftrifecta]]
            [clojure.test :refer [deftest is]]
            [hive-mcp.embeddings.protocol :as proto]
            [hive-mcp.embeddings.active :as active]))

(defn characterize-query
  [{:keys [template text]}]
  (role/shape-query template text))

(deftrifecta query-role-shaping
  hive-mcp.embeddings.role-test/characterize-query
  {:golden-path "test/golden/embeddings/query-role.edn"
   :cases {:default {:template nil :text "Where is the design?"}
           :raw {:template :raw :text "  keep whitespace  "}
           :revision {:template {:template :qwen3-retrieval-v1} :text "Where is the design?"}}
   :gen (gen/let [text gen/string-alphanumeric
                  revision (gen/elements [nil :raw :qwen3-retrieval-v1])]
          {:template revision :text text})
   :pred string?
   :num-tests 40
   :mutations [["unshaped-query" (fn [{:keys [text]}] text)]
               ["drops-query" (fn [_] "")]]})

(defn characterize-document
  [{:keys [template entry]}]
  (role/shape-document template entry))

(deftrifecta document-role-shaping
  hive-mcp.embeddings.role-test/characterize-document
  {:golden-path "test/golden/embeddings/document-role.edn"
   :cases {:default {:template nil :entry {:content "memory" :type "note"}}
           :raw-structured {:template :raw :entry {:content {:a 1} :type "note"}}
           :content-only {:template :content-only :entry {:content "memory" :type "note"}}
           :chroma {:template :metadata-envelope
                    :entry {:type "note" :tags ["alpha" "beta"] :content "memory"}}
           :untagged {:template :metadata-envelope :entry {:type "decision" :content "choice"}}}
   :gen (gen/let [text gen/string-alphanumeric
                  template (gen/elements [:raw :content-only :metadata-envelope])]
          {:template template :entry {:type "note" :tags ["alpha"] :content text}})
   :pred string?
   :num-tests 40
   :mutations [["always-raw" (fn [{:keys [entry]}] (:content entry))]
               ["always-empty" (fn [_] "")]]})

(deftest provider-port-receives-shaped-query
  (let [seen (atom [])
        provider (reify proto/EmbeddingProvider
                   (embed-text [_ text] (swap! seen conj text) [1.0])
                   (embed-batch [_ texts] (mapv (constantly [1.0]) texts))
                   (embedding-dimension [_] 1))]
    (is (= [1.0] (active/embed-text provider (role/shape-query :raw "plain"))))
    (is (= [1.0] (active/embed-text provider
                                    (role/shape-query :qwen3-retrieval-v1 "find memory"))))
    (is (= ["plain"
            (str "Instruct: " role/qwen-retrieval-instruction "\nQuery: find memory")]
           @seen))))

(ns hive-mcp.embeddings.ollama-test
  "The Ollama provider with its transport INJECTED as an :executor-fn (the
   provider's own port: the caller owns transport, no HTTP is touched). The
   golden pins construction, single and batch embeds, both executor error
   arms and the unknown-model refusal."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.embeddings.ollama :as ollama]
            [hive-mcp.embeddings.protocol :as proto]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- length-executor
  "Stub GPU executor: answers a 2-vector whose first coordinate is the text's
   length, so each input yields a distinct, checkable vector. Records requests."
  [seen]
  (fn [req]
    (swap! seen conj req)
    {:ok {:gpu/output {:vectors [[(double (count (get-in req [:gpu/payload :text]))) 0.5]]}}}))

(defn- provider [opts]
  (ollama/->provider (merge {:host "http://stub"} opts)))

(defn- ex-of [thunk]
  (try (thunk) :no-throw
       (catch clojure.lang.ExceptionInfo e [(ex-message e) (dissoc (ex-data e) :supported)])))

(deftest-golden executor-routed-provider-golden
  "test/golden/hive-mcp/embeddings/ollama-executor.edn"
  (let [seen (atom [])
        p    (provider {:model "nomic-embed-text" :executor-fn (length-executor seen)})
        big  (provider {:model "mxbai-embed-large" :executor-fn (length-executor seen)})
        decl (provider {:model "custom-embed" :declared {:dimension 12 :num-ctx 512}
                        :executor-fn (length-executor seen)})]
    {:record    (select-keys p [:host :model])
     :spec      (into {} (:spec p))
     :dims      [(proto/embedding-dimension p) (proto/embedding-dimension big)
                 (proto/embedding-dimension decl)]
     :one       (proto/embed-text p "abc")
     :batch     (proto/embed-batch p ["a" "bb" "cccc"])
     :empty     (proto/embed-batch p [])
     :requests  @seen
     :exec-err  (ex-of #(proto/embed-text (provider {:model "nomic-embed-text"
                                                     :executor-fn (constantly {:error :gpu/oom})})
                                          "x"))
     :non-result (ex-of #(proto/embed-text (provider {:model "nomic-embed-text"
                                                      :executor-fn (constantly {:weird 1})})
                                           "x"))
     :unknown   (ex-of #(provider {:model "no-such-model" :executor-fn identity}))}))

(deftest provider-validates-against-the-port-schema
  (is (m/validate proto/Provider
                  (provider {:model "nomic-embed-text" :executor-fn (length-executor (atom []))}))))

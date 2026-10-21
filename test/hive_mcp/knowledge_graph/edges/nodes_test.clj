(ns hive-mcp.knowledge-graph.edges.nodes-test
  "Node-level reads over the KG edge set, inside a fresh thread-local
   DataScript store (the KG store port injected by the harness), and the
   tag lookup over the in-memory Chroma transport + MockEmbedder (its
   cross-context port), including the loud degrade to [] when no embedding
   provider is configured."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.chroma.client :as client]
            [hive-mcp.chroma.connection :as chroma-conn]
            [hive-mcp.chroma.crud :as crud]
            [hive-mcp.config.test-support :as cfg]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.knowledge-graph.edges.m14-support :as sup]
            [hive-mcp.knowledge-graph.edges.nodes :as nodes]
            [hive-mcp.knowledge-graph.edges.write :as write]
            [hive-mcp.test-fixtures :as fixtures]
            [hive-mcp.test.stub.chroma-transport :as transport]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-golden node-reads-golden
  "test/golden/hive-mcp/knowledge-graph/edges-nodes-reads.edn"
  (sup/in-fresh-store
   (fn []
     (sup/seed! write/add-edge!)
     {:all        (sort (nodes/get-all-node-ids))
      :all-nil    (sort (nodes/get-all-node-ids nil))
      :scope-s1   (sort (nodes/get-all-node-ids "s1"))
      :scope-none (nodes/get-all-node-ids "no-such-scope")
      :n1-both    (sort (nodes/neighbors "n1"))
      :n3-in      (sort (nodes/neighbors "n3" :in))
      :n3-out     (nodes/neighbors "n3" :out) ; only a self-loop: excluded
      :n2-nil     (sort (nodes/neighbors "n2" nil))
      :bad-dir    (try (nodes/neighbors "n1" :sideways)
                       (catch clojure.lang.ExceptionInfo e (ex-data e)))})))

(defn- with-chroma
  "Run F with the in-memory transport and a mock provider (or none)."
  [provider f]
  (let [prior-t (client/transport)
        prior-p (active/get-embedding-provider)]
    (client/set-transport! (transport/->transport))
    (if provider (active/set-embedding-provider! provider) (active/reset-embedding-provider!))
    (chroma-conn/reset-collection-cache!)
    (try (cfg/with-config {:embedder {:no-embed-types #{} :routes {}}} (f))
         (finally
           (chroma-conn/reset-collection-cache!)
           (client/set-transport! prior-t)
           (if prior-p (active/set-embedding-provider! prior-p) (active/reset-embedding-provider!))))))

(deftest-golden node-ids-by-tag-golden
  "test/golden/hive-mcp/knowledge-graph/edges-nodes-by-tag.edn"
  {:served   (with-chroma (fixtures/->MockEmbedder 8)
               (fn []
                 (crud/index-memory-entry! {:id "e1" :content "a" :type "note" :tags ["x" "y"]})
                 (crud/index-memory-entry! {:id "e2" :content "b" :type "note" :tags ["y"]})
                 {:x (nodes/node-ids-by-tag "x")
                  :y (sort (nodes/node-ids-by-tag "y"))
                  :z (nodes/node-ids-by-tag "zz")}))
   :degraded (with-chroma nil #(nodes/node-ids-by-tag "x"))})

(deftest limit-is-the-documented-cap
  (is (= 10000 nodes/node-ids-by-tag-limit)))

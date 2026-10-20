(ns hive-mcp.chroma.search-trifecta-test
  "The pure where-clause builders as schema trifectas, and the search
   entry points under instrumentation against the in-memory transport."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.chroma.client :as client]
            [hive-mcp.chroma.connection :as conn]
            [hive-mcp.chroma.crud :as crud]
            [hive-mcp.chroma.search :as search]
            [hive-mcp.config.test-support :as cfg]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.test-fixtures :as fixtures]
            [hive-mcp.test.stub.chroma-transport :as transport]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private contracted
  #{#'search/build-where-clause #'search/build-where-document-clause
    #'search/search-similar #'search/search-by-id #'search/search-federated})

(defn- chroma-without-server [f]
  (let [prior-transport (client/transport)
        prior-provider  (active/get-embedding-provider)]
    (client/set-transport! (transport/->transport))
    (active/set-embedding-provider! (fixtures/->MockEmbedder 16))
    (conn/reset-collection-cache!)
    (mi/instrument! {:filters [(mi/-filter-var contracted)]})
    (try
      (cfg/with-config {:embedder {:no-embed-types #{} :routes {}}} (f))
      (finally
        (mi/unstrument! {:filters [(mi/-filter-var contracted)]})
        (conn/reset-collection-cache!)
        (client/set-transport! prior-transport)
        (active/set-embedding-provider! prior-provider)))))

(use-fixtures :each chroma-without-server)

(def ^:private Filters
  [:map
   [:type {:optional true} [:maybe [:enum "note" "decision" :note]]]
   [:project-ids {:optional true} [:maybe [:vector {:gen/max 3} [:enum "p1" "p2" "global"]]]]])

(deftrifecta-from-schema where-clause
  hive-mcp.chroma.search/build-where-clause
  {:in Filters
   :out search/WhereClause
   :rel (fn [{:keys [type project-ids]} out]
          (and (= type (:type out))
               (= (some-> project-ids vec) (get-in out [:project-id :$in]))
               (= (nil? out) (and (nil? type) (nil? project-ids)))))
   :golden-path "test/golden/chroma/build-where-clause.edn"
   :num-tests 200})

(deftrifecta-from-schema where-document-clause
  hive-mcp.chroma.search/build-where-document-clause
  {:in [:maybe [:vector {:gen/max 4} [:enum "a" "b" "c" "scope:x"]]]
   :out search/WhereDocumentClause
   :rel (fn [tags out]
          (case (count tags)
            0 (nil? out)
            1 (= {:$not_contains (first tags)} out)
            (= (mapv (fn [t] {:$not_contains t}) tags) (:$and out))))
   :golden-path "test/golden/chroma/build-where-document-clause.edn"
   :num-tests 200})

(deftest where-document-clause-arms
  (testing "each arm validates against itself and fails the other"
    (let [[one many] (m/children (m/schema [:or search/NotContains
                                            [:map {:closed true}
                                             [:$and [:vector {:min 2} search/NotContains]]]]))
          a (search/build-where-document-clause ["a"])
          b (search/build-where-document-clause ["a" "b"])]
      (is (m/validate one a))
      (is (not (m/validate many a)))
      (is (m/validate many b))
      (is (not (m/validate one b))))))

(deftest search-entry-points-honour-contracts
  (let [id (crud/index-memory-entry! {:type "note" :content "hello world"
                                      :tags ["a"] :project-id "p1"})]
    (testing "typed search answers the indexed record"
      (let [hits (search/search-similar "hello" :limit 3 :type "note")]
        (is (= [id] (mapv :id hits)))
        (is (number? (:distance (first hits))))))
    (testing "tag exclusion removes it"
      (is (empty? (search/search-similar "hello" :type "note" :exclude-tags ["Tags: a"]))))
    (testing "federated search with no ingest addon equals the default search"
      (is (= (mapv :id (search/search-similar "hello" :type "note"))
             (mapv :id (search/search-federated "hello" :type "note")))))
    (testing "search-by-id"
      (is (= id (:id (search/search-by-id id))))
      (is (nil? (search/search-by-id "missing"))))))

(ns hive-mcp.chroma.crud-contract-test
  "chroma.crud under instrumentation over the in-memory transport: a CRUD
   round trip pinned as a golden, both arms of collection-stats, and the
   staleness field mapping as a schema trifecta."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.chroma.client :as client]
            [hive-mcp.chroma.connection :as conn]
            [hive-mcp.chroma.crud :as crud]
            [hive-mcp.chroma.helpers :as h]
            [hive-mcp.config.test-support :as cfg]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.test-fixtures :as fixtures]
            [hive-mcp.test.stub.chroma-transport :as transport]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private contracted
  #{#'crud/index-memory-entry! #'crud/index-memory-entries! #'crud/get-entry-by-id
    #'crud/read-collections #'crud/query-entries #'crud/query-grounded-from
    #'crud/update-entry! #'crud/staleness-updates #'crud/update-staleness!
    #'crud/delete-entry! #'crud/find-duplicate #'crud/collection-stats
    #'crud/delete-all-entries!})

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

(deftrifecta-from-schema staleness-mapping
  hive-mcp.chroma.crud/staleness-updates
  {:in [:map
        [:staleness-alpha {:optional true} [:int {:min 0 :max 9}]]
        [:alpha {:optional true} [:int {:min 0 :max 9}]]
        [:staleness-source {:optional true} [:enum :direct :transitive]]
        [:source {:optional true} [:enum :direct :transitive]]
        [:depth {:optional true} [:int {:min 0 :max 3}]]]
   :out crud/StalenessUpdates
   :rel (fn [in out]
          (every? (fn [[port short]]
                    (= (get out port) (if (some? (get in port)) (get in port) (get in short))))
                  [[:staleness-alpha :alpha] [:staleness-source :source]
                   [:staleness-depth :depth] [:staleness-beta :beta]]))
   :golden-path "test/golden/chroma/crud-staleness-updates.edn"
   :num-tests 200})

(defn- round-trip
  "Each step bound in order: a large map literal does not evaluate its
   values in source order."
  []
  (let [hash        (h/content-hash "body one")
        id          (crud/index-memory-entry! {:type "note" :content "body one" :tags ["x" "y"]
                                               :content-hash hash :project-id "p1"
                                               :grounded-from "/disc/a.md"})
        batch-ids   (crud/index-memory-entries! [{:id "batch-1" :type "decision" :content "b1"}
                                                 {:id "batch-2" :type "decision" :content "b2"}])
        read-colls  (count (crud/read-collections "note"))
        got         (select-keys (crud/get-entry-by-id id) [:type :content :tags :project-id])
        missing     (crud/get-entry-by-id "no-such-id")
        queried     (sort (map :content (crud/query-entries :type "decision" :limit 10)))
        grounded    (mapv :content (crud/query-grounded-from "/disc/a.md"))
        updated     (:tags (crud/update-entry! id {:tags ["z"]}))
        update-miss (crud/update-entry! "no-such-id" {:tags ["z"]})
        stale       (select-keys (crud/update-staleness! id {:alpha 3 :beta 4})
                                 [:staleness-alpha :staleness-beta])
        dup?        (= id (:id (crud/find-duplicate "note" hash)))
        stats       (crud/collection-stats)
        deleted?    (= id (crud/delete-entry! id))
        removed     (crud/delete-all-entries!)
        after       (crud/collection-stats)]
    {:batch-ids batch-ids :read-colls read-colls :got got :missing missing
     :queried (vec queried) :grounded grounded :updated updated
     :update-miss update-miss :stale stale :dup? dup? :stats stats
     :deleted? deleted? :removed removed :after after}))

(deftest-golden crud-round-trip
  "test/golden/chroma/crud-round-trip.edn"
  (round-trip))

(deftest collection-stats-arms
  (let [out-schema (-> (m/function-schemas) (get 'hive-mcp.chroma.crud)
                       (get 'collection-stats) :schema m/form last)
        [ok-arm err-arm] (m/children (m/schema out-schema))]
    (testing "a reachable store answers counts"
      (let [st (crud/collection-stats)]
        (is (m/validate ok-arm st))
        (is (not (m/validate err-arm st)))))
    (testing "a failing transport answers the error arm"
      (client/set-transport! (reify client/IChromaTransport
                               (-configure [_ _] nil)
                               (-get-collection [_ _] (throw (ex-info "down" {})))
                               (-create-collection [_ _ _] (throw (ex-info "down" {})))
                               (-delete-collection [_ _] nil)
                               (-add [_ _ _ _] nil)
                               (-get [_ _ _] (throw (ex-info "down" {})))
                               (-query [_ _ _ _] nil)
                               (-delete [_ _ _] nil)
                               (-update [_ _ _] nil)))
      (conn/reset-collection-cache!)
      (let [st (crud/collection-stats)]
        (is (m/validate err-arm st))
        (is (not (m/validate ok-arm st)))))))

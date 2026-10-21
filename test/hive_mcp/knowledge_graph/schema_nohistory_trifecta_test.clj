(ns hive-mcp.knowledge-graph.schema-nohistory-trifecta-test
  "Lock the derived-only noHistory policy without opening a persistent store."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.knowledge-graph.schema :as schema]
            [hive-test.trifecta :refer [deftrifecta]]))

(def derived-attrs
  #{:knowledge/grounded-at :knowledge/grounded-from :knowledge/source-hash
    :disc/content-hash :disc/analyzed-at :disc/last-read-at :disc/read-count
    :disc/certainty-alpha :disc/certainty-beta :disc/last-observation})

(def retained-attrs
  #{:knowledge/abstraction-level :knowledge/gaps :knowledge/source-type
    :disc/path :disc/git-commit :disc/project-id :disc/volatility-class})

(defn no-history?
  "Read the actual combined schema, not a copy of its attribute policy."
  [attr]
  (true? (get-in (schema/full-schema) [attr :db/noHistory])))

(deftest derived-history-policy
  (is (every? no-history? derived-attrs))
  (is (not-any? no-history? retained-attrs))
  (is (= :db.cardinality/many
         (get-in (schema/full-schema) [:knowledge/gaps :db/cardinality])))
  (is (= :db.unique/identity
         (get-in (schema/full-schema) [:disc/path :db/unique]))))

(deftrifecta derived-history-policy-trifecta
  hive-mcp.knowledge-graph.schema-nohistory-trifecta-test/no-history?
  {:golden-path "test/golden/hive-mcp/schema-nohistory.edn"
   :cases {:grounded-at :knowledge/grounded-at
           :source-hash :knowledge/source-hash
           :content-hash :disc/content-hash
           :certainty :disc/certainty-alpha
           :last-observation :disc/last-observation
           :identity :disc/path
           :audit-source :knowledge/source-type}
   :gen (gen/elements (vec (concat derived-attrs retained-attrs)))
   :pred boolean?
   :num-tests 100
   :mutations [["always-retain" (fn [_] false)]
               ["always-discard" (fn [_] true)]]})

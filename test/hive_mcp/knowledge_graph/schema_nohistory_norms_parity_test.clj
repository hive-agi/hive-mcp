(ns hive-mcp.knowledge-graph.schema-nohistory-norms-parity-test
  "schema.clj declares :db/noHistory for the datascript/datalevin paths; a
   datahike store only gets it through the filename-keyed norms under
   resources/hive_mcp/norms/kg. Lock the two together so a derived attr cannot
   be flagged in source yet keep accumulating history on a live datahike KG
   (KG-MEM-NOHISTORY-AUDIT, kanban 20260705003615-24e79445)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.knowledge-graph.schema :as schema]))

(def ^:private norms-dir "hive_mcp/norms/kg")

(defn- norm-tx-data
  "Every tx-data map of every norm file, in filename order."
  []
  (let [dir (io/file (io/resource norms-dir))]
    (->> (.listFiles dir)
         (filter #(.endsWith (.getName ^java.io.File %) ".edn"))
         (sort-by #(.getName ^java.io.File %))
         (mapcat #(:tx-data (edn/read-string (slurp %)))))))

(defn- norm-no-history-attrs []
  (into #{} (comp (filter :db/noHistory) (map :db/ident)) (norm-tx-data)))

(def ^:private retained-attrs
  "Source-of-truth or audit attrs: their history is the record, never flag them."
  #{:knowledge/abstraction-level :knowledge/gaps :knowledge/source-type
    :disc/path :disc/git-commit :disc/project-id :disc/volatility-class
    :kg-edge/predicate})

(def ^:private norm-010-attrs
  #{:knowledge/grounded-at :knowledge/grounded-from :knowledge/source-hash
    :disc/content-hash :disc/analyzed-at :disc/last-read-at :disc/read-count
    :disc/certainty-alpha :disc/certainty-beta :disc/last-observation
    :kg-edge/schema-text :kg-edge/schema-head :kg-edge/schema-subject})

(deftest every-source-no-history-attr-has-an-installing-norm
  (let [declared (into #{} (comp (filter (fn [[_ m]] (:db/noHistory m))) (map key))
                       (merge schema/kg-schema schema/knowledge-schema
                              schema/disc-schema schema/synth-schema))
        installed (norm-no-history-attrs)]
    (testing "no attr is noHistory in schema.clj but history-tracked on datahike"
      (is (empty? (remove installed declared))))))

(deftest norm-010-flags-exactly-the-derived-attrs
  (let [n010 (->> (io/resource (str norms-dir "/010-knowledge-disc-nohistory.edn"))
                  slurp edn/read-string :tx-data)]
    (is (= norm-010-attrs (set (map :db/ident n010))))
    (testing "alter-only form: noHistory and nothing else, so no index backfill
              or type change can refuse it on an existing store"
      (is (every? #(= #{:db/ident :db/noHistory} (set (keys %))) n010))
      (is (every? #(true? (:db/noHistory %)) n010)))))

(deftest retained-attrs-never-lose-history
  (is (empty? (filter (norm-no-history-attrs) retained-attrs)))
  (is (not-any? #(get-in (schema/full-schema) [% :db/noHistory]) retained-attrs)))

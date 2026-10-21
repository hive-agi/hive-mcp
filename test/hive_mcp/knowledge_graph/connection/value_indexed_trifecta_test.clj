(ns hive-mcp.knowledge-graph.connection.value-indexed-trifecta-test
  "Trifecta for `connection.avet/value-indexed?`: whether THIS database keeps
   an attribute in its value-first index. datalevin indexes every attribute;
   datascript and datahike only one whose schema carries :db/index or
   :db/unique. Assuming the index exists (the always-indexed mutant) is what
   broke the decay walk on a database created before norm 010."
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.knowledge-graph.connection.avet]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def value-indexed?
  "The private subject, re-bound so the trifecta can call it."
  @#'hive-mcp.knowledge-graph.connection.avet/value-indexed?)

(def ^:private gen-attr-schema
  (gen/one-of [(gen/return nil)
               (gen/fmap (fn [[i u c]]
                           (cond-> {}
                             i (assoc :db/index true)
                             u (assoc :db/unique :db.unique/identity)
                             c (assoc :db/cardinality :db.cardinality/one)))
                         (gen/tuple gen/boolean gen/boolean gen/boolean))]))

(def ^:private gen-args
  (gen/fmap (fn [[engine s]] [engine {:schema (if s {:k/a s} {})} :k/a])
            (gen/tuple (gen/elements ["datascript.db" "datahike.db" "datalevin.db"])
                       gen-attr-schema)))

(defn- oracle
  "Expected answer, stated independently of the subject."
  [engine db attr]
  (let [s (get (:schema db) attr)]
    (or (= "datalevin.db" engine) (contains? s :db/index) (contains? s :db/unique))))

(deftrifecta value-indexed-trifecta
  #'hive-mcp.knowledge-graph.connection.value-indexed-trifecta-test/value-indexed?
  {:golden-path "test/golden/knowledge_graph/value-indexed.edn"
   :apply?      true
   :cases       {:datalevin-unschemed ["datalevin.db" {:schema {}} :k/a]
                 :datahike-indexed    ["datahike.db" {:schema {:k/a {:db/index true}}} :k/a]
                 :datahike-unique     ["datahike.db" {:schema {:k/a {:db/unique :db.unique/identity}}} :k/a]
                 :datahike-unindexed  ["datahike.db" {:schema {:k/a {:db/cardinality :db.cardinality/one}}} :k/a]
                 :datascript-absent   ["datascript.db" {:schema {}} :k/a]
                 :datascript-indexed  ["datascript.db" {:schema {:k/a {:db/index true}}} :k/a]}
   :gen         gen-args
   :pred        boolean?
   :num-tests   300
   :mutations   [["always-indexed" (constantly true)]
                 ["ignores-unique" (fn [engine db attr]
                                     (or (= "datalevin.db" engine)
                                         (boolean (:db/index (get (:schema db) attr)))))]
                 ["datalevin-not-special" (fn [_ db attr]
                                            (let [s (get (:schema db) attr)]
                                              (boolean (or (:db/index s) (:db/unique s)))))]]})

(defspec value-indexed-agrees-with-the-oracle 300
  (prop/for-all [[engine db attr] gen-args]
    (= (oracle engine db attr) (value-indexed? engine db attr))))

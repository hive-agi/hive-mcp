(ns hive-mcp.tools.memory.crud.query-project-id-tag-trifecta-test
  "Golden + property + mutation pinning for the `project-id:<id>` query tag.

   `memory query tags=[\"topic:microservices\"]` answered [] although the
   entries carry :project-id \"topic:microservices\" (kanban
   20260519145016-4561f6bc, issue 3): the project-id is a field, not a tag.
   A `project-id:<id>` tag is now read as a :project-id predicate, split off
   the stored-tag predicates before the store query.

   Mutants are self-contained and never call the subject var."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [clojure.data.json :as json]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-test.isolation :as iso]
            [hive-mcp.isolation-methods]
            [hive-spi.memory.ports :as ports]
            [hive-mcp.test.stub.memory-store :as mem-stub]
            [hive-mcp.tools.memory.crud.query :as q]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private tag-pool
  ["kind:axiom" "note" "project-id:topic:a" "project-id:topic:b" "project-id:" "scope:project:topic:a"])

(def ^:private gen-tags
  (gen/vector (gen/elements tag-pool) 0 5))

(defn- sound-split? [{:keys [tags project-ids]}]
  (and (vector? tags) (vector? project-ids)
       (not-any? #(str/starts-with? % "project-id:") tags)
       (every? (complement str/blank?) project-ids)
       (= project-ids (distinct project-ids))))

(deftrifecta split-project-id-tags-contract
  hive-mcp.tools.memory.crud.query/split-project-id-tags
  {:golden-path "test/golden/tools/memory/split-project-id-tags.edn"
   :cases       {:empty            []
                 :nil-tags         nil
                 :plain-only       ["kind:axiom" "note"]
                 :pid-only         ["project-id:topic:microservices"]
                 :mixed            ["kind:axiom" "project-id:topic:a" "note"]
                 :blank-id-dropped ["project-id:" "note"]
                 :dedup            ["project-id:topic:a" "project-id:topic:a" "project-id:topic:b"]}
   :gen         gen-tags
   :pred        sound-split?
   :num-tests   200
   :mutations   [["no split — the original bug, the pid tag stays a stored-tag predicate"
                  (fn [tags] {:tags (vec tags) :project-ids []})]
                 ["keep prefix — project-id field compared with the whole tag"
                  (fn [tags] {:tags (vec (remove #(str/starts-with? % "project-id:") tags))
                              :project-ids (vec (distinct (filter #(str/starts-with? % "project-id:") tags)))})]
                 ["drop the plain tags"
                  (fn [tags] {:tags []
                              :project-ids (into [] (comp (filter #(str/starts-with? % "project-id:"))
                                                          (map #(subs % 11))
                                                          (remove str/blank?)
                                                          (distinct))
                                                 tags)})]]
   :assert      (fn []
                  (is (= {:tags ["kind:axiom"] :project-ids ["topic:microservices"]}
                         (q/split-project-id-tags ["project-id:topic:microservices" "kind:axiom"])))
                  (is (= {:tags ["scope:project:topic:a"] :project-ids []}
                         (q/split-project-id-tags ["scope:project:topic:a"]))
                      "a scope tag is a stored tag, not a project-id predicate"))})

(deftest apply-project-id-filter-contract
  (let [entries [{:id "a" :project-id "topic:a"}
                 {:id "b" :project-id "topic:b"}
                 {:id "c"}]]
    (testing "empty predicate keeps every entry"
      (is (= ["a" "b" "c"] (mapv :id (q/apply-project-id-filter [] entries))))
      (is (= ["a" "b" "c"] (mapv :id (q/apply-project-id-filter nil entries)))))
    (testing "only entries whose :project-id is named survive"
      (is (= ["a"] (mapv :id (q/apply-project-id-filter ["topic:a"] entries))))
      (is (= ["a" "b"] (mapv :id (q/apply-project-id-filter ["topic:b" "topic:a"] entries)))))))

;; ============================================================
;; handle-query through the IMemoryStore port (stub, no live store)
;; ============================================================

(def ^:dynamic *store* nil)

(defn- with-observing-store [f]
  (mem-stub/with-stub-store
    (fn []
      (let [store (mem-stub/->observing (mem-stub/->stub))]
        (mem-stub/install! store)
        (binding [*store* store] (f))))))

(use-fixtures :each with-observing-store (iso/with-isolations :kg-conn))

(defn- seed! [entry]
  (ports/add-entry! (:inner *store*) entry))

(defn- result-ids [resp]
  (->> (json/read-str (or (:text resp) (get-in resp [:content 0 :text])) :key-fn keyword)
       (mapv :id)
       sort
       vec))

(deftest handle-query-matches-bare-project-id
  (seed! {:id "ms-1" :type "ingestion" :content "saga" :tags ["kind:axiom" "scope:project:topic:microservices"]
          :project-id "topic:microservices" :duration "long" :expires ""})
  (seed! {:id "ddia-1" :type "ingestion" :content "raft" :tags ["kind:axiom" "scope:project:topic:data-intensive"]
          :project-id "topic:data-intensive" :duration "long" :expires ""})
  (testing "project-id:<id> selects by the entry's :project-id field, regardless of pwd"
    (let [resp (q/handle-query {:type "ingestion" :tags ["project-id:topic:microservices"]
                                :directory "/tmp"})]
      (is (not (:isError resp)))
      (is (= ["ms-1"] (result-ids resp)))))
  (testing "composes with stored-tag predicates"
    (let [resp (q/handle-query {:tags ["project-id:topic:data-intensive" "kind:axiom"]
                                :directory "/tmp"})]
      (is (= ["ddia-1"] (result-ids resp)))))
  (testing "an unknown project-id answers empty, not everything"
    (let [resp (q/handle-query {:tags ["project-id:topic:none"] :directory "/tmp"})]
      (is (= [] (result-ids resp))))))

(ns hive-mcp.tools.catchup.memory-stub-conformance-test
  "The stub memory store answers the query shapes catchup sends exactly as
   the port's reference semantics do, so catchup tests run against it prove
   catchup, not the stub.

   Reference: `hive-spi.memory.entry/query` (filters :type, :project-id,
   :project-ids OR, :tags AND; :order-by; :limit), the production temporal
   rule `hive-mcp.tools.memory.crud.query/entry-after?` for :created-after
   (strict ISO-8601 string `>`, the comparison the Milvus adapter pushes as
   `created > \"...\"`) applied before the limit, and the Chroma adapter's
   `project-fields` for :output-fields (backend columns to kebab keys).

   Out of scope: the reference, like the stub, sorts by :order-by before
   cutting to :limit. Milvus has no server-side ORDER BY and cuts in scan
   order, so fetch-window saturation is not reproduced here.

   Golden: catchup's own query shapes over the fixture entries. Property:
   generated entries and query options. Mutants of the stub's filter must be
   caught."
  (:require [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.memory.store.chroma :as chroma-store]
            [hive-mcp.test.stub.memory-query :as mq]
            [hive-mcp.test.stub.memory-store :as stub]
            [hive-mcp.tools.catchup.hcr-fixture :as fx]
            [hive-mcp.tools.catchup.hierarchy :as hier]
            [hive-mcp.tools.memory.crud.query :as crud-query]
            [hive-spi.memory.entry :as entry]
            [hive-spi.memory.ports :as ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- reference
  "Rows the port contract answers for OPTS over ENTRIES."
  [entries {:keys [created-after output-fields] :as opts}]
  (let [in-range (filter #(crud-query/entry-after? % :created created-after) entries)
        rows     (entry/query in-range (dissoc opts :created-after :output-fields))]
    (if (seq output-fields) (chroma-store/project-fields rows output-fields) rows)))

(defn- ids-of [opts rows]
  (let [ids (map :id rows)]
    (vec (if (:order-by opts) ids (sort ids)))))

(defn- both*
  "Seed a stub with ENTRIES, then answer OPTS from the stub and from the
   reference over the entries as the stub stored them."
  [[entries opts :as in]]
  (let [store     (stub/->stub entries)
        stub-rows (ports/query-entries store opts)
        ref-rows  (reference (vals (stub/entries store)) opts)]
    {:in in
     :stub {:ids (ids-of opts stub-rows) :rows (set stub-rows)}
     :reference {:ids (ids-of opts ref-rows) :rows (set ref-rows)}}))

(defn both
  "[entries opts] -> {:in :stub :reference}, rows and ids from each side."
  [in]
  (both* in))

(defn- agreed [{:keys [stub reference]}]
  (if (= stub reference) (:ids stub) {:diverged {:stub (:ids stub) :reference (:ids reference)}}))

(def ^:private real-matches? @#'mq/matches?)

(defn- stub-under
  "Probe mutant: `both` with the stub's filter replaced by F."
  [f]
  (fn [in]
    (try
      (alter-var-root #'mq/matches? (constantly f))
      (both* in)
      (finally (alter-var-root #'mq/matches? (constantly real-matches?))))))

(defn- minutes-ago [m] (str (.minusSeconds (java.time.Instant/now) (long (* 60 m)))))

(def ^:private meta-fields hier/metadata-projection)

(defn- fixture-case [opts] [(fx/memory-entries) opts])

(def ^:private gen-opts
  (gen/let [type     (gen/elements [nil "axiom" "principle" "note"])
            pid      (gen/elements [nil "a" "global"])
            pids     (gen/elements [nil ["a"] ["a" "ws" "a1"] ["far" "b"]])
            tags     (gen/elements [nil ["catchup-priority"] ["scope:global"] ["catchup-priority" "scope:global"]])
            limit    (gen/choose 1 30)
            fields   (gen/elements [nil meta-fields])
            after    (gen/elements [nil 5 12])]
    (cond-> {:limit limit :order-by [:created :desc]}
      type   (assoc :type type)
      pid    (assoc :project-id pid)
      pids   (assoc :project-ids pids)
      tags   (assoc :tags tags)
      fields (assoc :output-fields fields)
      after  (assoc :created-after (minutes-ago after)))))

(def ^:private gen-entries
  (gen/fmap (fn [specs]
              (vec (map-indexed (fn [i [type pid tags]]
                                  {:id (str "m" i) :type type :project-id pid :tags tags
                                   :content (str "m" i) :created (minutes-ago (inc i))})
                                specs)))
            (gen/vector (gen/tuple (gen/elements ["axiom" "principle" "note" "decision"])
                                   (gen/elements ["a" "ws" "a1" "b" "far" "global"])
                                   (gen/vector (gen/elements ["catchup-priority" "scope:global"
                                                              "scope:project:a" "session-summary"])
                                               0 3))
                        0 20)))

(deftrifecta stub-store-answers-catchup-queries-like-the-port
  hive-mcp.tools.catchup.memory-stub-conformance-test/both
  {:golden-path "test/golden/catchup/memory-stub-conformance.edn"
   :cases {:axioms-scan         (fixture-case {:type "axiom" :limit 200 :output-fields meta-fields})
           :descendant-scan     (fixture-case {:project-ids ["a"] :limit 50 :output-fields meta-fields})
           :hierarchy-in-list   (fixture-case {:project-ids ["a" "ws" "a1"] :limit 50})
           :global-pierce       (fixture-case {:project-id "global" :limit 200 :output-fields meta-fields})
           :priority-principles (fixture-case {:type "principle" :tags ["catchup-priority"] :limit 500
                                               :output-fields meta-fields})
           :tags-are-and        (fixture-case {:tags ["catchup-priority" "scope:global"] :limit 100})
           :limit-cuts-newest   (fixture-case {:type "axiom" :limit 3 :order-by [:created :desc]})
           :fresh-sessions      (fixture-case {:type "note" :tags ["session-summary"] :limit 25
                                               :created-after (minutes-ago 33.5)
                                               :order-by [:created :desc] :output-fields meta-fields})}
   :xf agreed
   :gen (gen/tuple gen-entries gen-opts)
   :pred (fn [{:keys [stub reference]}] (= stub reference))
   :num-tests 150
   :mutations [["tags-not-anded"        (stub-under (fn [e opts now]
                                                      (real-matches? e (cond-> opts (:tags opts)
                                                                         (assoc :tags (take 1 (:tags opts))))
                                                                     now)))]
               ["project-ids-ignored"   (stub-under (fn [e opts now] (real-matches? e (dissoc opts :project-ids) now)))]
               ["created-after-ignored" (stub-under (fn [e opts now] (real-matches? e (dissoc opts :created-after) now)))]]})

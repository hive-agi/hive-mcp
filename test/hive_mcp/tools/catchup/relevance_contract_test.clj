(ns hive-mcp.tools.catchup.relevance-contract-test
  "Schema-derived trifectas for catchup.relevance (pure)."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.catchup.relevance :as rel]
            [hive-mcp.tools.catchup.shapes :as shapes]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private TagsIn
  "Tags mixing noise markers, compound domain tags and plain ones."
  [:vector {:gen/max 5}
   [:enum "agent:ling-1" "scope:project:p" "axiom" "kanban" "priority-high"
    "hive-knowledge" "clojure" "malli_schema" "java.memory" "typography" "catchup-priority"]])

(deftrifecta-from-schema topic-tags-drops-noise
  hive-mcp.tools.catchup.relevance/topic-tags
  {:in              [:cat TagsIn]
   :out             rel/Vocabulary
   :rel             (fn [[tags] out]
                      (and (not-any? #(contains? out %) ["agent:ling-1" "scope:project:p" "axiom" "kanban" "priority-high"])
                           (every? #(contains? out %)
                                   (remove #(or (str/includes? % ":") (#{"axiom" "kanban"} %)
                                                (str/starts-with? % "priority-"))
                                           tags))))
   :golden-path     "test/golden/catchup/relevance-topic-tags.edn"
   :num-tests       200})

(def ^:private ProjectIdIn
  [:maybe [:enum "hive-mcp" "funeraria" "global" "hive-knowledge-base" "a-b-c"]])

(deftrifecta-from-schema project-keywords-derivation
  hive-mcp.tools.catchup.relevance/project-keywords
  {:in          [:cat ProjectIdIn]
   :out         rel/ProjectKeywords
   :rel         (fn [[pid] out]
                  (if (or (nil? pid) (= "global" pid))
                    (= #{} out)
                    (and (contains? out pid)
                         (set/subset? (disj (set (str/split pid #"-")) "hive") out))))
   :golden-path "test/golden/catchup/relevance-project-keywords.edn"
   :num-tests   100})

(def ^:private CtxIn
  [:map
   [:project-id [:maybe [:enum "p" "hive-mcp"]]]
   [:project-keywords [:set {:gen/max 3} [:enum "hive-mcp" "mcp" "clojure"]]]
   [:vocabulary [:set {:gen/max 3} [:enum "malli" "typography" "knowledge"]]]])

(def ^:private ScoredEntry
  [:map [:tags [:vector {:gen/max 4}
                [:enum "catchup-priority" "scope:project:p" "scope:project:hive-mcp" "mcp"
                 "clojure" "malli" "typography" "windows-ntlm" "axiom"]]]])

(defn- reference-score [{:keys [tags]} {:keys [project-id project-keywords vocabulary]}]
  (let [ts (set tags)]
    (if (or (ts "catchup-priority") (and project-id (ts (str "scope:project:" project-id))))
      1.0
      (let [n (count (set/intersection (rel/topic-tags ts) (set/union project-keywords vocabulary)))]
        (cond (>= n 2) 1.0 (= n 1) 0.5 :else 0.0)))))

(deftrifecta-from-schema axiom-relevance-steps
  hive-mcp.tools.catchup.relevance/axiom-relevance
  {:in              [:cat ScoredEntry CtxIn]
   :out             rel/Score
   :rel             (fn [[e ctx] out] (= out (reference-score e ctx)))
   :classify        (fn [_ out] out)
   :classify-domain #{0.0 0.5 1.0}
   :classify-floor  10
   :golden-path     "test/golden/catchup/relevance-axiom-relevance.edn"
   :num-tests       300})

(deftrifecta-from-schema filter-by-relevance-threshold
  hive-mcp.tools.catchup.relevance/filter-by-relevance
  {:in  [:cat [:vector {:gen/max 5} ScoredEntry] CtxIn [:enum 0.0 0.3 0.5 1.0]]
   :out [:vector ScoredEntry]
   :rel (fn [[es ctx th] out]
          (= out (filterv #(>= (reference-score % ctx) th) es)))
   :num-tests 200})

(deftrifecta-from-schema build-context-assembles
  hive-mcp.tools.catchup.relevance/build-context
  {:in  [:cat [:map [:project-id [:maybe [:enum "p" "hive-mcp"]]]
               [:co-loaded-entries [:vector {:gen/max 3} ScoredEntry]]]]
   :out [:map {:closed true}
         [:project-id [:maybe :string]] [:project-keywords rel/ProjectKeywords] [:vocabulary rel/Vocabulary]]
   :rel (fn [[{:keys [project-id co-loaded-entries]}] out]
          (and (= project-id (:project-id out))
               (= (rel/project-keywords project-id) (:project-keywords out))
               (= (rel/context-vocabulary co-loaded-entries) (:vocabulary out))))
   :num-tests 100})

(deftest context-vocabulary-unions-entry-tags
  (is (= #{"malli" "clojure"} (rel/context-vocabulary [{:tags ["malli" "axiom"]} {:tags ["clojure"]}])))
  (is (= #{} (rel/context-vocabulary nil))))

(deftest project-keywords-separator-only-id-bug
  ;; BUG (recorded, not fixed): an id of only separators yields a set with nil.
  (is (= #{nil "-"} (rel/project-keywords "-"))))

(deftest default-threshold-drops-zero-overlap
  (let [ctx (rel/build-context {:project-id "hive-mcp"})]
    (is (= [{:tags ["mcp"]}] (rel/filter-by-relevance [{:tags ["mcp"]} {:tags ["windows-ntlm"]}] ctx)))))

(deftest entry-schema-is-the-shared-one
  (is (some? shapes/Entry)))

(ns hive-mcp.agent.hints.domain-trifecta-test
  "Property + golden + mutation facets for the pure hint-domain calculations,
   derived from their malli contracts (hive-schemas.test)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.agent.hints.domain :as d]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Id
  "A whitespace-free token, as a memory id is."
  [:string {:min 1 :gen/fmap (fn [s] (apply str "i" (filter #(Character/isLetterOrDigit ^char %) s)))}])

(def Ids [:vector {:max 12} Id])

(defn- roundtrips?
  [ids out]
  (if (empty? ids)
    (nil? out)
    (= ids (d/parse-memory-ids (str/join "\n" out)))))

(deftrifecta-from-schema read-id-section-trifecta
  hive-mcp.agent.hints.domain/read-id-section
  {:in Ids
   :out d/Section
   :rel roundtrips?
   :golden-path "test/golden/agent/hints-read-id-section.edn"
   :num-tests 200})

(deftrifecta-from-schema axiom-section-trifecta
  hive-mcp.agent.hints.domain/axiom-section
  {:in Ids
   :out d/Section
   :rel (fn [ids out]
          (if (empty? ids)
            (nil? out)
            (= (vec (take 10 ids)) (d/parse-axiom-ids (str/join "\n" out)))))
   :golden-path "test/golden/agent/hints-axiom-section.edn"
   :num-tests 200})

(deftrifecta-from-schema parse-memory-ids-trifecta
  hive-mcp.agent.hints.domain/parse-memory-ids
  {:in [:maybe :string]
   :out [:maybe [:vector :string]]
   :rel (fn [text out]
          (every? (fn [id] (and (seq id) (not (re-find #"\s" id)))) out))
   :golden-path "test/golden/agent/hints-parse-memory-ids.edn"})

(deftrifecta-from-schema meaningful-tags-trifecta
  hive-mcp.agent.hints.domain/meaningful-tags
  {:in d/Strings
   :out [:vector :string]
   :rel (fn [tags out]
          (and (every? #(not (or (str/starts-with? % "scope:")
                                 (str/starts-with? % "agent:")
                                 (= % "catchup-priority")))
                       out)
               (= (count out)
                  (count (remove #(or (str/starts-with? % "scope:")
                                      (str/starts-with? % "agent:")
                                      (= % "catchup-priority"))
                                 tags)))))
   :golden-path "test/golden/agent/hints-meaningful-tags.edn"})

(deftrifecta-from-schema query-section-trifecta
  hive-mcp.agent.hints.domain/query-section
  {:in [:vector {:max 6} [:string {:min 1 :gen/fmap (fn [s] (apply str "q" (filter #(Character/isLetterOrDigit ^char %) s)))}]]
   :out d/Section
   :rel (fn [qs out]
          (if (empty? qs)
            (nil? out)
            (= qs (d/parse-search-queries (str/join "\n" out)))))
   :golden-path "test/golden/agent/hints-query-section.edn"})

(deftest or-val-nil-coalesces
  (is (= :d (d/or-val nil :d)))
  (is (= :d (d/or-val false :d)))
  (is (= 0 (d/or-val 0 :d))))

(deftest kg-section-roundtrips-depth
  (let [out (d/kg-section ["a" "b"] 3)]
    (is (= {:seeds ["a" "b"] :depth 3} (d/parse-kg-traversals (str/join "\n" out))))
    (is (= 2 (:depth (d/parse-kg-traversals (str/join "\n" (d/kg-section ["a"] nil))))))
    (is (nil? (d/kg-section [] 2)))))

(deftest tag-section-roundtrips
  (is (= [["a" "b"] ["c"]] (d/parse-tag-queries (str/join "\n" (d/tag-section [["a" "b"] ["c"]])))))
  (is (nil? (d/tag-section []))))

(deftest git-section-renders-flags
  (is (= ["### Git Status" "- **Branch**: main" "- **Uncommitted changes**: yes"
          "- **Last commit**: abc" ""]
         (d/git-section {:branch "main" :uncommitted true :last-commit "abc"})))
  (is (nil? (d/git-section nil))))

(deftest scope-visible-checks-tags-and-project
  (is (d/scope-visible? #{"scope:x"} #{} {:tags ["scope:x"]}))
  (is (d/scope-visible? #{} #{"p"} {:project-id "p"}))
  (is (not (d/scope-visible? #{"scope:x"} #{"p"} {:tags ["other"] :project-id "q"}))))

(golden/deftest-golden-fn hint-text-parsers-golden
  "test/golden/agent/hints-text-parsers.edn"
  (fn []
    (let [text (str/join "\n" (concat (d/axiom-section ["a1" "a2"])
                                      (d/read-id-section ["r1" "r2" "r3" "r4" "r5" "r6"])
                                      (d/query-section ["alpha" "beta"])
                                      (d/tag-section [["x" "y"] ["z"]])
                                      (d/kg-section ["s1" "s2"] 4)))]
      {:memory-ids     (d/parse-memory-ids text)
       :axiom-ids      (d/parse-axiom-ids text)
       :queries        (d/parse-search-queries text)
       :tags           (d/parse-tag-queries text)
       :kg             (d/parse-kg-traversals text)
       :meaningful     (d/meaningful-tags ["scope:p" "agent:a" "catchup-priority" "keep" "also"])
       :nothing        [(d/parse-memory-ids nil) (d/parse-kg-traversals "")]})))

(deftest-contract-coverage hints-domain-contract-coverage
  {:paths ["src/hive_mcp/agent/hints/domain.clj"]})

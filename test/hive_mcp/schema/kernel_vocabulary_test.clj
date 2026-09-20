;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.schema.kernel-vocabulary-test
  "What the kernel may say about a memory TYPE and a KG RELATION when no
   domain is mounted.

   The split under test: a type TOKEN's safety is kernel (it guards filter
   expressions, EDN persistence and keyword interning), the type TAXONOMY is
   the memory domain's, and the relation VOCABULARY is the knowledge graph's.
   So `safe-type?` answers with no domain present, while the relation schema
   opens up rather than validating against an enum nobody defines."
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [hive-mcp.memory.type-registry :as type-registry]
            [hive-mcp.schema.memory :as mem-schema]
            [hive-mcp.schema.tools :as tools-schema]
            [hive-mcp.schema.type-token :as token]
            [hive-mcp.swarm.adapters.soft :as soft]))

(deftest a-type-token-is-judged-without-any-taxonomy
  (testing "safe tokens"
    (is (token/safe-type? "decision"))
    (is (token/safe-type? :axiom))
    (is (token/safe-type? "my-own-type_2"))
    (is (token/safe-type? (apply str (repeat token/max-type-length "a")))))
  (testing "unsafe tokens, each a real hazard rather than a style rule"
    (is (not (token/safe-type? "type with spaces")))
    (is (not (token/safe-type? "type\"quote")))
    (is (not (token/safe-type? "../path")))
    (is (not (token/safe-type? "2leading-digit")))
    (is (not (token/safe-type? (apply str (repeat (inc token/max-type-length) "a")))))
    (is (not (token/safe-type? "")))
    (is (not (token/safe-type? 123)))))

(deftest the-memory-schema-validates-a-token-not-a-taxonomy
  (is (m/validate mem-schema/MemoryType "decision"))
  (is (m/validate mem-schema/MemoryType "a-type-no-registry-has-heard-of"))
  (is (not (m/validate mem-schema/MemoryType "no spaces allowed"))))

(deftest the-requested-type-marker-round-trips
  (let [tag (token/requested-type-tag "axiom")]
    (is (= "requested-type:axiom" tag))
    (is (= "axiom" (token/requested-type-of ["scope:global" tag "other"])))
    (is (nil? (token/requested-type-of ["scope:global" "axiom"])))
    (is (nil? (token/requested-type-tag "not a token")))))

(deftest the-old-names-still-answer-through-the-taxonomy-namespace
  (testing "the memory domain keeps its names; they call through the kernel var"
    (is (= (token/sanitize-type "  Pattern ") (type-registry/sanitize-type "  Pattern ")))
    (is (= (token/safe-type? "decision") (type-registry/safe-type? "decision")))
    (is (= token/max-type-length type-registry/max-type-length))
    (is (= (token/requested-type-of ["requested-type:axiom"])
           (type-registry/requested-type-of ["requested-type:axiom"])))))

(deftest the-relation-schema-is-an-enum-with-the-graph-and-open-without-it
  (testing "with the KG domain present"
    (let [schema (tools-schema/KGRelationType)]
      (is (= :enum (first schema)))
      (is (m/validate schema "implements"))
      (is (not (m/validate schema "not-a-relation")))))
  (testing "with the KG domain gone, any non-blank string passes"
    (binding [soft/*resolve* (constantly nil)]
      (let [schema (tools-schema/KGRelationType)]
        (is (m/validate schema "implements"))
        (is (m/validate schema "a-relation-this-build-cannot-know"))
        (is (not (m/validate schema "")))
        (is (not (m/validate schema nil)))))))

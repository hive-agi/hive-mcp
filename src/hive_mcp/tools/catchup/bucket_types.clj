(ns hive-mcp.tools.catchup.bucket-types
  "Value object: the memory types that can land in a catchup bucket.

   Every def below names a single memory type used by the bucket-splitting
   pipeline in bundle/split-by-type.  The `all` set is built from the
   single-type defs so that a type added here is automatically tracked
   everywhere.  bundle-cache/bundle-types is an alias for `all` — a literal
   copy would drift.

   The :expiring bucket in split-by-type accepts entries of ANY memory type,
   so this set is NOT the exhaustive inventory of types that can reach a
   bucket.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def axiom
  "Memory type for approved, immutable axioms."
  "axiom")

(def axiom-candidate
  "Memory type for axiom nominations awaiting human review."
  "axiom-candidate")

(def principle
  "Memory type for principles — heuristics that guide behaviour."
  "principle")

(def convention
  "Memory type for conventions — established patterns within a project."
  "convention")

(def decision
  "Memory type for decisions — recorded choices with rationale."
  "decision")

(def snippet
  "Memory type for code snippets with metadata."
  "snippet")

(def note
  "Memory type for free-form notes, including session summaries and
   wrap-generated syntheses."
  "note")

(def all
  "Set of all memory types that can land in a catchup bucket (except the
   :expiring bucket, which accepts any type).  Built from the individual
   defs so the set is never out of sync."
  #{axiom axiom-candidate principle convention decision snippet note})

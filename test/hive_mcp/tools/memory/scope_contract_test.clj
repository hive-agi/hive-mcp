(ns hive-mcp.tools.memory.scope-contract-test
  "Contract facets for hive-mcp.tools.memory.scope (pure scope-tag helpers)."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.memory.domain :as domain]
            [hive-mcp.tools.memory.schema :as ms]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private TagVec
  [:vector {:max 4} [:enum "a" "b" "scope:global" "scope:project:x" "scope:project:y"]])

(deftrifecta-from-schema retag-scope-ensures-the-new-tag-once
  #'scope/retag-scope
  {:in [:cat TagVec ms/ScopeTag ms/ScopeTag]
   :out [:vector :string]
   :rel (fn [[tags old new] out]
          (and (= 1 (count (filter #{new} out)))
               (= new (peek out))
               (not-any? #{old} (butlast out))))
   :golden-path "test/golden/hive-mcp/tools/memory/retag-scope.edn"
   :num-tests 200})

(deftrifecta-from-schema inject-project-scope-adds-only-when-absent
  #'scope/inject-project-scope
  {:in [:cat TagVec [:enum "global" "x" "hive-mcp"]]
   :out [:vector :string]
   :rel (fn [[tags pid] out]
          (if (some #(.startsWith ^String % "scope:") tags)
            (= (vec tags) out)
            (= (conj (vec tags) (if (= pid "global") "scope:global" (str "scope:project:" pid)))
               out)))
   :classify (fn [[tags] _] (if (some #(.startsWith ^String % "scope:") tags) :kept :added))
   :classify-domain #{:kept :added}
   :golden-path "test/golden/hive-mcp/tools/memory/inject-project-scope.edn"
   :num-tests 200})

(deftrifecta-from-schema matches-hierarchy-scopes-is-set-intersection
  #'scope/matches-hierarchy-scopes?
  {:in [:cat [:map {:closed true} [:tags TagVec]]
        [:maybe [:set {:max 2} [:enum "a" "scope:global" "scope:project:x"]]]]
   :out ms/ScopeMatch
   :rel (fn [[{:keys [tags]} valid] out]
          (if (nil? valid)
            (true? out)
            (= (boolean out) (boolean (some valid tags)))))
   :golden-path "test/golden/hive-mcp/tools/memory/matches-hierarchy-scopes.edn"
   :num-tests 200})

(deftrifecta-from-schema effective-directory-prefers-a-named-one
  #'scope/effective-directory
  {:in [:cat ms/DirectoryInput]
   :out [:maybe :string]
   :rel (fn [[{:keys [directory current]}] out]
          (if (and (string? directory) (not (clojure.string/blank? directory)))
            (= directory out)
            (= current out)))
   :num-tests 200})

(deftest scope-filters-and-tags
  (is (= "scope:global" (scope/make-scope-tag "global")))
  (is (= "scope:project:x" (scope/make-scope-tag "x")))
  (is (m/validate domain/ScopeFilterMalli (scope/derive-scope-filter "all" "x")))
  (is (= :scope/auto (:adt/variant (scope/derive-scope-filter nil "x"))))
  (is (= ["x" "global"] (scope/resolve-scope-chain "x")))
  (is (contains? (scope/expand-scope-tags "x") "scope:global"))
  (is (nil? (scope/derive-hierarchy-scope-filter "all")))
  (is (= #{"scope:global"} (scope/derive-hierarchy-scope-filter "global")))
  (is (true? (scope/matches-scope? {:tags []} "all")))
  (is (nil? (scope/explicit-project-id ["a" "scope:project:  "])))
  (is (= "x" (scope/explicit-project-id ["scope:project:x"]))))

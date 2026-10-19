(ns hive-mcp.project.m16-facet-test
  "Trifectas + goldens for project.scope (pure scope algebra, own state
   snapshot/restored) and project.tree (pure tree builders)."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [hive-mcp.project.scope :as scope]
            [hive-mcp.project.tree :as tree]
            [hive-schemas.test :as hst]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- out-schema [nsym fsym]
  (-> (m/function-schemas) (get-in [nsym fsym :schema]) m/children second))

;; -----------------------------------------------------------------------------
;; Pure scope algebra — no registry state involved (colon-chained ids)
;; -----------------------------------------------------------------------------

(def ^:private ScopeId
  "Colon-delimited scope ids plus the boundary cases callers pass."
  [:or [:enum "global" "scope:global" "all" "" nil]
   [:string {:gen/elements ["hive" "hive-mcp" "hive-mcp:agora" "hive-mcp:agora:debate"
                            "a" "a:b" "a:b:c:d" "solo"]}]])

(hst/deftrifecta-from-schema visible-scopes-contract
  hive-mcp.project.scope/visible-scopes
  {:in [:cat ScopeId]
   :out (out-schema 'hive-mcp.project.scope 'visible-scopes)
   :rel (fn [[in] out]
          (and (vector? out)
               (every? string? out)
               (= "global" (last out))
               ;; non-blank, non-global input includes itself first
               (if (and (string? in)
                        (not (str/blank? in))
                        (not= in "scope:global")
                        (not= in "global"))
                 (= (first out) (str/replace in #"^scope:project:" ""))
                 (= ["global"] out))))
   :golden-path "test/golden/hive-mcp/m16-visible-scopes.edn"})

(hst/deftrifecta-from-schema scope-tag-contract
  hive-mcp.project.scope/scope->tag
  {:in [:cat ScopeId]
   :out (out-schema 'hive-mcp.project.scope 'scope->tag)
   :rel (fn [[in] out]
          (if (or (nil? in) (= in "global") (= in "scope:global") (str/blank? (str in)))
            (= "scope:global" out)
            (str/starts-with? out "scope:project:")))
   :golden-path "test/golden/hive-mcp/m16-scope-tag.edn"})

(hst/deftrifecta-from-schema scope-contains-contract
  hive-mcp.project.scope/scope-contains?
  {:in [:cat ScopeId ScopeId]
   :out (out-schema 'hive-mcp.project.scope 'scope-contains?)
   :rel (fn [[p c] out]
          ;; reflexive on equal inputs; global contains everything non-nil
          (and (if (= p c) (true? out) true)
               (if (and (= p "global") (some? c)) (true? out) true)))
   :golden-path "test/golden/hive-mcp/m16-scope-contains.edn"})

;; -----------------------------------------------------------------------------
;; Registry-touching scope fns — state snapshot/restored around each case
;; -----------------------------------------------------------------------------

(defn- config-fixture [f]
  (let [saved-projects @#'scope/project-configs
        saved-aliases @#'scope/reverse-alias-index]
    (scope/clear-config-cache!)
    (try (f)
         (finally
           (reset! @#'scope/project-configs saved-projects)
           (reset! @#'scope/reverse-alias-index saved-aliases)
           (scope/clear-config-cache!)
           (reset! @#'scope/project-configs saved-projects)
           (reset! @#'scope/reverse-alias-index saved-aliases)))))

(clojure.test/use-fixtures :each config-fixture)

(deftest alias-resolution-test
  (scope/register-project-config! "hive-mcp" {:project-id "hive-mcp" :aliases ["hm" "mcp"]})
  (is (= "hive-mcp" (scope/resolve-project-id "hm")))
  (is (= "hive-mcp" (scope/resolve-project-id "mcp")))
  (is (= "unregistered" (scope/resolve-project-id "unregistered")))
  (is (nil? (scope/resolve-project-id nil)))
  (is (= {:project-id "hive-mcp" :aliases ["hm" "mcp"]}
         (scope/get-project-config "hm")))
  (is (= {"hm" "hive-mcp" "mcp" "hive-mcp"} (scope/get-alias-index)))
  (scope/deregister-project-config! "hive-mcp")
  (is (nil? (scope/get-project-config "hive-mcp")))
  (is (= {} (scope/get-alias-index))))

(deftest parent-scope-resolution-test
  (scope/register-project-config! "child" {:project-id "child" :parent-id "parent"})
  (is (= "parent" (scope/get-parent-scope "child")))
  (is (= "hive-mcp" (scope/get-parent-scope "hive-mcp:agora")))
  (is (= "global" (scope/get-parent-scope "toplevel")))
  (is (nil? (scope/get-parent-scope "global")))
  (is (nil? (scope/get-parent-scope nil)))
  (is (= ["child" "parent" "global"] (scope/visible-scopes "child"))))

(golden/deftest-golden scope-algebra-golden
  "test/golden/hive-mcp/m16-scope-algebra.edn"
  {:parent-of-nested (scope/get-parent-scope "a:b:c")
   :visible-of-nested (scope/visible-scopes "a:b:c")
   :visible-of-nil (scope/visible-scopes nil)
   :tag-of-project (scope/scope->tag "hive-mcp")
   :tag-of-global (scope/scope->tag "global")
   :tag-of-nil (scope/scope->tag nil)
   :tags-of-nested (scope/visible-scope-tags "a:b")
   :full-of-global (scope/full-hierarchy-scope-tags nil)
   :contains-global-child (scope/scope-contains? "global" "hive-mcp")
   :contains-parent (scope/scope-contains? "a" "a:b")
   :contains-sibling (scope/scope-contains? "a:b" "a:c")
   :contains-nil-nil (scope/scope-contains? nil nil)
   :filter-all (scope/derive-hierarchy-scope-filter "all")
   :filter-nil (scope/derive-hierarchy-scope-filter nil)
   :current-project-nil (scope/get-current-project-id nil)
   :infer-from-nowhere (scope/infer-scope-from-path "/nonexistent-m16-path/x")})

;; -----------------------------------------------------------------------------
;; project.tree — pure tree builders over in-memory entity lists
;; -----------------------------------------------------------------------------

(def ^:private ProjectEntity
  [:map
   [:project/id [:enum "root" "mid" "leaf" "other"]]
   [:project/parent-id {:optional true} [:enum "root" "mid" "leaf" "ghost"]]])

(hst/deftrifecta-from-schema build-project-tree-contract
  hive-mcp.project.tree/build-project-tree
  {:in [:cat [:vector {:gen/max 5} ProjectEntity]]
   :out (out-schema 'hive-mcp.project.tree 'build-project-tree)
   :rel (fn [[entities] tree]
          (let [by-id (:by-id tree)]
            (and (map? by-id)
                 ;; every entity with an id is indexed
                 (every? #(contains? by-id (:project/id %)) entities)
                 ;; roots are exactly the entities without a parent
                 (= (set (:roots tree))
                    (set (map :project/id (remove :project/parent-id entities)))))))
   :golden-path "test/golden/hive-mcp/m16-build-project-tree.edn"})

(def ^:private sample-tree
  (tree/build-project-tree
   [{:project/id "root"}
    {:project/id "mid" :project/parent-id "root"}
    {:project/id "leaf" :project/parent-id "mid"}
    {:project/id "leaf2" :project/parent-id "mid"}
    {:project/id "island"}]))

(golden/deftest-golden tree-traversal-golden
  "test/golden/hive-mcp/m16-tree-traversal.edn"
  {:tree sample-tree
   :descendants-of-root (tree/get-descendants sample-tree "root")
   :descendants-of-leaf (tree/get-descendants sample-tree "leaf")
   :descendants-of-missing (tree/get-descendants sample-tree "nope")
   :ancestors-of-leaf (tree/get-ancestors sample-tree "leaf")
   :ancestors-of-root (tree/get-ancestors sample-tree "root")
   :ancestors-of-missing (tree/get-ancestors sample-tree "nope")})

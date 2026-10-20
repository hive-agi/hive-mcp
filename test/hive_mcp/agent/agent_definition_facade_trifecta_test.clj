(ns hive-mcp.agent.agent-definition-facade-trifecta-test
  "Facets for the agent-definition FACADE. Its `(def x #'owner/x)` aliases
   capture the Var, so the facade carries its own contracts and its own
   coverage gate (`:paths` names the facade file, which is what makes the
   aliases count)."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.agent-definition :as ad]
            [hive-mcp.agent.agent-definition.spec :as spec]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Def
  "A definition map that reaches each source and tool arm."
  [:map
   [:agent-type [:string {:min 1}]]
   [:description :string]
   [:source {:optional true} spec/Source]
   [:tools {:optional true} [:vector :string]]])

(deftrifecta-from-schema built-in-trifecta
  hive-mcp.agent.agent-definition/built-in?
  {:in Def
   :out :boolean
   :rel (fn [d out] (= out (= :built-in (:source d))))
   :num-tests 200})

(deftrifecta-from-schema plugin-trifecta
  hive-mcp.agent.agent-definition/plugin?
  {:in Def
   :out :boolean
   :rel (fn [d out] (= out (= :plugin (:source d))))
   :num-tests 200})

(deftrifecta-from-schema valid-trifecta
  hive-mcp.agent.agent-definition/valid?
  {:in :any
   :out :boolean
   :rel (fn [x out] (= out (:valid (ad/validate x))))
   :num-tests 200})

(deftrifecta-from-schema summarize-trifecta
  hive-mcp.agent.agent-definition/summarize
  {:in Def
   :out :string
   :rel (fn [d out]
          (and (.startsWith ^String out (:agent-type d))
               (.endsWith ^String out (:description d))))
   :num-tests 200})

(deftest validate-arms-are-disjoint
  (let [good (ad/validate {:agent-type "x" :description "d" :system-prompt "p"})
        bad  (ad/validate 1)
        ok-s [:map [:valid [:= true]] [:data :map]]
        bad-s [:map [:valid [:= false]] [:errors :any]]]
    (is (m/validate ok-s good))
    (is (not (m/validate bad-s good)))
    (is (m/validate bad-s bad))
    (is (not (m/validate ok-s bad)))
    (is (m/validate spec/Validation good))
    (is (m/validate spec/Validation bad))))

(deftest record-boundary
  (let [rec (ad/make-agent-def {:agent-type "x" :description "d" :system-prompt "p"})]
    (is (true? (ad/agent-def? rec)))
    (is (false? (ad/agent-def? {:agent-type "x"})))
    (is (= :built-in (:source rec)))
    (is (= :role/standalone (:hivemind-role rec)))
    (is (map? (ad/->map rec)))
    (is (not (record? (ad/->map rec))))
    (is (not (contains? (ad/->map rec) :tools)))
    (is (= {:a 1} (ad/->map {:a 1})))
    (is (thrown? clojure.lang.ExceptionInfo (ad/validate-or-throw! {})))))

(deftest loaders-answer-their-error-arms
  (is (m/validate spec/LoadResult (ad/load-agent-file "/nonexistent/agent.md" :user)))
  (is (= {:agents [] :errors []} (ad/load-agents-dir "/nonexistent/dir" :user)))
  (is (m/validate spec/FrontmatterResult (ad/parse-frontmatter "---\nname: a\n---\nbody")))
  (is (m/validate spec/CoerceResult
                  (ad/frontmatter->agent-def {:name "a" :description "d"} "body" :user))))

(golden/deftest-golden-fn agent-definition-facade-golden
  "test/golden/agent/agent-definition-facade.edn"
  (fn []
    (let [d {:agent-type "explore" :description "Fast" :system-prompt "You search."}
          rec (ad/make-agent-def d)]
      {:validate-good (ad/validate d)
       :validate-bad (ad/validate 1)
       :predicates (mapv #(% rec) [ad/built-in? ad/plugin? ad/has-all-tools?])
       :custom (ad/custom? (assoc d :source :user))
       :summary (ad/summarize d)
       :list (ad/list-summary [d])
       :as-map (ad/->map rec)
       :merged (ad/merge-definitions [d] [(assoc d :description "Slow")])})))

(deftest-contract-coverage agent-definition-facade-contract-coverage
  {:paths ["src/hive_mcp/agent/agent_definition.clj"]})

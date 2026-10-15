(ns hive-mcp.agent.agent-definition.compose-trifecta-test
  "Facets for the agent-definition composition layer (compose, frontmatter,
   validate), derived from their malli contracts; one coverage gate for the
   directory."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.agent-definition.compose :as c]
            [hive-mcp.agent.agent-definition.frontmatter :as fm]
            [hive-mcp.agent.agent-definition.schema :as schema]
            [hive-mcp.agent.agent-definition.validate :as v]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Def
  "A valid agent definition with a string prompt."
  [:map
   [:agent-type [:string {:min 1 :max 8}]]
   [:description [:string {:max 12}]]
   [:system-prompt [:string {:max 12}]]
   [:source schema/Source]])

(def Defs [:vector {:max 6} Def])

(deftrifecta-from-schema summarize-trifecta
  hive-mcp.agent.agent-definition.compose/summarize
  {:in Def
   :out :string
   :rel (fn [d s]
          (and (str/starts-with? s (:agent-type d))
               (str/includes? s (str "(" (name (:source d)) ")"))
               (str/ends-with? s (:description d))))
   :mutation false
   :golden-path "test/golden/agent/compose-summarize.edn"})

(deftrifecta-from-schema merge-definitions-trifecta
  hive-mcp.agent.agent-definition.compose/merge-definitions
  {:in [:cat Defs Defs]
   :out [:vector Def]
   :rel (fn [[a b] out]
          (let [types (map :agent-type out)
                prio  {:built-in 0 :plugin 1 :project 2 :user 3}]
            (and (= (count types) (count (set types)))
                 (= (set types) (set (map :agent-type (concat a b))))
                 ;; the winner is of the highest source priority offered
                 (every? (fn [d]
                           (= (apply max (map (comp prio :source)
                                              (filter #(= (:agent-type d) (:agent-type %))
                                                      (concat a b))))
                              (prio (:source d))))
                         out))))
   :mutation false
   :golden-path "test/golden/agent/compose-merge-definitions.edn"
   :num-tests 200})

(deftrifecta-from-schema validate-trifecta
  hive-mcp.agent.agent-definition.validate/validate
  {:in Def
   :out schema/Validation
   :rel (fn [d r] (and (true? (:valid r)) (= d (:data r))))
   :mutation false
   :golden-path "test/golden/agent/validate-valid.edn"})

(deftest validate-rejects-and-explains
  (let [bad {:agent-type 1 :description "d" :system-prompt "p"}
        r   (v/validate bad)]
    (is (false? (:valid r)))
    (is (contains? (:errors r) :agent-type))
    (is (false? (v/valid? bad)))
    (is (contains? (v/explain bad) :agent-type))
    (is (nil? (v/explain {:agent-type "a" :description "d" :system-prompt "p"})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid agent definition for <unknown>"
                          (v/validate-or-throw! {})))
    (is (= {:agent-type "a" :description "d" :system-prompt "p"}
           (v/validate-or-throw! {:agent-type "a" :description "d" :system-prompt "p"})))))

(deftest predicates-and-lookup
  (let [b {:agent-type "b" :source :built-in} p {:agent-type "p" :source :plugin}
        u {:agent-type "u" :source :user :tools ["x"]}]
    (is (true? (c/built-in? b))) (is (false? (c/built-in? p)))
    (is (true? (c/plugin? p)))
    (is (= :user (c/custom? u))) (is (nil? (c/custom? b)))
    (is (true? (c/has-all-tools? b))) (is (false? (c/has-all-tools? u)))
    (is (= u (c/find-by-type "u" [b p u]))) (is (nil? (c/find-by-type "zz" [b p u])))
    (is (= "s" (c/get-system-prompt {:system-prompt "s"})))
    (is (= "dyn" (c/get-system-prompt {:system-prompt (fn [] "dyn")})))
    (is (= "b (built-in) — \np (plugin) — " (c/list-summary [(assoc b :description "") (assoc p :description "")])))))

(golden/deftest-golden-fn frontmatter-golden
  "test/golden/agent/compose-frontmatter.edn"
  (fn []
    (let [raw "---\nname: scout\ndescription: finds things\ntools: [read_file, grep]\nmodel: INHERIT\nmax-turns: \"7\"\nhivemind-role: role/worker\nspawn-mode: headless\nskills: a, b\n---\nYou scout.\n"
          parsed (fm/parse-frontmatter raw)]
      {:parsed parsed
       :agent  (fm/frontmatter->agent-def (:frontmatter parsed) (:content parsed) :project
                                          {:file-path "/x/y/scout.md" :base-dir "/x/y"})
       :no-fm  (fm/parse-frontmatter "just text")
       :bad-yaml (-> (fm/parse-frontmatter "---\na: [unclosed\n---\nbody\n") (select-keys [:content]) )
       :missing-desc (fm/frontmatter->agent-def {:name "n"} "" :user)
       :not-agent (fm/frontmatter->agent-def {} "" :user)
       :all-tools (:tools (fm/frontmatter->agent-def {:name "n" :description "d" :tools "*"} "" :user))
       :default-prompt (:system-prompt (fm/frontmatter->agent-def {:name "n" :description "d"} "  " :user))})))

(deftest loading-from-disk
  (let [dir (doto (io/file (System/getProperty "java.io.tmpdir") (str "m02-agents-" (random-uuid))) (.mkdirs))
        _   (spit (io/file dir "a.md") "---\nname: a1\ndescription: d1\n---\nbody")
        _   (spit (io/file dir "doc.md") "no frontmatter")
        _   (spit (io/file dir "nodesc.md") "---\nname: nd\n---\nbody")
        r   (c/load-agents-dir (str dir) :project)]
    (testing "agents, errors, skipped docs"
      (is (= ["a1"] (map :agent-type (:agents r))))
      (is (= 1 (count (:errors r))))
      (is (= {:agents [] :errors []} (c/load-agents-dir "/nonexistent-m02" :user))))
    (testing "single file"
      (is (= "a1" (:agent-type (c/load-agent-file (str dir "/a.md") :user))))
      (is (nil? (c/load-agent-file (str dir "/doc.md") :user)))
      (is (string? (:error (c/load-agent-file (str dir "/missing.md") :user)))))
    (testing "active-agents merges built-ins and custom, user beats built-in"
      (let [cwd (doto (io/file dir "proj") .mkdirs)
            _   (.mkdirs (io/file cwd ".claude/agents"))
            _   (spit (io/file cwd ".claude/agents/a1.md") "---\nname: a1\ndescription: proj\n---\nx")
            res (c/active-agents [{:agent-type "a1" :source :built-in :description "b"}] (str cwd))]
        (is (= "proj" (:description (c/find-by-type "a1" (:active res)))))
        (is (= 2 (count (:all res))))
        (is (every? vector? [(:active res) (:all res)]))
        (is (some #(= [(str cwd "/.claude/agents") :project] %) (c/find-agent-dirs (str cwd))))))))

(deftest-contract-coverage agent-definition-contract-coverage
  {:paths ["src/hive_mcp/agent/agent_definition/compose.clj"
           "src/hive_mcp/agent/agent_definition/frontmatter.clj"
           "src/hive_mcp/agent/agent_definition/validate.clj"]})

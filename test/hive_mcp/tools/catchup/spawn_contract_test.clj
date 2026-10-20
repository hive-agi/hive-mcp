(ns hive-mcp.tools.catchup.spawn-contract-test
  "catchup.spawn and the catchup.scope facade over the IMemoryStore PORT
   (seeded StubMemoryStore) and the git *sh* port bound to a stub."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.tools.catchup.contract-fixtures :as fx]
            [hive-mcp.tools.catchup.git :as git]
            [hive-mcp.tools.catchup.scope :as scope]
            [hive-mcp.tools.catchup.spawn :as spawn]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each fx/with-seeded-store)

(defn- fixed-git [_git & args]
  (cond (some #{"rev-parse"} args) {:exit 0 :out "main\n"}
        (some #{"status"} args)    {:exit 0 :out ""}
        :else                      {:exit 0 :out "abc1234 - init\n"}))

(deftest-golden spawn-context-full-over-stub
  "test/golden/catchup/spawn-context-port.edn"
  (fx/with-seeded-store
    #(binding [git/*sh* fixed-git]
       (spawn/spawn-context "/nonexistent-m26/p"))))

(deftest spawn-context-modes
  (binding [git/*sh* fixed-git]
    (let [s (spawn/spawn-context "/nonexistent-m26/p")]
      (is (str/includes? s "## Project Context (Auto-Injected)"))
      (is (str/includes? s "- **Branch**: main"))
      (is (str/includes? s "1. conv one") "priority conventions carry content")
      ;; BUG (recorded, not fixed): :full mode reads axioms through
      ;; axiom-cache/query-axioms, which fetches the metadata projection
      ;; (no :content), so every axiom line renders as "N. null".
      (is (str/includes? s "1. null")))
    (testing ":hints without the agent domain falls back to :full"
      (is (string? (spawn/spawn-context "/nonexistent-m26/p" {:mode :hints}))))
    (testing ":ref without cached refs falls back to :full"
      (is (string? (spawn/spawn-context "/nonexistent-m26/p" {:mode :ref}))))))

(deftest scope-facade-project-name
  (is (nil? (scope/get-current-project-name)))
  (is (= "b" (scope/get-current-project-name "/nonexistent-m26/a/b")))
  (is (nil? (scope/get-current-project-name "/")) "the root has no last segment"))

(ns hive-mcp.tools.git-local-root-facet-test
  "Golden facet for hive-mcp.tools.git-local-root/refusal over stub checker
   and repo ports: one row per commit scenario, so a change to what is
   refused, or to the refusal text, shows as a snapshot diff."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.git-local-root :as lr]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- violations
  "Stand-in for hive-git.local-root/violations: `../` escapes, `#!bad` does
   not parse, anything else is clean."
  [file content]
  (cond
    (nil? content) []
    (str/includes? content "#!bad") [{:file file :reason :unparseable}]
    (str/includes? content "../") [{:file file :reason :local-root :line 2
                                    :coord 'acme/lib :path "../lib"}]
    :else []))

(defn- checker
  "A checker port over an in-memory `index` {path content}; `error` forces a
   non-violation failure from refuse-commit."
  [index error]
  {:git :stub-git
   :violations violations
   :refuse-commit (fn [_ctx paths]
                    (let [vs (into [] (mapcat #(violations % (get index %)))
                                   (distinct (concat ["deps.edn" "bb.edn"] paths)))]
                      (cond
                        error {:error :git/show-failed :message error}
                        (seq vs) {:error :git/local-root :violations vs}
                        :else {:ok {:checked (vec paths)}})))})

(defn- repo [{:keys [staged modified tree]}]
  {:staged-paths   (fn [_] (vec staged))
   :modified-paths (fn [_] (vec modified))
   :read-file      (fn [_ p] (get tree p))})

(def ^:private scenarios
  {"no-hive-git"        [nil {} false]
   "clean-index"        [(checker {"deps.edn" "{:deps {}}"} nil) {:staged ["src/a.clj"]} false]
   "staged-escape"      [(checker {"deps.edn" "{:deps {a/b {:local/root \"../x\"}}}"} nil)
                         {:staged ["deps.edn"]} false]
   "staged-unparseable" [(checker {"sub/bb.edn" "#!bad"} nil) {:staged ["sub/bb.edn"]} false]
   "checker-error"      [(checker {} "index unreadable") {:staged []} false]
   "all-clean-tree"     [(checker {} nil) {:modified ["README.md"] :tree {"deps.edn" "{}"}} true]
   "all-dirty-tree"     [(checker {} nil)
                         {:modified ["deps.edn" "x/deps.edn"]
                          :tree {"deps.edn" "{:deps {a/b {:local/root \"../a\"}}}"
                                 "x/deps.edn" "{:deps {c/d {:local/root \"../c\"}}}"}}
                         true]
   "dirty-tree-not-all" [(checker {} nil)
                         {:modified ["deps.edn"] :tree {"deps.edn" "{:local/root \"../a\"}"}}
                         false]})

(defn- outcome [[c r all?]]
  (some-> (lr/refusal c (repo r) "/repo" all?) (select-keys [:isError :text])))

(golden/deftest-golden refusal-scenarios-golden
  "test/golden/hive-mcp/tools/git-local-root-refusal.edn"
  (into (sorted-map) (map (fn [[k v]] [k (outcome v)])) scenarios))

(deftest refused-exactly-when-something-escapes-or-fails
  (is (= #{"staged-escape" "staged-unparseable" "checker-error" "all-dirty-tree"}
         (set (keep (fn [[k v]] (when (outcome v) k)) scenarios)))))

(deftest hive-git-checker-is-nil-or-a-full-port
  (let [c (lr/hive-git-checker)]
    (is (or (nil? c) (every? #(contains? c %) [:refuse-commit :violations :git])))))

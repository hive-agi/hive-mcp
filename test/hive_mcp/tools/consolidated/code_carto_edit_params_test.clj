(ns hive-mcp.tools.consolidated.code-carto-edit-params-test
  "Every carto EDIT command's params reach the contributed `carto` handler
   through the consolidated `code` tool, unchanged, and the schema a ling
   receives for `code` is open so a strict client may send them
   (card 20261001165544-09eb42a9)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.registry :as agent-reg]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.tools.consolidated.code :as code]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def edit-calls
  "One representative call per carto edit command, with the params carto
   needs for it (from `carto describe of=<command>`)."
  {"create-file"      {:path "/w/src/a.clj" :content "(ns a)\n" :scope "/w"}
   "write-form"       {:qn "a/f" :new-body "(defn f [] 1)" :scope "/w" :preview? true}
   "insert-form"      {:new-body "(def x 1)" :after "a/f" :before "a/g" :scope "/w"}
   "delete-form"      {:qn "a/f" :file "/w/src/a.clj" :scope "/w"}
   "ns-edit"          {:op "add-require" :ns "a" :as "b" :refer ["c"] :new-as "d" :new-ns "e"}
   "refactor-tx"      {:ops [{:op "write-form" :qn "a/f" :new-body "(defn f [])"}] :scope "/w"}
   "rewrite"          {:pattern "(inc ?x)" :to "(+ ?x 1)" :ensure-require ["a"] :apply? true}
   "raw-splice"       {:file "/w/src/a.clj" :new-text "x" :start-line 1 :end-line 2}
   "edn-edit"         {:file "/w/deps.edn" :key-path [":paths"] :edn-op "assoc-in" :new-body "[\"src\"]"}
   "rename-symbol"    {:qn "a/f" :new-name "g" :scope "/w"}
   "ensure-require"   {:file "/w/src/a.clj" :ns "b" :as "b"}
   "extract-namespace" {:source-qns ["a/f"] :target-ns "c" :target-file "/w/src/c.clj"}
   "move-namespace"   {:namespace "a" :target-ns "d" :target-file "/w/src/d.clj"}})

(defn- dispatch-through-code
  "Mount a stub `carto` subdomain on the real `code` tool, build the MCP
   table, and send `code carto <cmd>` with params. Returns what the stub saw."
  [cmd params]
  (let [seen (atom nil)
        addon ::stub-carto]
    (try
      (ext/contribute-commands! "code" addon
                                {"carto" {:handler (fn [p] (reset! seen p) {:type "text" :text "ok"})}})
      (let [table   (routes/tool-table [(routes/make-tool code/tool-def)])
            handler (get-in table ["code" :handler])]
        (handler (assoc params :command (str "carto " cmd)))
        @seen)
      (finally (ext/retract-commands! "code" addon)))))

(deftest every-carto-edit-param-reaches-the-handler
  (doseq [[cmd params] edit-calls]
    (testing cmd
      (let [seen (dispatch-through-code cmd params)]
        (is (some? seen) "the carto subdomain handler ran")
        (is (str/ends-with? (str (:command seen)) cmd) "the command names the edit verb")
        (is (= params (select-keys seen (keys params)))
            "every edit param arrives at the handler unchanged")))))

(deftest the-ling-schema-for-code-is-open
  (let [before @agent-reg/registry]
    (try
      (reset! agent-reg/registry {})
      (agent-reg/register! [code/tool-def])
      (let [schema (first (agent-reg/get-schemas ["code"]))]
        (testing "a strict client may send params the code schema does not list"
          (is (true? (get-in schema [:inputSchema :additionalProperties]))))
        (testing "the model is told the schema is abridged"
          (is (str/includes? (:description schema) "Compact schema"))))
      (finally (reset! agent-reg/registry before)))))

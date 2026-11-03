(ns hive-mcp.tools.consolidated.hot.branch-test
  "`hot branch` the way an agent uses it: command strings into the real `hot`
   handler, then calls of the tool it extended. The target tool is a real merged
   composite; only its lookup is rebound."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.dispatch.branches :as branches]
            [hive-mcp.tools.composite :as composite]
            [hive-mcp.tools.consolidated.hot :as hot]
            [hive-mcp.tools.consolidated.hot.branch :as branch]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private tool "hot-branch-test-tool")

(def ^:private target
  (composite/build-merged-handler
   tool
   {:status (fn [_] {:type "text" :text "core status"})
    :belt   {:scan (fn [_] {:type "text" :text "core belt scan"})}}))

(use-fixtures :each
  (fn [t]
    (branches/clear!)
    (binding [branch/*tool-handler* (fn [n] (when (= tool n) target))]
      (try (t) (finally (branches/clear!))))))

(defn- hot! [command & {:as params}]
  (hot/handle-hot (assoc params :command command :tool (get params :tool tool))))

(defn- call [command & {:as params}]
  (target (assoc params :command command)))

(defn- ok? [response]
  (and (not (:isError response)) (:ok? (json/read-str (:text response) :key-fn keyword))))

;; The vars a `symbol` branch resolves to. Public so requiring-resolve finds them.
(defn symbol-handler [_] "from a var")
(defn symbol-wrap [next-handler] (fn [p] (update (next-handler p) :text str " via var")))

(deftest an-agent-grafts-a-verb-from-source-and-calls-it
  (is (ok? (hot! "branch add" :path "belt hello"
                 :code "(fn [{:keys [name]}] (str \"hi \" name))"
                 :schema {:name {:type "string"}}
                 :summary "Greets")))
  (is (= "hi ana" (:text (call "belt hello" :name "ana"))))
  (is (= "core belt scan" (:text (call "belt scan")))))

(deftest plain-data-is-answered-as-json
  (hot! "branch add" :path "data" :code "(fn [_] {:n 1})")
  (is (= {:n 1} (json/read-str (:text (call "data")) :key-fn keyword))))

(deftest code-may-define-helpers-before-its-last-form
  (hot! "branch add" :path "two" :code "(defn- twice [x] (* 2 x)) (fn [_] (str (twice 21)))")
  (is (= "42" (:text (call "two")))))

(deftest a-verb-map-grafts-several-verbs-at-once
  (hot! "branch add" :path "belt" :code "{:a (fn [_] \"A\") :b (fn [_] \"B\")}")
  (is (= "A" (:text (call "belt a"))))
  (is (= "B" (:text (call "belt b")))))

(deftest around-from-source-wraps-a-verb
  (is (ok? (hot! "branch around" :path "status"
                 :code "(fn [next] (fn [p] (update (next p) :text clojure.string/upper-case)))")))
  (is (= "CORE STATUS" (:text (call "status")))))

(deftest around-may-replace-with-plain-data
  (hot! "branch around" :path "status" :code "(fn [_] (fn [_] \"mine now\"))")
  (is (= "mine now" (:text (call "status")))))

(deftest a-symbol-stays-a-var
  (hot! "branch add" :path "v"
        :symbol "hive-mcp.tools.consolidated.hot.branch-test/symbol-handler")
  (is (= "from a var" (:text (call "v"))))
  (let [original symbol-handler]
    (try
      (alter-var-root #'symbol-handler (constantly (fn [_] "reloaded")))
      (is (= "reloaded" (:text (call "v"))) "a reload reaches the branch")
      (finally (alter-var-root #'symbol-handler (constantly original))))))

(deftest around-from-a-symbol
  (hot! "branch around" :path "status"
        :symbol "hive-mcp.tools.consolidated.hot.branch-test/symbol-wrap")
  (is (= "core status via var" (:text (call "status")))))

(deftest a-file-supplies-the-fn
  (let [f (doto (io/file (System/getProperty "java.io.tmpdir")
                         (str "hot-branch-" (System/nanoTime) ".clj"))
            (spit "(fn [_] \"from a file\")"))]
    (try
      (hot! "branch add" :path "f" :file (str f))
      (is (= "from a file" (:text (call "f"))))
      (finally (io/delete-file f true)))))

(deftest a-graft-over-a-route-is-refused-and-withdrawn
  (let [r (hot! "branch add" :path "belt scan" :code "(fn [_] \"hijack\")")]
    (is (:isError r))
    (is (str/includes? (:text r) "occupied"))
    (is (str/includes? (:text r) "branch around")))
  (is (empty? (branches/of tool)))
  (is (= "core belt scan" (:text (call "belt scan")))))

(deftest an-around-with-no-target-is-refused-and-withdrawn
  (let [r (hot! "branch around" :path "nowhere" :code "(fn [n] n)")]
    (is (:isError r))
    (is (str/includes? (:text r) "no-target")))
  (is (empty? (branches/of tool))))

(deftest bad-input-answers-an-error-and-registers-nothing
  (testing "broken source"
    (is (str/includes? (:text (hot! "branch add" :path "x" :code "(fn [")) "could not load")))
  (testing "source with no forms"
    (is (:isError (hot! "branch add" :path "x" :code "  "))))
  (testing "read-eval is off"
    (is (:isError (hot! "branch add" :path "x" :code "#=(+ 1 1)"))))
  (testing "not a function"
    (is (:isError (hot! "branch around" :path "status" :code "42"))))
  (testing "no implementation"
    (is (:isError (hot! "branch add" :path "x"))))
  (testing "no path"
    (is (:isError (hot! "branch add" :code "(fn [_] 1)"))))
  (testing "no tool"
    (is (:isError (hot/handle-hot {:command "branch add" :path "x" :code "(fn [_] 1)"}))))
  (is (empty? (branches/of tool))))

(deftest an-unknown-tool-is-reported-not-assumed-fine
  (let [r (hot/handle-hot {:command "branch add" :tool "no-such-tool" :path "x"
                           :code "(fn [_] 1)"})]
    (is (str/includes? (:text r) "dormant"))))

(deftest a-throwing-branch-is-contained-and-removable
  (hot! "branch add" :path "boom" :code "(fn [_] (throw (ex-info \"kaput\" {})))")
  (let [r (call "boom")]
    (is (:isError r))
    (is (str/includes? (:text r) "kaput")))
  (is (= "core status" (:text (call "status"))))
  (is (not (:isError (hot! "branch remove" :path "boom"))))
  (is (:isError (call "boom"))))

(deftest list-shows-what-is-registered
  (hot! "branch add" :path "a b" :code "(fn [_] 1)" :summary "S" :owner "me")
  (let [{:keys [branches refused]}
        (json/read-str (:text (hot! "branch list")) :key-fn keyword)]
    (is (= [{:tool tool :kind "graft" :path "a b" :owner "me" :summary "S"}] branches))
    (is (empty? refused))))

(deftest remove-by-owner-takes-all-of-its-branches
  (hot! "branch add" :path "a" :code "(fn [_] 1)" :owner "me")
  (hot! "branch add" :path "b" :code "(fn [_] 1)" :owner "me")
  (hot! "branch add" :path "c" :code "(fn [_] 1)" :owner "you")
  (is (str/includes? (:text (hot/handle-hot {:command "branch remove" :owner "me"})) "2"))
  (is (= ["you"] (map :owner (branches/of tool)))))

(deftest a-tool-with-no-tree-is-refused-rather-than-falsely-ok
  (binding [branch/*tool-handler* (fn [_] (fn [_] {:type "text" :text "my own switch"}))]
    (let [r (hot! "branch add" :path "x" :code "(fn [_] 1)")]
      (is (:isError r))
      (is (str/includes? (:text r) "verb tree"))))
  (is (empty? (branches/of tool))))

(def branch-runs
  "Counted by a branch written as source, so public."
  (atom 0))

(deftest the-check-runs-nothing-the-branch-wrote
  (reset! branch-runs 0)
  (hot! "branch add" :path "once"
        :code "(fn [_] (swap! hive-mcp.tools.consolidated.hot.branch-test/branch-runs inc) \"ran\")")
  (is (zero? @branch-runs))
  (is (= "ran" (:text (call "once"))))
  (is (= 1 @branch-runs)))

(deftest removing-nothing-says-so
  (is (:isError (hot! "branch remove" :path "absent"))))

(deftest hot-branch-cannot-lock-itself-out
  (is (:isError (hot/handle-hot {:command "branch around" :tool "hot" :path "branch"
                                 :code "(fn [_] (fn [_] \"stuck\"))"}))))

(deftest help-answers-bare-and-named
  (is (str/includes? (:text (hot/handle-hot {:command "branch"})) "around"))
  (is (str/includes? (:text (hot/handle-hot {:command "branch help"})) "around")))

(deftest the-tool-declares-every-param-the-verbs-read
  (let [props (get-in hot/tool-def [:inputSchema :properties])]
    (doseq [p ["tool" "path" "code" "symbol" "file" "owner" "summary" "schema" "order"]]
      (is (contains? props p) p))))

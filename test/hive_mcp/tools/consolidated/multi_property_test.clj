(ns hive-mcp.tools.consolidated.multi-property-test
  "Property tests for multi tool batch validation and routing."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.string :as str]
            [hive-mcp.tools.consolidated.multi :as multi]
            [hive-mcp.tools.result-bridge :as rb]
            [hive-mcp.dsl.verbs :as verbs]))

;; ── Generators ────────────────────────────────────────────────────────────────

(def gen-tool-name
  (gen/elements ["memory" "kg" "agent" "kanban" "session" "config"
                 "preset" "magit" "hivemind"]))

(def gen-string-keyed-params
  "Simulates raw MCP JSON params with string keys."
  (gen/hash-map "tool" gen-tool-name
                "command" (gen/return "help")))

;; ── P1: handle-multi with help command never throws ──────────────────────────

(defspec multi-help-never-throws 50
  (prop/for-all [tool gen-tool-name]
                (let [result (multi/handle-multi {"tool" tool "command" "help"})]
                  (and (map? result)
                       (contains? result :text)))))

;; ── P2: handle-multi with unknown tool returns error ─────────────────────────

(defspec multi-unknown-tool-returns-error 50
  (prop/for-all [tool (gen/such-that
                       #(and (not (str/blank? %))
                             (nil? (multi/get-tool-handler %)))
                       gen/string-alphanumeric
                       100)]
                (let [result (multi/handle-multi {"tool" tool "command" "help"})]
                  (:isError result))))

;; ── P3: handle-multi with no params returns help text ────────────────────────

(deftest multi-empty-params-returns-help
  (testing "handle-multi with empty params returns help"
    (let [result (multi/handle-multi {})]
      (is (not (:isError result)))
      (is (str/includes? (:text result) "Multi tool")))))

;; ── P4: handle-multi rejects both dsl and operations ─────────────────────────

(deftest multi-dsl-and-operations-mutual-exclusion
  (testing "providing both dsl and operations returns error"
    (let [result (multi/handle-multi {"dsl" [["m+", {"c" "x"}]]
                                      "operations" [{"id" "1" "tool" "memory"}]})]
      (is (:isError result))
      (is (str/includes? (:text result) "Cannot specify both")))))

;; ── P5: batch with nil operations returns error ──────────────────────────────

(deftest multi-batch-nil-operations-error
  (testing "batch with nil operations returns error"
    (let [result (multi/handle-multi {"operations" nil})]
      ;; nil operations with no tool = help text
      (is (not (:isError result))))))

;; ── P6: batch with empty operations returns error ────────────────────────────

(deftest multi-batch-empty-operations-error
  (testing "batch with empty operations returns error"
    (let [result (multi/handle-multi {"operations" []})]
      (is (:isError result))
      (is (str/includes? (:text result) "empty")))))

;; ── P7: keywordize-map used in handle-multi normalizes string keys ───────────

(defspec keywordize-preserves-all-values 100
  (prop/for-all [m (gen/map gen/string-alphanumeric gen/string-alphanumeric {:max-elements 5})]
                (let [kw-map (rb/keywordize-map m)]
                  (= (count m) (count kw-map)))))

;; ── P8: strict DSL params ─────────────────────────────────────────────────

(def ^:private consolidated-tool-defs
  {"kanban" 'hive-mcp.tools.consolidated.kanban/tool-def
   "memory" 'hive-mcp.tools.consolidated.memory/tool-def
   "preset" 'hive-mcp.tools.consolidated.preset/tool-def})

(deftest verb-params-keys-are-advertised-by-target-tool
  (testing "every verb-params key is an inputSchema property of the verb's tool, or :id / :directory"
    (doseq [[v accepted] verbs/verb-params
            :let [tool  (:tool (get verbs/verb-table v))
                  sym   (get consolidated-tool-defs tool)
                  props (->> @(requiring-resolve sym) :inputSchema :properties keys
                             (map keyword) set)]]
      (is (some? sym) (str v " targets a tool this property does not cover: " tool))
      (is (empty? (remove (into props #{:id :directory}) accepted))
          (str v " lists keys its tool does not advertise")))))

(defspec dsl-unknown-param-surfaces-per-op-error 30
  (prop/for-all [k (gen/such-that
                    #(not (contains? (into (get verbs/verb-params "b?")
                                           verbs/verb-meta-params)
                                     (verbs/expand-param-key %)))
                    gen/string-alphanumeric
                    100)]
                (let [result (multi/handle-multi {"dsl" [["b?" {k "x"}]]})]
                  (and (str/includes? (:text result) "Unknown param(s) for b?")
                       (str/includes? (:text result) "\"success\":false")))))

(deftest dsl-unknown-param-reports-like-unknown-verb
  (testing "multi reports a rejected param and an unknown verb in the same per-op shape"
    (let [param (:text (multi/handle-multi {"dsl" [["b?" {"id" "x"}]]}))
          verb  (:text (multi/handle-multi {"dsl" [["zz" {}]]}))]
      (is (str/includes? param "Operation '$0': Unknown param(s) for b?: id"))
      (is (str/includes? verb "Operation '$0': Unknown verb: zz")))))

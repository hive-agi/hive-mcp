(ns hive-mcp.multi.param-coerce-test
  "Regression for MULTI-CLUSTER-KINDS-STRINGIFIED (kanban 20260930233616-5348ac33).

   `multi {tool: cluster, command: search, kinds: [\"pods\"]}` reached the
   cluster tool with kinds as the JSON TEXT `[\"pods\"]`, because the client
   types arguments by multi's schema and multi does not declare `kinds`. The
   target then swept one kind per character. multi now decodes forwarded
   params against the TARGET's inputSchema."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.multi.param-coerce :as pc]
            [hive-mcp.tools.consolidated.multi :as multi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private probe-schema
  "Shaped like hive-k8s's cluster schema: string property names, keyword
   inner keys, additionalProperties false."
  {:type "object"
   :properties {"command"   {:type "string"}
                "namespace" {:type "string"}
                "kinds"     {:type "array" :items {:type "string"}}
                "labels"    {:type "object"}
                "limit"     {:type "integer"}
                "ratio"     {:type "number"}
                "correlate" {:type "boolean"}
                "either"    {:anyOf [{:type "string"} {:type "array"}]}}
   :required ["command"]
   :additionalProperties false})

;; ---------------------------------------------------------------------------
;; Pure: coercion-spec / coerce-params
;; ---------------------------------------------------------------------------

(deftest coercion-spec-keeps-only-single-non-string-types
  (is (= {:kinds "array" :labels "object" :limit "integer"
          :ratio "number" :correlate "boolean"}
         (pc/coercion-spec probe-schema)))
  (testing "keyword property names and string inner keys read the same"
    (is (= {:kinds "array"}
           (pc/coercion-spec {"properties" {:kinds {"type" "array"}}}))))
  (testing "nil schema declares nothing"
    (is (= {} (pc/coercion-spec nil)))))

(deftest stringified-array-is-decoded-not-split
  (let [res (pc/coerce-params probe-schema
                              {:command "search"
                               :kinds "[\"securitypolicies\",\"pods\"]"})]
    (is (= ["securitypolicies" "pods"] (get-in res [:ok :kinds])))
    (is (not= [\[ \"] (take 2 (seq (get-in res [:ok :kinds]))))
        "the defect: the target saw characters, one kind each")))

(deftest every-declared-type-is-decoded
  (is (= {:ok {:command "search"
               :kinds ["pods"]
               :labels {"app" "x"}
               :limit 20
               :ratio 0.5
               :correlate false
               :namespace "default"}}
         (pc/coerce-params probe-schema
                           {:command "search"
                            :kinds "[\"pods\"]"
                            :labels "{\"app\": \"x\"}"
                            :limit "20"
                            :ratio "0.5"
                            :correlate "false"
                            :namespace "default"}))))

(deftest already-typed-and-undeclared-values-pass-through
  (let [params {:command "search" :kinds ["pods"] :labels {"a" "b"}
                :limit 3 :either "[\"x\"]" :extra "[1]"}]
    (is (= {:ok params} (pc/coerce-params probe-schema params))
        "non-strings are untouched; anyOf and undeclared params are the target's"))
  (testing "nil schema is a no-op"
    (is (= {:ok {:kinds "[\"pods\"]"}}
           (pc/coerce-params nil {:kinds "[\"pods\"]"})))))

(deftest undecodable-value-is-refused-naming-the-param
  (doseq [[k v] {:kinds "pods" :labels "app=x" :limit "many" :correlate "maybe"}]
    (let [res (pc/coerce-params probe-schema {:command "search" k v})]
      (is (contains? res :error) (str k " " (pr-str v)))
      (is (= (name k) (:param res)))
      (is (re-find (re-pattern (str "^" (name k) ": ")) (:message res))))))

;; ---------------------------------------------------------------------------
;; Through handle-multi: a recording stub tool registered like an addon's
;; ---------------------------------------------------------------------------

(def ^:private probe-name "multi-param-coerce-probe")

(def ^:private received (atom nil))

(defn- with-probe-tool [f]
  (reset! received nil)
  (ext/register-tool! {:name probe-name
                       :description "records the params multi forwards"
                       :inputSchema probe-schema
                       :handler (fn [params]
                                  (reset! received params)
                                  {:type "text" :text "ok"})})
  (try (f) (finally (ext/deregister-tool! probe-name))))

(use-fixtures :each with-probe-tool)

(deftest multi-forwards-target-typed-params
  (testing "string-keyed JSON params, as the MCP wire delivers them"
    (let [result (multi/handle-multi {"tool" probe-name
                                      "command" "search"
                                      "kinds" "[\"securitypolicies\",\"pods\"]"
                                      "limit" "5"
                                      "labels" "{\"app\":\"atendimento\"}"})]
      (is (not (:isError result)) (pr-str result))
      (is (= ["securitypolicies" "pods"] (:kinds @received)))
      (is (= 5 (:limit @received)))
      (is (= {"app" "atendimento"} (:labels @received)))
      (is (not (contains? @received :tool))))))

(deftest multi-refuses-undecodable-param-before-the-target-runs
  (let [result (multi/handle-multi {"tool" probe-name
                                    "command" "search"
                                    "kinds" "pods"})]
    (is (:isError result))
    (is (re-find #"kinds" (str (:text result))))
    (is (nil? @received) "the target never saw the raw text")))

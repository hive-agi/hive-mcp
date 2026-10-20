(ns hive-mcp.tools.root-tool-contract-test
  "The MCP root serves only tools operable from their schema alone
   (hive-addon.tool-contract): every core tool satisfies the contract, and both
   entry points to the root refuse one that does not."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.tool-contract :as contract]
            [hive-addon.tool-contract.test :refer [assert-root-tools]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.tools.registry :as registry]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private probe-name "root-contract-probe")

(use-fixtures :each
  (fn [f]
    (ext/deregister-tool! probe-name)
    (try (f) (finally (ext/deregister-tool! probe-name)))))

(deftest every-core-tool-satisfies-the-root-contract
  (assert-root-tools (registry/core-tools)))

(def ^:private conforming
  {:name        probe-name
   :description "Probe tool."
   :inputSchema {:type "object" :properties {"x" {:type "string" :description "x"}}}
   :handler     (fn [_] {:ok true})})

(defn- contract-type [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest register-tool-refuses-an-empty-schema-and-registers-nothing
  (doseq [[label bad] {"empty properties" (assoc-in conforming [:inputSchema :properties] {})
                       "no schema"        (dissoc conforming :inputSchema)
                       "no description"   (dissoc conforming :description)}]
    (testing label
      (is (= :hive-addon/root-tool-contract
             (contract-type #(ext/register-tool! bad))))
      (is (not-any? #(= probe-name (:name %)) (ext/get-registered-tools))))))

(deftest register-tool-accepts-a-conforming-tool
  (is (= probe-name (ext/register-tool! conforming)))
  (is (some #(= probe-name (:name %)) (ext/get-registered-tools))))

(deftest make-tool-checks-the-tool's-own-schema-before-async-params-are-added
  (is (= :hive-addon/root-tool-contract
         (contract-type #(routes/make-tool (assoc-in conforming [:inputSchema :properties] {})))))
  (let [made (routes/make-tool conforming)]
    (is (contains? (get-in made [:inputSchema :properties]) "x"))
    (is (< 1 (count (get-in made [:inputSchema :properties])))
        "async params are still merged into a conforming tool")))

;; A property with no :description is only a WARNING in hive-addon's
;; RootToolDef (hive-addon.tool-contract/warnings). For the root tools
;; hive-mcp OWNS it is a VIOLATION here: a client without source access
;; operates a parameter only through its description. Addon-served roots
;; (carto, cluster, firecrawl, observe, self_steer, swarm-view, role,
;; hive_vtranslate, ...) stay outside this check until each owning repo
;; describes its properties; then the rule moves into RootToolDef itself.
;; TODO(kanban 20261001000454-01498cca): promote the rule into hive-addon RootToolDef.
(defn- undescribed
  "{tool-name [property-key]} for every property of `tools` lacking a
   non-blank :description; {} when every property is described."
  [tools]
  (into (sorted-map)
        (keep (fn [t]
                (when-let [ks (seq (map #(nth (:path %) 2) (contract/warnings t)))]
                  [(:name t) (vec ks)])))
        tools))

(deftest every-core-tool-property-is-described
  (testing "the tool defs hive-mcp owns"
    (is (= {} (undescribed (registry/core-tools)))))
  (testing "as advertised, with the async params make-tool merges in"
    (is (= {} (undescribed (map routes/make-tool (registry/core-tools)))))))

(deftest an-undescribed-property-is-a-violation
  (doseq [[label prop] {"no description"    {:type "string"}
                        "blank description" {:type "string" :description "  "}}]
    (testing label
      (is (= {probe-name ["y"]}
             (undescribed [(assoc-in conforming [:inputSchema :properties "y"] prop)]))))))

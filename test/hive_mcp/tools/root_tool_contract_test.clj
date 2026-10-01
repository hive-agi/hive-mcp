(ns hive-mcp.tools.root-tool-contract-test
  "The MCP root serves only tools operable from their schema alone
   (hive-addon.tool-contract): every core tool satisfies the contract, and both
   entry points to the root refuse one that does not."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
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

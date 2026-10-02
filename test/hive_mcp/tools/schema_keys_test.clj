(ns hive-mcp.tools.schema-keys-test
  "Every tool schema the host emits has provider-legal property keys only.

   Measured live 2026-10-02 under 1.9.0: carto contributed params named
   `kg-rank?`, `preview?`, `apply?`... to `code`/`carto`; the host forwarded
   them, and every new ling died on Anthropic 400 \"tools.N.custom.
   input_schema.properties: Property keys should match pattern
   '^[a-zA-Z0-9_.-]{1,64}$'\"."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.registry :as agent-reg]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.tools.registry :as reg]
            [hive-mcp.tools.schema-keys :as sk]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def provider-pattern #"^[a-zA-Z0-9_.-]{1,64}$")

(defn- offending
  "{tool-name [illegal-key ...]} over `tools`, empty when all are legal.
   Checked against the provider's pattern literally, not through `sk`."
  [tools]
  (into {}
        (keep (fn [t]
                (let [bad (remove #(re-matches provider-pattern
                                               (if (keyword? %) (name %) (str %)))
                                  (keys (get-in t [:inputSchema :properties])))]
                  (when (seq bad) [(:name t) (vec bad)]))))
        tools))

(deftest legal-properties-maps-the-question-mark-to-its-alias
  (testing "a `?` key becomes the `_` alias carto folds back to `:x?`"
    (is (= {"preview_" {:type "boolean"} "scope" {:type "string"}}
           (sk/legal-properties {"preview?" {:type "boolean"} "scope" {:type "string"}})))
    (is (= {:kg-rank_ {:type "boolean"}}
           (sk/legal-properties {:kg-rank? {:type "boolean"}}))))
  (testing "an alias already declared wins; the `?` key is dropped"
    (is (= {:apply_ {:description "documented"}}
           (sk/legal-properties {:apply? {} :apply_ {:description "documented"}}))))
  (testing "an illegal key with no legal spelling is dropped"
    (is (= {"ok" 1} (sk/legal-properties {"ok" 1 "has space" 2 "?" 3}))))
  (testing "a legal map is returned as is"
    (let [m {"a" 1 :b-c 2 "d.e_f" 3}]
      (is (identical? m (sk/legal-properties m))))))

(deftest legal-tool-keeps-required-consistent
  (let [t (sk/legal-tool {:name "t"
                          :inputSchema {:type "object"
                                        :properties {"dry-run?" {:type "boolean"} "x" {}}
                                        :required ["dry-run?" "x"]}})]
    (is (= #{"dry-run_" "x"} (set (keys (get-in t [:inputSchema :properties])))))
    (is (= ["dry-run_" "x"] (get-in t [:inputSchema :required]))))
  (is (= {:name "bare"} (sk/legal-tool {:name "bare"}))))

(defn- with-illegal-contribution
  "Run f while an addon contributes `?` keys to a real root, as carto does."
  [f]
  (let [root (:name (first (reg/get-consolidated-tools)))]
    (ext/register-schema! :schema-keys-test root {:kg-rank? {:type "boolean"}
                                                  "verify?" {:type "boolean"}
                                                  "preview_" {:type "boolean"}})
    (try (f root)
         (finally (ext/retract-schemas-by-owner! :schema-keys-test)))))

(deftest contract-every-advertised-schema-is-provider-legal
  (with-illegal-contribution
    (fn [root]
      (let [tools (reg/get-advertised-tools)]
        (is (= {} (offending tools)))
        (testing "the contribution is still advertised, under its alias"
          (let [props (get-in (first (filter #(= root (:name %)) tools)) [:inputSchema :properties])]
            (is (contains? props :kg-rank_))
            (is (contains? props "verify_"))))
        (testing "the compact projection too"
          (is (= {} (offending (reg/get-advertised-tools {:compact-schema? true})))))))))

(deftest contract-every-server-table-schema-is-provider-legal
  (with-illegal-contribution
    (fn [_]
      (is (= {} (offending (keep #(try (routes/make-tool %) (catch Throwable _ nil))
                                 (reg/get-consolidated-tools))))))))

(deftest contract-every-ling-schema-is-provider-legal
  ;; agent.registry/get-schemas is what hive-agent's catalog projects into a
  ;; ling's provider tool list.
  (let [saved @agent-reg/registry]
    (try
      (reset! agent-reg/registry
              {"code" {:name "code" :handler identity
                       :inputSchema {:type "object"
                                     :properties {"command" {:type "string"}
                                                  "kg-rank?" {:type "boolean"}
                                                  :dry-run? {:type "boolean"}}}}})
      (is (= {} (offending (agent-reg/get-schemas nil))))
      (is (contains? (get-in (first (agent-reg/get-schemas nil)) [:inputSchema :properties])
                     "kg-rank_"))
      (finally (reset! agent-reg/registry saved)))))

(deftest mutant-unprojected-surface-is-caught
  ;; The contract above must fail on the 1.9.0 surface: a schema carrying the
  ;; contributed `?` keys verbatim.
  (is (= {"code" [:kg-rank?]}
         (offending [{:name "code" :inputSchema {:properties {:kg-rank? {} "command" {}}}}]))))

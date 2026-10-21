(ns hive-mcp.agent.provider.contract-coverage-test
  "Gate: every public fn under src/hive_mcp/agent/provider carries an `m/=>`
   contract or a REASONED exemption, and no exemption is stale. Also the
   trifectas of the domain predicates (`provider.model`), the collectors
   (`provider.collect`, read against this JVM's config) and the preflight
   boundary with its transport seam bound to a stub. The policy stratum has its
   own trifecta namespace."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.provider.collect :as collect]
            [hive-mcp.agent.provider.model :as model]
            [hive-mcp.agent.provider.preflight :as preflight]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :as hst]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage provider-contracts-gate
  {:paths ["src/hive_mcp/agent/provider"]
   :exempt {}})

(def ^:private EntryLike
  "Seed entries, dispatch-routed entries, malformed maps and non-maps."
  [:or (into [:enum] (vals model/seed-registry))
   [:map [:dispatch [:enum :anthropic-oauth :chatgpt-oauth :subscription]]]
   [:map [:api-url [:enum "https://x.test/v1/models"]]]
   [:enum nil "entry" 7]])

(hst/deftrifecta-from-schema valid-provider-entry-contract
  hive-mcp.agent.provider.model/valid-provider-entry?
  {:in        EntryLike
   :out       :boolean
   ;; every seeded entry is valid; a URL that does not end in
   ;; /chat/completions, or a non-map, is not
   :rel       (fn [in out]
                (cond
                  (some #{in} (vals model/seed-registry)) (true? out)
                  (not (map? in))                         (false? out)
                  (:api-url in)                           (false? out)
                  :else                                   true))
   :n-cases   16
   :golden-path "test/golden/agent/provider/model-valid-entry.edn"})

(hst/deftrifecta-from-schema dispatch-routed-contract
  hive-mcp.agent.provider.model/dispatch-routed?
  {:in        EntryLike
   :out       :boolean
   :rel       (fn [in out] (= out (boolean (and (map? in) (:dispatch in)))))
   :n-cases   16
   :golden-path "test/golden/agent/provider/model-dispatch-routed.edn"})

(hst/deftrifecta-from-schema openai-compat-contract
  hive-mcp.agent.provider.model/openai-compat?
  {:in        EntryLike
   :out       :boolean
   ;; exactly the maps that are not dispatch-routed
   :rel       (fn [in out] (= out (and (map? in) (nil? (:dispatch in)))))
   :n-cases   16})

(hst/deftrifecta-from-schema config-providers-contract
  hive-mcp.agent.provider.collect/config-providers
  {:in        [:cat]
   :out       [:maybe :map]})

(hst/deftrifecta-from-schema config-priority-contract
  hive-mcp.agent.provider.collect/config-priority
  {:in        [:cat]
   :out       [:maybe [:sequential :any]]})

(hst/deftrifecta-from-schema agent-type-defaults-contract
  hive-mcp.agent.provider.collect/agent-type-defaults
  {:in        [:enum :ling :drone "ling" :nope nil]
   :out       [:maybe :map]
   ;; a keyword and its name read the same entry
   :rel       (fn [in out] (= out (collect/agent-type-defaults (some-> in name keyword))))})

(hst/deftrifecta-from-schema present-secret-keys-contract
  hive-mcp.agent.provider.collect/present-secret-keys
  {:in        [:or [:vector {:max 4} [:enum :venice-api-key :groq-api-key :zz-unset-key]]
               [:set [:enum :openrouter-api-key :zz-unset-key]] :nil]
   :out       [:set :keyword]
   ;; a subset of what was asked, and an unconfigured key is never present
   :rel       (fn [in out]
                (and (every? (set in) out) (not (contains? out :zz-unset-key))))})

(hst/deftrifecta-from-schema secret-value-contract
  hive-mcp.agent.provider.collect/secret-value
  {:in        [:enum :zz-unset-key :venice-api-key "zz-unset-key"]
   :out       [:maybe :string]
   :rel       (fn [in out] (if (= :zz-unset-key in) (nil? out) true))})

(def ^:private Refusal
  (-> (m/function-schemas)
      (get-in ['hive-mcp.agent.provider.preflight 'refusal :schema])
      m/children second))

(deftest preflight-refusal-contract-test
  (testing "no provider asks nothing"
    (is (nil? (preflight/refusal nil))))
  (testing "an unknown or keyless provider has nothing to refuse"
    (is (nil? (preflight/refusal :zz-no-such-provider)))
    (is (nil? (preflight/refusal :ollama-compat))))
  (testing "whatever the probe answers, the answer is a refusal map or nil"
    (doseq [answer [{:status 401 :body nil}
                    {:status 200 :body {:data {:limit_remaining 0}}}
                    {:status 200 :body {:data {:limit_remaining 5}}}]]
      (binding [preflight/*http-get* (fn [_url _headers _timeout] answer)]
        (is (m/validate Refusal (preflight/refusal :openrouter)) (pr-str answer)))))
  (testing "a probe that throws is inconclusive and passes"
    (binding [preflight/*http-get* (fn [_ _ _] (throw (ex-info "network down" {})))]
      (is (m/validate Refusal (preflight/refusal :openrouter))))))

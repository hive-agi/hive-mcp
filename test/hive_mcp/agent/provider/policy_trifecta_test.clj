(ns hive-mcp.agent.provider.policy-trifecta-test
  "Trifectas for the provider PROMOTE stratum (`hive-mcp.agent.provider.policy`),
   driven by its contracts, over a small fixed registry: a slice of the seed
   plus config that gives entries models, aliases and a refusal list. Every
   relation restates the docstring's rule without calling the subject."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.provider.model :as model]
            [hive-mcp.agent.provider.policy :as policy]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private registry
  "Seed entries for one provider of each kind, models configured on some,
   a config-only keyless provider and one entry with its own refusal list."
  (-> (select-keys model/seed-registry [:anthropic :chatgpt :kimi :openrouter :venice :groq])
      (assoc-in [:venice :default-model] "qwen3")
      (assoc-in [:venice :available-models] ["qwen3" "llama-4"])
      (assoc-in [:anthropic :default-model] "claude-sonnet-4-6")
      (assoc-in [:anthropic :model-aliases] {"opus" "claude-opus-4-1"})
      (assoc-in [:chatgpt :default-model] "gpt-5")
      (assoc-in [:groq :deny-models] ["^llama"])
      (assoc :zeta {:api-url "https://api.zeta.test/v1/chat/completions" :secret-key nil})))

(def ^:private Reg
  "The fixed registry as a schema. A map literal in second position would read
   as properties, hence the explicit empty properties."
  [:= {} registry])

(def ^:private Provider (into [:enum :nope] (sort (keys registry))))

(def ^:private Model
  [:enum "qwen3" "llama-4" "claude-sonnet-4-6" "anthropic/claude-opus-4" "opus" "claude"
   "gpt-5" "openai/gpt-4o" "gpt-oss-120b" "o3" "kimi-k2" "codex" "mistral-large" "venice:qwen3"
   "groq:llama-4" "nope:x" "  Sonnet "])

(defn- compat? [p] (let [e (get registry p)] (and (map? e) (not (:dispatch e)))))

(hst/deftrifecta-from-schema overlay-contract
  hive-mcp.agent.provider.policy/overlay
  {:in        [:cat Reg
               [:or [:map-of [:enum :venice :groq :new "str-key"]
                     [:or [:map [:default-model [:enum "m1" "m2"]]] [:enum false nil 42]]]
                    [:enum nil "not-a-map"]]]
   :out       model/Registry
   ;; map merges (config wins), false/nil removes, anything else is ignored,
   ;; and a non-map config leaves the seed alone
   :rel       (fn [[seed cfg] out]
                (if-not (map? cfg)
                  (= seed out)
                  (every? (fn [k]
                            (let [v (get cfg k ::absent)]
                              (cond
                                (= ::absent v)           (= (get seed k) (get out k))
                                (map? v)                 (= (merge (get seed k) v) (get out k))
                                (or (nil? v) (false? v)) (not (contains? out k))
                                :else                    (= (get seed k) (get out k)))))
                          (distinct (concat (keys seed) (filter keyword? (keys cfg)))))))
   :num-tests 200})

(hst/deftrifecta-from-schema discovery-order-contract
  hive-mcp.agent.provider.policy/discovery-order
  {:in        [:cat Reg [:= model/seed-priority]
               [:or [:vector {:max 4} [:enum :venice "groq" :anthropic :zeta :nope :openai]]
                    [:enum nil "not-sequential"]]]
   :out       [:vector :keyword]
   :rel       (fn [[reg seed-order configured] out]
                (and (every? compat? out)
                     (if (sequential? configured)
                       (= out (filterv compat? (map keyword configured)))
                       (and (= (take (count (filter compat? seed-order)) out)
                               (filter compat? seed-order))
                            (= (set out) (set (filter compat? (keys reg))))))))
   :num-tests 200})

(hst/deftrifecta-from-schema available-contract
  hive-mcp.agent.provider.policy/available
  {:in        [:cat Reg [:vector {:max 5} Provider]
               [:set [:enum :venice-api-key :groq-api-key :openrouter-api-key]]]
   :out       [:vector :keyword]
   ;; order kept; a provider passes when it needs no secret or its key is present
   :rel       (fn [[reg order present] out]
                (= out (filterv #(let [k (:secret-key (get reg %))]
                                   (or (nil? k) (contains? present k)))
                                order)))
   :num-tests 200})

(hst/deftrifecta-from-schema secret-keys-of-contract
  hive-mcp.agent.provider.policy/secret-keys-of
  {:in        [:cat Reg [:vector {:max 5} Provider]]
   :out       [:vector :keyword]
   :rel       (fn [[reg order] out] (= out (vec (keep #(:secret-key (get reg %)) order))))})

(hst/deftrifecta-from-schema validate-provider-contract
  hive-mcp.agent.provider.policy/validate-provider
  {:in        [:cat Reg Provider]
   :out       [:maybe model/UnknownProvider]
   :rel       (fn [[reg p] out] (= (nil? out) (contains? reg p)))
   :n-cases   12})

(hst/deftrifecta-from-schema validate-model-contract
  hive-mcp.agent.provider.policy/validate-model
  ;; :venice is the provider that declares :available-models, so it is most
  ;; of the domain: the refusal arm must be reached by the seeded cases too
  {:in        [:cat Reg [:enum :venice :groq :nope] [:enum "qwen3" "mistral-large" "o3" nil]]
   :out       [:maybe model/UnknownModel]
   ;; only a provider that declares :available-models refuses
   :rel       (fn [[reg p mdl] out]
                (let [avail (:available-models (get reg p))]
                  (= (some? out) (boolean (and (seq avail) (not (some #{mdl} avail)))))))
   :num-tests 200
   :n-cases   16
   :golden-path "test/golden/agent/provider/policy-validate-model.edn"})

(hst/deftrifecta-from-schema model-refusal-contract
  hive-mcp.agent.provider.policy/model-refusal
  {:in        [:cat Reg Provider Model]
   :out       [:maybe model/ModelDenied]
   ;; only a keyed OpenAI-compat entry refuses, naming one of its own patterns
   :rel       (fn [[reg p _] out]
                (or (nil? out)
                    (and (compat? p)
                         (some? (:secret-key (get reg p)))
                         (some #{(:pattern out)}
                               (get (get reg p) :deny-models model/subscription-only-models)))))
   :num-tests 300
   :golden-path "test/golden/agent/provider/policy-model-refusal.edn"})

(hst/deftrifecta-from-schema unresolved-routing-contract
  hive-mcp.agent.provider.policy/unresolved-routing
  {:in        [:cat [:enum :ling :drone "worker" nil]
               [:map [:provider {:optional true} [:enum :venice :groq]]
                [:model {:optional true} [:enum "qwen3"]]]]
   :out       [:maybe model/UnresolvedRouting]
   :rel       (fn [[_ {:keys [provider model]}] out]
                (if (and provider model)
                  (nil? out)
                  (= (:error out) (if provider :model-not-configured :provider-not-configured))))
   :golden-path "test/golden/agent/provider/policy-unresolved-routing.edn"})

(hst/deftrifecta-from-schema parse-model-prefix-contract
  hive-mcp.agent.provider.policy/parse-model-prefix
  {:in        [:cat Reg [:or Model :nil]]
   :out       [:tuple [:maybe :keyword] [:maybe :string]]
   ;; only a prefix naming a known provider is split off
   :rel       (fn [[reg mdl] [p tail]]
                (if p
                  (and (contains? reg p) (= mdl (str (name p) ":" tail)))
                  (= mdl tail)))
   :num-tests 200})

(hst/deftrifecta-from-schema claude-model-name-contract
  hive-mcp.agent.provider.policy/claude-model-name?
  {:in        [:or Model :nil :int]
   :out       [:maybe :boolean]
   :rel       (fn [in out]
                (if (string? in)
                  (= out (str/starts-with? (if (str/starts-with? in "anthropic/")
                                             (subs in 10)
                                             in)
                                           "claude-"))
                  (nil? out)))
   :n-cases   16
   :golden-path "test/golden/agent/provider/policy-claude-model-name.edn"})

(hst/deftrifecta-from-schema subscription-route-contract
  hive-mcp.agent.provider.policy/subscription-route
  {:in        [:cat Reg [:or Model :nil]]
   :out       [:maybe [:map [:provider :keyword] [:model [:maybe :string]]]]
   ;; a route only ever lands on a subscription provider
   :rel       (fn [[_ mdl] out]
                (or (nil? out)
                    (and (string? mdl) (contains? #{:anthropic :chatgpt :kimi} (:provider out)))))
   :num-tests 200
   :golden-path "test/golden/agent/provider/policy-subscription-route.edn"})

(hst/deftrifecta-from-schema routing-refusal-contract
  hive-mcp.agent.provider.policy/routing-refusal
  {:in        [:cat Reg Provider Model]
   :out       [:maybe model/ModelUnroutable]
   ;; an OpenAI-compat provider never refuses here
   :rel       (fn [[_ p _] out] (or (nil? out) (not (compat? p))))
   :num-tests 200
   :golden-path "test/golden/agent/provider/policy-routing-refusal.edn"})

(hst/deftrifecta-from-schema missing-secret-contract
  hive-mcp.agent.provider.policy/missing-secret
  {:in        [:cat Reg Provider [:set [:enum :venice-api-key :anthropic-api-key]]]
   :out       [:maybe model/SecretMissing]
   :rel       (fn [[reg p present] out]
                (let [k (:secret-key (get reg p))]
                  (= (some? out) (boolean (and (compat? p) k (not (contains? present k)))))))
   :num-tests 200})

(hst/deftrifecta-from-schema credential-refusal-contract
  hive-mcp.agent.provider.policy/credential-refusal
  ;; every probe answer carries both keys, so the seeded cases reach both
  ;; refusal arms; a nil or partial answer is covered in refusal-arms-discriminate-test
  {:in        [:cat [:enum :openrouter :venice]
               [:map [:status [:enum 200 401 403 500]] [:remaining [:enum 0 -1 0.5]]]]
   :out       [:maybe model/CredentialRefused]
   :rel       (fn [[_ {:keys [status remaining]}] out]
                (cond
                  (#{401 403} status) (= :provider-credential-rejected (:error out))
                  (and (int? status) (<= 200 status 299) (number? remaining) (<= remaining 0))
                  (= :provider-quota-exhausted (:error out))
                  :else (nil? out)))
   :num-tests 200
   :n-cases   16
   :golden-path "test/golden/agent/provider/policy-credential-refusal.edn"})

(hst/deftrifecta-from-schema strip-anthropic-prefix-contract
  hive-mcp.agent.provider.policy/strip-anthropic-prefix
  {:in        [:or Model :nil]
   :out       [:maybe :string]
   :rel       (fn [in out]
                (if (and in (str/starts-with? in "anthropic/"))
                  (= (str "anthropic/" out) in)
                  (= in out)))
   :n-cases   16
   :golden-path "test/golden/agent/provider/policy-strip-anthropic-prefix.edn"})

(hst/deftrifecta-from-schema resolve-routing-contract
  hive-mcp.agent.provider.policy/resolve-routing
  {:in        [:cat Reg
               [:map
                [:provider {:optional true} [:enum :venice "groq" :anthropic]]
                [:model {:optional true} Model]
                [:type-defaults {:optional true}
                 [:map [:provider [:enum :groq]] [:model [:enum "mistral-large"]]]]
                [:fallback-provider {:optional true} [:enum :openrouter]]]]
   :out       model/Routing
   ;; an explicit provider always wins
   :rel       (fn [[_ {:keys [provider]}] out]
                (or (nil? provider) (= (keyword provider) (:provider out))))
   :num-tests 300
   :golden-path "test/golden/agent/provider/policy-resolve-routing.edn"})

(deftest routing-precedence-test
  (testing "prefix, then subscription route, then type default, then fallback"
    (is (= {:provider :venice :model "qwen3"} (policy/resolve-routing registry {:model "venice:qwen3"})))
    (is (= {:provider :anthropic :model "claude-opus-4"}
           (policy/resolve-routing registry {:model "anthropic/claude-opus-4"})))
    (is (= {:provider :groq :model "mistral-large"}
           (policy/resolve-routing registry {:type-defaults {:provider :groq :model "mistral-large"}})))
    (is (= {:provider :openrouter :model nil}
           (policy/resolve-routing registry {:fallback-provider :openrouter})))
    (is (= {:provider nil :model nil} (policy/resolve-routing registry nil)))))

(deftest refusal-arms-discriminate-test
  (let [rejected      (policy/credential-refusal :venice {:status 401})
        drained       (policy/credential-refusal :venice {:status 200 :remaining 0})
        [arm-a arm-b] (rest model/CredentialRefused)]
    (is (m/validate arm-a rejected))
    (is (not (m/validate arm-b rejected)))
    (is (m/validate arm-b drained))
    (is (not (m/validate arm-a drained))))
  (testing "a nil or partial probe answer is inconclusive"
    (is (nil? (policy/credential-refusal :venice nil)))
    (is (nil? (policy/credential-refusal :venice {:status 200})))
    (is (nil? (policy/credential-refusal :venice {:remaining 0})))))

(deftest provider-schemas-generate-test
  (doseq [s [model/Registry model/SecretKeys model/Routing model/RoutingRequest model/CredentialProbe
             model/UnknownProvider model/UnknownModel model/ModelDenied model/UnresolvedRouting
             model/ModelUnroutable model/SecretMissing model/CredentialRefused]]
    (is (seq (mg/sample s {:size 4})) (pr-str s))))

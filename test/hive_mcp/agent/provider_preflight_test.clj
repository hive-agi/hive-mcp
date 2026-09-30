(ns hive-mcp.agent.provider-preflight-test
  "Spawn fail-fast: an unroutable model/provider pair and a dead provider key
   are refused at spawn time, not at the ling's first turn.

   No network: the provider registry, the secret lookup and the HTTP transport
   are all stubbed. The registry is a fixture, so the checks are shown to read
   the OPEN registry rather than a hard-coded provider list."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.provider :as provider]
            [hive-mcp.agent.provider.collect :as collect]
            [hive-mcp.agent.provider.policy :as policy]
            [hive-mcp.agent.provider.preflight :as preflight]
            [hive-mcp.agent.ling.headless-registry :as headless-registry]
            [hive-mcp.tools.agent.spawn :as spawn]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private registry
  {:anthropic {:dispatch :anthropic-oauth
               :secret-key :anthropic-api-key
               :default-model "claude-opus-5-5"}
   :chatgpt   {:dispatch :chatgpt-oauth :secret-key nil}
   :codex     {:dispatch :subscription :secret-key nil}
   :acme      {:api-url "https://acme.test/v1/chat/completions"
               :secret-key :acme-api-key
               :auth-probe {:kind :acme-key-info :url "https://acme.test/key"}}
   :keyed     {:api-url "https://keyed.test/v1/chat/completions"
               :secret-key :keyed-api-key
               :auth-probe {:kind :openrouter-key-info :url "https://keyed.test/key"}}
   :local     {:api-url "http://localhost:11434/v1/chat/completions"
               :secret-key nil}})

(defmacro ^:private with-world
  "Run BODY against the fixture registry with SECRETS {secret-key value} and
   HTTP-GET as the probe transport. Any real network call fails the test."
  [secrets http-get & body]
  `(let [secrets# ~secrets]
     (with-redefs [provider/effective-registry (constantly registry)
                   collect/secret-value        (fn [k#] (get secrets# k#))
                   collect/present-secret-keys (fn [ks#] (into #{} (filter #(contains? secrets# %)) ks#))]
       (binding [preflight/*http-get* ~http-get]
         ~@body))))

(defn- no-network [url _ _]
  (throw (ex-info "network touched in a unit test" {:url url})))

;; =============================================================================
;; Routing: a pair no client can serve is refused at resolve time
;; =============================================================================

(deftest routing-refusal-test
  (testing "an OpenAI-family model forced onto the Anthropic client is refused"
    (let [err (policy/routing-refusal registry :anthropic "openai/gpt-6-sol")]
      (is (= :model-unroutable-on-provider (:error err)))
      (is (= :anthropic (:provider err)))
      (is (re-find #"provider" (:fix err)))))

  (testing "a dispatch provider still carries its own family, aliases and default"
    (is (nil? (policy/routing-refusal registry :anthropic "claude-opus-5-5")))
    (is (nil? (policy/routing-refusal registry :anthropic "anthropic/claude-sonnet-4-6")))
    (is (nil? (policy/routing-refusal registry :anthropic "opus")))
    (is (nil? (policy/routing-refusal registry :chatgpt "gpt-6-sol"))))

  (testing "an OpenAI-compat provider or a dispatch entry with no routed family refuses nothing here"
    (is (nil? (policy/routing-refusal registry :acme "openai/gpt-6-sol")))
    (is (nil? (policy/routing-refusal registry :codex "anything"))))

  (testing "a nil model is not a routing question"
    (is (nil? (policy/routing-refusal registry :anthropic nil)))))

(deftest resolve-provider-model-routes-or-refuses-test
  (with-redefs [provider/effective-registry        (constantly registry)
                provider/best-available-provider   (constantly nil)
                collect/agent-type-defaults        (constantly nil)]
    (testing "openai/<model> with no provider routes to the ChatGPT client, never Anthropic"
      (is (= {:provider :chatgpt :model "gpt-6-sol"}
             (provider/resolve-provider-model {:model "openai/gpt-6-sol" :agent-type :ling}))))

    (testing "openai/<model> explicitly on :anthropic throws at spawn, naming the pair"
      (let [e (is (thrown? clojure.lang.ExceptionInfo
                           (provider/resolve-provider-model
                            {:provider :anthropic :model "openai/gpt-6-sol" :agent-type :ling})))]
        (is (= :model-unroutable-on-provider (:error (ex-data e))))))))

;; =============================================================================
;; Credentials: pure verdict over a probe answer
;; =============================================================================

(deftest credential-refusal-test
  (testing "401 and 403 refuse"
    (is (= :provider-credential-rejected
           (:error (policy/credential-refusal :acme {:status 403}))))
    (is (= :provider-credential-rejected
           (:error (policy/credential-refusal :acme {:status 401})))))

  (testing "a 2xx with no credit left refuses"
    (is (= :provider-quota-exhausted
           (:error (policy/credential-refusal :acme {:status 200 :remaining 0})))))

  (testing "a usable key, an unlimited key, or an inconclusive answer passes"
    (is (nil? (policy/credential-refusal :acme {:status 200 :remaining 3.5})))
    (is (nil? (policy/credential-refusal :acme {:status 200 :remaining nil})))
    (is (nil? (policy/credential-refusal :acme {:status 500})))
    (is (nil? (policy/credential-refusal :acme {:status nil})))))

(deftest missing-secret-test
  (testing "a keyed OpenAI-compat provider without its secret refuses"
    (is (= :provider-secret-missing
           (:error (policy/missing-secret registry :acme #{})))))
  (testing "with the secret, a keyless provider, or a dispatch provider passes"
    (is (nil? (policy/missing-secret registry :acme #{:acme-api-key})))
    (is (nil? (policy/missing-secret registry :local #{})))
    (is (nil? (policy/missing-secret registry :anthropic #{})))))

;; =============================================================================
;; Preflight boundary, stubbed transport
;; =============================================================================

(deftest preflight-refusal-test
  (testing "no secret configured: refused before any network call"
    (with-world {} no-network
      (is (= :provider-secret-missing (:error (preflight/refusal :keyed))))))

  (testing "a key over its limit (the openrouter 403) fails the spawn"
    (with-world {:keyed-api-key "k"} (fn [_ _ _] {:status 403 :body nil})
      (is (= :provider-credential-rejected (:error (preflight/refusal :keyed))))))

  (testing "an exhausted limit_remaining fails the spawn"
    (with-world {:keyed-api-key "k"}
      (fn [_ _ _] {:status 200 :body {:data {:limit_remaining 0}}})
      (is (= :provider-quota-exhausted (:error (preflight/refusal :keyed))))))

  (testing "the probe sends the configured key as a bearer token"
    (let [seen (atom nil)]
      (with-world {:keyed-api-key "sekrit"}
        (fn [url headers _] (reset! seen [url headers])
          {:status 200 :body {:data {:limit_remaining 10}}})
        (is (nil? (preflight/refusal :keyed))))
      (is (= ["https://keyed.test/key" {"Authorization" "Bearer sekrit"}] @seen))))

  (testing "a transport failure is inconclusive and passes"
    (with-world {:keyed-api-key "k"} (fn [_ _ _] (throw (java.io.IOException. "down")))
      (is (nil? (preflight/refusal :keyed)))))

  (testing "a probe kind no method knows is skipped: the registry stays open"
    (with-world {:acme-api-key "k"} no-network
      (is (nil? (preflight/refusal :acme)))))

  (testing "a new probe kind is added by a method, not by editing preflight"
    (defmethod preflight/probe-credential ::test-kind [_ _] {:status 401})
    (try
      (with-redefs [provider/effective-registry
                    (constantly (assoc-in registry [:acme :auth-probe :kind] ::test-kind))
                    collect/secret-value        (constantly "k")
                    collect/present-secret-keys (fn [ks] (set ks))]
        (is (= :provider-credential-rejected (:error (preflight/refusal :acme)))))
      (finally (remove-method preflight/probe-credential ::test-kind))))

  (testing "keyless and dispatch-routed providers, and nil, pass without network"
    (with-world {} no-network
      (is (nil? (preflight/refusal :local)))
      (is (nil? (preflight/refusal :anthropic)))
      (is (nil? (preflight/refusal nil))))))

;; =============================================================================
;; Spawn gate
;; =============================================================================

(deftest spawn-preflight-gate-test
  (testing "a headless backend spawn is checked"
    (with-redefs [headless-registry/registered-headless (constantly #{:stub-backend})
                  preflight/refusal (fn [p] {:error :provider-credential-rejected :provider p :fix "x"})]
      (is (= :provider-credential-rejected
             (:error (spawn/provider-preflight-refusal :stub-backend :keyed))))))

  (testing "a terminal mode runs its own CLI and is not checked"
    (with-redefs [headless-registry/registered-headless (constantly #{:stub-backend})
                  preflight/refusal (fn [_] (throw (ex-info "must not be called" {})))]
      (is (nil? (spawn/provider-preflight-refusal :vterm :keyed)))
      (is (nil? (spawn/provider-preflight-refusal :stub-backend nil))))))

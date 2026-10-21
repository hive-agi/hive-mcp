(ns hive-mcp.agent.provider.preflight-test
  "Golden over `hive-mcp.agent.provider.preflight/refusal`, with both of its
   ports injected: the host config through `*config-source*` (a static
   source carrying the secrets and a custom provider) and the probe
   transport through `*http-get*` (a stub answering by key). No network and
   no developer config is read."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.agent.provider.preflight :as pf]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.source :as source]
            [hive-test.golden :refer [deftest-golden-fn]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- stub-http-get
  "Answers the openrouter key-info probe by the bearer token it was sent."
  [_url headers _timeout]
  (let [auth (some (fn [[k v]] (when (= "authorization" (.toLowerCase (str k))) v)) headers)]
    (cond
      (= auth "Bearer sk-rejected") {:status 401 :body nil}
      (= auth "Bearer sk-empty")    {:status 200 :body {:data {:limit_remaining 0}}}
      (= auth "Bearer sk-boom")     (throw (ex-info "connection refused" {}))
      :else                         {:status 200 :body {:data {:limit_remaining 12.5}}})))

;; A custom provider whose secret (named so no env var supplies it) is absent.
(def ^:private custom
  {:f02-custom {:api-url "https://h/v1/chat/completions" :secret-key :f02-missing-key}})

(defn- refusal-with [secrets provider]
  (binding [config/*config-source* (source/static-source {:secrets secrets :llm-providers custom})
            pf/*http-get*          stub-http-get]
    (pf/refusal provider)))

(defn- strip-fix [r] (some-> r (update :fix string?)))

(deftest-golden-fn preflight-refusal-golden
  "test/golden/agent/provider-preflight-refusal.edn"
  (fn []
    {:no-provider     (refusal-with {} nil)
     :keyless         (refusal-with {} :ollama-compat)
     :subscription    (refusal-with {} :codex)
     :missing-secret  (refusal-with {} :f02-custom)
     :custom-has-key  (refusal-with {:f02-missing-key "k"} :f02-custom)
     :openrouter-ok   (refusal-with {:openrouter-api-key "sk-good"} :openrouter)
     :rejected        (strip-fix (refusal-with {:openrouter-api-key "sk-rejected"} :openrouter))
     :quota-exhausted (strip-fix (refusal-with {:openrouter-api-key "sk-empty"} :openrouter))
     :probe-throws    (refusal-with {:openrouter-api-key "sk-boom"} :openrouter)}))

(deftest fix-text-names-the-provider-test
  (is (re-find #"openrouter" (:fix (refusal-with {:openrouter-api-key "sk-rejected"} :openrouter)))))

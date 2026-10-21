(ns hive-mcp.agent.provider.model-test
  "Trifectas for the provider-entry predicates of
   `hive-mcp.agent.provider.model`, over a domain that mixes entries drawn
   from `ProviderEntry` itself (every dispatch arm and the OpenAI-compat
   default) with malformed entries and non-maps."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.agent.provider.model :as pm]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Entry
  "A valid ProviderEntry, a near-miss map, or a non-map."
  [:or
   pm/ProviderEntry
   [:map [:dispatch [:enum :anthropic-oauth :subscription :bogus]] [:secret-key [:enum "str" 7]]]
   [:map [:api-url [:enum "http://x/v1" ""]] [:secret-key [:maybe :keyword]]]
   [:enum nil "entry" 0 [:a]]])

(deftrifecta-from-schema valid-provider-entry?-trifecta
  hive-mcp.agent.provider.model/valid-provider-entry?
  {:in          Entry
   :out         :boolean
   :rel         (fn [e out] (= out (m/validate pm/ProviderEntry e)))
   :num-tests   300
   :n-cases     12
   :golden-path "test/golden/agent/provider-model-valid-entry.edn"})

(deftrifecta-from-schema dispatch-routed?-trifecta
  hive-mcp.agent.provider.model/dispatch-routed?
  {:in        Entry
   :out       :boolean
   :rel       (fn [e out] (= out (and (map? e) (some? (:dispatch e)) (not (false? (:dispatch e))))))
   :num-tests 300
   :n-cases   12})

(deftrifecta-from-schema openai-compat?-trifecta
  hive-mcp.agent.provider.model/openai-compat?
  {:in        Entry
   :out       :boolean
   :rel       (fn [e out] (= out (and (map? e) (not (pm/dispatch-routed? e)))))
   :num-tests 300
   :n-cases   12})

(deftest entry-arms-test
  (is (true? (pm/openai-compat? {:api-url "https://h/v1/chat/completions" :secret-key nil})))
  (is (true? (pm/dispatch-routed? {:dispatch :anthropic-oauth :secret-key :k})))
  (is (false? (pm/openai-compat? nil))))

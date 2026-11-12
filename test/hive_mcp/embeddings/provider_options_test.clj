;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.embeddings.provider-options-test
  "provider-options is the data-driven replacement for build-resolved's
   `case impl` ladder. The secret lookup is a port, so these tests inject a
   stub map instead of redefining hive-mcp.config.core/get-secret."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.embeddings.service :as service]))

(def ^:private secrets
  {:openrouter-api-key "or-key" :openai-api-key "oa-key" :venice-api-key "vv-key"})

(deftest ollama-options-come-from-the-spec
  (testing ":max-tokens becomes :num-ctx, :host defaults to localhost"
    (is (= {:host "http://localhost:11434" :num-ctx 8192 :vram-mb nil}
           (service/provider-options secrets {:impl :ollama :max-tokens 8192})))
    (is (= {:host "http://gpu:11434" :num-ctx 512 :vram-mb 4096}
           (service/provider-options secrets {:impl :ollama :host "http://gpu:11434"
                                               :max-tokens 512 :vram-mb 4096})))))

(deftest keyed-providers-read-their-own-secret-through-the-port
  (doseq [[impl k] {:openrouter "or-key" :openai "oa-key" :venice "vv-key"}]
    (is (= {:api-key k} (service/provider-options secrets {:impl impl}))
        (str impl " reads its own key"))))

(deftest an-unknown-impl-gets-no-options
  (is (= {} (service/provider-options secrets {:impl :mystery})))
  (is (= {} (service/provider-options (constantly "leak") {:impl :mystery}))
      "an unknown impl never asks the secret port"))

(deftest every-ladder-arm-is-a-registered-impl
  (is (= #{:ollama :openrouter :openai :venice}
         (set (keys service/impl->options)))))

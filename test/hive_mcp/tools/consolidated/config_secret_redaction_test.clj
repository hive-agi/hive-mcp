(ns hive-mcp.tools.consolidated.config-secret-redaction-test
  "Every config tool response redacts secrets to state + source."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [hive-mcp.tools.consolidated.config :as config-tool]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.merge :as merge]
            [hive-mcp.config.source :as src]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private fake-api-key "qZ7xK9wJ3mVb8nR2tY6pL4hF")
(def ^:private fake-token "Xw8Qj2Vn5Kz9Rt3Lm7Yp1Hc6")
(def ^:private fake-password 98765432)

(def ^:private fakes [fake-api-key fake-token (str fake-password)])

(defn- fixture-config []
  (-> merge/default-config
      (assoc-in [:secrets :openrouter-api-key] fake-api-key)
      (assoc-in [:secrets :venice-api-key] "pass:hive/venice")
      (assoc-in [:secrets :groq-api-key] nil)
      (assoc-in [:services :fixture :auth-token] fake-token)
      (assoc-in [:services :fixture :password] fake-password)
      (assoc-in [:services :fixture :host] "http://fixture-host")))

(defn- leaked-substring
  "First substring of a fake secret longer than 4 chars present in `text`."
  [text]
  (first (for [s fakes
               i (range (- (count s) 4))
               :let [sub (subs s i (+ i 5))]
               :when (str/includes? text sub)]
           sub)))

(defn- call [params]
  (binding [config/*config-source* (src/atom-source (atom (fixture-config)) merge/default-config)]
    (config-tool/handle-config params)))

(defn- parsed [r] (json/read-str (:text r) :key-fn keyword))

(deftest every-response-redacts-secrets
  (doseq [params [{:command "validate"}
                  {:command "list"}
                  {:command "get" :key "secrets"}
                  {:command "get" :key "secrets.openrouter-api-key"}
                  {:command "get" :key "services"}
                  {:command "get" :key "services.fixture"}
                  {:command "get" :key "services.fixture.auth-token"}
                  {:command "set" :key "services.fixture.api-key" :value fake-api-key}
                  {:command "set" :key "secrets.groq-api-key" :value fake-token}]]
    (testing (pr-str (dissoc params :value))
      (let [r (call params)]
        (is (string? (:text r)))
        (is (nil? (leaked-substring (:text r))))))))

(deftest secrets-report-state-and-source
  (let [v (:value (parsed (call {:command "get" :key "secrets"})))]
    (is (= {:state "set" :source "config"} (:openrouter-api-key v)))
    (is (= {:state "set" :source "pass"} (:venice-api-key v)))
    (is (= {:state "unset"} (:groq-api-key v))))
  (testing "sensitive key names outside :secrets"
    (let [v (:value (parsed (call {:command "get" :key "services.fixture"})))]
      (is (= {:state "set" :source "config"} (:auth-token v)))
      (is (= {:state "set" :source "config"} (:password v)))
      (is (= "http://fixture-host" (:host v)) "non-secret values pass through"))))

(deftest non-secret-set-echoes-value
  (let [p (parsed (call {:command "set" :key "embeddings.ollama.host" :value "http://h:1"}))]
    (is (= "http://h:1" (:value p)))
    (is (= "http://h:1" (:raw-input p)))))

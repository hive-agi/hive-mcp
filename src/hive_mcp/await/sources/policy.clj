(ns hive-mcp.await.sources.policy
  "Operator allowlists for the polling await sources. A polling source is a
   read primitive over everything the hive-mcp process can reach, so what
   it may reach is the OPERATOR's decision, read from config, and enforced
   when the awaiter is created (before anything is recorded):

     :await {:sql  {:connection-refs [\"k8s:sisf/staging-pg-ro/url\" ...]}
             :k8s  {:contexts [\"blw\"] :namespaces [\"sisf\"]}
             :prom {:targets {\"default\" \"http://prometheus:9090\"
                              \"baston\"  \"http://...\"}}}

   Default DENY: an empty or absent list refuses every request of that
   kind, and the refusal names the config key to set. The prom target
   \"default\" also falls back to services.prometheus.url.

   *policy* overrides config (the test seam)."
  (:require [clojure.string :as str]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:dynamic *policy*
  "The policy map ({:sql .. :k8s .. :prom ..}) to use instead of config,
   or nil."
  nil)

(defn- config-at [path]
  (try ((requiring-resolve 'hive-mcp.config.core/get-in-config) path)
       (catch Throwable _ nil)))

(defn current
  "The await source policy in force."
  []
  (or *policy*
      (let [p (or (config-at [:await]) {})
            prom-url (config-at [:services :prometheus :url])]
        (cond-> p
          (and prom-url (nil? (get-in p [:prom :targets "default"])))
          (assoc-in [:prom :targets "default"] (str prom-url))))))

(m/=> current [:=> [:cat] :map])

(defn refusal [source message key-path]
  (ex-info (str "await " (name source) ": " message
                " (operator allowlist config :await " (str/join " " (map pr-str key-path)) ")")
           {:error :await/not-allowed :source source :config [:await key-path]}))

(defn allowed!
  "Throw unless `value` is in the allowlist at `key-path` of the policy.
   Answers value."
  [source key-path value what]
  (let [allow (set (map str (get-in (current) key-path)))]
    (when-not (contains? allow (str value))
      (throw (refusal source (str what " " (pr-str value) " is not allowlisted") key-path)))
    value))

(defn prom-target-url
  "The configured URL of prom target `target`, or throw."
  [target]
  (let [url (get-in (current) [:prom :targets (str target)])]
    (when-not (and (string? url) (re-matches #"https?://[^\s#?]+" url))
      (throw (refusal :prom-predicate (str "target " (pr-str target) " has no configured URL")
                      [:prom :targets])))
    (str/replace url #"/+$" "")))

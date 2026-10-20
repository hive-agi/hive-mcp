(ns hive-mcp.multi.registry.aliases
  "Param-alias registry — short keys → full keywords.

   Used by the DSL parse path to expand `{\"c\" \"hi\"}` → `{:content \"hi\"}`.
   Owner :multi/core seeds the 9 default aliases from hive-mcp.dsl.verbs/param-aliases."
  (:require [malli.core :as m]
            [hive-mcp.multi.registry.owned :as owned]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private state
  (atom {:by-short {}     ;; "c" → {:full :content :owner kw}
         :by-owner {}}))

(defn register!
  "Register a param alias. Returns :ok | :replaced | :conflict.

   entry shape: {:full keyword?}"
  [owner short-key entry]
  (owned/register! state :by-short owner short-key entry
                   "[multi.registry.aliases]" :short))

(m/=> register! [:=> [:cat [:or :keyword :string] :string [:map [:full :keyword]]] [:enum :ok :replaced :conflict]])

(defn deregister-by-owner!
  "Remove all aliases registered by `owner`. Returns set of removed short keys."
  [owner]
  (owned/deregister-by-owner! state :by-short owner))

(m/=> deregister-by-owner! [:=> [:cat [:or :keyword :string]] [:set :string]])

(defn lookup
  "Return {:full kw :owner kw} for a short key, or nil."
  [short-key]
  (owned/lookup state :by-short short-key))

(m/=> lookup [:=> [:cat :string] [:maybe [:map [:full :keyword] [:owner [:or :keyword :string]]]]])

(defn all-aliases
  "Map of {short-key → :full-keyword} across all owners."
  []
  (into {} (map (fn [[s {:keys [full]}]] [s full])) (owned/all state :by-short)))

(m/=> all-aliases [:=> [:cat] [:map-of :string :keyword]])

(defn snapshot []
  (let [s @state]
    {:version (hash s) :data s}))

(m/=> snapshot [:=> [:cat] [:map [:version :int] [:data :map]]])

(defn reset-for-test! []
  (reset! state {:by-short {} :by-owner {}}))

(m/=> reset-for-test! [:=> [:cat] :map])

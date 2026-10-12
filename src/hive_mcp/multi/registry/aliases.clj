(ns hive-mcp.multi.registry.aliases
  "Param-alias registry — short keys → full keywords.

   Used by the DSL parse path to expand `{\"c\" \"hi\"}` → `{:content \"hi\"}`.
   Owner :multi/core seeds the 9 default aliases from hive-mcp.dsl.verbs/param-aliases."
  (:require [hive-mcp.multi.registry.owned :as owned]))

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

(defn deregister-by-owner!
  "Remove all aliases registered by `owner`. Returns set of removed short keys."
  [owner]
  (owned/deregister-by-owner! state :by-short owner))

(defn lookup
  "Return {:full kw :owner kw} for a short key, or nil."
  [short-key]
  (owned/lookup state :by-short short-key))

(defn all-aliases
  "Map of {short-key → :full-keyword} across all owners."
  []
  (into {} (map (fn [[s {:keys [full]}]] [s full])) (owned/all state :by-short)))

(defn snapshot []
  (let [s @state]
    {:version (hash s) :data s}))

(defn reset-for-test! []
  (reset! state {:by-short {} :by-owner {}}))

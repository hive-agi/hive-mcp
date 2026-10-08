(ns hive-mcp.multi.registry.verbs
  "DSL verb-code → {tool, command} registry.

   Mirrors registry.tools shape. Verbs are the concise sentence-form of
   batch ops: `[\"m+\" {\"c\" \"hello\"}]` → {tool memory command add content hello}.

   Owner :multi/core seeds the existing 36 verbs from hive-mcp.dsl.verbs/verb-table.
   Addons add verbs via the :multi/verb hook key."
  (:require [hive-mcp.multi.registry.owned :as owned]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private state
  (atom {:by-code  {}     ;; "m+" → {:tool "memory" :command "add" :owner kw}
         :by-owner {}}))

(defn register!
  "Register a DSL verb. Returns :ok | :replaced | :conflict.

   entry shape: {:tool string :command string}"
  [owner code entry]
  (owned/register! state :by-code owner code entry
                   "[multi.registry.verbs]" :code))

(defn deregister-by-owner!
  "Remove every verb registered by `owner`. Returns set of removed codes."
  [owner]
  (owned/deregister-by-owner! state :by-code owner))

(defn lookup
  "Return {:tool :command :owner} for a verb code, or nil."
  [code]
  (owned/lookup state :by-code code))

(defn all-codes
  "Sorted vector of all registered verb codes."
  []
  (vec (sort (keys (owned/all state :by-code)))))

(defn snapshot []
  (let [s @state]
    {:version (hash s) :data s}))

(defn reset-for-test! []
  (reset! state {:by-code {} :by-owner {}}))

(ns hive-mcp.multi.registry.batchables
  "Tool-name → Batchable record registry.

   When a tool ships an explicit hive-mcp.batch.protocol/Batchable impl,
   addons register it here so the multi engine can substitute the optimized
   single-store-call path for the LSP-clean DefaultBatchableAdapter fallback.

   Per the LSP guarantee, every tool — registered or not — is satisfied by
   SOMETHING that implements Batchable; lookup-or-default in the façade returns
   the default adapter for any tool without an explicit record."
  (:require [malli.core :as m]
            [hive-mcp.multi.registry.owned :as owned]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private state
  (atom {:by-name  {}     ;; "memory" → {:record record :owner kw}
         :by-owner {}}))

(defn register!
  "Register a Batchable record for tool-name. Returns :ok | :replaced | :conflict.

   entry shape: {:record any? (must satisfy Batchable)}"
  [owner tool-name entry]
  (owned/register! state :by-name owner tool-name entry
                   "[multi.registry.batchables]" :tool-name))

(m/=> register! [:=> [:cat [:or :keyword :string] :string [:map [:record :any]]] [:enum :ok :replaced :conflict]])

(defn deregister-by-owner!
  "Remove all batchables registered by `owner`. Returns set of removed names."
  [owner]
  (owned/deregister-by-owner! state :by-name owner))

(m/=> deregister-by-owner! [:=> [:cat [:or :keyword :string]] [:set :string]])

(defn lookup
  "Return {:record :owner} for a tool-name, or nil."
  [tool-name]
  (owned/lookup state :by-name tool-name))

(m/=> lookup [:=> [:cat :string] [:maybe [:map [:record :any] [:owner [:or :keyword :string]]]]])

(defn snapshot []
  (let [s @state]
    {:version (hash s) :data s}))

(m/=> snapshot [:=> [:cat] [:map [:version :int] [:data :map]]])

(defn reset-for-test! []
  (reset! state {:by-name {} :by-owner {}}))

(m/=> reset-for-test! [:=> [:cat] :map])

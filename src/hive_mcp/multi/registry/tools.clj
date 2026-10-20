(ns hive-mcp.multi.registry.tools
  "Tool-name → handler registry — the dispatch source for `multi`.

   One ns, one shape (SRP). Two indices per swap! to make
   deregister-by-owner! O(owner-keys).

   Owner = addon-id keyword. Synthetic owner :multi/core represents
   the consolidated/multi tool-handlers seeded at boot.

   Conflict policy:
     same key + same owner   → idempotent silent replace
     same key + different owner → :multi/registry-conflict warn-log,
                                   first-write-wins"
  (:require [malli.core :as m]
            [hive-mcp.multi.registry.owned :as owned]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private state
  (atom {:by-name  {}    ;; "memory" → {:handler fn :owner kw :registered-at inst}
         :by-owner {}})) ;; kw → #{"memory" "memory_v2"}

(defn register!
  "Register a tool handler under owner. Returns :ok | :replaced | :conflict.
   A :multi/core-owned entry is overridable by any addon owner (returns :replaced).

   entry shape: {:handler ifn :batchable (some-fn nil? any?)}"
  [owner tool-name entry]
  (owned/register! state :by-name owner tool-name entry
                   "[multi.registry.tools]" :tool-name
                   {:stamp-fn #(assoc % :registered-at (java.time.Instant/now))
                    :overridable-owner :multi/core}))

(m/=> register! [:=> [:cat [:or :keyword :string] :string [:map [:handler [:fn {:gen/elements [identity]} ifn?]] [:batchable {:optional true} :any]]] [:enum :ok :replaced :conflict]])

(defn deregister-by-owner!
  "Remove every tool registered by `owner`. Returns set of removed names."
  [owner]
  (owned/deregister-by-owner! state :by-name owner))

(m/=> deregister-by-owner! [:=> [:cat [:or :keyword :string]] [:set :string]])

(defn lookup
  "Return the registered entry for a tool-name, or nil."
  [tool-name]
  (owned/lookup state :by-name tool-name))

(m/=> lookup [:=> [:cat :string] [:maybe [:map [:owner [:or :keyword :string]] [:handler [:fn {:gen/elements [identity]} ifn?]]]]])

(defn all-names
  "Return a sorted vector of registered tool names."
  []
  (vec (sort (keys (:by-name @state)))))

(m/=> all-names [:=> [:cat] [:vector :string]])

(defn snapshot
  "Immutable snapshot for deterministic plan compilation.

   :version is `(hash @state)` so plans serialized with this version
   can be replayed against a registry whose hash matches."
  []
  (let [s @state]
    {:version (hash s) :data s}))

(m/=> snapshot [:=> [:cat] [:map [:version :int] [:data :map]]])

(defn reset-for-test!
  "Test-only — clear all entries. Do NOT call from production code."
  []
  (reset! state {:by-name {} :by-owner {}}))

(m/=> reset-for-test! [:=> [:cat] :map])

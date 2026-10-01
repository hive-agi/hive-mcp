(ns hive-mcp.server.registration
  "Tool registration specs and discovery filtering.

   Bounded context: Tool/resource registration and discovery.

   Handles:
   - Hivemind message validation specs
   - Phase 2 strangle: tools/list override to hide deprecated tools"
  (:require [jsonrpc4clj.server :as jsonrpc-server]
            [clojure.spec.alpha :as s]
            [taoensso.timbre :as log]
            [io.modelcontext.clojure-sdk.server]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Hivemind Message Specs (validation for server lifecycle events)
;; =============================================================================

(s/def ::hivemind-message
  (s/keys :req-un [::agent-id ::event-type ::message]))

(s/def ::agent-id string?)
(s/def ::event-type keyword?)
(s/def ::message string?)

;; =============================================================================
;; tools/list projection: hide what discovery must not advertise
;;
;; Hidden tools stay callable (tools/call reads the same table); tools/list
;; drops them. WHICH tools are hidden is a registry of rules, so a new reason
;; to hide is one `register-hide-rule!`, never an edit to the projection.
;; =============================================================================

(defonce ^:private hide-rules (atom {}))

(defn register-hide-rule!
  "Register PRED (tool-def -> truthy when tools/list must hide it) under ID.
   Pass a var to keep the rule reloadable. Returns ID."
  [id pred]
  (swap! hide-rules assoc id pred)
  id)

(defn unregister-hide-rule!
  "Drop the hide rule registered under ID. Returns ID."
  [id]
  (swap! hide-rules dissoc id)
  id)

(defn hide-rule-ids
  "Ids of the registered hide rules."
  []
  (set (keys @hide-rules)))

(defn deprecated-tool?
  "A tool-def flagged :deprecated."
  [tool]
  (boolean (:deprecated tool)))

(register-hide-rule! :deprecated #'deprecated-tool?)

(defn hidden?
  "Pure. True when any of RULES (a map id -> pred) hides TOOL."
  [rules tool]
  (boolean (some (fn [[_ pred]] (pred tool)) rules)))

(defn visible-tools
  "Pure. The tool-defs tools/list advertises from ENTRIES (the SDK table's
   {:tool :handler} values), in order, under RULES."
  [rules entries]
  (into [] (comp (map :tool) (remove #(hidden? rules %))) entries))

(defn receive-tools-list
  "The tools/list method body: the context's live table through the hide rules."
  [_ context _params]
  (let [entries (vals @(:tools context))
        visible (visible-tools @hide-rules entries)
        hidden  (- (count entries) (count visible))]
    (when (pos? hidden)
      (log/debug "Hiding" hidden "tools from tools/list"))
    {:tools visible}))

(defn tools-list-installed?
  "True when the SDK method table answers tools/list through this namespace."
  []
  (= #'receive-tools-list (get-method jsonrpc-server/receive-request "tools/list")))

(defn install-tools-list!
  "Install `receive-tools-list` (by var) as the tools/list method, replacing
   the SDK's unfiltered one whenever it was loaded later. Idempotent; returns
   true when installed."
  []
  (.addMethod ^clojure.lang.MultiFn jsonrpc-server/receive-request "tools/list" #'receive-tools-list)
  (tools-list-installed?))

(install-tools-list!)

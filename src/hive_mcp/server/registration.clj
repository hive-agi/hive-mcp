(ns hive-mcp.server.registration
  "Tool registration specs and discovery filtering.

   Bounded context: Tool/resource registration and discovery.

   Handles:
   - Hivemind message validation specs
   - Phase 2 strangle: tools/list override to hide deprecated tools"
  (:require [jsonrpc4clj.server :as jsonrpc-server]
            [clojure.spec.alpha :as s]
            [taoensso.timbre :as log]
            [io.modelcontext.clojure-sdk.server]
            [malli.core :as m]))
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

(def ToolDef
  "A tool-def as tools/list sees it: a map naming the tool, optionally
   flagged :deprecated."
  [:map [:name :string] [:deprecated {:optional true} [:maybe :boolean]]])

(def HidePred
  "A hide rule: tool-def -> truthy when tools/list must hide it. A var (the
   reloadable form register-hide-rule! asks for) or any other IFn."
  [:fn {:error/message "must be callable"
        :gen/elements [:deprecated (constantly false) (comp boolean :deprecated)]}
   ifn?])

(def Rules
  "Registered hide rules, id -> predicate."
  [:map-of :any HidePred])

(def Entry
  "One SDK table value: {:tool tool-def :handler h}."
  [:map [:tool ToolDef] [:handler {:optional true} :any]])

(defn register-hide-rule!
  "Register PRED (tool-def -> truthy when tools/list must hide it) under ID.
   Pass a var to keep the rule reloadable. Returns ID."
  [id pred]
  (swap! hide-rules assoc id pred)
  id)

(m/=> register-hide-rule! [:=> [:cat :any HidePred] :any])

(defn unregister-hide-rule!
  "Drop the hide rule registered under ID. Returns ID."
  [id]
  (swap! hide-rules dissoc id)
  id)

(m/=> unregister-hide-rule! [:=> [:cat :any] :any])

(defn hide-rule-ids
  "Ids of the registered hide rules."
  []
  (set (keys @hide-rules)))

(m/=> hide-rule-ids [:=> [:cat] [:set :any]])

(defn deprecated-tool?
  "A tool-def flagged :deprecated."
  [tool]
  (boolean (:deprecated tool)))

(m/=> deprecated-tool? [:=> [:cat [:maybe :map]] :boolean])

(register-hide-rule! :deprecated #'deprecated-tool?)

(defn hidden?
  "Pure. True when any of RULES (a map id -> pred) hides TOOL."
  [rules tool]
  (boolean (some (fn [[_ pred]] (pred tool)) rules)))

(m/=> hidden? [:=> [:cat Rules ToolDef] :boolean])

(defn visible-tools
  "Pure. The tool-defs tools/list advertises from ENTRIES (the SDK table's
   {:tool :handler} values), in order, under RULES."
  [rules entries]
  (into [] (comp (map :tool) (remove #(hidden? rules %))) entries))

;; ENTRIES is `(vals table)`, nil for an empty table.
(m/=> visible-tools [:=> [:cat Rules [:maybe [:sequential Entry]]] [:vector ToolDef]])

(defn listed-tools
  "Pure. What tools/list returns: `visible-tools` sorted by :name.

   The live table is a hash map ({name entry}), so its `vals` come back in
   hash order, and that order shifts whenever a tool is registered, refreshed
   or hot-reloaded. Clients put the tool list in their prompt prefix, so a
   reshuffle busts their prompt cache. Sorting by name makes the listing a
   function of the visible tool SET alone (MCP 2026-07-28: servers SHOULD
   return tools in a deterministic order). Names are unique table keys, so the
   order is total."
  [rules entries]
  (vec (sort-by (comp str :name) (visible-tools rules entries))))

(m/=> listed-tools [:=> [:cat Rules [:maybe [:sequential Entry]]] [:vector ToolDef]])

(defn receive-tools-list
  "The tools/list method body: the context's live table through the hide
   rules, sorted by name (`listed-tools`)."
  [_ context _params]
  (let [entries (vals @(:tools context))
        visible (listed-tools @hide-rules entries)
        hidden  (- (count entries) (count visible))]
    (when (pos? hidden)
      (log/debug "Hiding" hidden "tools from tools/list"))
    {:tools visible}))

(def SdkContext
  "The part of an SDK context tools/list reads: its :tools atom of
   {name entry}."
  [:map [:tools [:fn {:gen/schema [:map-of :string Entry] :gen/fmap atom}
                 #(instance? clojure.lang.IDeref %)]]])

(m/=> receive-tools-list [:=> [:cat :any SdkContext :any] [:map [:tools [:vector ToolDef]]]])

(defn tools-list-installed?
  "True when the SDK method table answers tools/list through this namespace."
  []
  (= #'receive-tools-list (get-method jsonrpc-server/receive-request "tools/list")))

(m/=> tools-list-installed? [:=> [:cat] :boolean])

(defn install-tools-list!
  "Install `receive-tools-list` (by var) as the tools/list method, replacing
   the SDK's unfiltered one whenever it was loaded later. Idempotent; returns
   true when installed."
  []
  (.addMethod ^clojure.lang.MultiFn jsonrpc-server/receive-request "tools/list" #'receive-tools-list)
  (tools-list-installed?))

(m/=> install-tools-list! [:=> [:cat] :boolean])

(install-tools-list!)

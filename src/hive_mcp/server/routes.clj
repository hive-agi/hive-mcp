(ns hive-mcp.server.routes
  "MCP server route definitions and tool dispatch.

   Composes identity (routes.identity) and middleware (routes.middleware)
   into tool definitions and server specs.

   This namespace is the public API — callers require only this ns."
  (:require [hive-mcp.server.routes.identity :as id]
            [hive-mcp.server.routes.middleware :as mw]
            [hive-mcp.tools.registry :as tools]
            [hive-spi.swarm.guards :as guards]
            [hive-mcp.server.registration]              ; side-effect: tools/list defmethod
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.addons.core :as addons]
            [taoensso.timbre :as log]
            [clojure.spec.alpha :as s]
            [hive-mcp.tools.composite :as composite]
            [hive-mcp.dispatch.handler :as dispatch]
            [hive-addon.tool-contract :as tool-contract]
            [hive-mcp.tools.schema-keys :as schema-keys]
            [hive-mcp.tools.async-params :as async-params]
            [malli.core :as m]
            [hive-mcp.server.routes.wire-coerce :as wire-coerce]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; =============================================================================
;; Re-exports for backward compatibility
;; =============================================================================

(def normalize-content              #'id/normalize-content)
(def find-last-text-idx             #'id/find-last-text-idx)
(def wrap-delimited-block           #'id/wrap-delimited-block)
(def wrap-piggyback                 #'id/wrap-piggyback)
(def wrap-memory-piggyback-content  #'id/wrap-memory-piggyback-content)
(def extract-agent-id               #'id/extract-agent-id)
(def extract-caller-id              #'id/extract-caller-id)
(def extract-project-id             #'id/extract-project-id)
(def extract-caller-identity        #'id/extract-caller-identity)
(def extract-project-scope          #'id/extract-project-scope)

(def wrap-handler-retry             #'mw/wrap-handler-retry)
(def wrap-handler-nats-notify       #'mw/wrap-handler-nats-notify)
(def wrap-handler-context           #'mw/wrap-handler-context)
(def wrap-handler-normalize         #'mw/wrap-handler-normalize)
(def wrap-handler-compress          #'mw/wrap-handler-compress)
(def wrap-handler-response          #'mw/wrap-handler-response)
(def wrap-handler-default-async-for-commands #'mw/wrap-handler-default-async-for-commands)
(def wrap-handler-async             #'mw/wrap-handler-async)
(def wrap-handler-piggybacks        #'mw/wrap-handler-piggybacks)
(def build-middleware-chain         #'mw/build-middleware-chain)


;; =============================================================================
;; Specs for Tool Definitions
;; =============================================================================

(s/def ::tool-def
  (s/keys :req-un [::name ::description ::inputSchema ::handler]))

(s/def ::name string?)
(s/def ::description string?)
(s/def ::inputSchema map?)
(s/def ::handler dispatch/handler?)

(s/def ::tool-response
  (s/keys :req-un [::content]))

(s/def ::content (s/coll-of map?))


;; =============================================================================
;; Tool Definition Conversion
;; =============================================================================

(s/fdef make-tool
  :args (s/cat :tool-def ::tool-def)
  :ret ::tool-response)

(defn make-tool
  "Convert a tool definition with :handler to SDK format.
   Wraps handler with the standard middleware chain.

   A consolidated tool first folds the params its addon contributions declare
   into its inputSchema (composite/build-merged-tool): contributions already
   ROUTE through effective-handlers, but the MCP layer forwards only declared
   params, so without this fold a contributed verb could not receive its own
   arguments (`swarm ling-wave dispatch` answered :wave/no-providers to every
   spelling). Done here, on every (re)build of the tool table, so a late
   contribution reaches the advertised schema as soon as the reactive surface
   refreshes it.

   The def is checked against the MCP root tool contract
   (`hive-addon.tool-contract/assert-root-tool!`) BEFORE the async params are
   merged in, so a tool whose own schema is empty throws here.

   EVERY tool advertises `async` and `async-timeout-ms`
   (`async-params/with-async-params`), because every tool's chain runs
   `wrap-handler-async`. A tool declaring its own `async` keeps it.

   The emitted schema has provider-legal property keys only
   (`schema-keys/legal-tool`): an addon param such as `preview?` is
   advertised as its `preview_` alias.

   WIRE-NUMBERS: the raw handler is wrapped so a declared integer / number /
   boolean param that arrives as its text (\"8\", \"false\") is decoded against
   the tool's inputSchema (its own names AND their advertised aliases) before
   the handler sees it; text that does not parse passes through for the
   handler's own validation (`wire-coerce/wrap-handler-wire-coerce`)."
  [{:keys [consolidated] :as tool-def}]
  (let [{:keys [name description handler deprecated default-async-commands] :as checked}
        (tool-contract/assert-root-tool!
         (if consolidated (composite/build-merged-tool tool-def) tool-def))
        schema-ext (ext/get-schema-extensions name)
        merged-schema (:inputSchema
                       (async-params/with-async-params
                        (cond-> checked
                          schema-ext (update-in [:inputSchema :properties] merge schema-ext))))
        legal (schema-keys/legal-tool
               (cond-> {:name name
                        :description description
                        :inputSchema merged-schema}
                 deprecated (assoc :deprecated true)))
        wire-schema {:properties (merge (:properties merged-schema)
                                        (get-in legal [:inputSchema :properties]))}]
    (assoc legal :handler (mw/build-middleware-chain
                           (wire-coerce/wrap-handler-wire-coerce handler wire-schema)
                           name default-async-commands))))

(def ToolDefIn
  "A tool definition with a raw :handler."
  [:map [:name :string] [:description {:optional true} :any]
   [:inputSchema {:optional true} :map] [:handler :any]])

(def MadeTool
  "An SDK-format tool: advertised def plus the middleware-wrapped handler."
  [:map [:name :string] [:inputSchema :map] [:handler :any]
   [:deprecated {:optional true} [:= true]]])

(m/=> make-tool [:=> [:cat ToolDefIn] MadeTool])

(defn collect-surface-inputs
  "COLLECT: read every source of the advertised surface once, into a
   `tools/SurfaceInputs` value. The only impure step of the table; everything
   after it is `tools/advertised-tools`.

   A child ling sees the same layers with `tools/child-excluded-tool-names`
   dropped, whichever layer would have supplied them."
  []
  {:base     (tools/core-tools)
   :dynamic  (vec (ext/get-registered-tools))
   :addon    (vec (:installed (addons/resolve-addon-tools)))
   :absorbed (tools/absorbed-root-names)
   :excluded (if (guards/child-ling?) tools/child-excluded-tool-names #{})
   :visible  (tools/visible-root-names)})

(m/=> collect-surface-inputs [:=> [:cat] [:map [:base [:sequential :map]] [:dynamic [:vector :map]]
                                          [:addon [:vector :map]] [:absorbed [:set :string]]
                                          [:excluded [:set :string]] [:visible [:maybe [:set :string]]]]])

(defn advertised-tool-defs
  "The advertised tool defs (raw handlers), deduped and gated: the one surface
   boot, every refresh, every transport and agent delegation agree on."
  []
  (tools/advertised-tools (collect-surface-inputs)))

(m/=> advertised-tool-defs [:=> [:cat] [:sequential [:map [:name :string]]]])

;; =============================================================================
;; Server Spec Building
;; =============================================================================

(defn tool-table
  "PROMOTE: SDK-format tools (make-tool output) -> the table a transport's
   `:tools` atom holds, {name {:tool <def sans handler> :handler h}}. Pure."
  [made-tools]
  (into {} (map (fn [t] [(:name t) {:tool (dissoc t :handler) :handler (:handler t)}]))
        made-tools))

(def Table
  "A transport's tool table: name -> {:tool def-sans-handler :handler h}."
  [:map-of :any [:map [:tool :map] [:handler :any]]])

(m/=> tool-table [:=> [:cat [:maybe [:sequential [:map [:name :any]]]]] Table])

(defn build-server-spec
  "Build the MCP server spec from the ONE advertised surface
   (`advertised-tool-defs`), so a transport created from it starts with
   exactly the table every refresh installs.

   ROLE BRANCHING (Self-Call Prevention) lives in `collect-surface-inputs`:
   a child ling gets the child-excluded names dropped."
  []
  (let [defs  (advertised-tool-defs)
        hidden (count (filter :deprecated defs))]
    (log/info "Building server spec with" (count defs) "tools"
              "(" (- (count defs) hidden) "visible," hidden "deprecated/gated)"
              (if (guards/child-ling?)
                (str "child-ling role " (guards/get-role) " depth " (guards/ling-depth))
                "coordinator"))
    {:name    "hive-mcp"
     :version "0.1.0"
     :tools   (mapv make-tool defs)}))

(m/=> build-server-spec [:=> [:cat] [:map [:name :string] [:version :string] [:tools [:vector MadeTool]]]])


;; =============================================================================
;; Hot Reload & Debug
;; =============================================================================

;; =============================================================================
;; Live tool surfaces: every transport reads ONE advertised table
;;
;; A transport (stdio, MCP-HTTP, the nREPL server-context, ...) serves tools
;; out of its own SDK context's `:tools` atom. Each registers that atom here as
;; a SURFACE; `refresh-surfaces!` computes the table once and installs it into
;; every registered surface with a single reset! apiece. Adding a transport is
;; a `register-surface!` call; adding a new KIND of surface is a defmethod of
;; `install-table!`. Surfaces are plain data dispatched through a multimethod,
;; so a reload of this namespace reaches the entries a defonce already holds.
;; =============================================================================

(def ToolSurface
  "A registered surface: its :surface/kind selects the `install-table!`
   method, the rest is that kind's own data."
  [:map [:surface/kind :keyword]])

(defonce ^:private tool-surfaces
  (atom {}))

(defonce ^:private advertised-view
  ;; name -> tool def (sans handler) of the table last installed. What
  ;; `refresh-surfaces!` diffs against to report the changed names.
  (atom {}))

(defmulti install-table!
  "Install TABLE ({name {:tool .. :handler ..}}) into SURFACE with ONE atomic
   write. Returns truthy when installed, nil when the surface has nowhere to
   install right now (e.g. its context is not up yet)."
  (fn [surface _table] (:surface/kind surface)))

(defmethod install-table! :tools-atom
  [{:surface/keys [tools-atom]} table]
  (reset! tools-atom table)
  true)

(defmethod install-table! :context-atom
  [{:surface/keys [context-atom]} table]
  (when-let [tools-atom (some-> context-atom deref :tools)]
    (reset! tools-atom table)
    true))

(defn register-surface!
  "Register SURFACE (a ToolSurface) under ID, replacing any previous one of
   that id. Idempotent. Returns ID."
  [id surface]
  (swap! tool-surfaces assoc id surface)
  id)

(m/=> register-surface! [:=> [:cat :any ToolSurface] :any])

(defn unregister-surface!
  "Forget the surface registered under ID. Returns ID."
  [id]
  (swap! tool-surfaces dissoc id)
  id)

(m/=> unregister-surface! [:=> [:cat :any] :any])

(defn registered-surfaces
  "id -> surface, for diagnostics and tests."
  []
  @tool-surfaces)

(m/=> registered-surfaces [:=> [:cat] [:map-of :any ToolSurface]])

(defn changed-tool-names
  "Names whose advertised def differs between OLD and NEW (name -> def), added
   and removed ones included. Pure; sorted."
  [old new]
  (into (sorted-set)
        (remove #(= (get old %) (get new %)))
        (concat (keys old) (keys new))))

(m/=> changed-tool-names [:=> [:cat [:maybe [:map-of :any :any]] [:maybe [:map-of :any :any]]] [:set :any]])

(defn- install-everywhere!
  "BOUNDARY: install TABLE into every registered surface. A surface that
   throws is logged and reported, and does not stop the others."
  [table]
  (reduce (fn [acc [id surface]]
            (try
              (if (install-table! surface table)
                (update acc :surfaces conj id)
                acc)
              (catch Throwable t
                (log/warn "tool surface install failed" {:surface id :error (ex-message t)})
                (update acc :failed conj id))))
          {:surfaces [] :failed []}
          @tool-surfaces))

(defn notification-targets
  "Select successfully installed surfaces that can notify a connected client.
   No change to the advertised view means no notification, even if the tool
   handlers were rebuilt during a hot reload."
  [changed installed surfaces]
  (if (seq changed)
    (filterv #(some? (:surface/notify! (get surfaces %))) installed)
    []))

(m/=> notification-targets [:=> [:cat [:maybe [:sequential :any]] [:maybe [:sequential :any]] [:maybe [:map-of :any :any]]]
                            [:vector :any]])

(defn refresh-surfaces!
  "Compute the advertised table ONCE and install it into every registered
   surface. Returns {:count unique-tools :changed [names] :surfaces [ids]
   :failed [ids]}; :changed is relative to the previously installed table."
  []
  (let [table (tool-table (mapv make-tool (advertised-tool-defs)))
        view  (update-vals table :tool)
        out   (install-everywhere! table)
        [old] (reset-vals! advertised-view view)
        res   (assoc out
                     :count   (count table)
                     :changed (vec (changed-tool-names old view)))]
    (doseq [id (notification-targets (:changed res) (:surfaces res) @tool-surfaces)]
      (try ((:surface/notify! (get @tool-surfaces id)))
           (catch Throwable t
             (log/warn "tool surface notification failed"
                       {:surface id :error (ex-message t)}))))
    (when (seq (:changed res))
      ;; IDE WebSocket clients share a JSON-RPC stream rather than an SDK
      ;; context, so notify them separately when their tool view changes.
      (try
        (when ((requiring-resolve 'hive-mcp.transport.websocket/connected?))
          ((requiring-resolve 'hive-mcp.transport.websocket/notify!)
           "notifications/tools/list_changed" {}))
        (catch Throwable t
          (log/warn "WebSocket tool notification failed" {:error (ex-message t)}))))
    (log/info "Tool surfaces refreshed:" (:count res) "tools,"
              (count (:changed res)) "changed, into" (:surfaces res))
    res))

(def RefreshReport
  "refresh-surfaces!' answer."
  [:map [:count [:int {:min 0}]] [:changed [:vector :any]]
   [:surfaces [:vector :any]] [:failed [:vector :any]]])

(m/=> refresh-surfaces! [:=> [:cat] RefreshReport])

(defn refresh-tools!
  "Hot-reload all tools in the running server.

   SERVER-CONTEXT-ATOM's context is registered as a surface (idempotent, keyed
   by the atom's identity) so every later refresh reaches it too, then every
   registered surface is refreshed from the one table. Returns what
   `refresh-surfaces!` returns, nil when the atom holds no context."
  [server-context-atom]
  (when-let [context @server-context-atom]
    (register-surface! [:context-atom (System/identityHashCode server-context-atom)]
                       {:surface/kind :context-atom
                        :surface/context-atom server-context-atom
                        :surface/notify! (fn []
                                           ;; The stdio registration may already
                                           ;; notify this same SDK context.
                                           (when-not (some (fn [[_ surface]]
                                                             (and (= :tools-atom (:surface/kind surface))
                                                                  (identical? (:tools context)
                                                                              (:surface/tools-atom surface))
                                                                  (:surface/notify! surface)))
                                                           @tool-surfaces)
                                             (when-let [server (:server context)]
                                               ((requiring-resolve 'jsonrpc4clj.server/send-notification)
                                                server "notifications/tools/list_changed" {}))))})
    (refresh-surfaces!)))

(def AtomRef
  "A server context atom."
  [:fn {:gen/schema :nil :gen/fmap (fn [_] (atom nil))} #(instance? clojure.lang.IAtom %)])

(m/=> refresh-tools! [:=> [:cat AtomRef] [:maybe RefreshReport]])

(defn debug-tool-handler
  "Get info about a registered tool handler (for debugging)."
  [server-context-atom tool-name]
  (when-let [context @server-context-atom]
    (let [tools-atom (:tools context)
          tool-entry (get @tools-atom tool-name)]
      (when tool-entry
        {:name tool-name
         :handler-class (str (type (:handler tool-entry)))
         :tool-keys (keys (:tool tool-entry))}))))

(m/=> debug-tool-handler [:=> [:cat AtomRef :any]
                          [:maybe [:map [:name :any] [:handler-class :string] [:tool-keys [:maybe [:sequential :keyword]]]]]])

(defn register-tools-for-delegation!
  "Register the advertised surface for agent delegation. Same table as every
   transport (`advertised-tool-defs`), so a delegated call and an MCP call
   resolve a name to the same tool."
  []
  (let [register-tools! (requiring-resolve 'hive-mcp.agent.registry/register!)
        selected-tools  (advertised-tool-defs)]
    (register-tools! selected-tools)
    (log/info "Registered" (count selected-tools) "tools for agent delegation"
              (if (guards/child-ling?) "(child-ling restricted)" ""))
    (count selected-tools)))

(m/=> register-tools-for-delegation! [:=> [:cat] [:int {:min 0}]])
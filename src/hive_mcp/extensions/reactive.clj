(ns hive-mcp.extensions.reactive
  "Late addon contributions reach the advertised MCP surface without a restart.

   Composite tools are built once at boot from whatever had been contributed
   by then, and the advertised inputSchema is assembled when the server's
   tool table is (re)built. A contribution made AFTER boot — an addon mounted
   by `hot inject`, a remount from `hot reload`, a live `contribute-commands!`
   from a REPL — therefore dispatched immediately but stayed invisible: absent
   from its composite, absent from tools/list, its schema-extensions never
   re-read. This namespace subscribes to the registry's contribution events
   and does, for the one tool that changed, the three things a boot does."
  (:require [hive-addon.protocol :as proto]
            [hive-mcp.addons.core :as addon-core]
            [hive-mcp.dns.result :as result :refer [rescue]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.composite :as composite]
            [taoensso.timbre :as log]
            [hive-addon.registry.commands :as acmds]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def composite-descriptions
  "Composite tools built from addon contributions, name -> description prefix.
   A whitelist: a contribution to a name outside it folds into that tool's
   core definition through build-merged-handler instead."
  {"analysis" "Code analysis"})

(defn rebuild-composite!
  "Rebuild and re-register the composite tool `tool-name` from its current
   contributions, when it is whitelisted. Mirrors the boot one-shot: a name
   with NO contributions gets no composite (and loses the one it had, so a
   retraction that empties a tool does not leave a help-only shell behind).
   Returns the tool-def, or nil. The agent-delegation registry is NOT written
   here: it follows the advertised table in `refresh-surface!`, so an
   absorbed composite is absent there exactly as it is on the wire."
  [tool-name]
  (when-let [desc (get composite-descriptions tool-name)]
    (if (empty? (acmds/get-commands tool-name))
      (do (ext/deregister-tool! tool-name) nil)
      (let [t (composite/build-composite-tool tool-name desc)]
        (ext/register-tool! t)
        t))))

(m/=> rebuild-composite! [:=> [:cat [:maybe :string]] [:maybe :map]])

(defn redrain-schema-extensions!
  "Re-read every ACTIVE addon's (schema-extensions) into the registry — the
   map-shaped idiom (tool-name -> params); the DataScript-attribute sequence
   is not a tool schema. Each addon's params are registered OWNED by its id,
   so the addon's shutdown (retract-schemas-by-owner!) withdraws exactly
   them. Returns the tool names touched."
  []
  (into []
        (comp (filter #(= :active (:state %)))
              (keep (fn [{:keys [name]}]
                      (when-let [addon (:addon (addon-core/get-addon-entry name))]
                        [name addon])))
              (mapcat (fn [[addon-id addon]]
                        (let [exts (rescue nil (proto/schema-extensions addon))]
                          (when (map? exts)
                            (doseq [[tool-name props] exts]
                              (ext/register-schema! addon-id tool-name props))
                            (keys exts))))))
        (addon-core/list-addons)))

(m/=> redrain-schema-extensions! [:=> [:cat] [:vector :string]])

(def ^:dynamic *server-refresh*
  "Replaceable server boundary for isolated tests; production resolves live vars."
  (fn []
    (let [ctx-atom (some-> (requiring-resolve 'hive-mcp.server.core/server-context-atom) deref)]
      (if (and ctx-atom @ctx-atom)
        ((requiring-resolve 'hive-mcp.server.routes/refresh-tools!) ctx-atom)
        ((requiring-resolve 'hive-mcp.server.routes/refresh-surfaces!))))))

(def SurfaceRefresh
  "What `refresh-server-tools!` answers: the routes report merged with an Ok
   carrying it, or an Err naming why no surface was refreshed."
  [:or
   [:map [:ok :map]]
   [:map
    [:error [:enum :surface/refresh-failed :surface/refresh-unavailable]]
    [:cause :string]]])

(def AgentRefresh
  "What `refresh-agent-tools!` answers: the tool count, or the failure."
  [:or
   [:map [:ok? [:= true]] [:count :int]]
   [:map [:ok? [:= false]] [:error [:maybe :string]]]])

(defn refresh-server-tools!
  "Refresh advertised MCP surfaces. Returns a Result with the routes report
   on success (and its legacy report keys), or the cause on failure/absence."
  []
  (try
    (let [out (*server-refresh*)]
      (cond
        (seq (:failed out))
        (result/err :surface/refresh-failed {:cause (pr-str (:failed out)) :report out})

        (not (seq (:surfaces out)))
        (result/err :surface/refresh-unavailable {:cause "no MCP surface refreshed" :report out})

        :else (merge out (result/ok out))))
    (catch Throwable t
      (log/error "MCP tool surface refresh failed" {:error (ex-message t)})
      (result/err :surface/refresh-failed {:cause (or (ex-message t) (str t))}))))

(m/=> refresh-server-tools! [:=> [:cat] SurfaceRefresh])
(defn refresh-agent-tools!
  "Re-seat the agent-delegation registry from the advertised table. A failed
   refresh is an explicit error report, never a success-shaped count or nil."
  []
  (try
    {:ok? true
     :count ((requiring-resolve 'hive-mcp.agent.registry/refresh!))}
    (catch Throwable t
      (log/error "Agent tool registry refresh failed" {:error (ex-message t)})
      {:ok? false :error (ex-message t)})))

(m/=> refresh-agent-tools! [:=> [:cat] AgentRefresh])

(defn refresh-surface!
  "Bring the advertised surface up to date after a contribution to
   `tool-name` (nil: no composite to rebuild): rebuild its composite, re-drain
   schema-extensions, refresh every tool surface and the delegation registry
   from the one table. Each leg is rescued on its own; returns what each did,
   including an explicit failed agent-tools report when the refresh aborts."
  [tool-name]
  {:composite    (some? (rescue nil (rebuild-composite! tool-name)))
   :schema-tools (rescue [] (redrain-schema-extensions!))
   :server-tools (refresh-server-tools!)
   :agent-tools  (refresh-agent-tools!)})

(m/=> refresh-surface!
      [:=> [:cat [:maybe :string]]
       [:map
        [:composite :boolean]
        [:schema-tools [:vector :string]]
        [:server-tools SurfaceRefresh]
        [:agent-tools AgentRefresh]]])

(defn on-contribution
  "The contribution listener: bring the surface up to date for the tool the
   EVENT names. Registered BY VAR (see `install!`)."
  [{:keys [type tool-name addon-id]}]
  (let [out (refresh-surface! tool-name)]
    (log/debug "Contribution reached the surface"
               {:type type :tool tool-name :addon addon-id :refresh out})))

(m/=> on-contribution [:=> [:cat [:maybe :map]] :nil])

(defn contribution-listener
  "The listener `install!` registers: resolves `on-contribution` BY SYMBOL on
   every event and calls whatever is interned there now.

   Holding the var is not enough. hive-hot's core reload unloads a namespace
   and loads it afresh, which interns NEW vars, while the listener table (a
   defonce in hive-mcp.extensions.registry) keeps whatever it was given. A
   registered var would still point at the unloaded namespace's code; a
   symbol lookup per call always reaches the live one."
  [event]
  ((requiring-resolve `on-contribution) event))

(m/=> contribution-listener [:=> [:cat [:maybe :map]] :nil])

(defn install!
  "Subscribe to the registry's contribution events. Idempotent by id.
   Returns the listener id, as it always has.

   The listener is `contribution-listener`, which reaches `on-contribution`
   through its symbol on every call, never a fn value captured here: the
   listener table outlives a core reload, so a captured value would keep
   running the OLD code.

   Two things are subscribed, not one. The facade's own listener list is what
   a contribution through hive-mcp.extensions.registry notifies. The hive-addon
   listener seam is what a contribution made DIRECTLY to
   hive-addon.registry.commands notifies, and an addon that has migrated off
   the facade makes exactly that kind of contribution. That seam was armed by a
   delay which only a facade call forced, so a fully migrated fleet would have
   armed it never: commands would land in the store and the advertised surface
   would never rebuild. Arming it here, at install time, is what makes the
   migration safe to perform one addon at a time."
  []
  (let [id (ext/add-contribution-listener! :reactive-surface #'contribution-listener)]
    (rescue nil (ext/ensure-seam-listener!))
    id))

(m/=> install! [:=> [:cat] [:= :reactive-surface]])

(defn uninstall!
  "Unsubscribe. For tests."
  []
  (ext/remove-contribution-listener! :reactive-surface))

(m/=> uninstall! [:=> [:cat] [:= :reactive-surface]])

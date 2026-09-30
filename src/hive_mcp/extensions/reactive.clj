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
            [hive-mcp.dns.result :refer [rescue]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.composite :as composite]
            [taoensso.timbre :as log]
            [hive-addon.registry.commands :as acmds]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def composite-descriptions
  "Composite tools built from addon contributions, name -> description prefix.
   A whitelist: a contribution to a name outside it folds into that tool's
   core definition through build-merged-handler instead."
  {"analysis" "Code analysis"
   "overarch" "Architecture model"})

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

(defn redrain-schema-extensions!
  "Re-read every ACTIVE addon's (schema-extensions) into the registry — the
   map-shaped idiom (tool-name -> params); the DataScript-attribute sequence
   is not a tool schema. Returns the tool names touched."
  []
  (into []
        (comp (filter #(= :active (:state %)))
              (keep (fn [{:keys [name]}] (:addon (addon-core/get-addon-entry name))))
              (mapcat (fn [addon]
                        (let [exts (rescue nil (proto/schema-extensions addon))]
                          (when (map? exts)
                            (doseq [[tool-name props] exts]
                              (ext/register-schema! tool-name props))
                            (keys exts))))))
        (addon-core/list-addons)))

(defn refresh-server-tools!
  "Refresh every registered tool surface from the one advertised table
   (`hive-mcp.server.routes/refresh-surfaces!`), registering the nREPL
   server-context as a surface first when it is up. Returns the refresh
   report {:count :changed :surfaces :failed}, or nil when no surface took the
   table (no server running yet)."
  []
  (rescue nil
          (let [ctx-atom (some-> (requiring-resolve 'hive-mcp.server.core/server-context-atom)
                                 deref)
                out      (if (and ctx-atom @ctx-atom)
                           ((requiring-resolve 'hive-mcp.server.routes/refresh-tools!) ctx-atom)
                           ((requiring-resolve 'hive-mcp.server.routes/refresh-surfaces!)))]
            (when (seq (:surfaces out)) out))))

(defn refresh-agent-tools!
  "Re-seat the agent-delegation registry from the advertised table. Returns
   its tool count, nil when it could not be reached."
  []
  (rescue nil ((requiring-resolve 'hive-mcp.agent.registry/refresh!))))

(defn refresh-surface!
  "Bring the advertised surface up to date after a contribution to
   `tool-name` (nil: no composite to rebuild): rebuild its composite, re-drain
   schema-extensions, refresh every tool surface and the delegation registry
   from the one table. Each leg is rescued on its own; returns what each did."
  [tool-name]
  {:composite    (some? (rescue nil (rebuild-composite! tool-name)))
   :schema-tools (rescue [] (redrain-schema-extensions!))
   :server-tools (refresh-server-tools!)
   :agent-tools  (refresh-agent-tools!)})

(defn on-contribution
  "The contribution listener: bring the surface up to date for the tool the
   EVENT names. Registered BY VAR (see `install!`)."
  [{:keys [type tool-name addon-id]}]
  (let [out (refresh-surface! tool-name)]
    (log/debug "Contribution reached the surface"
               {:type type :tool tool-name :addon addon-id :refresh out})))

(defn install!
  "Subscribe to the registry's contribution events. Idempotent by id.
   Returns the listener id, as it always has.

   The listener is registered as the VAR #'on-contribution, never as a fn
   value: the listener table is a defonce that outlives a reload of this
   namespace, so a captured value would keep running the OLD code after a
   core reload. Through the var, every event runs the current definition.

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
  (let [id (ext/add-contribution-listener! :reactive-surface #'on-contribution)]
    (rescue nil (ext/ensure-seam-listener!))
    id))

(defn uninstall!
  "Unsubscribe. For tests."
  []
  (ext/remove-contribution-listener! :reactive-surface))

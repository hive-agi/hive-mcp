(ns hive-mcp.tools.composite
  "Build consolidated MCP tools dynamically from addon command contributions.

   Supports two modes:
   1. Pure composite (addon-only): tool has no core handlers, all commands from addons.
   2. Merged composite (core + addon): tool has canonical core handlers that addons
      can extend or override.

   Addon handlers override core handlers with the same name (addon wins).
   Re-resolves contributions on each call for hot-reload support."
  (:require [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.cli :as cli]
            [clojure.string :as str]
            [hive-mcp.dispatch.handler :as dispatch]
            [hive-addon.registry.commands :as acmds]
            [hive-mcp.dispatch.verbs :as verbs]
            [hive-mcp.extensions.dispatch-wrap :as dispatch-wrap]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Addon Contribution → Handler Map
;; =============================================================================

(defn- addon-commands->handlers
  "Convert addon command contributions to keyword->fn handler map.
   Supports both flat handlers and nested handler trees.

   Every handler is dispatched through
   hive-mcp.extensions.dispatch-wrap/wrap-addon-handler as (wrap addon-id
   handler): the registered :addon/wrap-handler extension, then the drain gate
   an unmount waits on.

   The gate is `dispatch/handler?` and NOT `fn?`, and this is the site where
   that mattered most: `fn?` is false for a var, so a var-registered handler
   took the else branch and skipped wrapping ENTIRELY, with no throw and no
   log. A silent loss of the addon wrapper is worse than a refusal, because
   nothing downstream can tell the unwrapped handler from a wrapped one."
  [tool-name]
  (when-let [commands (acmds/get-commands tool-name)]
    (into {} (map (fn [[cmd {:keys [handler addon]}]]
                    [(keyword cmd) (if (dispatch/handler? handler)
                                     (dispatch-wrap/wrap-addon-handler addon handler)
                                     handler)]))
          commands)))

(defn lazy-resolve-handlers
  "Lazily resolve a consolidated tool's `handlers` map by fully-qualified
   symbol, triggering ns load on first access (DIP).

   Replaces the static `c-X/handlers` reference pattern in domain-root
   consolidators (project, memory, code, swarm). Drops the compile-time
   coupling between a domain root and every subdomain ns it merges in —
   moving / renaming a subdomain consolidator no longer breaks compile,
   it just yields an empty handler tree at runtime.

   Returns the handlers tree on success, `{}` on miss. Caller merges
   into its canonical-handlers tree the same way the literal reference
   would have."
  [sym]
  (or (try (some-> (requiring-resolve sym) deref)
           (catch Throwable _ nil))
      {}))

(defn lazy-resolve-schema-props
  "Lazily resolve a consolidated tool's advertised inputSchema :properties by
   fully-qualified symbol of its `tool-def` (a map), its `tools` (a vector of
   tool-defs whose first entry is the root), or its `tool-defs` (a 0-arity fn
   returning that vector), triggering ns load on first access (DIP).

   Returns the properties map on success, `{}` on miss.

   Sibling of `lazy-resolve-handlers`: a domain root that folds a subdomain's
   HANDLERS must fold that subdomain's PARAMS too. The MCP layer forwards only
   params the called tool declares, so an undeclared one is dropped and the
   subdomain handler runs on its default instead."
  [sym]
  (or (try (let [v  (some-> (requiring-resolve sym) deref)
                 td (cond (fn? v)         (first (v))
                          (sequential? v) (first v)
                          :else           v)]
             (get-in td [:inputSchema :properties]))
           (catch Throwable _ nil))
      {}))

;; =============================================================================
;; Composite Handler Builder
;; =============================================================================

(defn- registered-tool-verbs
  "The `command` enum of the registered standalone tool named TOOL-NAME, or
   nil. A contributed subdomain that forwards to such a tool (`code carto` ->
   the `carto` tool) routes exactly those verbs."
  [tool-name]
  (some (fn [t]
          (when (= tool-name (:name t))
            (get-in t [:inputSchema :properties "command" :enum])))
        (ext/get-registered-tools)))

(defn contributed-verbs
  "The verbs the subdomain K contributed under TOOL-NAME routes, or nil when it
   declares none. Asked, first answer wins:
     - the contribution spec's :verbs (a collection, or a 0-arity fn)
     - the keys of a verb-map :handler
     - the command enum of a registered tool named K
   This is the ::cli/verb-oracle port an unknown-command hint consults, so a
   bare verb is offered only under the subdomains that really route it."
  [tool-name k]
  (let [cmd  (name k)
        spec (get (acmds/get-commands tool-name) cmd)
        v    (:verbs spec)
        h    (:handler spec)]
    (or (seq (if (fn? v) (v) v))
        (when (map? h) (seq (map name (keys h))))
        (seq (registered-tool-verbs cmd)))))

(defn- with-contribution-meta
  "HANDLERS with ADDON-CMDS' root keys recorded as ::cli/opaque-roots and the
   ::cli/verb-oracle port bound to TOOL-NAME's contributions."
  [handlers tool-name addon-cmds]
  (vary-meta handlers
             #(-> %
                  (update ::cli/opaque-roots (fnil into #{}) (keys addon-cmds))
                  (assoc ::cli/verb-oracle (partial contributed-verbs tool-name)))))

(defn build-composite-handler
  "Build a handler fn that dispatches to contributed addon handlers only.
   Re-resolves contributions on each call so hot-reload picks up changes.
   Use build-merged-handler when core handlers exist.

   Every root is contributed, so all of them are recorded under
   ::cli/opaque-roots in the tree's metadata, beside the ::cli/verb-oracle
   port that says which verbs each one routes."
  [tool-name]
  (fn [params]
    (let [addon-cmds (or (addon-commands->handlers tool-name) {})
          handlers   (with-contribution-meta addon-cmds tool-name addon-cmds)
          cli-fn     (cli/make-cli-handler handlers)]
      (cli-fn params))))

(defn subdomain-handler
  "Wrap INNER as the handler for SUBDOMAIN-NAME: strips the \"<subdomain> \"
   prefix off :command before calling INNER.

   A subdomain dispatches on the whole command string — `code carto search`
   reaches the subdomain owner as \"carto search\" — so the owner uses this to
   hand its inner router just \"search\". A command without the prefix is
   passed through unchanged."
  [subdomain-name inner]
  (fn [params]
    (let [full   (str (:command params))
          prefix (str subdomain-name " ")]
      (inner (assoc params :command (if (str/starts-with? full prefix)
                                      (subs full (count prefix))
                                      full))))))

(defn effective-handlers
  "The handler tree TOOL-NAME dispatches on right now: CANONICAL-HANDLERS merged
   with the commands addons have contributed under TOOL-NAME (addon wins).
   Re-resolved on every call, so a contribution registered later is visible.

   CANONICAL-HANDLERS may be the map itself or the VAR that holds it, and
   passing the var is what makes the core half of the tree reloadable too.
   Before this arm the addon half was re-resolved per call while the core half
   was whatever map existed when the consolidated namespace last loaded — so a
   reload of a leaf handler namespace reached dispatch only if the consolidated
   namespace above it happened to reload as well. `dispatch/current` is read
   HERE, at call time, and never hoisted: reading it at build time is the
   value-capture this exists to undo (20260817195749-0d407e9c).

   Contributed root keys are recorded under ::cli/opaque-roots in the returned
   map's METADATA: a contributed handler receives the whole :command and routes
   the remainder itself, so this tree cannot enumerate what lives beneath it.
   The ::cli/verb-oracle port beside it answers which verbs each opaque root,
   core or contributed, routes (`contributed-verbs`). The map value itself is
   identical to the plain merge."
  [tool-name canonical-handlers]
  (let [canonical (dispatch/current canonical-handlers)
        addon-cmds (addon-commands->handlers tool-name)]
    (with-contribution-meta (merge canonical addon-cmds) tool-name (or addon-cmds {}))))

(defn build-merged-handler
  "Build a handler fn that merges core handlers with addon contributions.
   Addon handlers override core handlers with the same name (addon wins).
   Re-resolves addon contributions on each call for hot-reload.

   canonical-handlers: keyword->fn map (or nested tree) from a consolidated
   tool, or — preferred in this repo — the VAR holding it. The var spelling is
   what makes the CORE half of the tree reload-transparent: the returned
   closure then holds an indirection rather than a snapshot of the map, so a
   reload reaches dispatch without the consolidated namespace having to be
   reloaded in the same pass. See `effective-handlers`.

   tool-name: string name used for addon contribution lookup.

   Optional coerce-schema: passed through to cli/make-cli-handler."
  ([tool-name canonical-handlers]
   (build-merged-handler tool-name canonical-handlers nil))
  ([tool-name canonical-handlers coerce-schema]
   (fn [params]
     (let [cli-fn (cli/make-cli-handler
                   (effective-handlers tool-name canonical-handlers)
                   coerce-schema)]
       (cli-fn params)))))

;; =============================================================================
;; Composite Tool Definition Builder
;; =============================================================================

(defn build-composite-tool
  "Build a consolidated tool definition from addon contributions only.
   description-prefix: e.g. \"Code analysis\"
   Returns tool-def map identical in shape to other consolidated tools."
  [tool-name description-prefix]
  (let [commands (acmds/get-commands tool-name)
        cmd-names (vec (sort (keys commands)))
        all-params (apply merge-with merge (map :params (vals commands)))
        handler (build-composite-handler tool-name)]
    {:name tool-name
     :consolidated true
     :composite true
     :description (str description-prefix ": "
                       (str/join ", " cmd-names)
                       ". Use command='help' to list all.")
     :inputSchema {:type "object"
                   :properties (merge
                                {"command" {:type "string"
                                            :enum (conj cmd-names "help")
                                            :description (str tool-name " operation to perform")}}
                                all-params)
                   :required ["command"]}
     :handler handler}))

(defn- union-property
  "Fold an addon's schema property onto the core's under the same name.
   Equal specs collapse to one; different specs become an anyOf carrying both,
   descriptions joined, so a core `tasks` of [{file task}] and an addon's
   [string] can coexist without either side losing its shape. A plain
   merge here would let the addon silently retype a core parameter."
  [core addon]
  (cond
    (nil? core)      addon
    (nil? addon)     core
    (= core addon)   core
    :else
    (let [variants (fn [p] (if (and (map? p) (:anyOf p)) (:anyOf p) [p]))
          alts     (vec (distinct (concat (variants core) (variants addon))))
          descs    (->> [core addon] (map :description) (remove str/blank?) distinct)]
      (cond-> {:anyOf alts}
        (seq descs) (assoc :description (str/join " | " descs))))))

(def ^:private max-summary-chars 200)

(defn contribution-summary
  "One-line purpose of a command contribution SPEC: its :summary, else the
   first line of its :description, capped at `max-summary-chars`; nil when
   it declares neither."
  [{:keys [summary description]}]
  (let [s (or (when-not (str/blank? summary) summary)
              (when-not (str/blank? description) (first (str/split-lines description))))]
    (when-not (str/blank? s)
      (let [s (str/trim s)]
        (if (> (count s) max-summary-chars)
          (str (subs s 0 (dec max-summary-chars)) "…")
          s)))))

(defn contributed-commands-text
  "ADDON-CMDS (command name -> contribution spec) as one sentence for a tool
   description: every contributed command with its one-line purpose. nil when
   there are none."
  [addon-cmds]
  (when (seq addon-cmds)
    (str "Addon-contributed (`<name> help` lists a subdomain's verbs): "
         (str/join "; " (for [[cmd spec] (sort-by key addon-cmds)]
                          (if-let [s (contribution-summary spec)]
                            (str cmd " - " s)
                            (str cmd))))
         ".")))

(defn contributed-command-names
  "Every command string ADDON-CMDS (command name -> contribution spec) makes
   callable, sorted: each command name, and for a contribution whose :handler
   is a verb map, each `<command> <verb>` too. The verbs are what a caller
   actually sends (`ship land`, `belt scan`); an enum holding only the
   subdomain name would refuse every one of them."
  [addon-cmds]
  (vec (sort (distinct
              (for [[cmd {:keys [handler]}] addon-cmds
                    command (cons cmd (when (map? handler)
                                        (map #(str cmd " " (name %)) (keys handler))))]
                command)))))

(defn fold-verb-contributions
  "TOOL-DEF with the verbs contributed through hive-mcp.dispatch.verbs folded
   into its advertised surface. The roots read are the tool-def's
   :verb-roots, else its own :name. For the root named like the tool itself
   the contributed verb names join the `command` enum (when it has one);
   for a folded root (`<root> <verb>` under a domain tool) the enum gains
   `<root> <verb>` likewise. Every contributed :params joins the properties
   through `union-property`, so a contribution never silently retypes a core
   param. Unchanged when nothing was contributed."
  [tool-def]
  (let [tool-name (:name tool-def)
        roots     (or (seq (:verb-roots tool-def)) [tool-name])
        params    (apply merge-with union-property
                         (keep verbs/contributed-params roots))
        names     (for [r roots
                        v (verbs/contributed-verb-names r)]
                    (if (= r tool-name) v (str r " " v)))
        core-enum (get-in tool-def [:inputSchema :properties "command" :enum])]
    (cond-> tool-def
      (and (seq core-enum) (seq names))
      (assoc-in [:inputSchema :properties "command" :enum]
                (vec (sort (distinct (concat core-enum names)))))

      (seq params)
      (update-in [:inputSchema :properties]
                 #(merge-with union-property % params)))))

(defn build-merged-tool
  "Fold the current addon contributions to TOOL-NAME into a consolidated
   tool-def's advertised surface: every contributed command's :params joins
   the inputSchema properties, the `command` enum — when the core declares
   one — grows the contributed command names, and the description gains one
   line naming each contributed command with its purpose
   (`contributed-commands-text`). Verb-level contributions
   (hive-mcp.dispatch.verbs) fold in too, through `fold-verb-contributions`.
   Returns the tool-def unchanged when nothing has been contributed.

   Routing already folded contributions in (effective-handlers); this is the
   SCHEMA half. The MCP layer forwards only the params a tool declares, so a
   contributed verb whose params are absent here is reachable but cannot
   receive its own arguments. Applied from server.routes/make-tool on every
   (re)build of the tool table, so a late contribution reaches the schema the
   moment the reactive surface refreshes it.

   The enum is extended only when the core has one: a root whose `command` is
   free text (the swarm root routes by subdomain prefix) must not acquire an
   enum made of addon names alone, which would refuse every core command."
  [core-tool-def]
  (let [tool-name       (:name core-tool-def)
        addon-cmds      (acmds/get-commands tool-name)
        addon-cmd-names (contributed-command-names addon-cmds)
        addon-params    (apply merge-with union-property
                               (keep :params (vals (or addon-cmds {}))))
        core-enum       (get-in core-tool-def [:inputSchema :properties "command" :enum])]
    (fold-verb-contributions
     (if (empty? addon-cmds)
       core-tool-def
       (cond-> core-tool-def
         (seq core-enum)
         (assoc-in [:inputSchema :properties "command" :enum]
                   (vec (sort (distinct (concat core-enum addon-cmd-names)))))

         (seq addon-params)
         (update-in [:inputSchema :properties]
                    #(merge-with union-property % addon-params))

         true
         (update :description #(str/join " " (remove str/blank? [% (contributed-commands-text addon-cmds)])))

         true
         (assoc :composite true))))))

;; =============================================================================
;; Handler Map for Registry Introspection
;; =============================================================================

(defn build-composite-handlers
  "Build handler map for registry introspection (consolidated-handler-maps).
   Returns keyword->fn map compatible with cli/extract-commands."
  [tool-name]
  (let [commands (acmds/get-commands tool-name)]
    (into {:help (fn [_] {:type "text" :text "help"})}
          (map (fn [[cmd {:keys [handler]}]]
                 [(keyword cmd) handler])
               commands))))

(defn build-merged-handlers
  "Build handler map merging core + addon for registry introspection.
   canonical-handlers: keyword->fn map from a consolidated tool, or the VAR
   that holds it — resolved here, at call time, for the same reason
   `effective-handlers` resolves it."
  [tool-name canonical-handlers]
  (let [canonical  (dispatch/current canonical-handlers)
        addon-cmds (addon-commands->handlers tool-name)]
    (if addon-cmds
      (merge canonical addon-cmds)
      canonical)))

;; =============================================================================
;; Batch Builder
;; =============================================================================

(defn build-all-composite-tools
  "Build tool definitions for tool names that (a) have addon contributions and
   (b) are explicitly listed in `descriptions`. The descriptions map acts as a
   whitelist — tools not listed keep their core consolidated tool-def (and
   pick up addon commands via merge semantics in `build-merged-handler`).

   Without this filter, addon-only composites silently overwrite tools like
   `memory` whose 41 canonical verbs would disappear from dispatch — leaving
   only addon-contributed verbs callable."
  [descriptions]
  (vec (for [tool-name (acmds/contributed-tool-names)
             :when (contains? descriptions tool-name)
             :let [desc (get descriptions tool-name)]]
         (build-composite-tool tool-name desc))))
(ns hive-mcp.extensions.registry
  "Opaque extension registry for optional capabilities.

   Provides a thread-safe registry where external projects can register
   implementations at startup. Consumers look up extensions by opaque
   keyword keys without knowing which project provides them.

   Usage:
     ;; Registration (at startup, by extension project)
     (register! :gs/struct-cmp my-cmp-fn)

     ;; Consumption (anywhere in hive-mcp)
     (if-let [f (get-extension :gs/struct-cmp)]
       (f node-a node-b)
       default-value)

   Thread safety: All operations are atomic via atom + swap!.
   Idempotent: Re-registering the same key replaces silently."
  (:require [hive-mcp.protocols.registry :as reg]
            [malli.core :as m]
            [hive-addon.registry.commands :as addon-cmds]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Registry State
;; =============================================================================

(defonce ^:private ext-slot (reg/multi-slot {}))

(defonce ^:private tool-slot (reg/multi-slot {}))

;; Schema-extension ledger, OWNED: {tool-name {owner-id SchemaContribution}}.
;; See the Schema Extension Registry section for the value objects and the
;; read-time merge. Named differently from the merge-only atom it replaces so
;; a reload of this namespace in a live image starts from the new shape rather
;; than reinterpreting the old one.
(defonce ^:private schema-ledger
  (atom {}))

;; Composite tool command contributions.
;; Shape: {"analysis" {"lint" {:handler fn :params {...} :description "..." :addon :kondo} ...}}
;; The store itself lives in hive-addon.registry.commands, NOT here.
;; An addon depends on hive-di / hive-contracts / hive-addon, never on hive-mcp
;; core, so hive-addon owns the seam and this namespace is only a thin facade
;; over it. A second atom on this side made contribute! and get-commands two
;; implementations of one concept: an addon migrated to the correct seam would
;; mount, report :status :ok, and silently vanish from the tool surface,
;; because nothing here ever read hive-addon's store.
(defn- all-contributions
  "The whole contribution tree, in the shape this facade has always returned:
   {tool-name {command-name spec}}. Rebuilt from hive-addon on each call rather
   than cached, so there is exactly one source of truth."
  []
  (into {}
        (map (juxt identity addon-cmds/get-commands))
        (addon-cmds/contributed-tool-names)))

;; Listeners notified after a command contribution or retraction, so the
;; advertised surface can follow a contribution made AFTER boot.
;; Shape: {listener-id (fn [{:type :contribute|:retract :tool-name .. :addon-id ..}])}
(defonce ^:private contribution-listeners (atom {}))

;; =============================================================================
;; Public API
;; =============================================================================

(defn register!
  "Register an extension function under an opaque keyword key.
   Thread-safe, idempotent. Re-registration replaces the previous value."
  [k f]
  {:pre [(keyword? k) (ifn? f)]}
  (reg/reg-put! ext-slot k f)
  k)

(defn register-many!
  "Register multiple extensions at once from a map of {keyword fn}.
   Thread-safe, atomic."
  [m]
  {:pre [(map? m)]}
  (reg/reg-merge! ext-slot m))

(defn get-extension
  "Look up a registered extension by keyword key.
   Returns the function if registered, or default (nil if not provided)."
  ([k]
   (get (reg/reg-snapshot ext-slot) k))
  ([k default]
   (get (reg/reg-snapshot ext-slot) k default)))

(defn extension-available?
  "Check if an extension is registered under the given key."
  [k]
  (contains? (reg/reg-snapshot ext-slot) k))

(defn registered-keys
  "Return the set of all registered extension keys."
  []
  (set (keys (reg/reg-snapshot ext-slot))))

(defn deregister!
  "Remove an extension registration. Returns the key."
  [k]
  (reg/reg-remove! ext-slot k)
  k)

(defn clear-all!
  "Remove all registrations (fn + schema + tool + contributions). Intended for testing only."
  []
  (reg/reg-clear! ext-slot)
  (reset! schema-ledger {})
  (reg/reg-clear! tool-slot)
  (addon-cmds/clear!)
  nil)

;; =============================================================================
;; Schema Extension Registry
;; =============================================================================

;; Schema extensions are OWNED. An addon contributes parameters to a host tool's
;; advertised inputSchema, and the contribution is filed under the addon's id so
;; that shutting the addon down can take back exactly what it gave. The earlier
;; merge-only map had no owner to retract by: an evicted addon's params stayed
;; advertised for the life of the JVM.
;;
;; Strata, bottom up:
;;   value objects  SchemaOwner / ParamProperties / SchemaContribution / SchemaLedger
;;   pure           effective-schema-properties, contribute-schema,
;;                  retract-schema-owner, schema-owner-tools
;;   boundary       register-schema! / get-schema-extensions /
;;                  retract-schemas-by-owner! over the `schema-ledger` atom
;;
;; Precedence (read time): contributions are merged in the order they were
;; LAST registered, so on a clashing parameter name the most recent registration
;; wins. That is exactly what the merge-only map did, so a caller that never
;; names an owner sees no change. Re-registering under the same owner merges into
;; that owner's slot and moves it to the top.

(def default-schema-owner
  "Owner of a schema registration that names none (the 2-arity register-schema!)."
  ::host)

(def SchemaOwner
  "Who contributed a schema extension: an addon id (string or keyword) or
   `default-schema-owner`. Opaque; compared with `=`."
  [:and :any [:fn some?]])

(def ParamProperties
  "JSON-schema properties a contribution adds: {\"param_name\" {:type ...}}."
  [:map-of :any :any])

(def SchemaContribution
  "One owner's contribution to one tool. :seq orders contributions for the
   read-time merge (larger = registered later = wins a clash)."
  [:map [:seq nat-int?] [:props ParamProperties]])

(def ToolSchemaContributions
  [:map-of SchemaOwner SchemaContribution])

(def SchemaLedger
  "{tool-name {owner SchemaContribution}}."
  [:map-of :string ToolSchemaContributions])

(defn effective-schema-properties
  "Pure. The advertised properties for one tool: every owner's props merged in
   :seq order (later wins). nil when nothing is contributed."
  [tool-contributions]
  (some->> (vals tool-contributions)
           (sort-by :seq)
           (map :props)
           (apply merge)
           not-empty))

(defn- next-schema-seq
  [ledger]
  (inc (transduce (comp (mapcat vals) (map :seq)) max 0 (vals ledger))))

(defn- drop-shadowed-duplicates
  "PROPS minus every param a NAMED owner already contributes with the same
   value. Applied to anonymous registrations only: a re-drain that republishes
   an addon's params without naming it must not leave an unowned copy behind
   that outlives the addon's retraction."
  [tool-contributions props]
  (let [owned (into {}
                    (comp (remove (fn [[o _]] (= o default-schema-owner)))
                          (map (comp :props val)))
                    tool-contributions)]
    (into {} (remove (fn [[k v]] (and (contains? owned k) (= v (get owned k))))) props)))

(defn- claim-from-anonymous
  "TOOL-CONTRIBUTIONS with every param in PROPS that the anonymous slot holds
   with the SAME value removed from that slot: a named owner claiming a param
   takes it over, so its retraction takes the param with it. An emptied
   anonymous slot disappears."
  [tool-contributions props]
  (if-let [anon (get tool-contributions default-schema-owner)]
    (let [left (into {} (remove (fn [[k v]] (and (contains? props k) (= v (get props k)))))
                     (:props anon))]
      (if (seq left)
        (assoc-in tool-contributions [default-schema-owner :props] left)
        (dissoc tool-contributions default-schema-owner)))
    tool-contributions))

(defn contribute-schema
  "Pure. LEDGER with OWNER's PROPS merged into its slot for TOOL-NAME, stamped
   as the newest registration.

   Ownership is kept exact in both directions of arrival. An anonymous
   registration drops params a named owner already contributes with the same
   value; a named registration claims such params from the anonymous slot.
   Either way a param is owned by whoever can retract it."
  [ledger owner tool-name props]
  (let [anon?    (= owner default-schema-owner)
        existing (get ledger tool-name {})
        props    (if anon? (drop-shadowed-duplicates existing props) props)]
    (if (and anon? (empty? props))
      ledger
      (let [s (next-schema-seq ledger)]
        (assoc ledger tool-name
               (-> (if anon? existing (claim-from-anonymous existing props))
                   (update owner (fn [c] {:seq s :props (merge (:props c) props)}))))))))

(defn retract-schema-owner
  "Pure. LEDGER without anything OWNER contributed; a tool left with no owner
   disappears."
  [ledger owner]
  (into {}
        (keep (fn [[tool contribs]]
                (let [left (dissoc contribs owner)]
                  (when (seq left) [tool left]))))
        ledger))

(defn schema-owner-tools
  "Pure. Sorted names of the tools OWNER contributes to in LEDGER."
  [ledger owner]
  (into [] (comp (filter (fn [[_ contribs]] (contains? contribs owner))) (map key))
        (sort-by key ledger)))

(defn register-schema!
  "Register schema properties for a tool, owned by OWNER (an addon id).
   Properties is a map of {\"param_name\" {:type ... :description ...}}.
   The 2-arity files them under `default-schema-owner`. Thread-safe.
   Returns tool-name."
  ([tool-name properties]
   (register-schema! default-schema-owner tool-name properties))
  ([owner tool-name properties]
   {:pre [(some? owner) (string? tool-name) (map? properties)]}
   (swap! schema-ledger contribute-schema owner tool-name properties)
   tool-name))

(defn get-schema-extensions
  "The advertised schema property extensions for a tool: every owner's
   contribution merged at read time (see Precedence above). Returns map or nil,
   the same shape the merge-only registry returned."
  [tool-name]
  (effective-schema-properties (get @schema-ledger tool-name)))

(defn schema-contributions
  "Who contributes what to TOOL-NAME: {owner props}. For inspection; callers
   that advertise a schema read `get-schema-extensions`."
  [tool-name]
  (update-vals (get @schema-ledger tool-name {}) :props))

(defn clear-all-schemas!
  "Remove all schema registrations. Intended for testing only."
  []
  (reset! schema-ledger {})
  nil)

;; =============================================================================
;; Tool Registry (dynamic MCP tool definitions)
;; =============================================================================

(defn register-tool!
  "Register a full MCP tool definition for dynamic discovery.
   Tool-def must have :name (string) and :handler (ifn?).
   Thread-safe, idempotent. Last-write-wins by tool name."
  [tool-def]
  {:pre [(string? (:name tool-def)) (ifn? (:handler tool-def))]}
  (reg/reg-put! tool-slot (:name tool-def) tool-def)
  (:name tool-def))

(defn get-registered-tools
  "Return seq of all dynamically registered tool definitions."
  []
  (vals (reg/reg-snapshot tool-slot)))

(defn deregister-tool!
  "Remove a dynamically registered tool by name. Returns the name."
  [tool-name]
  (reg/reg-remove! tool-slot tool-name)
  tool-name)

(defn clear-all-tools!
  "Remove all tool registrations. Intended for testing only."
  []
  (reg/reg-clear! tool-slot)
  nil)

;; =============================================================================
;; Composite Tool Command Contributions
;; =============================================================================

(defn add-contribution-listener!
  "Register `f` to be called with {:type :contribute|:retract :tool-name s
   :addon-id a :commands [..]} after every contribute-commands! /
   retract-commands! / retract-all-by-addon!. Idempotent by id. A listener that
   throws is ignored — it must never break a contribution."
  [listener-id f]
  {:pre [(keyword? listener-id) (ifn? f)]}
  (swap! contribution-listeners assoc listener-id f)
  listener-id)

(defn remove-contribution-listener!
  "Drop a contribution listener. Returns the id."
  [listener-id]
  (swap! contribution-listeners dissoc listener-id)
  listener-id)

(defn- notify-contribution!
  [event]
  (doseq [[_ f] @contribution-listeners]
    (try (f event) (catch Throwable _ nil))))

;; Have we registered our notifier with hive-addon's own listener seam?
;;
;; A `delay`, not a load-time side effect: the probe runs at the first
;; contribution, so a hive-addon that arrives later is still picked up and
;; merely LOADING this namespace mutates nothing.
;;
;; Soft-resolved because hive-mcp pins hive-addon at a version that may predate
;; the seam. When it is absent this is false and the facade notifies directly,
;; exactly as it always did. When it is present, hive-addon notifies instead,
;; and it does so for contributions made by ANY caller, which is the point: an
;; addon that stops going through this facade and calls the registry directly
;; still reaches the advertised tool surface.
;;
;; notify-contribution! already guards every listener with its own try/catch, so
;; it satisfies hive-addon's contract that a listener must not throw (that
;; registry is in the portable three-host stratum and cannot catch for us).
(defonce ^:private seam-registered?
  (delay
    (boolean
     (when-let [add! (try (requiring-resolve 'hive-addon.registry.commands/add-listener!)
                          (catch Throwable _ nil))]
       (try (add! ::host-surface notify-contribution!) true
            (catch Throwable _ false))))))

(defn ensure-seam-listener!
  "Register this host's notifier with hive-addon's listener seam NOW, and
   return true when that seam is in play.

   Idempotent, and the reason it is public: `seam-registered?` is a delay that
   only `notify-once!` forces, and `notify-once!` runs only inside a call to
   this facade. An addon that has migrated off the facade and calls
   hive-addon.registry.commands/contribute! directly would therefore contribute
   into a store no listener watches: the commands land, and the advertised tool
   surface never rebuilds. The host must start listening at install time,
   before any addon mounts, rather than on the first call to the very facade
   the migration exists to stop using."
  []
  @seam-registered?)

(defn- notify-once!
  "Notify the host's contribution listeners exactly once.

   Two notifiers now exist for one event, and firing both would deliver every
   contribution twice: hive-addon's seam (which we register with) and this
   facade's direct call. Whichever is in play, a listener sees the event once."
  [event]
  (when-not @seam-registered?
    (notify-contribution! event)))

(defn contribute-commands!
  "Register commands that compose into a named composite tool.
   tool-name: \"analysis\", addon-id: :kondo
   commands: {\"lint\" {:handler fn :params {\"path\" {...}} :description \"...\"}}
   Notifies the contribution listeners afterwards.

   The contribution itself lands in hive-addon.registry.commands. This name
   stays public so the addons that soft-resolve it keep working while they
   migrate to the hive-addon seam one at a time; the listener notification is
   what this host adds on top, and hive-addon does not own it."
  [tool-name addon-id commands]
  (addon-cmds/contribute! tool-name addon-id commands)
  (notify-once! {:type :contribute :tool-name tool-name :addon-id addon-id
                         :commands (mapv name (keys commands))})
  (all-contributions))

(defn retract-commands!
  "Remove all commands contributed by an addon from a tool. Notifies the
   contribution listeners afterwards."
  [tool-name addon-id]
  (addon-cmds/retract! tool-name addon-id)
  (notify-once! {:type :retract :tool-name tool-name :addon-id addon-id})
  (all-contributions))

(defn retract-all-by-addon!
  "Remove all contributions from an addon across all tools (for shutdown).
   Notifies the contribution listeners once per tool the addon had touched."
  [addon-id]
  ;; the touched set must be read BEFORE the retraction, or there is nothing
  ;; left to attribute the notifications to
  (let [touched (into [] (keep (fn [[tn cmds]]
                                 (when (some #(= addon-id (:addon (val %))) cmds) tn)))
                      (all-contributions))]
    (addon-cmds/retract-all! addon-id)
    (doseq [tn touched]
      (notify-once! {:type :retract :tool-name tn :addon-id addon-id}))
    (all-contributions)))

(defn retract-schemas-by-owner!
  "Remove every schema extension OWNER contributed, across all tools (for
   shutdown). The schema-side twin of retract-all-by-addon!.

   Notifies the contribution listeners once per touched tool with
   {:type :retract-schema :tool-name tn :addon-id owner}, so the advertised
   inputSchema is rebuilt without the retracted params. Notified DIRECTLY, not
   through notify-once!: hive-addon's listener seam carries command events only
   and never sees a schema retraction. Returns the touched tool names."
  [owner]
  (let [[before _] (swap-vals! schema-ledger retract-schema-owner owner)
        touched    (schema-owner-tools before owner)]
    (doseq [tn touched]
      (notify-contribution! {:type :retract-schema :tool-name tn :addon-id owner}))
    touched))

(m/=> effective-schema-properties
      [:=> [:cat [:maybe ToolSchemaContributions]] [:maybe ParamProperties]])

(m/=> contribute-schema
      [:=> [:cat SchemaLedger SchemaOwner :string ParamProperties] SchemaLedger])

(m/=> retract-schema-owner
      [:=> [:cat SchemaLedger SchemaOwner] SchemaLedger])

(def ExtensionFn
  "Schema for a registered extension value: any invokable."
  [:fn ifn?])

(m/=> get-extension
      [:function
       [:=> [:cat :keyword] [:maybe ExtensionFn]]
       [:=> [:cat :keyword :any] :any]])
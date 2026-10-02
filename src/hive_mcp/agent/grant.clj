(ns hive-mcp.agent.grant
  "Wiring for capability grants: where a session's grant is recorded, how a
   caller's effective grant is found through lineage, and the two checks the
   host runs (at spawn and at tool dispatch).

   The grant DOMAIN (the subset law, attenuate, refusal) lives in the
   hive-agent addon, `hive-agent.loop.grant`. hive-mcp does not depend on
   hive-agent, so it is reached by soft resolution. Without it:
     - nobody can be given a grant (a spawn that asks for one is refused),
     - a session whose lineage carries no grant is unrestricted, as before,
     - a session whose lineage DOES carry a grant fails closed.

   The registry is a parameter (`get-slave`), so the lineage walk is pure.

   IDENTITY CAVEAT: the caller id is self-asserted by the transport until
   spawn credentials are verified (kanban IDENTITY-VERIFIED). This gate stops
   mistakes, not a hostile child."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def slave-attr
  "Registry attribute holding a session's recorded grant (wire form)."
  :slave/grant)

(def max-lineage-depth
  "Bound on the ancestor walk, the same bound enclaves use."
  32)

;; =============================================================================
;; Domain (soft-resolved from hive-agent)
;; =============================================================================

(defn- soft [sym]
  (try (requiring-resolve sym) (catch Throwable _ nil)))

(defn domain
  "The grant domain fns, or nil when the hive-agent addon is not loaded."
  []
  (let [fs {:attenuate (soft 'hive-agent.loop.grant/attenuate)
            :refusal   (soft 'hive-agent.loop.grant/refusal)
            :from-wire (soft 'hive-agent.loop.grant/from-wire)
            :->wire    (soft 'hive-agent.loop.grant/->wire)}]
    (when (every? some? (vals fs)) fs)))

(defn registry-get-slave
  "The live registry port: the swarm row for `id`, or nil. Resolved late (the
   swarm registry is wired after this namespace loads); a missing registry
   reads as no row, so nothing is gated. The only effectful fn here; every
   check below takes the port as a parameter."
  [id]
  (when-let [f (soft 'hive-mcp.swarm.datascript.queries/get-slave)]
    (try (f id) (catch Throwable _ nil))))

;; =============================================================================
;; Lineage (pure over get-slave)
;; =============================================================================

(defn slave-ids
  "Registry ids a caller may be registered under: itself, and for a spawned
   agent (`<slave-id>:<instance>`) its slave id. A coordinator lane is never
   looked up by its bare agent part, which every session shares."
  [caller-id]
  (let [agent (first (str/split caller-id #":" 2))]
    (cond-> [caller-id]
      (and (not= agent caller-id) (not (str/starts-with? agent "coordinator"))) (conj agent))))

(defn- parent-of [slave]
  (let [p (or (:slave/parent slave) (:slave/parent-id slave))
        p (if (map? p) (:slave/id p) p)]
    (when-not (str/blank? (some-> p str)) (str p))))

(defn lineage
  "Registry rows from the caller up to its root, nearest first, the caller's
   own row (when it has one) first. Cycle-safe, bounded."
  [get-slave caller-id]
  (loop [id caller-id seen #{caller-id} acc []]
    (let [slave (some get-slave (slave-ids id))
          acc   (cond-> acc slave (conj slave))
          up    (some-> slave parent-of)]
      (if (or (nil? up) (contains? seen up) (>= (count acc) max-lineage-depth))
        acc
        (recur up (conj seen up) acc)))))

(defn recorded-grant
  "The nearest grant recorded on the caller's lineage (wire form), or nil:
   no grant anywhere means unrestricted."
  [get-slave caller-id]
  (when-not (str/blank? (some-> caller-id str))
    (some slave-attr (lineage get-slave (str caller-id)))))

(defn depth-of
  "Lineage depth of a caller: its registry depth, else 0 (a coordinator)."
  [get-slave caller-id]
  (or (when-not (str/blank? (some-> caller-id str))
        (some :slave/depth (take 1 (lineage get-slave (str caller-id)))))
      0))

;; =============================================================================
;; Spawn
;; =============================================================================

(defn child-grant
  "The grant a spawn records for its child, as a decision map:

     {:grant nil}            nothing recorded anywhere and nothing asked:
                             the child is unrestricted, exactly as today.
     {:grant <wire>}         the effective grant to store and report.
     {:refused <message> :data {...}}   the spawn must not happen.

   `parent-wire` is the parent's recorded grant (nil = unrestricted),
   `requested` the raw `grant` parameter (nil = share), `child-depth` the
   depth the child will sit at. `dom` is `domain` (nil when unloaded)."
  [dom parent-wire requested child-depth]
  (cond
    (and (nil? parent-wire) (nil? requested))
    {:grant nil}

    (nil? dom)
    {:refused (str "Grants need the hive-agent addon, which is not loaded: "
                   "cannot " (if requested "honour the requested grant" "apply the parent's grant")
                   ". Nothing was spawned.")}

    :else
    (let [{:keys [attenuate refusal from-wire ->wire]} dom
          parent (from-wire parent-wire)]
      (if-let [no (refusal parent {:capability :spawn :depth child-depth})]
        {:refused (str "SPAWN DENIED by grant: " (:message no)
                       " Request it from your parent with `hivemind ask`.")
         :data    (select-keys no [:grant/missing])}
        (let [res (attenuate parent (if (map? requested) (from-wire requested) requested))]
          (if (contains? res :error)
            {:refused (str "SPAWN DENIED: " (:message res))
             :data    (dissoc res :message)}
            {:grant (->wire (:ok res))}))))))

;; =============================================================================
;; Tool dispatch
;; =============================================================================

(def always-permitted
  "Tool entries no grant can take away: the path a refused child uses to ask
   its parent for more. Gating it would leave a refusal with no remedy."
  #{"hivemind:ask" "hivemind:messages" "hivemind:respond"})

(defn call-refusal
  "nil when a call by `caller-id` to `tool`/`command` is permitted, else the
   refusal text. Pure over `get-slave` and `dom`."
  [dom get-slave caller-id tool command]
  (when-let [wire (recorded-grant get-slave caller-id)]
    (let [entry (if (str/blank? (some-> command name)) (str tool) (str tool ":" (name command)))]
      (when-not (contains? always-permitted entry)
        (if (nil? dom)
          (str "REFUSED by grant - " entry ": this session holds a grant but the "
               "hive-agent addon that evaluates grants is not loaded, so the call "
               "fails closed.")
          (let [{:keys [refusal from-wire]} dom]
            (when-let [no (refusal (from-wire wire) {:capability :tool :tool tool :command command})]
              (str "REFUSED by grant - " entry "\n\n" (:message no)
                   "\n\nMissing capability: " (pr-str (:grant/missing no))
                   "\nRequest it from your parent with `hivemind ask`, naming the "
                   "capability and why you need it. Do not retry another way."))))))))

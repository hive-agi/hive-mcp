(ns hive-mcp.channel.broadcast-ledger
  "How many broadcasts a swarm has already spent, so the volume gate can bite.

   hive-mcp.channel.broadcast-policy can refuse a broadcast on volume alone,
   but only when its caller says how many went before. Nothing tracked that,
   so the gate was unreachable in production: the closed reason set stopped a
   caller from inventing a justification, and stopped nothing from repeating
   an admissible one. `:shared-discovery` five hundred times is five hundred
   admissible broadcasts.

   This is the counter that closes it. The unit is the PROJECT, because the
   cost a broadcast imposes is paid by the readers of one swarm, and two
   projects running at once are two separate audiences that should not spend
   each other's budget.

   ## The window slides, and it prunes on write

   A count since process start would refuse forever once a long-lived server
   crossed the budget. So an entry ages out: only broadcasts inside the window
   are spent, and a project whose entries have all aged out stops being
   tracked at all. That is also what bounds the map. There is no eviction
   policy to get wrong, because an idle project empties itself.

   ## Pure core, one atom

   Every decision lives in a pure function over a ledger VALUE; the atom is a
   held value and three thin wrappers. A test needs no fixture and no clock:
   it passes its own `now`."
  (:require [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const default-window-ms
  "How far back the volume gate looks, in milliseconds.

   Ten minutes. Long enough that a burst cannot be spread just past the edge of
   it, short enough that a swarm which legitimately broadcast its budget an
   hour ago is not still refused. Override via config path
   [:hivemind :broadcast-window-ms]."
  600000)

(defn- config-value
  "Read a config path, nil on any failure. Lazy requiring-resolve keeps the
   channel layer free of a load-time dep on config bootstrap."
  [path]
  (try
    (when-let [f (requiring-resolve 'hive-mcp.config.core/get-in-config)]
      (f path))
    (catch Exception _ nil)))

(defn window-ms
  "The configured window, falling back to `default-window-ms`. A non-positive
   or non-numeric setting is ignored rather than honoured: a zero window would
   silently disable the gate, which is the failure this namespace exists to
   end."
  []
  (let [v (config-value [:hivemind :broadcast-window-ms])]
    (if (and (number? v) (pos? v)) (long v) default-window-ms)))

;; =============================================================================
;; Pure core: a ledger is {project-id [timestamp ...]}
;; =============================================================================

(defn- key-for
  "The ledger key for a project. A blank or missing project-id is still an
   audience, so it is metered under one shared key rather than escaping the
   gate by being unnamed."
  [project-id]
  (let [s (str project-id)]
    (if (str/blank? s) "global" s)))

(defn live
  "The timestamps of `stamps` still inside the window ending at `now`.
   Strictly inside: an entry exactly `window` old has aged out."
  [stamps now window]
  (into [] (filter #(> (long %) (- (long now) (long window)))) stamps))

(defn prune
  "Drop every aged-out entry from `ledger`, and every project left with none.

   Dropping the empty projects is what bounds the map: a project that stops
   broadcasting stops being tracked, so there is no eviction policy and no
   growth without traffic."
  [ledger now window]
  (reduce-kv (fn [acc k stamps]
               (let [kept (live stamps now window)]
                 (if (seq kept) (assoc acc k kept) acc)))
             {}
             (or ledger {})))

(defn spent
  "How many broadcasts `project-id` has been admitted inside the window."
  [ledger project-id now window]
  (count (live (get (or ledger {}) (key-for project-id) []) now window)))

(defn spend
  "Record one admitted broadcast for `project-id` at `now`.

   Prunes the whole ledger on the way through, so the map is trimmed by the
   traffic that would otherwise grow it, with no sweeper to schedule."
  [ledger project-id now window]
  (let [pruned (prune ledger now window)
        k (key-for project-id)]
    (assoc pruned k (conj (get pruned k []) (long now)))))

;; =============================================================================
;; The held value
;; =============================================================================

(defonce ^:private ledger
  (atom {}))

(defn spent-recently
  "How many broadcasts this project has been admitted inside the live window.
   What a caller hands broadcast-policy as `:recent-broadcasts`."
  ([project-id] (spent-recently project-id (System/currentTimeMillis)))
  ([project-id now]
   (spent @ledger project-id now (window-ms))))

(defn record-broadcast!
  "Charge one admitted broadcast to this project's budget.

   Called only when the policy ADMITTED the broadcast: a refused one costs the
   readers nothing, so charging for it would let a rejected request push a
   later legitimate one over the edge."
  ([project-id] (record-broadcast! project-id (System/currentTimeMillis)))
  ([project-id now]
   (swap! ledger spend project-id now (window-ms))
   nil))

(defn reset-ledger!
  "Forget every recorded broadcast. For tests, and for an operator who has
   dealt with whatever caused the burst."
  []
  (reset! ledger {})
  nil)

(defn snapshot
  "The live ledger as {project-id count}, for diagnostics. Reading it decides
   nothing, so a count here never affects what the gate does."
  ([] (snapshot (System/currentTimeMillis)))
  ([now]
   (let [w (window-ms)]
     (reduce-kv (fn [acc k stamps] (assoc acc k (count (live stamps now w))))
                {}
                (prune @ledger now w)))))

(ns hive-mcp.session.current
  "The running process's own SessionRef.

   Thin impure adapter over the pure algebra in hive-mcp.session.identity: it
   reads the environment and the swarm store, projects a `world` snapshot, and
   asks identity/resolve-ref. All the rules live over there; this namespace only
   supplies the facts.

   Resolution order for the id, most specific first:
     1. HIVE_SESSION_ID / CLAUDE_SESSION_ID  -- an explicit id from the host
     2. CLAUDE_SWARM_SLAVE_ID                -- this process is a ling
     3. the slave/coordinator row for that agent, if the store has one
     4. a per-process UUID, minted once

   Step 4 matters: a bare editor session has no slave row and no id from the
   host, and giving every such session the SAME id (which is what a date did)
   is the bug. Minting one per process keeps concurrent sessions apart, and the
   id is stable for the process's life, which is the session's life."
  (:require [hive-mcp.session.identity :as sid]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private process-session-id
  "Minted once per JVM. delay, not an atom: the id must not change under a
   caller mid-session, and two threads must not mint two ids."
  (delay (str "sess-" (subs (str (java.util.UUID/randomUUID)) 0 12))))

(defn- env [k] (System/getenv k))

(defn host-session-id
  "The session id the host handed us, if any. Pure in its argument so tests can
   pass an explicit env map instead of touching System/getenv."
  ([] (host-session-id {"HIVE_SESSION_ID"        (env "HIVE_SESSION_ID")
                        "CLAUDE_SESSION_ID"      (env "CLAUDE_SESSION_ID")
                        "CLAUDE_SWARM_SLAVE_ID"  (env "CLAUDE_SWARM_SLAVE_ID")}))
  ([env-map]
   (some (fn [k] (let [v (get env-map k)]
                   (when (and (string? v) (seq v)) v)))
         ["HIVE_SESSION_ID" "CLAUDE_SESSION_ID" "CLAUDE_SWARM_SLAVE_ID"])))

(defn agent-id
  "This process's agent id, when it is a swarm ling."
  []
  (env "CLAUDE_SWARM_SLAVE_ID"))

(defn- safe-resolve
  "requiring-resolve that never throws. The swarm store is an optional
   dependency here: a wrap in a plain editor session must work with no swarm
   loaded at all, so a missing var means 'no world', not a failure."
  [sym]
  (try (requiring-resolve sym) (catch Throwable _ nil)))

(defn world-snapshot
  "Project the swarm store into the `world` shape identity/resolve-ref expects.
   Returns {:slaves {} :coordinators {}} when the store is absent or unreadable
   -- every caller then degrades to an :adhoc session, which is correct."
  []
  (try
    (let [list-slaves (safe-resolve 'hive-mcp.swarm.datascript/get-all-slaves)
          list-coords (safe-resolve 'hive-mcp.swarm.datascript.coordination/get-all-coordinators)
          slaves (when list-slaves (list-slaves))
          coords (when list-coords (list-coords))]
      {:slaves (into {} (map (juxt :slave/id
                                   (fn [s]
                                     {:slave/id         (:slave/id s)
                                      :slave/depth      (:slave/depth s)
                                      :slave/parent-id  (or (:slave/parent-id s)
                                                            (get-in s [:slave/parent :slave/id]))
                                      :slave/session-id (:slave/session-id s)
                                      :slave/project-id (:slave/project-id s)})))
                     slaves)
       :coordinators (into {} (map (juxt :coordinator/id identity)) coords)})
    (catch Throwable t
      (log/debug "session/current: no swarm world available:" (ex-message t))
      {:slaves {} :coordinators {}})))

(defn session-ref
  "This process's SessionRef. Pass :project-id so an ad-hoc session can be
   adopted by the coordinator of the project it is working in.

   Always returns a VALID ref: when nothing else identifies the session, the
   per-process id is used, so the caller never has to handle a nil id."
  ([] (session-ref {}))
  ([{:keys [project-id world session-id]}]
   (let [world (or world (world-snapshot))
         aid   (agent-id)
         sid'  (or session-id (host-session-id) @process-session-id)
         ref   (sid/resolve-ref world {:agent-id   aid
                                       :project-id project-id
                                       :session-id sid'})]
     (if (sid/valid? ref)
       ref
       (sid/session-ref {:id sid' :kind :adhoc :project-id project-id :agent-id aid})))))

(defn session-id
  "Just the id of this process's session. The value that belongs on a row so a
   wrap can tell whose it is."
  ([] (:session/id (session-ref)))
  ([opts] (:session/id (session-ref opts))))

(defn parent-of-fn
  "A session-id -> parent-session-id lookup over a world snapshot, for the
   ownership walks in hive-mcp.session.identity."
  ([] (parent-of-fn (world-snapshot)))
  ([{:keys [slaves] :as world}]
   (let [by-session (into {} (keep (fn [[aid s]]
                                     (when-let [sess (:slave/session-id s)]
                                       [sess aid]))
                                   slaves))]
     (fn [session-id]
       (when-let [aid (get by-session session-id)]
         (:slave/session-id (sid/root-slave world aid)))))))

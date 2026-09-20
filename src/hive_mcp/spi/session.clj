;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.spi.session
  "Session lifecycle the kernel TRIGGERS but does not implement: noting that a
   session started, and installing the hooks that wrap it at the end.

   HIVE-KERNEL E6a. `server.routes.middleware` marked every request's session
   start through `crystal.core`, and `server.lifecycle` registered the crystal
   hooks at boot; both are hive-memory extraction targets. Crystallizing a
   session is memory domain work, while KNOWING a session started is the
   kernel's business, so the kernel keeps the call and loses the require.

   `record-session-start!` runs on EVERY request, so the resolution is cached,
   misses included. `install!` / `uninstall!` / `reset-cache!` clear it, which
   is how a memory addon mounted after boot is picked up.

   With no memory domain both calls are no-ops: sessions are still served, and
   nothing is crystallized because there is nowhere to crystallize into."
  (:require [hive-mcp.swarm.adapters.soft :as soft]))

(defprotocol ISessionLifecycle
  (-record-session-start! [this agent-id]
    "Note that AGENT-ID started a session. Called per request; must be cheap
     and must never throw.")
  (-register-session-hooks! [this registry]
    "Install the session hooks (auto-wrap on session end) into REGISTRY."))

(def noop
  "No memory domain: nothing records a session, nothing wraps one."
  (reify ISessionLifecycle
    (-record-session-start! [_ _] nil)
    (-register-session-hooks! [_ _] nil)))

(def ^:private host-syms
  {::record-start   'hive-mcp.crystal.core/record-session-start!
   ::register-hooks 'hive-mcp.crystal.hooks/register-hooks!})

(defonce ^:private cache
  ^{:doc "{port-key -> fn | ::absent}. Cleared by install!/uninstall!/reset-cache!."}
  (atom {}))

(defn- host-fn
  [port-key]
  (let [hit (get @cache port-key)]
    (cond
      (= ::absent hit) nil
      (some? hit)      hit
      :else            (let [f (soft/resolve-soft (host-syms port-key))]
                         (swap! cache assoc port-key (or f ::absent))
                         f))))

(def ^:private host-adapter
  "Delegates to `hive-mcp.crystal.*` while those namespaces are still in core,
   and to `noop` once they are not."
  (reify ISessionLifecycle
    (-record-session-start! [_ agent-id]
      (when-let [f (host-fn ::record-start)] (f agent-id)))
    (-register-session-hooks! [_ registry]
      (when-let [f (host-fn ::register-hooks)] (f registry)))))

(defonce ^:private installed (atom nil))

(defn reset-cache!
  "Forget which host functions resolved. Returns nil."
  []
  (reset! cache {})
  nil)

(defn install!
  "Install IMPL as the session lifecycle. Returns IMPL."
  [impl]
  (reset! installed impl)
  (reset-cache!)
  impl)

(defn uninstall!
  "Drop the installed implementation. Returns nil."
  []
  (reset! installed nil)
  (reset-cache!)
  nil)

(defn current
  "The implementation calls are routed to right now."
  []
  (or @installed host-adapter))

(defn record-session-start!
  "Note that AGENT-ID started a session."
  [agent-id]
  (-record-session-start! (current) agent-id))

(defn register-session-hooks!
  "Install the session hooks into REGISTRY."
  [registry]
  (-register-session-hooks! (current) registry))

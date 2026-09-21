;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.spi.metrics
  "Metrics port: what the KERNEL is allowed to know about telemetry.

   HIVE-KERNEL step E2. `hive-mcp.telemetry.*` is a hive-observability
   extraction target, so a kernel namespace may not require it. The kernel
   calls the four functions below; an implementation arrives one of two ways:

   - an addon (or the host, at boot) calls `install!` with an `IMetrics`;
   - nobody installs anything, and the port late-binds by SYMBOL to the
     in-core `hive-mcp.telemetry.prometheus` for as long as that namespace
     still lives here. Once it leaves, the same call answers the Noop.

   Same shape and same reason as `hive-mcp.swarm.adapters.soft`, whose
   `resolve-soft` does the late binding here too. Host-local, like the K1
   ports: hive-spi has never released a metrics port, and pinning an
   unreleased sibling would land staging red (memory 20260919203856-2bb9451d).

   Resolution is cached, including a miss, because `dispatch` calls
   `inc-events!` on every event. `install!`, `uninstall!` and
   `reset-cache!` clear the cache, so a namespace that arrives later (an
   addon mounting its telemetry) is picked up on the next call."
  (:require [hive-mcp.swarm.adapters.soft :as soft]))

(defprotocol IMetrics
  "Every metric the kernel reports. An implementation returns nil from all
   four; callers never read a return value."
  (-inc-events! [this type severity]
    "Count one event. TYPE is the event id or a category keyword, SEVERITY
     one of :info :warn :error :fatal.")
  (-observe-request-duration! [this tool seconds]
    "Observe a duration in SECONDS (double) for the named TOOL (string).")
  (-set-lings-active! [this n]
    "Set the active-ling gauge to N.")
  (-handle-effect! [this effect-data]
    "Handle a `:prometheus` event effect: {:counter kw :labels {} :histogram {}}."))

(def noop
  "The answer when no implementation is present: every call is a no-op."
  (reify IMetrics
    (-inc-events! [_ _ _] nil)
    (-observe-request-duration! [_ _ _] nil)
    (-set-lings-active! [_ _] nil)
    (-handle-effect! [_ _] nil)))

;; ---------------------------------------------------------------------------
;; Late binding to the in-core telemetry namespace, by symbol
;; ---------------------------------------------------------------------------

(def ^:private host-syms
  {::inc-events        'hive-mcp.telemetry.prometheus/inc-events-total!
   ::observe-duration  'hive-mcp.telemetry.prometheus/observe-request-duration!
   ::set-lings-active  'hive-mcp.telemetry.prometheus/set-lings-active!
   ::handle-effect     'hive-mcp.telemetry.prometheus/handle-prometheus-effect!})

(defonce ^:private cache
  ^{:doc "{port-key -> fn | ::absent}. Cleared by install!/uninstall!/reset-cache!."}
  (atom {}))

(defn- host-fn
  "The host function behind PORT-KEY, or nil when its namespace is absent.
   Both outcomes are cached: the kernel asks on every dispatched event."
  [port-key]
  (let [hit (get @cache port-key)]
    (cond
      (= ::absent hit) nil
      (some? hit)      hit
      :else            (let [f (soft/resolve-soft (host-syms port-key))]
                         (swap! cache assoc port-key (or f ::absent))
                         f))))

(def ^:private host-adapter
  "Delegates to `hive-mcp.telemetry.prometheus` while that namespace is still
   in core, and to `noop` once it is not."
  (reify IMetrics
    (-inc-events! [_ type severity]
      (if-let [f (host-fn ::inc-events)] (f type severity) nil))
    (-observe-request-duration! [_ tool seconds]
      (if-let [f (host-fn ::observe-duration)] (f tool seconds) nil))
    (-set-lings-active! [_ n]
      (if-let [f (host-fn ::set-lings-active)] (f n) nil))
    (-handle-effect! [_ effect-data]
      (if-let [f (host-fn ::handle-effect)] (f effect-data) nil))))

;; ---------------------------------------------------------------------------
;; Registry
;; ---------------------------------------------------------------------------

(defonce ^:private installed (atom nil))

(defn reset-cache!
  "Forget which host functions resolved. Returns nil."
  []
  (reset! cache {})
  nil)

(defn install!
  "Install IMPL as the metrics implementation. Returns IMPL."
  [impl]
  (reset! installed impl)
  (reset-cache!)
  impl)

(defn uninstall!
  "Drop the installed implementation; calls fall back to the late-bound host
   namespace, then to `noop`. Returns nil."
  []
  (reset! installed nil)
  (reset-cache!)
  nil)

(defn current
  "The implementation calls are routed to right now."
  []
  (or @installed host-adapter))

;; ---------------------------------------------------------------------------
;; What the kernel calls
;; ---------------------------------------------------------------------------

(defn inc-events!
  "Count one event of TYPE at SEVERITY. Never throws on a missing backend."
  [type severity]
  (-inc-events! (current) type severity))

(defn observe-request-duration!
  "Observe SECONDS for TOOL."
  [tool seconds]
  (-observe-request-duration! (current) tool seconds))

(defn set-lings-active!
  "Set the active-ling gauge to N."
  [n]
  (-set-lings-active! (current) n))

(defn handle-effect!
  "Handle a `:prometheus` event effect."
  [effect-data]
  (-handle-effect! (current) effect-data))

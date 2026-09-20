;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.spi.emacs
  "What the KERNEL may ask of an Emacs host: evaluate elisp, and place a ling
   on a daemon.

   HIVE-KERNEL step E3. Emacs is leaving hive-mcp entirely (decision
   20260919004258-69303a5c), so no kernel namespace may require
   `hive-mcp.emacs-ext.*`. Three did: `tools.core` (feature probes and the
   `with-elisp` macro), `addons.doctor` (its emacs health check) and
   `swarm.sync` (which daemon a ling binds to).

   Two protocols, because they are two questions with different honest
   answers when no Emacs is present:

   - IElispEval  answers a FAILED eval result, in the shape callers already
                 destructure ({:success false :error ...}), so a probe reads
                 as \"feature absent\" rather than throwing;
   - IEmacsDaemons answers nil, which is what the spawn path already treats
                 as \"no daemon placement\", so a ling still spawns.

   Host-local, like the other kernel ports (memory 20260919203856-2bb9451d).
   With nothing installed it late-binds BY SYMBOL through
   `hive-mcp.swarm.adapters.soft` for as long as `emacs-ext` ships in core,
   and answers the Noop once it does not."
  (:require [hive-mcp.swarm.adapters.soft :as soft]))

(defprotocol IElispEval
  "Evaluating elisp in a host Emacs."
  (-eval-elisp [this code]
    "Evaluate CODE. Returns {:success bool :result str :error str}.")
  (-eval-elisp-with-timeout [this code timeout-ms]
    "Evaluate CODE, giving up after TIMEOUT-MS. Same result shape.")
  (-emacs-running? [this]
    "True when a host Emacs is reachable."))

(defprotocol IEmacsDaemons
  "Placing a ling on an Emacs daemon."
  (-ensure-default-daemon! [this])
  (-select-daemon-for-ling [this ling-id]
    "{:daemon-id ... :reason ...} for LING-ID, or nil when there is no host.")
  (-bind-ling! [this daemon-id slave-id])
  (-unbind-ling! [this daemon-id slave-id])
  (-get-daemon-for-ling [this slave-id])
  (-default-daemon-id [this]))

(def no-host-result
  "The eval result when no Emacs host is present. Shaped like a failed eval,
   because that is what it is: callers destructure :success/:result/:error and
   already handle a failure."
  {:success false
   :result  nil
   :error   "no Emacs host in this build"})

(def noop
  "The answer with no Emacs: every eval fails cleanly, every daemon question
   answers nil."
  (reify
    IElispEval
    (-eval-elisp [_ _] no-host-result)
    (-eval-elisp-with-timeout [_ _ _] no-host-result)
    (-emacs-running? [_] false)
    IEmacsDaemons
    (-ensure-default-daemon! [_] nil)
    (-select-daemon-for-ling [_ _] nil)
    (-bind-ling! [_ _ _] nil)
    (-unbind-ling! [_ _ _] nil)
    (-get-daemon-for-ling [_ _] nil)
    (-default-daemon-id [_] nil)))

;; ---------------------------------------------------------------------------
;; Late binding to the in-core emacs-ext namespaces, by symbol
;; ---------------------------------------------------------------------------

(def ^:private host-syms
  {::eval-elisp          'hive-mcp.emacs-ext.client/eval-elisp
   ::eval-with-timeout   'hive-mcp.emacs-ext.client/eval-elisp-with-timeout
   ::emacs-running?      'hive-mcp.emacs-ext.client/emacs-running?
   ::ensure-default      'hive-mcp.emacs-ext.daemon-store/ensure-default-daemon!
   ::select-daemon       'hive-mcp.emacs-ext.daemon-store/select-daemon-for-ling
   ::bind-ling           'hive-mcp.emacs-ext.daemon-store/bind-ling!
   ::unbind-ling         'hive-mcp.emacs-ext.daemon-store/unbind-ling!
   ::daemon-for-ling     'hive-mcp.emacs-ext.daemon-store/get-daemon-for-ling
   ::default-daemon-id   'hive-mcp.emacs-ext.daemon-store/default-daemon-id})

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
  "Delegates to `hive-mcp.emacs-ext.*` while those namespaces are still in
   core, and to `noop` once they are not."
  (reify
    IElispEval
    (-eval-elisp [_ code]
      (if-let [f (host-fn ::eval-elisp)] (f code) no-host-result))
    (-eval-elisp-with-timeout [_ code timeout-ms]
      (if-let [f (host-fn ::eval-with-timeout)] (f code timeout-ms) no-host-result))
    (-emacs-running? [_]
      (if-let [f (host-fn ::emacs-running?)] (boolean (f)) false))
    IEmacsDaemons
    (-ensure-default-daemon! [_]
      (when-let [f (host-fn ::ensure-default)] (f)))
    (-select-daemon-for-ling [_ ling-id]
      (when-let [f (host-fn ::select-daemon)] (f ling-id)))
    (-bind-ling! [_ daemon-id slave-id]
      (when-let [f (host-fn ::bind-ling)] (f daemon-id slave-id)))
    (-unbind-ling! [_ daemon-id slave-id]
      (when-let [f (host-fn ::unbind-ling)] (f daemon-id slave-id)))
    (-get-daemon-for-ling [_ slave-id]
      (when-let [f (host-fn ::daemon-for-ling)] (f slave-id)))
    (-default-daemon-id [_]
      (when-let [f (host-fn ::default-daemon-id)] (f)))))

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
  "Install IMPL as the Emacs host. Returns IMPL."
  [impl]
  (reset! installed impl)
  (reset-cache!)
  impl)

(defn uninstall!
  "Drop the installed host; calls fall back to the late-bound namespaces, then
   to `noop`. Returns nil."
  []
  (reset! installed nil)
  (reset-cache!)
  nil)

(defn current
  "The Emacs host calls are routed to right now."
  []
  (or @installed host-adapter))

;; ---------------------------------------------------------------------------
;; What the kernel calls
;; ---------------------------------------------------------------------------

(defn eval-elisp
  "Evaluate CODE in the host Emacs. Never throws for want of a host."
  [code]
  (-eval-elisp (current) code))

(defn eval-elisp-with-timeout
  "Evaluate CODE, giving up after TIMEOUT-MS."
  [code timeout-ms]
  (-eval-elisp-with-timeout (current) code timeout-ms))

(defn emacs-running?
  "True when a host Emacs is reachable."
  []
  (-emacs-running? (current)))

(defn ensure-default-daemon! [] (-ensure-default-daemon! (current)))
(defn select-daemon-for-ling [ling-id] (-select-daemon-for-ling (current) ling-id))
(defn bind-ling! [daemon-id slave-id] (-bind-ling! (current) daemon-id slave-id))

(defn unbind-ling! [daemon-id slave-id] (-unbind-ling! (current) daemon-id slave-id))
(defn get-daemon-for-ling [slave-id] (-get-daemon-for-ling (current) slave-id))
(defn default-daemon-id [] (-default-daemon-id (current)))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.spi.emacs
  "What the KERNEL may ask of an Emacs host: place a ling on a daemon.

   HIVE-KERNEL step E3. Emacs is leaving hive-mcp entirely (decision
   20260919004258-69303a5c), so no kernel namespace may require
   `hive-mcp.emacs-ext.*`. `swarm.sync` (which daemon a ling binds to) did.

   The eval half of this port (IElispEval) was removed once the doctor moved
   to the :editor/feature? vessel op and the tools.core facade was deleted:
   it had no callers left.

   IEmacsDaemons answers nil when no Emacs is present, which is what the spawn
   path already treats as \"no daemon placement\", so a ling still spawns.

   Host-local, like the other kernel ports (memory 20260919203856-2bb9451d).
   With nothing installed it late-binds BY SYMBOL through
   `hive-mcp.extensions.soft` for as long as `emacs-ext` ships in core,
   and answers the Noop once it does not."
  (:require [hive-mcp.extensions.soft :as soft]))

(defprotocol IEmacsDaemons
  "Placing a ling on an Emacs daemon."
  (-ensure-default-daemon! [this])
  (-select-daemon-for-ling [this ling-id]
    "{:daemon-id ... :reason ...} for LING-ID, or nil when there is no host.")
  (-bind-ling! [this daemon-id slave-id])
  (-unbind-ling! [this daemon-id slave-id])
  (-get-daemon-for-ling [this slave-id])
  (-default-daemon-id [this]))

(def noop
  "The answer with no Emacs: every daemon question answers nil."
  (reify
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
  {::ensure-default      'hive-mcp.emacs-ext.daemon-store/ensure-default-daemon!
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

(defn ensure-default-daemon! [] (-ensure-default-daemon! (current)))
(defn select-daemon-for-ling [ling-id] (-select-daemon-for-ling (current) ling-id))
(defn bind-ling! [daemon-id slave-id] (-bind-ling! (current) daemon-id slave-id))

(defn unbind-ling! [daemon-id slave-id] (-unbind-ling! (current) daemon-id slave-id))
(defn get-daemon-for-ling [slave-id] (-get-daemon-for-ling (current) slave-id))
(defn default-daemon-id [] (-default-daemon-id (current)))

(ns hive-mcp.emacs-ext.daemon-store
  "Extension-registry façade for `:emacs/daemon-store-*` keys.

   Replaces the legacy `hive-mcp.emacs.daemon-store` delegation shim.
   Used by `hive-mcp.swarm.sync` for ling↔daemon binding bookkeeping.
   When hive-emacs is not loaded, getters return nil and effecting fns
   are no-ops — swarm sync degrades gracefully (no daemon affinity).

   Decisions: 20260429195812-0c5dfe8d, 20260429230453-7e7627cc (Phase 2)."
  (:require [hive-mcp.extensions.registry :as ext]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def DaemonId
  "An Emacs daemon id: its socket name (e.g. \"server\")."
  [:string {:min 1}])

(def Daemon
  "A daemon entity as hive-emacs's store projects it: namespaced
   `:emacs-daemon/*` attributes, `:lings` a set of bound slave ids."
  [:map
   [:emacs-daemon/id DaemonId]
   [:emacs-daemon/status {:optional true} :keyword]
   [:emacs-daemon/lings {:optional true} [:maybe [:set :string]]]])

(def Selection
  "The daemon choice for a ling spawn: hive-emacs answers the chosen id (nil
   when no daemon is eligible) and the keyword reason the selector gave; a
   lighter contributor may answer the bare daemon id."
  [:or
   [:map
    [:daemon-id [:maybe :string]]
    [:reason :keyword]]
   DaemonId])

(def TxReport
  "An effecting call's answer, forwarded unchanged: hive-emacs's store answers
   a transaction report (a map); a lighter contributor (the in-process test
   stub) answers the daemon id or true. nil when nothing was written (unknown
   daemon, already registered, or hive-emacs not loaded)."
  [:maybe [:or :map :boolean DaemonId]])

(defn ensure-default-daemon! []
  (when-let [f (ext/get-extension :emacs/daemon-store-ensure-default!)] (f)))

(m/=> ensure-default-daemon! [:=> [:cat] TxReport])

(defn select-daemon-for-ling [ling-id]
  (when-let [f (ext/get-extension :emacs/daemon-store-select-for-ling)]
    (f ling-id)))

;; The parameter is named ling-id, but the one caller (swarm.sync) passes the
;; selector's opts map {:project-id ...}; both reach the implementation as is.
(m/=> select-daemon-for-ling
      [:=> [:cat [:maybe [:or :string [:map [:project-id {:optional true} [:maybe :string]]]]]]
       [:maybe Selection]])

(defn bind-ling! [daemon-id slave-id]
  (when-let [f (ext/get-extension :emacs/daemon-store-bind-ling!)]
    (f daemon-id slave-id)))

(m/=> bind-ling! [:=> [:cat [:maybe :string] [:maybe :string]] TxReport])

(defn unbind-ling! [daemon-id slave-id]
  (when-let [f (ext/get-extension :emacs/daemon-store-unbind-ling!)]
    (f daemon-id slave-id)))

(m/=> unbind-ling! [:=> [:cat [:maybe :string] [:maybe :string]] TxReport])

(defn get-daemon-for-ling [slave-id]
  (when-let [f (ext/get-extension :emacs/daemon-store-get-for-ling)]
    (f slave-id)))

(m/=> get-daemon-for-ling [:=> [:cat [:maybe :string]] [:maybe [:or Daemon DaemonId]]])

(defn default-daemon-id []
  (when-let [f (ext/get-extension :emacs/daemon-store-default-id)] (f)))

(m/=> default-daemon-id [:=> [:cat] [:maybe DaemonId]])

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.events.contributions
  "Who registers event handlers and effects, when the kernel does not know them.

   The mechanism is `hive-mcp.spi.contributions`, shared with every other
   thing a domain gives the kernel. This namespace is the EVENTS view of it:
   the two kinds (`:handlers`, `:effects`), the manifest of domains still
   shipped inside core, and `declared-events`, which is what
   `events.handlers/expected-events` is built from.

   A domain contributes

       (contribute! :handlers :kanban
                    {:install! #'register-handlers!
                     :events   #{:kanban/done :kanban/sync}
                     :addon    :hive-kanban})

   from its addon's `initialize!`. Until it is an addon it is declared instead
   in `resources/hive-mcp/event-contributions.edn`, whose symbols are resolved
   at boot rather than required. An entry that does not resolve is ABSENT: the
   domain is not in this build, its events drop out of `declared-events`, and
   `verify-handlers!` stops demanding handlers nobody promised."
  (:require [hive-mcp.spi.contributions :as contrib]))

(def manifest-resource
  "Classpath resource naming the event domains still shipped inside core."
  "hive-mcp/event-contributions.edn")

(def kinds
  "What can be contributed here. :handlers register event handlers, :effects
   register effects and coeffects."
  #{:handlers :effects})

(defn contribute!
  "Register KEY as a contributor of KIND (:handlers or :effects). ENTRY is
   {:install! ifn, :events #{event-id ...}, :addon kw}; :install! is called
   with no arguments by `register-all!`. Returns KEY."
  [kind key entry]
  {:pre [(contains? kinds kind) (ifn? (:install! entry))]}
  (contrib/contribute! kind key (merge {:events #{}} entry)))

(defn forget!
  "Drop KEY from KIND. Returns nil."
  [kind key]
  (contrib/forget! kind key))

(defn contributed
  "Everything contributed for KIND, as {key entry}."
  [kind]
  (contrib/contributed kind))

(defn declared-events
  "Every event id the contributed HANDLERS say they register. The union is
   what the kernel may honestly expect to find in the registry."
  []
  (into #{} (mapcat :events) (vals (contributed :handlers))))

(defn read-manifest
  "The in-core manifest as data: {:handlers [entry ...] :effects [entry ...]}."
  []
  (contrib/read-manifest manifest-resource))

(defn load-manifest!
  "Contribute every in-core event domain the manifest names. Returns
   {kind {:contributed [key ...] :absent [key ...]}}."
  []
  (contrib/load-manifest! manifest-resource))

(defn register-all!
  "Call every contributed :install! for KIND. Returns
   {:ran [key ...] :failed {key throwable}}."
  [kind]
  (contrib/register-all! kind))

(defn reset!!
  "Forget every event contribution. For tests; the live boot path re-runs
   `load-manifest!` and the addons re-contribute at initialize!."
  []
  (run! contrib/reset-kind! kinds)
  nil)

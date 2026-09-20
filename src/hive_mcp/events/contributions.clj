;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.events.contributions
  "Who registers event handlers and effects, when the kernel does not know them.

   HIVE-KERNEL, the seam behind twelve census waivers. `events.handlers` and
   `events.effects` used to REQUIRE every domain module and call its
   `register-*!` in a fixed list: nine handler domains and three effect
   domains that all leave the kernel with an addon. A require is exactly what
   a kernel namespace may not hold, and the list is exactly what OCP says must
   not be edited to add a domain.

   So a domain CONTRIBUTES instead:

       (contribute! :handlers :kanban
                    {:register! #'register-handlers!
                     :events    #{:kanban/done :kanban/sync}
                     :addon     :hive-kanban})

   and the kernel runs whatever is contributed. Two things contribute:

   - an ADDON at `initialize!`, which is where every domain ends up;
   - `load-manifest!`, for a domain still SHIPPED INSIDE core. The manifest
     (`resources/hive-mcp/event-contributions.edn`) names each such domain and
     its register fn, and it is resolved BY SYMBOL through
     `hive-mcp.swarm.adapters.soft`, so it is a declaration rather than a
     require. Every entry leaves that file the day its addon contributes for
     itself; when the file is empty the kernel carries no domain events at all.

   A missing namespace is not an error here. It means the domain is not in
   this build, which is the normal state after extraction: the entry is
   skipped and reported, its events drop out of `declared-events`, and
   `events.handlers/verify-handlers!` stops demanding handlers nobody promised."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [hive-mcp.swarm.adapters.soft :as soft]
            [taoensso.timbre :as log]))

(def manifest-resource
  "Classpath resource naming the domains still shipped inside core."
  "hive-mcp/event-contributions.edn")

(def kinds
  "What can be contributed. :handlers register event handlers, :effects
   register effects and coeffects."
  #{:handlers :effects})

(defonce ^:private registry
  ^{:doc "{kind -> {key -> {:register! ifn :events set :addon kw :source kw}}}"}
  (atom {}))

(defn contribute!
  "Register KEY as a contributor of KIND. ENTRY is
   {:register! ifn, :events #{event-id ...}, :addon kw}; :register! is the only
   required key, and it is called with no arguments by `register-all!`.
   Re-contributing the same key replaces it, which is what a re-mount should
   do. Returns KEY."
  [kind key entry]
  {:pre [(contains? kinds kind) (some? key) (ifn? (:register! entry))]}
  (swap! registry assoc-in [kind key] (merge {:events #{} :source :addon} entry))
  key)

(defn forget!
  "Drop KEY from KIND. Returns nil. An addon calls this at `shutdown!`; a test
   calls it to undo its own contribution."
  [kind key]
  (swap! registry update kind dissoc key)
  nil)

(defn contributed
  "Everything contributed for KIND, as {key entry}."
  [kind]
  (get @registry kind {}))

(defn declared-events
  "Every event id the contributed HANDLERS say they register. The union is
   what the kernel may honestly expect to find in the registry."
  []
  (into #{} (mapcat :events) (vals (contributed :handlers))))

(defn- load-entry!
  "Resolve one manifest entry and contribute it. Returns [key :contributed] or
   [key :absent]: an unresolvable symbol means the domain left with its addon."
  [kind {:keys [key register events addon]}]
  (if-let [f (soft/resolve-soft register)]
    (do (contribute! kind key {:register! f
                               :events    (set events)
                               :addon     addon
                               :source    :manifest})
        [key :contributed])
    (do (log/debug "event contribution absent, skipping" {:kind kind :key key :register register})
        [key :absent])))

(defn read-manifest
  "The manifest as data: {:handlers [entry ...] :effects [entry ...]}.
   An absent resource reads as an empty manifest, which is the shape the
   kernel ends at once every domain has become an addon."
  []
  (if-let [r (io/resource manifest-resource)]
    (edn/read-string (slurp r))
    {}))

(defn load-manifest!
  "Contribute every in-core domain the manifest names. Returns
   {:contributed [key ...] :absent [key ...]} per kind."
  []
  (let [manifest (read-manifest)]
    (into {}
          (for [kind (keys manifest)
                :when (contains? kinds kind)]
            (let [outcomes (mapv #(load-entry! kind %) (get manifest kind))]
              [kind {:contributed (mapv first (filter #(= :contributed (second %)) outcomes))
                     :absent      (mapv first (filter #(= :absent (second %)) outcomes))}])))))

(defn register-all!
  "Call every contributed :register! for KIND. One contributor throwing does
   not stop the rest: the kernel's own registration must not be hostage to a
   domain module. Returns {:ran [key ...] :failed {key throwable}}."
  [kind]
  (reduce (fn [acc [key {:keys [register!]}]]
            (try
              (register!)
              (update acc :ran conj key)
              (catch Throwable t
                (log/error t "event contribution failed to register" {:kind kind :key key})
                (assoc-in acc [:failed key] t))))
          {:ran [] :failed {}}
          (contributed kind)))

(defn reset!!
  "Forget every contribution. For tests; the live boot path re-runs
   `load-manifest!` and the addons re-contribute at initialize!."
  []
  (reset! registry {})
  nil)

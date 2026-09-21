;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.spi.contributions
  "One registry for everything a DOMAIN gives the kernel that the kernel may
   not require: event handlers, effects, tool roots, and whatever the next
   extraction needs.

   The shape, stated once:

       (contribute! :tools :kanban {:install! #'load-and-register :addon :hive-kanban})
       (register-all! :tools)

   and the kernel runs what was contributed rather than a list it holds. Two
   contributors, one mechanism:

   - an ADDON at `initialize!`, which is where every domain ends up;
   - a MANIFEST resource for domains still shipped inside core.
     `load-manifest!` resolves each entry's symbols through
     `hive-mcp.swarm.adapters.soft`, so the kernel declares an in-core domain
     without requiring it, and an entry that does not resolve is ABSENT rather
     than fatal: that is the normal state once the domain becomes an addon.

   A manifest entry is {:key kw, :install! sym, :addon kw} plus whatever the
   caller's kind needs; `load-manifest!` resolves every value whose key is
   listed in `resolve-keys` and keeps the rest verbatim. Kinds are open: the
   registry does not know what :tools means, only that something contributed
   under that word."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [hive-mcp.swarm.adapters.soft :as soft]
            [taoensso.timbre :as log]))

(defonce ^:private registry
  ^{:doc "{kind -> {key -> entry}}; entry carries at least :install! or the
          kind's own payload, plus :addon and :source."}
  (atom {}))

(defonce ^:private seq-counter (atom 0))

(defn contribute!
  "Register KEY as a contributor of KIND with ENTRY. Re-contributing the same
   key replaces it, which is what a re-mount should do, and KEEPS its original
   position: a domain that re-registers must not move in the order.

   Every entry carries a `:contrib/seq`, because the registry is a map and the
   things built from it are ORDERED (a tool array whose order wobbles between
   boots costs the caller its whole cached prompt prefix). Read it back through
   `ordered`. Returns KEY."
  [kind key entry]
  {:pre [(keyword? kind) (some? key) (map? entry)]}
  (let [existing (get-in @registry [kind key])
        position (or (:contrib/seq existing) (swap! seq-counter inc))]
    (swap! registry assoc-in [kind key]
           (merge {:source :addon} entry {:contrib/seq position})))
  key)

(defn forget!
  "Drop KEY from KIND. Returns nil. An addon calls this at `shutdown!`."
  [kind key]
  (swap! registry update kind dissoc key)
  nil)

(defn contributed
  "Everything contributed for KIND, as {key entry}. Unordered; use `ordered`
   when the result feeds something whose order is observable."
  [kind]
  (get @registry kind {}))

(defn ordered
  "Contributions of KIND as [[key entry] ...] in CONTRIBUTION order: manifest
   order first (the manifest is read top to bottom), then addons in mount
   order."
  [kind]
  (sort-by (comp :contrib/seq val) (contributed kind)))

(defn entry
  "The entry contributed under [KIND KEY], or nil."
  [kind key]
  (get-in @registry [kind key]))

(defn reset-kind!
  "Forget every contribution of KIND. Returns nil."
  [kind]
  (swap! registry dissoc kind)
  nil)

(defn reset-all!
  "Forget every contribution of every kind. For tests."
  []
  (reset! registry {})
  nil)

;; ---------------------------------------------------------------------------
;; Manifests
;; ---------------------------------------------------------------------------

(defn read-manifest
  "RESOURCE parsed as EDN: {kind [entry ...]}. An absent resource reads as an
   empty manifest, which is the shape the kernel ends at once every domain has
   become an addon."
  [resource]
  (if-let [r (io/resource resource)]
    (edn/read-string (slurp r))
    {}))

(defn- resolve-entry
  "Resolve the symbol-valued keys of ENTRY named in RESOLVE-KEYS. Returns the
   entry with those keys resolved, or nil when a REQUIRED one does not
   resolve (the domain is not in this build)."
  [entry resolve-keys required-key]
  (reduce (fn [acc k]
            (if-let [sym (get entry k)]
              (if-let [v (soft/resolve-soft sym)]
                (assoc acc k v)
                (if (= k required-key)
                  (reduced nil)
                  (dissoc acc k)))
              acc))
          entry
          resolve-keys))

(defn load-manifest!
  "Contribute every entry RESOURCE declares.

   OPTS:
     :resolve-keys  entry keys whose value is a SYMBOL to resolve (default
                    #{:install!})
     :required-key  the key that must resolve for the entry to count as
                    present (default :install!)

   Returns {kind {:contributed [key ...] :absent [key ...]}}."
  ([resource] (load-manifest! resource nil))
  ([resource {:keys [resolve-keys required-key]
              :or   {resolve-keys #{:install!} required-key :install!}}]
   (let [manifest (read-manifest resource)]
     (into {}
           (for [[kind entries] manifest]
             [kind
              (reduce (fn [acc {:keys [key] :as e}]
                        (if-let [resolved (resolve-entry e resolve-keys required-key)]
                          (do (contribute! kind key (assoc resolved :source :manifest))
                              (update acc :contributed conj key))
                          (do (log/debug "contribution absent, skipping"
                                         {:kind kind :key key})
                              (update acc :absent conj key))))
                      {:contributed [] :absent []}
                      entries)])))))

(defn register-all!
  "Call every contributed :install! for KIND, in contribution order. One
   contributor throwing does not stop the rest: the kernel's own wiring must
   not be hostage to a domain module. Returns
   {:ran [key ...] :failed {key throwable}}."
  [kind]
  (reduce (fn [acc [key {:keys [install!]}]]
            (if-not (ifn? install!)
              acc
              (try
                (install!)
                (update acc :ran conj key)
                (catch Throwable t
                  (log/error t "contribution failed to install" {:kind kind :key key})
                  (assoc-in acc [:failed key] t)))))
          {:ran [] :failed {}}
          (ordered kind)))

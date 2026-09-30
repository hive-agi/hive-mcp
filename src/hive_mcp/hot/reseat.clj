(ns hive-mcp.hot.reseat
  "Re-seating: the post-reload repair for namespaces whose live INSTANCES a
   namespace reload cannot reach.

   A reload redefines a record's class. Every instance built before it keeps
   the OLD class, and whatever holds such an instance (a manager, a registry,
   a system map) keeps answering with it. Only the namespace that defined the
   record knows how to rebuild the instance and where it is held, so the
   repair is an OPEN registry keyed by that namespace (OCP): a namespace that
   holds live record instances registers its re-seater; the reloader never
   names one.

   Value objects:
     Reseater     — (fn [loaded] -> any), loaded = the namespace names the
                    pass loaded, as strings.
     ReseatPlan   — [[ns-sym Reseater] ...] in load order.
     ReseatReport — [{:ns str :result any} | {:ns str :error str} ...].

   Collect: the registry. Promote: `reseat-plan` (pure). Boundary:
   `run-plan!` and `reseat!`.

   `via-var` is the seam every wiring that must survive a reload goes
   through: clj-reload REMOVES a namespace before loading it again, so even a
   registered var object goes stale; a symbol resolved per call does not."
  (:require [hive-dsl.result :as r]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Domain
;; =============================================================================

(def Reseater
  "What a namespace registers: called with the loaded namespace names."
  [:fn {:error/message "a re-seater must be invocable"} ifn?])

(def ReseatPlan
  [:vector [:tuple :symbol Reseater]])

(def ReseatReport
  [:vector [:or
            [:map [:ns :string] [:result :any]]
            [:map [:ns :string] [:error :string]]]])

;; =============================================================================
;; Collect
;; =============================================================================

(defonce ^:private registry (atom {}))

(defn register-reseater!
  "Register `f` as the re-seater of `ns-sym`. Keyed, so a namespace that
   registers on load replaces its previous entry on every reload."
  [ns-sym f]
  {:pre [(symbol? ns-sym) (m/validate Reseater f)]}
  (swap! registry assoc ns-sym f)
  ns-sym)

(defn unregister-reseater! [ns-sym]
  (swap! registry dissoc ns-sym)
  nil)

(defn registered
  "The registry now: {ns-sym Reseater}."
  []
  @registry)

;; =============================================================================
;; Promote
;; =============================================================================

(defn reseat-plan
  "The re-seaters a pass that loaded `loaded` must run, in load order, each
   once. A namespace that was not loaded holds no stale instance to repair."
  [registry loaded]
  (into []
        (comp (map symbol)
              (distinct)
              (keep (fn [n] (when-let [f (get registry n)] [n f]))))
        loaded))

;; =============================================================================
;; Boundary
;; =============================================================================

(defn run-plan!
  "Run every re-seater of `plan` with `loaded`. A throw is folded into that
   entry's report and never stops the ones after it."
  [plan loaded]
  (mapv (fn [[n f]]
          (let [res (r/try-effect (f loaded))]
            (if (r/err? res)
              {:ns (str n) :error (str (:message res))}
              {:ns (str n) :result (:ok res)})))
        plan))

(defn reseat!
  "Run the registered re-seaters for the namespaces a pass loaded."
  [loaded]
  (run-plan! (reseat-plan (registered) loaded) (vec loaded)))

(defn via-var
  "A fn that calls the var `qsym` names, resolving it on EVERY call. Unlike a
   fn value or a captured var object, it follows a reload that replaced the
   var, including one that removed and re-created the namespace."
  [qsym]
  {:pre [(qualified-symbol? qsym)]}
  (fn via-var-call [& args]
    (if-let [v (resolve qsym)]
      (apply v args)
      (throw (ex-info (str "via-var: " qsym " is not loaded") {:var qsym})))))

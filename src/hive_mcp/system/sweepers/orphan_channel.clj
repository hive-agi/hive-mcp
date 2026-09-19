(ns hive-mcp.system.sweepers.orphan-channel
  "ISweepable impl that detects IResourceOwner entries whose underlying
   ling process is dead and releases their resources.

   Walks `hive-mcp.system.registry/registered-resource-owners`, asks each owner
   for its `owner-id`, then consults `hive-mcp.agent.ling.spawn/find-ling` (via
   requiring-resolve) to decide liveness:

     - find-ling returns nil          → owner is orphaned → release-all! + unregister
     - find-ling not on classpath     → treat every owner as alive (conservative;
                                        avoids false orphaning when hive-agent
                                        or its spawn ns hasn't loaded yet)
     - release-all! or unregister     → errors logged, sweep continues

   Auto-registers with hive-mcp.system.registry on ns load."
  ;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
  ;;
  ;; SPDX-License-Identifier: AGPL-3.0-or-later
  (:require [hive-mcp.protocols.lifecycle :as lifecycle]
            [hive-mcp.system.registry :as reg]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Liveness probe
;; =============================================================================

(defn- resolve-find-ling
  "Return hive-mcp.agent.ling.spawn/find-ling if resolvable, else nil.
   Caller treats nil as 'assume alive' — see ns docstring."
  []
  (try (requiring-resolve 'hive-mcp.agent.ling.spawn/find-ling)
       (catch Throwable _ nil)))

(defn- orphan?
  "Return true iff `id` fails to resolve through find-ling.
   When find-ling itself is nil (spawn ns not loaded) return false so no
   owner is ever falsely orphaned.

   Takes the ID rather than the owner: the id is asked for ONCE per sweep, by
   `owner-id*`, and carried from there. Asking twice let the two calls disagree
   about whether the owner even has one."
  [find-ling id]
  (if (nil? find-ling)
    false
    (try
      (not (boolean (find-ling id)))
      (catch Throwable t
        (log/warn t "orphan-channel sweep: find-ling threw; assuming alive"
                  {:owner-id id})
        false))))

(defn- owner-id*
  "The owner's id, or nil when asking for it THREW.

   Asked once per sweep and carried with the owner, because the id is the key
   `unregister-resource-owner!` removes by. Releasing an owner's resources and
   then unregistering `nil` removes nothing: the owner stays registered, the
   next sweep finds the same dead owner, and it is released again every five
   minutes forever, with every log line naming `:owner-id nil` so the loop is
   not even attributable.

   So a failure here is a CONDITION the caller must branch on, not a value to
   substitute (20260829145644-2dc45bb1). It is logged rather than swallowed,
   and the caller skips the owner instead of acting on a half-known one."
  [owner]
  (try
    (lifecycle/owner-id owner)
    (catch Throwable t
      (log/error t "orphan-channel sweep: owner-id threw; owner left registered")
      nil)))

;; =============================================================================
;; ISweepable impl
;; =============================================================================

(defrecord OrphanChannelSweep [find-ling-fn]
  lifecycle/ISweepable
  (sweep-interval-s [_] 300)            ; 5 minutes
  (sweep-name [_] "channels/orphan")
  (sweep! [_ _ctx]
    ;; Three passes, deliberately separate: IDENTIFY every owner, DECIDE which
    ;; are orphaned, then ACT. The identify pass is what makes the decision
    ;; total -- an owner whose id cannot be read is neither released nor
    ;; counted as swept, because releasing it would strand it registered.
    ;;
    ;; `find-ling-fn` is a constructor-injected seam, nil in production so the
    ;; probe is resolved per sweep as before. It exists because a test needs to
    ;; choose who is alive, and the alternative is `with-redefs` standing in
    ;; for a collaborator the record could simply hold (20260726172005-562432b9).
    (let [find-ling  (or find-ling-fn (resolve-find-ling))
          errors     (volatile! [])
          identified (mapv (juxt identity owner-id*)
                           (reg/registered-resource-owners))
          unknown    (filterv (comp nil? second) identified)
          orphaned   (filterv (fn [[_ id]] (orphan? find-ling id))
                              (filterv (comp some? second) identified))]
      (doseq [_ unknown]
        (vswap! errors conj {:owner-id nil
                             :error    "owner-id threw; owner left registered"}))
      (doseq [[o id] orphaned]
        (try
          (lifecycle/release-all! o)
          (reg/unregister-resource-owner! id)
          (catch Throwable t
            (log/error t "orphan-channel sweep: release failed"
                       {:owner-id id})
            (vswap! errors conj {:owner-id id
                                 :error    (.getMessage t)}))))
      {:swept (count orphaned) :errors @errors})))

;; =============================================================================
;; Auto-registration
;; =============================================================================

(def ^:private -registered?
  "Auto-registration. `def`, deliberately NOT `defonce`.

   `registry/register-sweep!` is keyed by sweep-name and documents that
   re-registering the same name silently overwrites, so repeated loads cannot
   produce a duplicate. `defonce` bought nothing against that and cost the one
   thing that matters: it never fires again, so a hot reload loads new sweep
   code and leaves the OLD record in the registry, and anything that
   unregisters this sweep unregisters it permanently -- no reload, not even
   `:reload`, can put it back (kanban 20260916134011-1246379c)."
  (do (reg/register-sweep! (->OrphanChannelSweep nil)) true))

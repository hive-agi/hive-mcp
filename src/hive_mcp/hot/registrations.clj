(ns hive-mcp.hot.registrations
  "Re-point the hive.events registries at the code a reload just loaded.

   A registration captures a function VALUE: `(reg-fx :shout handle-shout)`
   stores the fn object, not the var. hive-mcp's registrars run from `init`,
   not at namespace load, and several are gated by a defonce'd flag that
   clj-reload preserves, so after a core reload nothing re-runs them and the
   fx / cofx / event registries keep invoking the pre-reload closures.
   hive-hot reports exactly these as :stale-registrations.

   `rebind-stale!` closes that gap without knowing who registered what:
   hive.events.staleness names every entry whose fn was compiled from a var
   that now holds a different value, and each such entry is replaced, by
   key, with that var's current root. The replacement is compare-and-set per
   entry: an entry somebody re-registered in the meantime is left as it is.
   Anonymous handlers have no var and are out of scope here, as they are for
   the scan; a handler registered AS a var never goes stale.

   Soft-resolved: an older hive-events without the staleness scan answers
   nil and the reload report falls back to what hive-hot computed."
  (:require [hive.events.cofx :as cofx]
            [hive.events.fx :as fx]
            [hive.events.router :as router]
            [taoensso.timbre :as log]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- soft [sym] (try (requiring-resolve sym) (catch Throwable _ nil)))

(def ^:private registry-atoms
  "The registry atoms hive.events owns, by the name the staleness scan reports
   them under. Private defonces, read through find-var so the swap can be
   compare-and-set; nil when a hive-events release renames one."
  {:fx    'hive.events.fx/fx-registry
   :cofx  'hive.events.cofx/cofx-registry
   :event 'hive.events.router/event-registry})

(defn- registry-atom [registry]
  (when-let [sym (registry-atoms registry)]
    (some-> (find-var sym) deref)))

;; =============================================================================
;; Value schemas
;; =============================================================================

(def Handler
  "A registered handler fn."
  [:fn {:error/message "a fn" :gen/elements [inc dec identity]} fn?])

(def Entry
  "A registry entry: a bare handler fn, or a map holding one under :handler."
  [:or Handler [:map [:handler Handler]]])

(def RegistryKind
  "Which hive.events registry. An unknown one is answered nil/false, never
   refused."
  [:keyword {:gen/elements [:fx :cofx :event :other]}])

(def RegistrationRow
  [:map {:closed true} [:registry :keyword] [:id :any] [:owner :any]])

(defn fresh-entry
  "The entry `entry` becomes once its handler `stale` is replaced by `fresh`,
   or nil when `entry` no longer holds `stale` (somebody re-registered it;
   leave it alone). Pure."
  [entry stale fresh]
  (cond
    (identical? entry stale)                               fresh
    (and (map? entry) (identical? (:handler entry) stale)) (assoc entry :handler fresh)
    :else                                                  nil))

(m/=> fresh-entry [:=> [:cat :any :any Handler] [:maybe Entry]])

(defn- registry-fn [entry]
  (cond (var? entry)                              nil
        (fn? entry)                               entry
        (and (map? entry) (fn? (:handler entry))) (:handler entry)
        :else                                     nil))

(defn- current-entry [registry id]
  (case registry
    :fx    (fx/get-fx id)
    :cofx  (cofx/get-cofx id)
    :event (router/get-event id)
    nil))

(defn- write-entry!
  "Replace `id` in `registry` with `entry` through the public API. Used only
   when the registry atom cannot be found; unreg first so the replacement is
   not logged as an overwrite."
  [registry id entry]
  (case registry
    :fx    (do (fx/unreg-fx id) (fx/reg-fx id entry))
    :cofx  (do (cofx/unreg-cofx id) (cofx/reg-cofx id entry))
    :event (router/reg-event-fx id (:interceptors entry) (:handler entry))
    nil))

(defn rebind-entry!
  "Point registry entry `id` at `fresh` if it still holds `stale`. Answers
   true when it was replaced."
  [registry id stale fresh]
  (if-let [a (registry-atom registry)]
    (let [done? (volatile! false)]
      (swap! a (fn [m]
                 (vreset! done? false)
                 (if-let [e (some-> (get m id) (fresh-entry stale fresh))]
                   (do (vreset! done? true) (assoc m id e))
                   m)))
      @done?)
    (if-let [e (some-> (current-entry registry id) (fresh-entry stale fresh))]
      (do (write-entry! registry id e) true)
      false)))

(m/=> rebind-entry!
      [:=> [:cat RegistryKind [:any {:gen/elements [::not-registered "not-registered"]}] :any Handler] :boolean])

(defn- row [{:keys [registry id owner]}] {:registry registry :id id :owner owner})

(defn rebind-stale!
  "Rebind every registration owned by one of `namespaces` (symbols or strings)
   whose fn is no longer what its var holds. Answers
   {:rebound [{:registry :id :owner}] :remaining [{:registry :id :owner}]},
   :remaining being a rescan after the repair, or nil when hive-events has no
   staleness scan."
  [namespaces]
  (when-let [scan (soft 'hive.events.staleness/stale-entries)]
    (let [nses    (vec namespaces)
          stale   (if (seq nses) (scan {:namespaces nses}) [])
          rebound (into []
                        (keep (fn [{:keys [registry id var] :as e}]
                                (let [stale-f (registry-fn (current-entry registry id))]
                                  (when (and stale-f (rebind-entry! registry id stale-f @var))
                                    (row e)))))
                        stale)
          left    (if (seq nses) (mapv row (scan {:namespaces nses})) [])]
      (when (seq rebound)
        (log/info "core hot-reload: rebound stale registrations"
                  {:rebound (count rebound) :remaining (count left)}))
      {:rebound rebound :remaining left})))

(m/=> rebind-stale!
      [:=> [:cat [:sequential [:or :symbol :string]]]
       [:maybe [:map {:closed true}
                [:rebound [:vector RegistrationRow]]
                [:remaining [:vector RegistrationRow]]]]])

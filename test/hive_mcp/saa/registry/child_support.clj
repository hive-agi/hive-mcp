(ns hive-mcp.saa.registry.child-support
  "Shared generators, relation and lifecycle driver for the per-namespace
   facet suites of the five singleton SAA child registries (dispatch-modes,
   phase-providers, plan-stores, planners, scorers). Each suite passes in its
   own registry's vars, so the facets point at that namespace's contracted
   functions."
  (:require [hive-mcp.saa.types :as types]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Owner [:enum :owner/a :owner/b :saa/core])

(def Id
  "A few repeating ids reach :replaced / :conflict, a fresh uuid reaches :ok;
   a repeating id is drawn three times in four so :replaced clears its floor."
  [:or {:gen/schema [:tuple [:enum :id/x :id/y :saa/default] :uuid [:enum 0 1 2 3]]
        :gen/fmap (fn [[k u arm]] (if (zero? arm) u k))}
   [:enum :id/x :id/y :saa/default] :uuid])

(def RegisterArgs [:cat Owner Id [:map [:v :int]]])

(defn index-consistency-rel
  "Relation for register!: the owner index agrees with the outcome. A winning
   write (:ok, :replaced) leaves `id` in this owner's index set; a :conflict
   leaves it out (another owner holds the id). Either way the id is stored.

   Reads the snapshot, not all-ids: all-ids SORTS the ids and throws a
   ClassCastException once a keyword id and a uuid id are both registered
   (recorded as a defect in the f05 report), which these mixed ids reach."
  [snapshot]
  (fn [[owner id _] out]
    (let [data (:data (snapshot))
          owned (get-in data [:by-owner owner] #{})]
      (and (= (not= :conflict out) (contains? owned id))
           (contains? (:by-id data) id)))))

(defn deregister-rel
  "Relation for deregister-by-owner!: every removed id is gone from the
   registry and no remaining record belongs to `owner`. The fixture leaves
   the :saa/core seed in place, so the :saa/core draw removes real ids."
  [snapshot]
  (fn [[owner] removed]
    (let [by-id (:by-id (:data (snapshot)))]
      (and (not-any? #(contains? by-id %) removed)
           (not-any? #(= owner (:owner %)) (vals by-id))))))

(defn lifecycle
  "Drive one singleton registry from empty through its whole surface and
   answer a plain-data view of every step."
  [{:keys [register! lookup dereg ids snapshot restore! reset!]}]
  (let [reset-state (reset!)
        r1 (register! :owner/a :id/x {:v 1})
        r2 (register! :owner/a :id/x {:v 2})
        r3 (register! :owner/b :id/x {:v 3})
        r4 (register! :owner/b :id/y {:v 4})
        r5 (register! :owner/a :id/z {:v 5})
        snap (snapshot)
        held (mapv #(some-> (lookup %) (select-keys [:owner :v])) [:id/x :id/y :id/z :id/w])
        by-owner (into (sorted-map) (update-vals (get-in snap [:data :by-owner]) (comp vec sort)))
        removed (vec (sort (dereg :owner/a)))
        removed-again (dereg :owner/a)
        after-dereg (ids)
        restored (do (restore! snap) (ids))
        emptied (do (restore! nil) (ids))]
    {:reset reset-state :outcomes [r1 r2 r3 r4 r5] :held held :by-owner by-owner
     :removed removed :removed-again removed-again :after-dereg after-dereg
     :restored restored :emptied emptied
     :snapshot-valid (m/validate types/RegistrySnapshot snap)}))

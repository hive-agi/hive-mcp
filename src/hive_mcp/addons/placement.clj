(ns hive-mcp.addons.placement
  "Where an addon may run, declared as REASONS, never as a side.

   A manifest says why it is pinned, e.g.

     :addon/placement {:requires #{:display} :prefers :cluster}

   and a topology maps each reason to the side that can satisfy it. The
   word `local` outlives the topology it described; `needs a display` does
   not. Pure data and functions: nothing here routes, mounts or changes
   runtime behaviour (kanban 20260920154039-002c8d03, design memory
   20260920154005-6020b248)."
  (:require [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def reasons
  "The known placement reasons, each with what it means. Open set: a topology
   may name a reason this map does not, and `resolve-side` refuses a reason the
   topology cannot place rather than guessing."
  {:display    "needs a running editor, terminal or screen the operator sees"
   :workspace  "reads or writes the operator's checkout and dotfiles"
   :gpu        "needs a GPU"
   :bulk-store "holds or scans a large durable store (vectors, KG, transcripts)"
   :egress     "needs outbound network the far side may not have"})

(def Side
  "A side of a split deployment."
  [:enum :local :cluster])

(def Reason
  "A placement reason keyword."
  :keyword)

(def Placement
  "An addon's placement declaration. :requires is the reason set (empty means
   it runs anywhere); :prefers breaks a tie when every side can host it."
  [:map {:closed true}
   [:requires [:set Reason]]
   [:prefers {:optional true} Side]])

(def Topology
  "Reason -> the side that satisfies it, plus the :default side an addon with
   no requirement and no preference lands on."
  [:map
   [:default Side]
   [:reasons [:map-of Reason Side]]])

(def workstation
  "One process on the operator's machine: every reason is satisfied locally."
  {:default :local
   :reasons (zipmap (keys reasons) (repeat :local))})

(def split
  "The split hive of design 20260920154005-6020b248: the operator's process
   holds what needs the operator's machine; the cluster holds the heavy parts."
  {:default :cluster
   :reasons {:display    :local
             :workspace  :local
             :gpu        :cluster
             :bulk-store :cluster
             :egress     :cluster}})

(def Resolution
  "`resolve-side` answer: the side, or a refusal naming why."
  [:or
   [:map [:side Side] [:why [:enum :required :preferred :default]]]
   [:map [:error [:enum :unknown-reason :conflicting-reasons]]
    [:reasons [:set Reason]]]])

(defn resolve-side
  "The side TOPOLOGY places PLACEMENT on.

   Every required reason must map to a side, and they must agree; otherwise
   the answer is an :error naming the offending reasons, never a guess. With
   no requirement, :prefers wins, then the topology's :default."
  [topology {:keys [requires prefers]}]
  (let [table   (:reasons topology)
        unknown (into #{} (remove #(contains? table %)) requires)
        sides   (into #{} (map table) requires)]
    (cond
      (seq unknown)       {:error :unknown-reason :reasons unknown}
      (> (count sides) 1) {:error :conflicting-reasons :reasons (set requires)}
      (seq sides)         {:side (first sides) :why :required}
      prefers             {:side prefers :why :preferred}
      :else               {:side (:default topology) :why :default})))

(defn declared-placement
  "MANIFEST's :addon/placement when it is a valid declaration, else nil."
  [manifest]
  (let [p (:addon/placement manifest)]
    (when (m/validate Placement p) p)))

(def Gap
  "One manifest that fails the placement gate."
  [:map [:addon/id :string] [:gap [:enum :undeclared :invalid]]])

(defn placement-gaps
  "Every manifest in MANIFESTS (the universe the caller drew from disk) whose
   placement is missing (:undeclared) or malformed (:invalid), sorted by id."
  [manifests]
  (->> manifests
       (keep (fn [mf]
               (cond
                 (not (contains? mf :addon/placement)) {:addon/id (:addon/id mf) :gap :undeclared}
                 (nil? (declared-placement mf))        {:addon/id (:addon/id mf) :gap :invalid})))
       (sort-by :addon/id)
       vec))

(defn unwaived-gaps
  "GAPS whose addon id WAIVED (a set of ids) does not cover. An :invalid
   declaration is never waived: a waiver is for an addon that has not declared
   yet, not for one that declared wrong."
  [waived gaps]
  (filterv #(or (= :invalid (:gap %)) (not (contains? waived (:addon/id %)))) gaps))

(defn stale-waivers
  "Waived ids among MANIFESTS that now declare a valid placement: the waiver
   must be dropped, so the list only shrinks."
  [waived manifests]
  (->> manifests
       (filter #(and (contains? waived (:addon/id %)) (declared-placement %)))
       (map :addon/id)
       sort
       vec))

(m/=> resolve-side [:=> [:cat Topology Placement] Resolution])
(m/=> declared-placement [:=> [:cat [:maybe :map]] [:maybe Placement]])
(m/=> placement-gaps [:=> [:cat [:sequential :map]] [:vector Gap]])
(m/=> unwaived-gaps [:=> [:cat [:set :string] [:vector Gap]] [:vector Gap]])
(m/=> stale-waivers [:=> [:cat [:set :string] [:sequential :map]] [:vector :string]])

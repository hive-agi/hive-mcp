(ns hive-mcp.tools.memory.crud.edge-outcome
  "Outcome of the KG edge step of a memory add: the edge ids written, or one
   error row per requested edge when the edge write failed."
  (:require [taoensso.timbre :as log]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn edge-requests
  "The edges `kg-params` asks for, as [{:relation <name> :to <target-id>}] in
   creation order (implements, supersedes, depends-on, refines)."
  [{:keys [kg_implements kg_supersedes kg_depends_on kg_refines]}]
  (into []
        (mapcat (fn [[relation targets]]
                  (map (fn [target] {:relation relation :to target}) targets)))
        [["implements" kg_implements]
         ["supersedes" kg_supersedes]
         ["depends-on" kg_depends_on]
         ["refines"    kg_refines]]))

(defn- error-message
  [^Throwable t]
  (or (.getMessage t) (.getName (class t))))

(defn attempt-edges
  "Run `create!` (no args, returns the written edge ids) for `requests`.

   Returns {:edge-ids [ids] :edge-errors []} when create! returns, or
   {:edge-ids [] :edge-errors [{:relation :to :error}]} with one row per
   request when it throws (each row also carries :from when `from` is given).
   Never throws an Exception."
  ([create! requests] (attempt-edges create! requests nil))
  ([create! requests from]
   (try
     {:edge-ids (vec (create!)) :edge-errors []}
     (catch Exception e
       (log/error e "KG edge creation failed; entry kept without edges")
       {:edge-ids    []
        :edge-errors (mapv #(cond-> (assoc % :error (error-message e))
                              from (assoc :from from))
                           requests)}))))

(defn- edge-row
  [from {:keys [relation to]}]
  {:from from :to to :relation relation})

(defn outcome-check
  "How a caller learns the final outcome of edges a memory add could not
   confirm. nil when nothing is unconfirmed. `edge-errors` are edge writes that
   failed or outran their stage budget (they may still land);
   `link-pending` is the :kg-outgoing link of edges that exist.

   :unconfirmed_edges names EXACTLY which edges are unconfirmed, each
   {:from :to :relation}: the failed edge writes, plus, when the link is
   pending, every requested edge (`requests`; the link covers them all).
   :relink is the verb that re-creates one."
  ([entry-id edge-errors link-pending]
   (outcome-check entry-id edge-errors link-pending nil))
  ([entry-id edge-errors link-pending requests]
   (when (or (seq edge-errors) link-pending)
     (let [unconfirmed (into [] (distinct)
                             (map #(edge-row entry-id %)
                                  (concat edge-errors (when link-pending requests))))]
       {:status  "unconfirmed"
        :how     (str "memory get id=" entry-id " include_kg=true"
                      " (kg_outgoing lists the edges that landed), or"
                      " kg traverse start_node=" entry-id " direction=outgoing")
        :targets (vec (distinct (map :to edge-errors)))
        :unconfirmed_edges unconfirmed
        :relink  (str "kg edge from=" entry-id " to=<to> relation=<relation>"
                      " for each unconfirmed_edges row that is not in kg_outgoing")}))))

;;; =============================================================================
;;; Contracts
;;; =============================================================================

(def Relation
  "The four relations a memory add can request, in creation order."
  [:enum "implements" "supersedes" "depends-on" "refines"])

(def EdgeRequest
  [:map [:relation Relation] [:to :any]])

(def KgParams
  "The kg_* params of a memory add: each a seq of target entry ids, or nil."
  [:maybe
   [:map
    [:kg_implements {:optional true} [:maybe [:sequential :any]]]
    [:kg_supersedes {:optional true} [:maybe [:sequential :any]]]
    [:kg_depends_on {:optional true} [:maybe [:sequential :any]]]
    [:kg_refines {:optional true} [:maybe [:sequential :any]]]]])

(def EdgeOutcome
  "Edge ids written, or one error row per request when the write threw."
  [:map
   [:edge-ids [:vector :any]]
   [:edge-errors [:vector [:map [:relation Relation] [:to :any] [:error :string]
                           [:from {:optional true} :any]]]]])

(def ^:private OutcomeCheck
  [:maybe [:map [:status :string] [:how :string] [:targets [:vector :any]]
           [:unconfirmed_edges [:vector [:map [:from :any] [:to :any] [:relation :any]]]]
           [:relink :string]]])

(m/=> edge-requests [:=> [:cat KgParams] [:vector EdgeRequest]])

(m/=> attempt-edges [:function
                     [:=> [:cat ifn? [:sequential EdgeRequest]] EdgeOutcome]
                     [:=> [:cat ifn? [:sequential EdgeRequest] :any] EdgeOutcome]])

(m/=> outcome-check
  [:function
   [:=> [:cat :string [:maybe [:sequential :map]] [:maybe :map]] OutcomeCheck]
   [:=> [:cat :string [:maybe [:sequential :map]] [:maybe :map] [:maybe [:sequential :map]]]
    OutcomeCheck]])

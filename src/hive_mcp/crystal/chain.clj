(ns hive-mcp.crystal.chain
  "Per-scope chain of wrap summaries: a newly persisted session summary gets a
   :supersedes KG edge to the previous summary of the same project.

     Collect   ISummaryChain/-prior-summaries   candidate summaries of a project
     Promote   previous-summary                 pure choice of the predecessor
     Boundary  ISummaryChain/-link!             the edge write
     Pipeline  chain-wraps!                     one step per persisted wrap"
  (:require [clojure.string :as str]
            [hive-mcp.protocols.memory :as mem-proto]
            [malli.core :as m]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Domain
;; =============================================================================

(def summary-tag
  "Tag every persisted session summary carries."
  "session-summary")

(def Candidate
  "An entry as read back from the store: only these keys are consulted."
  [:map
   [:id {:optional true} [:maybe :string]]
   [:project-id {:optional true} [:maybe :string]]
   [:tags {:optional true} [:maybe [:sequential :string]]]])

(def ChainResult
  "One chained persist result. `:existing true` marks a predecessor that was
   already linked (a same-day fold keeps its id, so its edge is already there):
   it is reported, not written again."
  [:map
   [:id [:maybe :string]]
   [:project-id [:maybe :string]]
   [:supersedes [:maybe :string]]
   [:existing {:optional true} :boolean]
   [:skipped {:optional true} :boolean]
   [:error {:optional true} :string]])

(def ChainReport
  [:map
   [:linked [:int {:min 0}]]
   [:failed [:int {:min 0}]]
   [:results [:vector ChainResult]]])

;; =============================================================================
;; Port
;; =============================================================================

(defprotocol ISummaryChain
  (-prior-summaries [this project-id]
    "Session-summary entries stored for project-id, in any order.")
  (-link! [this from-id to-id project-id]
    "Write the :supersedes edge from-id -> to-id, at most once: answers
     `:existing` (and writes nothing) when that edge is already stored, else
     whatever the write returned. Throws on failure."))

;; =============================================================================
;; Promote (pure)
;; =============================================================================

(defn summary?
  "True when `entry` carries the session-summary tag."
  [entry]
  (boolean (some #{summary-tag} (:tags entry))))

(defn in-scope?
  "True when `entry` belongs to `project-id`, by its :project-id or by its
   `scope:project:<id>` tag."
  [project-id entry]
  (or (= project-id (:project-id entry))
      (boolean (some #{(str "scope:project:" project-id)} (:tags entry)))))

(defn previous-summary
  "Id of the summary `new-id` supersedes: the greatest id among `candidates`
   that is a session summary in `project-id` and sorts strictly before
   `new-id`. Entry ids are yyyyMMddHHmmss-prefixed, so id order is creation
   order. nil when there is none."
  [new-id project-id candidates]
  (reduce (fn [best {:keys [id] :as c}]
            (if (and (string? id)
                     (neg? (compare id new-id))
                     (summary? c)
                     (in-scope? project-id c)
                     (or (nil? best) (pos? (compare id best))))
              id
              best))
          nil
          candidates))

(m/=> previous-summary [:=> [:cat :string :string [:sequential Candidate]] [:maybe :string]])

;; =============================================================================
;; Pipeline
;; =============================================================================

(defn- chainable?
  [{:keys [id project-id success?]}]
  (and success? (string? id) (not (str/blank? id))
       (string? project-id) (not (str/blank? project-id))))

(defn chain-one!
  "Link one persist result to its predecessor through `chain`. Never throws:
   a failure is reported under :error.

   A same-day fold (`crystal.consolidate`) reports the EXISTING entry's id, so
   its predecessor is the one that entry already supersedes. `-link!` answers
   `:existing` for an edge already stored; the result then carries
   `:existing true` and no second edge is written."
  [chain {:keys [id project-id] :as result}]
  (let [base {:id id :project-id project-id :supersedes nil}]
    (if-not (chainable? result)
      (assoc base :skipped true)
      (try
        (if-let [prev (previous-summary id project-id (vec (-prior-summaries chain project-id)))]
          (let [written (-link! chain id prev project-id)]
            (cond-> (assoc base :supersedes prev)
              (= :existing written) (assoc :existing true)))
          base)
        (catch Throwable t
          (log/warn "wrap-chain: link failed for" id ":" (ex-message t))
          (assoc base :error (or (ex-message t) (str (class t)))))))))

(m/=> chain-one! [:=> [:cat :any :map] ChainResult])

(defn chain-wraps!
  "Link every successful result of a persist report (`hive-mcp.crystal.persist`
   PersistReport) to the previous summary of its project. Returns a
   ChainReport; never throws. `:linked` counts edges WRITTEN by this call: an
   edge that was already stored (a folded re-wrap) is not counted."
  [chain persist-report]
  (let [results (mapv (partial chain-one! chain) (:results persist-report))]
    {:linked  (count (filter #(and (:supersedes %) (not (:existing %))) results))
     :failed  (count (filter :error results))
     :results results}))

(m/=> chain-wraps! [:=> [:cat :any [:map [:results {:optional true} [:sequential :map]]]] ChainReport])

;; =============================================================================
;; Boundary: the store + KG adapter
;; =============================================================================

(def ^:private prior-limit 500)

(defrecord StoreChain [store add-edge! find-edge]
  ISummaryChain
  (-prior-summaries [_ project-id]
    (mem-proto/query-entries store {:type          "note"
                                    :tags          [summary-tag]
                                    :project-id    project-id
                                    :output-fields ["id" "project-id" "tags"]
                                    :order-by      [:created :desc]
                                    :limit         prior-limit}))
  (-link! [_ from-id to-id project-id]
    ;; The KG has no uniqueness on (from, to, relation): every add-edge! mints
    ;; a fresh edge. A same-day fold re-chains an entry whose edge is already
    ;; stored, so the write is guarded by a read. `find-edge` nil = no guard.
    (if (and find-edge (find-edge from-id to-id :supersedes))
      :existing
      (add-edge! {:from        from-id
                  :to          to-id
                  :relation    :supersedes
                  :scope       project-id
                  :created-by  "wrap-chain"
                  :source-type :automated}))))

(defn store-chain
  "The ISummaryChain over the registered `store-key` memory store and the KG
   edge writer (guarded by the KG edge reader, so a link is written once), or
   nil when the store or the writer is absent."
  ([] (store-chain :default))
  ([store-key]
   (when (mem-proto/store-set?)
     (when-let [add-edge! (try (requiring-resolve 'hive-mcp.knowledge-graph.edges.write/add-edge!)
                               (catch Throwable _kg-absent-means-no-chain nil))]
       (when-let [store (mem-proto/get-store store-key)]
         (->StoreChain store add-edge!
                       (try (requiring-resolve 'hive-mcp.knowledge-graph.edges.queries/find-edge)
                            (catch Throwable _no-reader-means-no-guard nil))))))))

;; Contracts for the predicates and the adapter factory. The crystal contract
;; gate (crystal.contract-coverage-test) requires every public fn here to carry
;; one; previous-summary, chain-one! and chain-wraps! carry theirs inline.
(m/=> summary? [:=> [:cat [:maybe Candidate]] :boolean])
(m/=> in-scope? [:=> [:cat [:maybe :string] [:maybe Candidate]] :boolean])
(m/=> store-chain [:function
                   [:=> [:cat] [:maybe [:fn #(satisfies? ISummaryChain %)]]]
                   [:=> [:cat :keyword] [:maybe [:fn #(satisfies? ISummaryChain %)]]]])

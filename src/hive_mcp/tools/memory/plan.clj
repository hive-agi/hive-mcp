(ns hive-mcp.tools.memory.plan
  "The nested `memory plan` subdomain — plan records as a first-class verb set.

   A `type=plan` entry is a plan record, and the MAIN memory store is its only
   authority. The dedicated plan collection (hive-mcp.plan.plans) is a vector
   INDEX over it: it answers ids fast and ranks them semantically, and nothing
   here ever renders an entry from index metadata alone. Every row goes through
   `facade/get-entry-by-id`, and an entry that is gone, or that is not a plan,
   is dropped — an index can lag a delete or carry a foreign id, and a verb
   that answered from it would report plans that no longer exist.

   Content follows the same rule in the other direction: `list`/`search`/
   `status` answer rows with a preview, and only `get` carries the content.
   Plans run to thousands of characters, so a listing that inlined them would
   spend the caller's context on plans it is only scanning.

   Status is a small machine (hive-mcp.plan.plans/transition-status), not a free
   field: `draft -> active -> completed`, plus anything -> `superseded`. A
   transition is judged BEFORE anything is written, and a refusal is answered
   as an MCP error with the machine's own message, so the caller learns the
   legal edges instead of getting a silently different plan back.

   Writes are mirrored onto the plans index so its `plan-status` metadata keeps
   matching the store. The mirror is best-effort by design: the facade write is
   authoritative and has already succeeded, so a failed index update must not
   fail the verb."
  (:require [clojure.string :as str]
            [hive-mcp.plan.plans :as plans]
            [hive-mcp.tools.core :as tcore :refer [mcp-error mcp-json]]
            [hive-mcp.vectordb.facade :as facade]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private default-limit 20)

(def ^:private max-limit
  "Ceiling on a caller-supplied :limit. The index answers metadata rows, but
   each surviving id still costs a store read to hydrate, so an uncapped limit
   turns one listing into hundreds of round trips."
  100)

(def ^:private preview-chars
  "Characters of content a non-`get` row carries."
  200)

(defn- plan-entry?
  "True when ENTRY is a plan. Stores answer :type as a string or a keyword."
  [entry]
  (= "plan" (some-> (:type entry) name)))

(defn- hydrate
  "The entry behind ID from the main store, or nil. nil covers three cases the
   verbs answer identically: no such id, an id whose entry was deleted, and an
   id that is not a plan — a `memory plan get` of a note has nothing to say."
  [id]
  (when-let [entry (facade/get-entry-by-id id)]
    (when (plan-entry? entry) entry)))

(defn- param
  "PARAMS' value for KEY, whichever of its two spellings the caller used. A
   snake_case `project_id` arrives from JSON and a kebab-case `:project-id`
   from Clojure callers; both name the same thing."
  [params snake kebab]
  (let [v (get params snake)]
    (if (some? v) v (get params kebab))))

(defn- preview [content]
  (when (string? content)
    (if (> (count content) preview-chars)
      (str (subs content 0 preview-chars) "…")
      content)))

(defn- row
  "One listing row: identity, status and the cheap metadata, with a preview in
   place of the content."
  [entry]
  (cond-> (select-keys entry [:id :type :tags :project-id :duration
                              :plan-status :steps-count :created :updated])
    (preview (:content entry)) (assoc :preview (preview (:content entry)))))

(defn- limit-of
  "LIMIT coerced to a positive int, defaulted and capped. A junk value falls
   back to the default rather than refusing the whole listing."
  [limit]
  (let [n (cond
            (integer? limit) limit
            (string? limit) (parse-long limit)
            :else nil)]
    (if (and n (pos? n))
      (min n max-limit)
      default-limit)))

(defn- project-scope
  "The project filter a call named, or nil for every project."
  [params]
  (let [pid (param params :project_id :project-id)]
    (when-not (str/blank? (str pid)) (str pid))))

(defn- requested-status [params]
  (param params :status :status))

(defn- invalid-status
  "A refusal for STATUS when it is not a plan status, else nil. A nil status is
   legal for the LISTING verbs (no filter)."
  [status]
  (when (and (some? status) (not (plans/plan-status? status)))
    (mcp-error (str "Invalid plan status: " (pr-str status)
                    ". Valid: " (str/join ", " plans/plan-statuses)))))

(defn- missing-id [params]
  (when (str/blank? (str (param params :id :id)))
    (mcp-error "Missing required param: id")))

(defn- apply-status!
  "Persist TO on entry ID: the store first (authoritative), then the index
   mirror. Returns the re-read entry with the new status, or the entry unchanged
   when it vanished between the read and the write."
  [entry to]
  (facade/update-entry! (:id entry) {:plan-status to})
  ;; Best-effort: the store already has it, so an index failure must not turn
  ;; this verb into an error and provoke a retry that re-writes the store.
  (plans/update-plan-status! (:id entry) to)
  (or (hydrate (:id entry)) entry))

(defn handle-plan-list
  "List plans, newest index hits first, optionally narrowed by :status and
   :project_id. Rows carry a preview, never the content."
  [params]
  (if-let [bad (invalid-status (requested-status params))]
    bad
    (let [ids (keep :id
                    (plans/query-plans :type "plan"
                                       :plan-status (requested-status params)
                                       :project-id (project-scope params)
                                       :limit (limit-of (param params :limit :limit))))
          rows (mapv row (keep hydrate ids))]
      (mcp-json {:results rows :count (count rows)}))))

(defn handle-plan-search
  "Semantic search over plans. The index ranks, the store renders."
  [params]
  (let [q (param params :query :query)]
    (if (str/blank? (str q))
      (mcp-error "Missing required param: query")
      (let [ids (keep :id (plans/search-plans (str q)
                                             :type "plan"
                                             :project-id (project-scope params)
                                             :limit (limit-of (param params :limit :limit))))
            rows (mapv row (keep hydrate ids))]
        (mcp-json {:results rows :count (count rows)})))))

(defn handle-plan-get
  "One plan in full, content included — the verb that answers a plan's body."
  [params]
  (if-let [bad (missing-id params)]
    bad
    (if-let [entry (hydrate (param params :id :id))]
      (mcp-json entry)
      (mcp-error (str "Plan not found: " (param params :id :id))))))

(defn handle-plan-status
  "Counts by status for a project scope. Counted from the index (it is a pure
   count, nothing to hydrate), with every status reported so a caller sees the
   whole vocabulary rather than a partial map."
  [params]
  (let [counts (frequencies (keep :plan-status
                                  (plans/query-plans :type "plan"
                                                     :project-id (project-scope params)
                                                     :limit max-limit)))]
    (mcp-json {:project-id (project-scope params)
               :counts (into (sorted-map)
                             (map (fn [s] [s (get counts s 0)]))
                             plans/plan-statuses)
               :total (reduce + 0 (vals counts))})))

(defn- transition!
  "Judge ID -> TO, then write it. A NOOP writes nothing (the store and the index
   already agree, so a write would only churn the index)."
  [params to]
  (if-let [bad (missing-id params)]
    bad
    (if-let [entry (hydrate (param params :id :id))]
      (let [{:keys [ok? message reason]} (plans/transition-status (:plan-status entry) to)]
        (if-not ok?
          (mcp-error (str "Plan " (:id entry) ": " message))
          (cond-> {:plan-status to :previous-status (:plan-status entry) :reason reason}
            (= :noop reason) (assoc :changed false)
            (not= :noop reason) (assoc :changed true
                                       :plan (apply-status! entry to)))))
      (mcp-error (str "Plan not found: " (param params :id :id))))))

(defn- transition-response
  "Wrap `transition!`'s payload in the MCP envelope."
  [params to]
  (let [out (transition! params to)]
    (if (:isError out) out (mcp-json out))))

(defn handle-plan-update
  "Move a plan to :status through the machine. Refused, with the legal edges in
   the message, when the edge does not exist."
  [params]
  (let [status (requested-status params)]
    (cond
      (str/blank? (str status)) (mcp-error "Missing required param: status")
      (invalid-status status) (invalid-status status)
      :else (transition-response params status))))

(defn handle-plan-archive
  "Supersede a plan. Archive is not a delete: the record stays, and stays
   searchable, because the decision path that produced it is often still worth
   reading after it was replaced."
  [params]
  (transition-response params "superseded"))

(def handlers
  "The `plan` verbs, stored as VARS so a reload of this namespace reaches the
   subdomain table."
  {:list    #'handle-plan-list
   :search  #'handle-plan-search
   :get     #'handle-plan-get
   :status  #'handle-plan-status
   :update  #'handle-plan-update
   :archive #'handle-plan-archive})

(m/=> plan-entry? [:=> [:cat [:maybe :map]] :boolean])
(m/=> preview [:=> [:cat :any] [:maybe :string]])
(m/=> limit-of [:=> [:cat :any] :int])
(m/=> project-scope [:=> [:cat :map] [:maybe :string]])
(m/=> invalid-status [:=> [:cat :any] [:maybe :map]])
(m/=> missing-id [:=> [:cat :map] [:maybe :map]])
(m/=> apply-status! [:=> [:cat :map :string] :map])
(m/=> handle-plan-list [:=> [:cat :map] :map])
(m/=> handle-plan-search [:=> [:cat :map] :map])
(m/=> handle-plan-get [:=> [:cat :map] :map])
(m/=> handle-plan-status [:=> [:cat :map] :map])
(m/=> transition! [:=> [:cat :map :string] :map])
(m/=> handle-plan-update [:=> [:cat :map] :map])
(m/=> handle-plan-archive [:=> [:cat :map] :map])

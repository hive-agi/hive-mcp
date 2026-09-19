(ns hive-mcp.spi.kanban
  "The kanban contract between the kernel and whatever component owns the board.

   Two role protocols, four methods. `IKanbanRead` is what a scheduler or a
   catchup needs to look at the board; `IKanbanWrite` is what a planner or a
   dispatcher needs to change it. A provider may implement either or both;
   the registry (hive-mcp.spi.kanban.registry) keys them separately.

   The port speaks ONE status vocabulary, the public one: todo, inprogress,
   inreview, done. A provider maps its own storage tags to it.

   Contract: no method throws. Read misses are [] or nil; write failures are
   a {:err WriteError} value.

   Protocols are wrapped in a `defonce`-guarded block: reloading this ns must
   not mint fresh protocol objects, which would orphan already-registered
   implementations.

   HOST-LOCAL: this is the kernel's copy of the port. It moves to
   hive-contracts (ns hive-contracts.kanban) once that library releases it;
   until then the kernel cannot require an unreleased coordinate."
  (:require [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Schemas
;; =============================================================================

(def Status
  "Public task status. `inprogress` and `inreview` are the wire spellings."
  [:enum "todo" "inprogress" "inreview" "done"])

(def Priority
  [:enum "high" "medium" "low"])

(def Task
  "One board entry. Open: a provider may attach more keys (tags, timestamps),
   and consumers must tolerate them."
  [:map
   [:id :string]
   [:title {:optional true} [:maybe :string]]
   [:status {:optional true} [:maybe Status]]
   [:priority {:optional true} [:maybe Priority]]
   [:description {:optional true} [:maybe :string]]
   [:tags {:optional true} [:maybe [:sequential :string]]]
   [:project {:optional true} [:maybe :string]]])

(def ListQuery
  "Read-side filter. `:directory` scopes the board the way a caller's cwd
   does; `:project-id` overrides it. Every key is optional; {} is the whole
   scoped board."
  [:map
   [:status {:optional true} [:maybe Status]]
   [:directory {:optional true} [:maybe :string]]
   [:project-id {:optional true} [:maybe :string]]
   [:include-descendants? {:optional true} [:maybe :boolean]]
   [:scope {:optional true} [:maybe [:enum "all"]]]
   [:tags {:optional true} [:maybe [:sequential :string]]]
   [:limit {:optional true} [:maybe :int]]])

(def TransitionRequest
  [:map
   [:task-id :string]
   [:new-status Status]
   [:directory {:optional true} [:maybe :string]]])

(def CreateRequest
  "Write-side create. `:context` is an opaque map a provider stores beside the
   task (the plan pipeline puts :plan-step-id, :files and :execution there)."
  [:map
   [:title :string]
   [:description {:optional true} [:maybe :string]]
   [:priority {:optional true} [:maybe Priority]]
   [:status {:optional true} [:maybe Status]]
   [:tags {:optional true} [:maybe [:sequential :string]]]
   [:context {:optional true} [:maybe :map]]
   [:directory {:optional true} [:maybe :string]]
   [:agent-id {:optional true} [:maybe :string]]])

(def WriteError
  "Why a write did not happen. `:error` is a provider-agnostic keyword;
   `:kanban/provider-unavailable` is the degraded answer."
  [:map
   [:error :keyword]
   [:message {:optional true} :string]])

(def TransitionResult
  "Result-shaped: {:ok Task} after the move, {:err WriteError} otherwise."
  [:or
   [:map [:ok Task]]
   [:map [:err WriteError]]])

(def CreateResult
  "Result-shaped: {:ok {:id ...}} for the new (or idempotently found) task,
   {:err WriteError} otherwise."
  [:or
   [:map [:ok [:map [:id :string]]]]
   [:map [:err WriteError]]])

;; =============================================================================
;; Protocols
;; =============================================================================

(defonce ^:private -protocols-defined
  (do
    (defprotocol IKanbanRead
      "Look at the board."
      (list-tasks [this query]
        "query conforms to ListQuery. Returns a vector of Task, [] when the
         board is empty or the provider is degraded. Never throws.")
      (get-task [this id]
        "Returns the Task with `id`, or nil on a miss. Never throws."))

    (defprotocol IKanbanWrite
      "Change the board."
      (transition! [this request]
        "request conforms to TransitionRequest. Returns TransitionResult.
         Never throws.")
      (create-task! [this request]
        "request conforms to CreateRequest. Returns CreateResult.
         Never throws."))
    :defined))

;; =============================================================================
;; Conformance: the cases every implementation, stub or real, must pass
;; =============================================================================

(defn- check [case-id ok? detail]
  {:case case-id :ok? (boolean ok?) :detail detail})

(defn- attempt [thunk]
  (try (thunk) (catch Throwable t t)))

(defn conformance
  "Run the contract cases against `read-impl` and `write-impl` and return a
   vector of {:case kw :ok? bool :detail any}. A stub is faithful exactly when
   every case the real provider passes is also true of the stub, so a test
   suite asserts (every? :ok? (conformance impl impl)) on both.

   `opts` may carry `:missing-id` (an id the board does not hold; default
   \"kanban-conformance-missing\") and `:directory`."
  ([read-impl write-impl] (conformance read-impl write-impl {}))
  ([read-impl write-impl {:keys [missing-id directory]
                          :or {missing-id "kanban-conformance-missing"}}]
   (let [listed (attempt #(list-tasks read-impl {:directory directory}))
         missed (attempt #(get-task read-impl missing-id))
         bad-tr (attempt #(transition! write-impl {:task-id missing-id
                                                  :new-status "done"
                                                  :directory directory}))
         bad-cr (attempt #(create-task! write-impl {:title "" :directory directory}))]
     [(check :list/returns-vector (vector? listed) listed)
      (check :list/every-task-valid
             (and (vector? listed) (every? #(m/validate Task %) listed))
             (when (vector? listed) (remove #(m/validate Task %) listed)))
      (check :get/miss-is-nil (nil? missed) missed)
      (check :transition/missing-task-is-err
             (and (map? bad-tr) (m/validate TransitionResult bad-tr) (contains? bad-tr :err))
             bad-tr)
      (check :create/blank-title-is-err
             (and (map? bad-cr) (m/validate CreateResult bad-cr) (contains? bad-cr :err))
             bad-cr)])))

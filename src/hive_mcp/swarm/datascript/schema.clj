(ns hive-mcp.swarm.datascript.schema
  "DataScript schema definitions for swarm hivemind coordination.

   Compat shim: moved to hive-agent.swarm.datascript.schema in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-datascript.swarm.schema" sym))

(defn- impl!
  "impl, after making sure the host adapters fill the swarm port slots.
   The adapters are resolved late: a static require would close a load
   cycle through the events adapter. A failure here (for instance a call
   made while that cycle is still loading) leaves the ports on their
   noops until a later call installs them."
  [sym]
  (try ((requiring-resolve 'hive-mcp.swarm.adapters.boot/ensure!))
       (catch Throwable _ nil))
  (impl sym))

(def agent-types @(impl 'agent-types))

(defn claude-model?
  {:arglists '([model])}
  [& args]
  (apply (impl! 'claude-model?) args))

;; nil, a string or a keyword all answer a boolean: only a string can name a
;; Claude model, everything else (nil included, which is the default) is read
;; by the same predicate rather than refused.
(m/=> claude-model? [:=> [:cat [:maybe [:or :string :keyword :symbol]]] :boolean])

;; =============================================================================
;; Value schemas for the swarm DataScript shims
;; =============================================================================
;;
;; They live here, in the existing schema shim, because swarm.* is a frozen
;; extraction prefix (no new namespace). They transcribe the docstrings and the
;; bodies of hive-datascript.swarm.*, where the behaviour lives. The enums are
;; literal on purpose: the delegated sets above are nil when the swarm library
;; is absent, and a schema has to load without it.

(def Conn
  "A swarm store connection: an atom holding the database value (DataScript,
   or Datalevin's closeable conn, which is an atom too)."
  [:fn {:error/message "must be a connection (an atom)"
        :gen/schema :nil
        :gen/fmap (fn [_] (atom {}))}
   (fn [x] (instance? clojure.lang.IAtom x))])

(def TxReport
  "A DataScript transaction report (a record, so a map)."
  [:map
   [:db-before :any]
   [:db-after :any]
   [:tx-data [:sequential :any]]
   [:tempids :map]])

(def MaybeTxReport
  "A transaction report, or nil when the addressed entity does not exist."
  [:maybe TxReport])

(def EntityId
  "A lookup id (slave, task, claim key, coordinator). An unknown or nil id is
   answered with nil or an empty result, never refused."
  [:maybe :string])

(def SlaveStatus
  "`slave-statuses`: the statuses `add-slave!` accepts."
  [:enum :initializing :zombie :idle :starting :working :spawning :terminated :error :blocked])

(def TaskStatus
  "`task-statuses`."
  [:enum :completed :queued :error :dispatched :timeout])

(def CriticalOp
  "`critical-op-types`: the operations that block a kill."
  [:enum :wrap :dispatch :commit])

(def CoordinatorStatus
  "`coordinator-statuses`."
  [:enum :stale :active :terminated])

(def Inst
  "A java.util.Date timestamp."
  inst?)

(def StringColl
  "A collection of strings: a sequence or a set (both are accepted where a
   body only seqs over it)."
  [:or [:sequential :string] [:set :string]])

(def Slave
  "A slave row as the queries read it back: refs resolved to ids, :db/id
   dropped. update-slave! writes any attribute, so the status is any keyword
   and the map is open."
  [:map
   [:slave/id :string]
   [:slave/name {:optional true} [:maybe :string]]
   [:slave/status {:optional true} :keyword]
   [:slave/depth {:optional true} :int]
   [:slave/parent {:optional true} [:maybe :string]]
   [:slave/current-task {:optional true} [:maybe :string]]
   [:slave/alive? {:optional true} :boolean]
   [:slave/created-at {:optional true} Inst]])

(def Task
  "A task row: :task/slave resolved to the slave id. `get-task` reads
   cardinality-many files as a set, the pull-based `get-completed-tasks` as a
   vector."
  [:map
   [:task/id :string]
   [:task/status {:optional true} :keyword]
   [:task/slave {:optional true} [:maybe :string]]
   [:task/prompt {:optional true} [:maybe :string]]
   [:task/files {:optional true} StringColl]
   [:task/started-at {:optional true} Inst]
   [:task/completed-at {:optional true} Inst]])

(def ClaimInfo
  "`get-claims-for-file`: one claim, whole-file keys only."
  [:map
   [:file :string]
   [:slave-id [:maybe :string]]
   [:task-id [:maybe :string]]
   [:created-at [:maybe Inst]]
   [:prior-hash [:maybe :string]]])

(def ClaimRow
  "One row of `get-all-claims`: a claim plus its span projection (:qn and
   :mode are nil for a whole-file claim)."
  [:map
   [:file :string]
   [:slave-id [:maybe :string]]
   [:task-id [:maybe :string]]
   [:created-at [:maybe Inst]]
   [:prior-hash [:maybe :string]]
   [:qn [:maybe :string]]
   [:mode [:maybe :keyword]]])

(def ClaimHistory
  "A claim-history row with its keys renamed for output."
  [:map
   [:id :string]
   [:file :string]
   [:slave-id :string]
   [:released-at Inst]
   [:prior-hash {:optional true} [:maybe :string]]
   [:released-hash {:optional true} [:maybe :string]]
   [:lines-added {:optional true} [:maybe :int]]
   [:lines-removed {:optional true} [:maybe :int]]])

(def Count
  "A non-negative count."
  [:int {:min 0}])

(def Atom
  "A mutable reference (an atom), such as a ling's stdout ring buffer."
  [:fn {:error/message "must be an atom"
        :gen/schema :nil
        :gen/fmap (fn [_] (atom {:lines [] :next-idx 0}))}
   (fn [x] (instance? clojure.lang.IAtom x))])

(def Threshold
  "An age threshold in milliseconds. The bodies only subtract and compare it."
  number?)

(defn- kwargs
  "Arglist tail for a `& {:keys [...]}` fn: nothing, alternating key/value
   pairs over KS, or (Clojure 1.11) one trailing map."
  [ks]
  [:alt
   [:cat]
   [:+ [:cat (into [:enum] ks) :any]]
   [:cat [:map-of (into [:enum] ks) :any]]])

(def IncludeStaleArgs
  "The `& {:keys [include-stale?]}` tail of the slave listings."
  (kwargs [:include-stale?]))

(def CompletedTaskArgs
  "The `& {:keys [slave-id since limit]}` tail of `get-completed-tasks`."
  (kwargs [:slave-id :since :limit]))

(def ClaimHistoryArgs
  "The `& {:keys [file slave-id since limit]}` tail of `get-recent-claim-history`."
  (kwargs [:file :slave-id :since :limit]))

(def SessionArgs
  "The `& {:keys [agent-id project-id session-ref parent-of]}` tail of the
   session-registry readers."
  (kwargs [:agent-id :project-id :session-ref :parent-of]))

(def ThresholdOpts
  "The optional `[{:keys [threshold-ms]}]` argument of the cleanup sweeps."
  [:? [:maybe [:map [:threshold-ms {:optional true} Threshold]]]])

(def StdoutLine
  "One line of a headless ling's stdout ring buffer."
  [:map [:idx [:int {:min 0}]] [:text :string] [:ts Inst]])

(def Coordinator
  "A coordinator row, :db/id dropped."
  [:map
   [:coordinator/id :string]
   [:coordinator/status {:optional true} CoordinatorStatus]
   [:coordinator/project {:optional true} :string]
   [:coordinator/pid {:optional true} :int]
   [:coordinator/session-id {:optional true} :string]
   [:coordinator/started-at {:optional true} Inst]
   [:coordinator/heartbeat-at {:optional true} Inst]])

(def CompletedTask
  "A completed-task registry row, :db/id dropped."
  [:map
   [:completed-task/id :string]
   [:completed-task/completed-at Inst]
   [:completed-task/title {:optional true} :string]
   [:completed-task/agent-id {:optional true} :string]
   [:completed-task/project-id {:optional true} :string]
   [:completed-task/session-id {:optional true} :string]])

(def KanbanMovement
  "A kanban-movement registry row, :db/id dropped."
  [:map
   [:kanban-movement/id :string]
   [:kanban-movement/task-id :string]
   [:kanban-movement/to :string]
   [:kanban-movement/at Inst]
   [:kanban-movement/from {:optional true} :string]
   [:kanban-movement/title {:optional true} :string]
   [:kanban-movement/agent-id {:optional true} :string]
   [:kanban-movement/project-id {:optional true} :string]
   [:kanban-movement/session-id {:optional true} :string]])

(def WrapRow
  "A wrap-queue row as `(pull ?e [*])` reads it (:db/id kept)."
  [:map
   [:wrap-queue/id :string]
   [:wrap-queue/processed? :boolean]
   [:wrap-queue/created-at Inst]
   [:wrap-queue/agent-id {:optional true} :string]
   [:wrap-queue/session-id {:optional true} :string]
   [:wrap-queue/project-id {:optional true} :string]
   [:wrap-queue/parent-session-id {:optional true} :string]
   [:wrap-queue/depth {:optional true} :int]
   [:wrap-queue/created-ids {:optional true} [:sequential :string]]
   [:wrap-queue/stats {:optional true} :map]])

(def coordinator-statuses @(impl 'coordinator-statuses))

(def critical-op-types @(impl 'critical-op-types))

(def daemon-health-levels @(impl 'daemon-health-levels))

(def daemon-statuses @(impl 'daemon-statuses))

(def ling-model-default @(impl 'ling-model-default))

(def olympus-layout-modes @(impl 'olympus-layout-modes))

(def schema @(impl 'schema))

(def slave-statuses @(impl 'slave-statuses))

(def spawn-modes @(impl 'spawn-modes))

(def task-statuses @(impl 'task-statuses))

(def task-types @(impl 'task-types))

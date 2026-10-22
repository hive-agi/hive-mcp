(ns hive-mcp.swarm.datascript.lings
  "Entity lifecycle operations for lings (slaves, tasks, claims).

   Compat shim: moved to hive-agent.swarm.datascript.lings in hive-agent.
   Every public var delegates there; see hive-mcp.swarm.delegate for behavior
   without the addon. `with-critical-op` expands to this namespace's
   enter/exit fns, which delegate."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [taoensso.timbre :as log]
            [hive-mcp.swarm.datascript.schema :as s]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-datascript.swarm.lings" sym))

(defn- impl!
  "impl, after the host adapters fill the swarm port slots (resolved late:
   a static require would close a load cycle)."
  [sym]
  (try ((requiring-resolve 'hive-mcp.swarm.adapters.boot/ensure!))
       (catch Throwable _ nil))
  (impl sym))

(defn add-slave! {:arglists '([slave-id {:keys [name status depth parent presets cwd project-id kanban-task-id], :or {status :idle, depth 1}}])} [& args] (apply (impl! 'add-slave!) args))

;; Contracts transcribe hive-datascript.swarm.lings, where the behaviour lives.
;; A missing entity answers nil; a :pre the store asserts (a string id, a known
;; status) is part of the input schema.
(def ^:private AddSlaveOpts
  [:map
   [:name {:optional true} [:maybe :string]]
   [:status {:optional true} s/SlaveStatus]
   [:depth {:optional true} :int]
   [:parent {:optional true} [:maybe :string]]
   [:presets {:optional true} [:maybe s/StringColl]]
   [:cwd {:optional true} [:maybe :string]]
   [:project-id {:optional true} [:maybe :string]]
   [:kanban-task-id {:optional true} [:maybe :string]]])

(m/=> add-slave! [:=> [:cat :string [:maybe AddSlaveOpts]] s/TxReport])
(defn add-task! {:arglists '([task-id slave-id {:keys [status prompt files], :or {status :dispatched}}])} [& args] (apply (impl! 'add-task!) args))

(m/=> add-task! [:=> [:cat [:maybe :string] :string
                         [:maybe [:map [:status {:optional true} s/TaskStatus]
                                  [:prompt {:optional true} [:maybe :string]]
                                  [:files {:optional true} [:maybe s/StringColl]]]]]
                 s/TxReport])
(defn add-to-wait-queue! {:arglists '([ling-id file-path])} [& args] (apply (impl! 'add-to-wait-queue!) args))

(m/=> add-to-wait-queue! [:=> [:cat :string :string] s/TxReport])
(defn append-stdout! {:arglists '([slave-id lines])} [& args] (apply (impl! 'append-stdout!) args))

;; A string or a collection of strings; answers the buffer's line count.
(m/=> append-stdout! [:=> [:cat :any [:or :string [:sequential :string]]] s/Count])
(defn archive-claim-to-history! {:arglists '([file-path {:keys [slave-id prior-hash released-hash lines-added lines-removed]}])} [& args] (apply (impl! 'archive-claim-to-history!) args))

(m/=> archive-claim-to-history!
      [:=> [:cat :string [:map [:slave-id :string]
                          [:prior-hash {:optional true} [:maybe :string]]
                          [:released-hash {:optional true} [:maybe :string]]
                          [:lines-added {:optional true} [:maybe :int]]
                          [:lines-removed {:optional true} [:maybe :int]]]]
       s/TxReport])
(defn can-kill? {:arglists '([slave-id])} [& args] (apply (impl! 'can-kill?) args))

(m/=> can-kill? [:=> [:cat s/EntityId]
                 [:or [:map [:can-kill? [:= true]] [:blocking-ops [:= #{}]]]
                  [:map [:can-kill? [:= false]] [:blocking-ops [:set {:min 1} :keyword]]]]])
(defn claim-age-ms {:arglists '([file-path])} [& args] (apply (impl! 'claim-age-ms) args))

(m/=> claim-age-ms [:=> [:cat s/EntityId] [:maybe :int]])
(def ^:private dead-statuses
  "Slave statuses whose claims no longer protect anything."
  #{:terminated :dead :killed :zombie})

(defn holder-live?
  "True when `slave-id` is a registered slave not in a dead status.

   The one liveness rule for claim holders: `claim-file!` takes over a key
   whose holder fails it, and `claim.registry/held-spans` (`:drop-dead?`) and
   `claim.gate` stop counting such a holder as a blocker. An unregistered id
   fails it too, matching what `claim-file!` has always done: a claim row
   cannot outlive the slave entity it references."
  [slave-id]
  (boolean
   (when slave-id
     (when-let [slave ((delegate/resolve-var "hive-datascript.swarm.queries" 'get-slave)
                       slave-id)]
       (not (contains? dead-statuses (:slave/status slave)))))))

;; boundary: reads the slave entity from the swarm store
(m/=> holder-live? [:=> [:cat [:maybe :string]] :boolean])

(defn- lease-stale?
  "True when the claim on `file-path` has had no activity for the lease window.

   Activity is the later of creation and the last `refresh-claim!` heartbeat
   (`claim.registry/key-stale?`). Resolved late (the registry requires this
   namespace); when it is unavailable the creation-age rule stands."
  [file-path]
  (if-let [f (try (requiring-resolve 'hive-mcp.swarm.claim.registry/key-stale?)
                  (catch Throwable _registry-absent-creation-age-stands nil))]
    (boolean (f file-path))
    (boolean ((impl! 'claim-stale?) file-path))))

(defn- live-foreign-holder
  "The id of the LIVE slave other than `slave-id` holding `file-path`, or nil.

   Live means `holder-live?` and holding a claim whose lease has not lapsed. A
   dead or stale holder is exactly the case the upsert exists for (a ling that
   died holding a file must not fence it off forever), so only a live one
   refuses."
  [file-path slave-id]
  (let [{holder :slave-id} ((impl! 'get-claim-info) file-path)]
    (when (and holder (not= holder slave-id)
               (holder-live? holder)
               (not (lease-stale? file-path)))
      holder)))

(defn- claim-lock
  "The one claim lock (the logic-db atom `coordinator/atomic-claim-files!` and
   `claim.registry/acquire!` hold), resolved late: a static require would
   close a load cycle. Reentrant, so taking it under either of those is free."
  []
  (if-let [f (try (requiring-resolve 'hive-mcp.swarm.logic/get-logic-db-atom)
                  (catch Throwable _ nil))]
    (f)
    ::no-logic-lock))

(defn claim-file!
  "Claim `file-path` (a claim key) for `slave-id`.

   The store's claim-file! UPSERTS on the unique key, so an unchecked call used
   to hand another live ling's claim to the caller without a word
   (SWARM-CLAIM-STEAL). This shim refuses that case under the claim lock and
   returns {:claimed? false :refused :held-by-live-slave :file :held-by}
   instead of writing. Re-claiming one's own key, or taking over a key held by
   a dead or stale slave, still upserts as before."
  {:arglists '([file-path slave-id & [{:keys [task-id prior-hash qn mode]}]])}
  [file-path slave-id & more]
  (locking (claim-lock)
    (if-let [holder (live-foreign-holder file-path slave-id)]
      (do (log/warn "claim refused: held by a live slave"
                    {:file file-path :held-by holder :requesting slave-id})
          {:claimed? false
           :refused  :held-by-live-slave
           :file     file-path
           :held-by  holder})
      (apply (impl! 'claim-file!) file-path slave-id more))))

;; The refusal map when another live slave holds the key, else the store's
;; upsert report.
(m/=> claim-file!
      ;; The opts are destructured with :keys, which reads any value (an
      ;; existing caller passes a bare task-id string, read as no opts).
      [:=> [:cat :string :string
            [:? [:or [:maybe [:map [:task-id {:optional true} [:maybe :string]]
                              [:prior-hash {:optional true} [:maybe :string]]
                              [:qn {:optional true} [:maybe :string]]
                              [:mode {:optional true} [:maybe :keyword]]]]
                 :string]]]
       [:or s/TxReport
        [:map [:claimed? [:= false]] [:refused [:= :held-by-live-slave]]
         [:file :string] [:held-by :string]]]])
(defn claim-stale? {:arglists '([file-path] [file-path threshold-ms])} [& args] (apply (impl! 'claim-stale?) args))

;; nil when there is no claim, else whether it is older than the threshold.
(m/=> claim-stale? [:function
                    [:=> [:cat s/EntityId] [:maybe :boolean]]
                    [:=> [:cat s/EntityId s/Threshold] [:maybe :boolean]]])
(defn cleanup-stale-claims! {:arglists '([] [threshold-ms])} [& args] (apply (impl! 'cleanup-stale-claims!) args))

(m/=> cleanup-stale-claims!
      [:function
       [:=> [:cat] [:map [:released-count s/Count] [:released-files [:vector :string]]]]
       [:=> [:cat s/Threshold] [:map [:released-count s/Count] [:released-files [:vector :string]]]]])
(defn cleanup-stdout-buffer! {:arglists '([slave-id])} [& args] (apply (impl! 'cleanup-stdout-buffer!) args))

(m/=> cleanup-stdout-buffer! [:=> [:cat :any] :boolean])
(defn complete-task! {:arglists '([task-id])} [& args] (apply (impl! 'complete-task!) args))

(m/=> complete-task! [:=> [:cat s/EntityId] s/MaybeTxReport])
(def default-stale-threshold-ms @(impl 'default-stale-threshold-ms))
(defn enter-critical-op! {:arglists '([slave-id op-type])} [& args] (apply (impl! 'enter-critical-op!) args))

(m/=> enter-critical-op! [:=> [:cat s/EntityId s/CriticalOp] s/MaybeTxReport])
(defn exit-critical-op! {:arglists '([slave-id op-type])} [& args] (apply (impl! 'exit-critical-op!) args))

(m/=> exit-critical-op! [:=> [:cat s/EntityId s/CriticalOp] s/MaybeTxReport])
(defn fail-task! {:arglists '([task-id status])} [& args] (apply (impl! 'fail-task!) args))

(m/=> fail-task! [:=> [:cat s/EntityId [:enum :error :timeout]] s/MaybeTxReport])
(defn get-all-claims {:arglists '([& args])} [& args] (apply (impl! 'get-all-claims) args))

(m/=> get-all-claims [:=> [:cat] [:sequential s/ClaimRow]])
(defn get-claim-info {:arglists '([& args])} [& args] (apply (impl! 'get-claim-info) args))

(m/=> get-claim-info [:=> [:cat s/EntityId] [:maybe s/ClaimInfo]])
(defn get-critical-ops {:arglists '([slave-id])} [& args] (apply (impl! 'get-critical-ops) args))

(m/=> get-critical-ops [:=> [:cat s/EntityId] [:set :keyword]])
(defn get-stale-claims {:arglists '([] [threshold-ms])} [& args] (apply (impl! 'get-stale-claims) args))

(def ^:private StaleClaim
  (into s/ClaimRow [[:age-ms :int] [:age-minutes :int]]))

(m/=> get-stale-claims [:function
                        [:=> [:cat] [:sequential StaleClaim]]
                        [:=> [:cat s/Threshold] [:sequential StaleClaim]]])
(defn get-stdout {:arglists '([slave-id] [slave-id n])} [& args] (apply (impl! 'get-stdout) args))

(m/=> get-stdout [:function
                  [:=> [:cat :any] [:vector s/StdoutLine]]
                  [:=> [:cat :any [:int {:min 0}]] [:vector s/StdoutLine]]])
(defn get-stdout-buffer-info {:arglists '([slave-id])} [& args] (apply (impl! 'get-stdout-buffer-info) args))

(m/=> get-stdout-buffer-info
      [:=> [:cat :any]
       [:maybe [:map [:line-count s/Count] [:next-idx s/Count]
                [:oldest-idx [:maybe s/Count]] [:newest-idx [:maybe s/Count]]
                [:max-lines s/Count]]]])
(defn get-stdout-since {:arglists '([slave-id since-idx])} [& args] (apply (impl! 'get-stdout-since) args))

(m/=> get-stdout-since [:=> [:cat :any :int] [:vector s/StdoutLine]])
(defn init-stdout-buffer! {:arglists '([slave-id])} [& args] (apply (impl! 'init-stdout-buffer!) args))

(m/=> init-stdout-buffer! [:=> [:cat :any] s/Atom])
(defn refresh-claim! {:arglists '([file-path])} [& args] (apply (impl! 'refresh-claim!) args))

(m/=> refresh-claim! [:=> [:cat s/EntityId] s/MaybeTxReport])
(defn release-claim! {:arglists '([file-path])} [& args] (apply (impl! 'release-claim!) args))

(m/=> release-claim! [:=> [:cat s/EntityId] s/MaybeTxReport])
(defn release-claims-for-slave! {:arglists '([slave-id])} [& args] (apply (impl! 'release-claims-for-slave!) args))

(m/=> release-claims-for-slave! [:=> [:cat s/EntityId] s/Count])
(defn release-claims-for-task! {:arglists '([task-id])} [& args] (apply (impl! 'release-claims-for-task!) args))

(m/=> release-claims-for-task! [:=> [:cat s/EntityId] s/Count])
(defn remove-slave! {:arglists '([slave-id])} [& args] (apply (impl! 'remove-slave!) args))

(m/=> remove-slave! [:=> [:cat s/EntityId] s/MaybeTxReport])
(defn reset-stdout-buffers! {:arglists '([])} [& args] (apply (impl! 'reset-stdout-buffers!) args))

;; reset! answers the new, empty registry.
(m/=> reset-stdout-buffers! [:=> [:cat] [:map-of {:max 0} :any :any]])
(def stdout-buffer-max-lines @(impl 'stdout-buffer-max-lines))
(def stdout-buffers @(impl 'stdout-buffers))
(defn update-slave! {:arglists '([slave-id updates])} [& args] (apply (impl! 'update-slave!) args))

(m/=> update-slave! [:=> [:cat s/EntityId :map] s/MaybeTxReport])
(defn update-task! {:arglists '([task-id updates])} [& args] (apply (impl! 'update-task!) args))

(m/=> update-task! [:=> [:cat s/EntityId :map] s/MaybeTxReport])

(defmacro with-critical-op
  "Execute body while holding a critical operation guard.
   Ensures the critical op is properly released even on exception."
  [slave-id op-type & body]
  `(do
     (enter-critical-op! ~slave-id ~op-type)
     (try
       ~@body
       (finally
         (exit-critical-op! ~slave-id ~op-type)))))

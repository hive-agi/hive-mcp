(ns hive-mcp.events.handlers.claim
  "File claim event handlers.

   Handles events related to file claim lifecycle:
   - :claim/file-released - When a file claim is released, notify waiting lings
   - :claim/notify-waiting - Send targeted shout to a ling that file is available

   Event Cascade:
   release-claim! → :claim/file-released → check wait-queue → :claim/notify-waiting → targeted shout"

  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]
            [hive-mcp.swarm.datascript :as ds]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- wait-queue-id
  "The :wait-queue/id a ling's wait on a file is stored under
   (hive-datascript.swarm.lings/add-to-wait-queue!)."
  [ling-id file-path]
  (str "wait:" ling-id ":" file-path))

(defn- query-waiting-lings
  "Query the swarm store snapshot for lings waiting on a specific file.

   Two kinds of waiter exist, and both must be woken:
   - a task that was QUEUED on the file (coordinator dispatch path), and
   - a ling whose claim was REFUSED and that parked itself in the wait queue
     (`Ling.claim-files!` -> add-to-wait-queue!). Lings are :dispatched, never
     :queued, so reading tasks alone never found them.

   Returns a vector of {:slave-id :task-id :parked?} maps, one per waiting
   ling (task-id is nil for a pure wait-queue entry). :parked? says the ling
   has a wait-queue row for this file that must be retracted once it is woken.
   A ling that waits both ways is reported once."
  [db file-path]
  (when (and db file-path)
    (let [queued (ds/q-db db
                          '[:find ?slave-id ?task-id
                            :in $ ?file
                            :where
                            [?t :task/files ?file]
                            [?t :task/status :queued]
                            [?t :task/id ?task-id]
                            [?t :task/slave ?s]
                            [?s :slave/id ?slave-id]]
                          file-path)
          parked (into #{}
                       (map first)
                       (ds/q-db db
                                '[:find ?ling-id
                                  :in $ ?file
                                  :where
                                  [?w :wait-queue/file ?file]
                                  [?w :wait-queue/ling-id ?ling-id]]
                                file-path))
          by-task (mapv (fn [[slave-id task-id]]
                          {:slave-id slave-id
                           :task-id task-id
                           :parked? (contains? parked slave-id)})
                        queued)
          seen (into #{} (map :slave-id) by-task)]
      (into by-task
            (comp (remove seen)
                  (map (fn [ling-id]
                         {:slave-id ling-id :task-id nil :parked? true})))
            (sort parked)))))

;; =============================================================================
;; Handler: :claim/file-released
;; =============================================================================

(defn handle-claim-file-released
  "Handler for :claim/file-released events.

   When a file claim is released, this handler:
   1. Queries for lings waiting on this file (queued tasks and wait-queue)
   2. Dispatches :claim/notify-waiting for each waiting ling
   3. Logs the release activity

   Expects event data:
   {:file        \"src/core.clj\"  ; The released file path
    :released-by \"ling-123\"}     ; Ling that released the claim (optional)

   Produces effects:
   - :log          - Log the release
   - :dispatch-n   - Dispatch notify events for all waiting lings"
  [coeffects [_ {:keys [file released-by]}]]
  (let [db (or (:db-snapshot coeffects) (ds/current-db))
        waiting-lings (query-waiting-lings db file)
        waiting-count (count waiting-lings)]

    (log/debug "File released:" file "by:" released-by
               "- waiting lings:" waiting-count)

    (if (seq waiting-lings)
      ;; Dispatch notification to each waiting ling
      {:log {:level :info
             :message (str "File " file " released by " released-by
                           " - notifying " waiting-count " waiting ling(s)")}
       :dispatch-n (mapv (fn [{:keys [slave-id task-id parked?]}]
                           [:claim/notify-waiting {:target-agent-id slave-id
                                                   :file file
                                                   :task-id task-id
                                                   :released-by released-by
                                                   :parked? parked?}])
                         waiting-lings)}
      ;; No waiting lings - just log
      {:log {:level :debug
             :message (str "File " file " released by " released-by
                           " - no waiting lings")}})))

;; =============================================================================
;; Handler: :claim/notify-waiting
;; =============================================================================

(defn handle-claim-notify-waiting
  "Handler for :claim/notify-waiting events.

   Sends a DIRECTED shout to the waiting ling that the file is available,
   from the ling that released it. A ling that parked itself in the wait
   queue is taken off it, so a later release of the same file does not wake
   it again.

   Expects event data:
   {:target-agent-id \"ling-worker-1\"  ; Ling to notify
    :file            \"src/core.clj\"   ; File that became available
    :task-id         \"task-123\"       ; Task that was waiting (optional)
    :released-by     \"ling-worker-7\"  ; Releasing ling (optional)
    :parked?         true}             ; Has a wait-queue row (optional)

   Produces effects:
   - :targeted-shout - Directed shout to the waiting ling
   - :ds-transact    - Retract the wait-queue row, only when :parked?"
  [_coeffects [_ {:keys [target-agent-id file task-id released-by parked?]}]]
  (log/debug "Notifying ling" target-agent-id "that file" file "is available")

  (cond-> {:targeted-shout {:target-agent-id target-agent-id
                            :sender-id released-by
                            :event-type :file-available
                            :data {:file file
                                   :task-id task-id
                                   :message (str "File " file " is now available")}}}
    parked? (assoc :ds-transact
                   [[:db/retractEntity
                     [:wait-queue/id (wait-queue-id target-agent-id file)]]])))

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-handlers!
  "Register file claim event handlers."
  []
  (ev/reg-event :claim/file-released
                [(ev/inject-cofx :db-snapshot)
                 interceptors/debug]
                handle-claim-file-released)

  (ev/reg-event :claim/notify-waiting
                [interceptors/debug]
                handle-claim-notify-waiting)

  (log/info "[hive-events] Claim handlers registered: :claim/file-released :claim/notify-waiting"))

(ns hive-mcp.test.stub.kanban
  "clojure.test fixtures and a recording double for the kanban ports
   (hive-mcp.spi.kanban).

   with-core-kanban       core's real provider (hive-mcp.tools.kanban.port)
                          over whatever memory store the test installed
   with-recording-kanban  a fresh atom-backed recording stub, empty board;
                          a test reaches it with (current-stub)

   The recording stub is held to the same `kanban/conformance` cases the real
   provider passes, and every call is appended to `:calls` as
   [method argument].

   Both fixtures restore the ports to the providers they found."
  (:require [hive-mcp.spi.kanban :as kanban]
            [hive-mcp.spi.kanban.registry :as registry]
            [hive-mcp.tools.kanban.port :as port]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Recording double
;; =============================================================================

(defn- ->status [s] (if (keyword? s) (name s) s))

(defrecord RecordingKanban [board calls]
  kanban/IKanbanRead
  (list-tasks [_ query]
    (swap! calls conj [:list-tasks query])
    (let [status (->status (:status query))]
      (->> (vals @board)
           (filter #(or (nil? status) (= status (:status %))))
           (sort-by :id)
           vec)))
  (get-task [_ id]
    (swap! calls conj [:get-task id])
    (get @board id))
  kanban/IKanbanWrite
  (transition! [_ {:keys [task-id new-status] :as req}]
    (swap! calls conj [:transition! req])
    (if-let [task (get @board task-id)]
      (let [moved (assoc task :status new-status)]
        (swap! board assoc task-id moved)
        {:ok moved})
      {:err {:error :kanban/not-found :message (str "no task " task-id)}}))
  (create-task! [_ {:keys [title] :as req}]
    (swap! calls conj [:create-task! req])
    (if (and (string? title) (seq title))
      (let [id (str "stub-" (inc (count @board)))]
        (swap! board assoc id {:id id :title title
                               :status (or (:status req) "todo")
                               :priority (or (:priority req) "medium")})
        {:ok {:id id}})
      {:err {:error :kanban/validation :message "title required"}})))

(defn recording-kanban
  "A RecordingKanban seeded with TASKS, a seq of Task maps."
  ([] (recording-kanban []))
  ([tasks]
   (->RecordingKanban (atom (into {} (map (juxt :id identity)) tasks)) (atom []))))

(defn board
  "The stub's current {id -> Task} snapshot."
  [stub]
  @(:board stub))

(defn calls
  "The stub's recorded calls, oldest first."
  [stub]
  @(:calls stub))

(defn install!
  "Register STUB under both kanban ports. Returns STUB."
  [stub]
  (registry/register! :IKanbanRead stub)
  (registry/register! :IKanbanWrite stub)
  stub)

;; =============================================================================
;; Fixtures
;; =============================================================================

(def ^:dynamic *stub*
  "The recording stub installed by `with-recording-kanban`."
  nil)

(defn current-stub
  "The recording stub the enclosing `with-recording-kanban` installed."
  []
  *stub*)

(defn- restore-ports!
  "Reinstall PRIOR providers (port -> impl) or revert to the Noops."
  [prior]
  (doseq [port [:IKanbanRead :IKanbanWrite]]
    (if-let [impl (get prior port)]
      (registry/register! port impl)
      (registry/unregister! port))))

(defn- prior-providers []
  (into {} (for [port [:IKanbanRead :IKanbanWrite]
                 :when (registry/registered? port)]
             [port (registry/provider port)])))

(defn with-core-kanban
  "Fixture: core's CoreKanban answers both ports for the duration of F."
  [f]
  (let [prior (prior-providers)]
    (try
      (port/register!)
      (f)
      (finally
        (restore-ports! prior)))))

(defn with-recording-kanban
  "Fixture: a fresh, empty recording stub answers both ports for F."
  [f]
  (let [prior (prior-providers)
        s     (recording-kanban)]
    (try
      (install! s)
      (binding [*stub* s] (f))
      (finally
        (restore-ports! prior)))))

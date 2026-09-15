(ns hive-mcp.channel.a2a
  "A2A (Agent2Agent) envelope vocabulary for the swarm channel.

   Pure calculations: no I/O, no state. This namespace owns the WIRE shape —
   what a hive shout looks like when it is spelled as an A2A Message — and the
   projection both ways. Routing decisions live in hive-mcp.channel.audience;
   delivery lives in hive-mcp.channel.piggyback.

   ## What A2A gives us and what it does not

   A2A addresses ONE agent per transport: the recipient is the URL the client
   posted to, so the Message carries no recipient field. A swarm bus multiplexes
   every agent over one channel, so the recipient has to travel IN the envelope.
   A2A's own answer to \"my transport needs a field the core spec lacks\" is the
   extension mechanism: declare a URI in `extensions`, carry the data in
   `metadata`. That is what `extension-uri` and the `hive.swarm/*` metadata keys
   are, and it is why routing is not a top-level key here — an envelope this
   namespace emits is a valid A2A Message that a stock client can parse, with
   the routing riding where the spec says vendor data rides.

   `contextId` is A2A's own grouping id and is used as such: it groups the
   messages of one exchange so a reader can fetch the exchange by id rather than
   receive every turn of it."
  (:require [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Extension identity
;; =============================================================================

(def ^:const extension-uri
  "The URI a Message declares in `extensions` when it carries swarm routing.
   A reader that does not know this URI still parses the envelope; it just
   cannot route it, which is the correct degradation."
  "https://hive-agi.org/a2a/ext/swarm-routing/v1")

(def ^:const meta-to
  "metadata key: the agent this message is addressed to (directed p2p)."
  "hive.swarm/to")

(def ^:const meta-from
  "metadata key: the agent that sent it. A2A's `role` distinguishes user from
   agent, which on a bus of peers says nothing about WHICH peer."
  "hive.swarm/from")

(def ^:const meta-broadcast
  "metadata key: true when the message is addressed to every reader."
  "hive.swarm/broadcast")

(def ^:const meta-broadcast-reason
  "metadata key: why a broadcast was justified. Broadcast without one is
   refused by hive-mcp.channel.broadcast-policy."
  "hive.swarm/broadcast-reason")

(def ^:const meta-ref
  "metadata key: a context-store id standing in for an elided payload. The row
   a reader sees carries the id; the body is fetched only if wanted."
  "hive.swarm/ref")

(def ^:const meta-event
  "metadata key: the hive event-type (progress, completed, error, ...). A2A has
   no event vocabulary below TaskState, and the digest routes on this."
  "hive.swarm/event")

(def ^:const meta-deliberate
  "metadata key: true when the agent CHOSE to send this, as opposed to runtime
   telemetry emitted on its behalf. The digest never collapses a deliberate row."
  "hive.swarm/deliberate")

;; =============================================================================
;; Closed vocabularies (A2A wire spellings)
;; =============================================================================

(def roles
  "A2A Role enum, wire spelling."
  #{"ROLE_USER" "ROLE_AGENT"})

(def task-states
  "A2A TaskState enum, wire spelling. All nine, including UNSPECIFIED."
  #{"TASK_STATE_UNSPECIFIED"
    "TASK_STATE_SUBMITTED"
    "TASK_STATE_WORKING"
    "TASK_STATE_COMPLETED"
    "TASK_STATE_FAILED"
    "TASK_STATE_CANCELED"
    "TASK_STATE_INPUT_REQUIRED"
    "TASK_STATE_REJECTED"
    "TASK_STATE_AUTH_REQUIRED"})

(def terminal-states
  "States after which a task accepts no further work."
  #{"TASK_STATE_COMPLETED" "TASK_STATE_FAILED"
    "TASK_STATE_CANCELED" "TASK_STATE_REJECTED"})

(def interrupted-states
  "States in which the task is waiting on somebody outside it."
  #{"TASK_STATE_INPUT_REQUIRED" "TASK_STATE_AUTH_REQUIRED"})

(defn terminal-state?
  "Is `s` a state after which the task is finished?"
  [s]
  (contains? terminal-states s))

(defn interrupted-state?
  "Is `s` a state in which the task awaits an outside party?"
  [s]
  (contains? interrupted-states s))

(def ^:private hive-event->task-state
  "hive event-type -> A2A TaskState. Events with no A2A counterpart map to
   WORKING rather than UNSPECIFIED: a `progress` shout is evidence the task is
   running, and UNSPECIFIED would throw that evidence away."
  {"started"   "TASK_STATE_SUBMITTED"
   "progress"  "TASK_STATE_WORKING"
   "completed" "TASK_STATE_COMPLETED"
   "error"     "TASK_STATE_FAILED"
   "failed"    "TASK_STATE_FAILED"
   "aborted"   "TASK_STATE_CANCELED"
   "cancelled" "TASK_STATE_CANCELED"
   "ask"       "TASK_STATE_INPUT_REQUIRED"})

(defn event->task-state
  "The A2A TaskState a hive event-type witnesses. Unknown events are WORKING —
   an agent that said something is, by that fact, working."
  [event-type]
  (let [k (cond
            (nil? event-type) nil
            (keyword? event-type) (name event-type)
            :else (str event-type))]
    (get hive-event->task-state k "TASK_STATE_WORKING")))

;; =============================================================================
;; Parts
;; =============================================================================

(defn text-part
  "A2A TextPart."
  [s]
  {:kind "text" :text (str s)})

(defn data-part
  "A2A DataPart — structured content."
  [m]
  {:kind "data" :data m})

(defn part-text
  "The text a Part carries, or nil for a part that carries none."
  [{:keys [kind text]}]
  (when (= "text" kind) text))

(defn parts-text
  "Concatenate the text of every TextPart in `parts`. Non-text parts contribute
   nothing, so a data-only message yields \"\"."
  [parts]
  (->> parts (keep part-text) (str/join "\n")))

;; =============================================================================
;; Identity
;; =============================================================================

(defn new-message-id
  "A fresh A2A messageId."
  []
  (str "msg-" (random-uuid)))

(defn new-context-id
  "A fresh A2A contextId. Opaque to clients by contract, so the shape is ours:
   a `ctx-` prefix keeps it visually distinct from a messageId in a transcript."
  []
  (str "a2actx-" (random-uuid)))

;; =============================================================================
;; Projection: hive shout <-> A2A Message
;; =============================================================================

(defn- non-blank
  [s]
  (when (and (string? s) (not (str/blank? s))) s))

(defn hive->a2a
  "Project an internal shout map onto an A2A Message.

   The shout shape is the one hive-mcp.hivemind.messaging publishes:
   {:agent-id :event-type :message :task :timestamp :project-id :shout-id
    :parent-id :broadcast? :deliberate? :to :context-id :broadcast-reason :ref}.

   Every hive-specific field lands in `metadata` under a `hive.swarm/` key, and
   the message declares `extension-uri` so a reader knows to look there."
  [{:keys [agent-id event-type message task shout-id to context-id
           broadcast? broadcast-reason deliberate? ref timestamp project-id]}]
  (let [body (or (non-blank message) (non-blank task) "")]
    (cond-> {:messageId (or shout-id (new-message-id))
             :role      "ROLE_AGENT"
             :parts     [(text-part body)]
             :extensions [extension-uri]
             :metadata  (cond-> {meta-from (str agent-id)
                                 meta-event (if (keyword? event-type)
                                              (name event-type)
                                              (str event-type))}
                          to               (assoc meta-to (str to))
                          broadcast?       (assoc meta-broadcast true)
                          broadcast-reason (assoc meta-broadcast-reason (str broadcast-reason))
                          deliberate?      (assoc meta-deliberate true)
                          ref              (assoc meta-ref (str ref))
                          timestamp        (assoc "hive.swarm/timestamp" timestamp)
                          project-id       (assoc "hive.swarm/project-id" (str project-id)))}
      context-id (assoc :contextId (str context-id))
      (non-blank task) (assoc :taskId (str task)))))

(defn a2a->hive
  "Project an A2A Message back onto the internal shout shape. Inverse of
   `hive->a2a` for every field that round-trips; a Message from a foreign A2A
   agent (no `hive.swarm/*` metadata) still yields a usable shout whose
   :agent-id is the messageId's sender when known and whose :message is the
   concatenated text."
  [{:keys [messageId contextId taskId parts metadata]}]
  (let [m (or metadata {})]
    (cond-> {:agent-id   (get m meta-from)
             :event-type (or (get m meta-event) "progress")
             :message    (parts-text parts)
             :shout-id   messageId}
      taskId                        (assoc :task taskId)
      contextId                     (assoc :context-id contextId)
      (get m meta-to)               (assoc :to (get m meta-to))
      (get m meta-broadcast)        (assoc :broadcast? true)
      (get m meta-broadcast-reason) (assoc :broadcast-reason (get m meta-broadcast-reason))
      (get m meta-deliberate)       (assoc :deliberate? true)
      (get m meta-ref)              (assoc :ref (get m meta-ref))
      (get m "hive.swarm/timestamp")  (assoc :timestamp (get m "hive.swarm/timestamp"))
      (get m "hive.swarm/project-id") (assoc :project-id (get m "hive.swarm/project-id")))))

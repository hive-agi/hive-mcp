(ns hive-mcp.channel.conversation-inbox
  "Per-agent conversation inbox — piggyback delivery for the Inter-Ling
   Conversation Protocol (decision 20260401222011-493b1a6e).

   Design (Stratification, decision 20260415135102-1d300fdc):
     - Pure storage layer. A bounded atom keyed by agent-id holding
       `{:tell [envelope ...] :ask [envelope ...] :respond [envelope ...]}`.
     - Effect handlers (`hive-mcp.events.effects.conversation/handle-inbox-push`)
       call `push!` for non-NATS delivery; the piggyback drainer
       (`hive-mcp.channel.piggyback-tap/drain-all!`) calls `drain!` to flush.
     - This namespace performs NO transport. NATS publishing lives in
       `hive-mcp.messaging.nats-conversation`.

   Piggyback shape (drained value):
     {:tell    [<tell-envelope> ...]    ;; oldest first
      :ask     [<ask-envelope> ...]
      :respond [<respond-envelope> ...]}

   Each envelope is the raw output of `hivemind.conversation/tell-envelope`,
   `ask-envelope`, or `respond-envelope` — keys :event-type :from :to
   :timestamp + protocol-specific (:message / :question / :ask-id / :answer).

   The renderer in piggyback-tap concatenates this map next to the existing
   ---MEMORY--- and ---HIVEMIND--- blocks under a new ---INBOX--- delimiter."

  (:require [taoensso.timbre :as log]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Storage
;; =============================================================================

(def ^:const default-section-cap
  "Maximum envelopes retained per (agent-id, section) before oldest are dropped.
   Defensive bound — without it, an offline ling could accumulate unbounded
   tells. Tunable via `set-section-cap!`."
  256)

(defonce ^:private *section-cap (atom default-section-cap))

(defn set-section-cap!
  "Override the per-section retention cap. Affects future pushes only."
  [n]
  {:pre [(pos-int? n)]}
  (reset! *section-cap n))

;; The body's own :pre refuses a non-positive cap; the contract leaves it to.
(m/=> set-section-cap! [:=> [:cat :any] [:int {:min 1}]])

(defonce ^{:doc "Map of agent-id -> {:tell [...] :ask [...] :respond [...]}.
   Each section is a vector of envelopes in insertion order. Drained
   atomically by `drain!` and cleared on read."}
  inboxes
  (atom {}))

(def ^:private empty-sections {:tell [] :ask [] :respond []})

;; =============================================================================
;; Helpers (pure)
;; =============================================================================

(defn- envelope->section
  "Map an envelope's :event-type to its inbox section keyword.
   Unknown event-types fall through to :tell so deliveries are never lost."
  [{:keys [event-type]}]
  (case event-type
    :conversation/tell    :tell
    :conversation/ask     :ask
    :conversation/respond :respond
    :tell))

(defn- conj-bounded
  "Append v to vector coll, dropping the oldest entries beyond cap."
  [coll v cap]
  (let [coll (or coll [])
        next (conj coll v)
        n    (count next)]
    (if (> n cap)
      (subvec next (- n cap))
      next)))

;; =============================================================================
;; Public API
;; =============================================================================

(def Sections
  "An agent's inbox: envelopes by section, oldest first. A drained inbox
   carries only its non-empty sections."
  [:map
   [:tell {:optional true} [:vector :map]]
   [:ask {:optional true} [:vector :map]]
   [:respond {:optional true} [:vector :map]]])

(defn push!
  "Append `envelope` to `agent-id`'s inbox in the section determined by its
   :event-type. Idempotency / dedup is the caller's job — pushes always grow
   the section (subject to the per-section cap)."
  [agent-id envelope]
  {:pre [(string? agent-id) (map? envelope)]}
  (let [section (envelope->section envelope)
        cap     @*section-cap]
    (swap! inboxes
           (fn [m]
             (let [sections (get m agent-id empty-sections)
                   updated  (update sections section conj-bounded envelope cap)]
               (assoc m agent-id updated))))
    (log/debug "[conv-inbox] push" agent-id section
               (select-keys envelope [:from :ask-id]))
    true))

;; The body's own :pre refuses a non-string agent or a non-map envelope.
(m/=> push! [:=> [:cat :any :any] [:= true]])

(defn peek*
  "Read a snapshot of the agent's inbox without clearing. Returns the
   sectioned map or nil if the inbox is empty / absent."
  [agent-id]
  (let [m (get @inboxes agent-id)]
    (when (and m (some seq (vals m)))
      m)))

(m/=> peek* [:=> [:cat :any] [:maybe Sections]])

(defn pending?
  "Quick predicate: any pending envelopes for this agent?"
  [agent-id]
  (boolean (peek* agent-id)))

(m/=> pending? [:=> [:cat :any] :boolean])

(defn drain!
  "Atomically read-and-clear the agent's inbox. Returns the sectioned map
   `{:tell [...] :ask [...] :respond [...]}` or nil when nothing is pending.

   Ordering: oldest envelopes first within each section. Sections present
   in the result only if non-empty (callers can `(seq (:tell ...))` etc.)."
  [agent-id]
  (let [drained (atom nil)]
    (swap! inboxes
           (fn [m]
             (let [sections (get m agent-id)]
               (if (or (nil? sections)
                       (every? empty? (vals sections)))
                 m
                 (do
                   (reset! drained
                           (into {}
                                 (filter (fn [[_ v]] (seq v)))
                                 sections))
                   (dissoc m agent-id))))))
    @drained))

(m/=> drain! [:=> [:cat :any] [:maybe Sections]])

(defn clear!
  "Clear a single agent's inbox. Returns true if anything was removed."
  [agent-id]
  (let [hit? (atom false)]
    (swap! inboxes
           (fn [m]
             (if (contains? m agent-id)
               (do (reset! hit? true) (dissoc m agent-id))
               m)))
    @hit?))

(m/=> clear! [:=> [:cat :any] :boolean])

(defn reset-all!
  "Drop every inbox. For tests."
  []
  (reset! inboxes {}))

(m/=> reset-all! [:=> [:cat] [:= {} {}]])

(defn agent-count
  "How many agents currently have inboxes registered?"
  []
  (count @inboxes))

(m/=> agent-count [:=> [:cat] [:int {:min 0}]])

(defn pending-count
  "Total envelopes pending for `agent-id` across all sections."
  [agent-id]
  (->> (get @inboxes agent-id) vals (map count) (reduce + 0)))

(m/=> pending-count [:=> [:cat :any] [:int {:min 0}]])

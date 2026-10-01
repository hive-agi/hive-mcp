(ns hive-mcp.channel.piggyback.sources
  "Open registry of piggyback message SOURCES: the port through which the
   piggyback channel learns what has been said without knowing who said it.

   Core piggyback is a READER. It merges, cursors, routes and formats rows;
   where the rows come from is not its business. The swarm's hivemind ring is
   one source, a test stub is another, an addon's own feed would be a third.
   Each is ONE registration here, keyed by a SourceId, so adding source N+1 is
   a `register!` and never an edit to the reader (OCP). Core works with ZERO
   sources: a read over an empty registry is an empty read, not an error.

   Strata (CPPB):
     Value objects  SourceId, SourceMessage, SourceOutcome, Collected
     Port           IPiggybackSource, one method (ISP)
     Collect        registered: a snapshot of the registry
     Pipeline       pull: invoke ONE source on the hive-dsl railway
     Promote        fold-outcomes: PURE, outcomes -> Collected
     Boundary       messages-from / collect-messages: run, log, yield rows

   A source that throws is RESCUED on its own: its failure is logged and the
   read carries every other source's rows. One broken contributor must never
   take down the tools/call it rides on.

   Capture-by-Var: a source registered as a VAR (#'f) is dereffed on every
   read, so a reload of that var is seen by the very next read. A fn VALUE is
   accepted too (LSP: every IPiggybackSource is substitutable for any other),
   but it is frozen until the next register!, so hot-reloadable code registers
   the var."
  (:require [hive-dsl.result :as r]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Value objects
;; =============================================================================

(def SourceId
  "Names one registration. Namespaced by the contributor, e.g.
   :piggyback.source/default or :my-addon/feed."
  :keyword)

(def SourceMessage
  "One row a source yields: the shape hive-mcp.channel.piggyback/get-messages
   reads. Only :agent-id and :timestamp are load-bearing for the cursor; the
   routing keys ride along when the author set them."
  [:map
   [:agent-id some?]
   [:timestamp int?]
   [:event-type {:optional true} some?]
   [:message {:optional true} [:maybe string?]]
   [:project-id {:optional true} [:maybe string?]]
   [:shout-id {:optional true} string?]])

(def SourceOutcome
  "What pulling one source produced: its id and a hive-dsl Result."
  [:map
   [:source-id SourceId]
   [:result [:or [:map [:ok sequential?]] [:map [:error keyword?]]]]])

(def Collected
  "The folded read: every row from every healthy source, and one failure
   record per source that could not be read."
  [:map
   [:messages [:vector map?]]
   [:failures [:vector [:map [:source-id SourceId] [:error keyword?]]]]])

;; =============================================================================
;; Port
;; =============================================================================

(defprotocol IPiggybackSource
  (-messages [source]
    "Every message this source currently holds, as a seq of SourceMessage.
     Called once per read. May throw: the pipeline rescues per source."))

(extend-protocol IPiggybackSource
  nil
  (-messages [_] nil)

  ;; Capture-by-Var: deref on EVERY read, so a reload is observed next read.
  clojure.lang.Var
  (-messages [v] (-messages (deref v)))

  ;; A slot (atom) holding a source: the legacy single-slot API is one.
  clojure.lang.IDeref
  (-messages [d] (-messages (deref d)))

  clojure.lang.Fn
  (-messages [f] (f)))

;; =============================================================================
;; Registry (Collect)
;; =============================================================================

(defonce ^{:private true
           :doc "SourceId -> IPiggybackSource. Survives reloads of this ns."}
  registry
  (atom {}))

(defn register!
  "Register SOURCE under ID, replacing any source already there. Idempotent.
   Register a var (#'f) for anything that must survive a hot reload.
   Returns ID."
  [id source]
  (swap! registry assoc id source)
  id)

(defn deregister!
  "Remove the source under ID. A no-op when none is there. Returns ID."
  [id]
  (swap! registry dissoc id)
  id)

(defn registered
  "Snapshot of the registry, sorted by SourceId so a read's row order is
   deterministic across calls."
  []
  (into (sorted-map) @registry))

;; =============================================================================
;; Pipeline — one source, on the railway
;; =============================================================================

(defn pull
  "Invoke one [id source] entry. -> SourceOutcome. Never throws: a source
   that throws becomes an :piggyback.source/failed Result."
  [[id source]]
  {:source-id id
   :result (r/try-effect* :piggyback.source/failed
             (vec (-messages source)))})

;; =============================================================================
;; Promote — pure
;; =============================================================================

(defn fold-outcomes
  "Fold SourceOutcomes into Collected. Pure. Rows keep source order, then
   row order within a source; a failed source contributes a failure record
   and no rows."
  [outcomes]
  (reduce (fn [acc {:keys [source-id result]}]
            (if (r/ok? result)
              (update acc :messages into (:ok result))
              (update acc :failures conj (assoc result :source-id source-id))))
          {:messages [] :failures []}
          outcomes))

;; =============================================================================
;; Boundary
;; =============================================================================

(defn messages-from
  "Read every source in SOURCES (a map or seq of [id source]) and return the
   merged rows. Each failure is logged at warn; it never reaches the caller."
  [sources]
  (let [{:keys [messages failures]} (fold-outcomes (map pull sources))]
    (doseq [{:keys [source-id message]} failures]
      (log/warn "piggyback source failed; read continues without it"
                {:source-id source-id :message message}))
    messages))

(defn collect-messages
  "Rows from every registered source. [] when none is registered."
  []
  (messages-from (registered)))

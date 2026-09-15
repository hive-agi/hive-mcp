(ns hive-mcp.channel.audience
  "Audience routing and progress digest for the HIVEMIND piggyback channel.

   Pure calculations: no I/O, no state, no requires beyond clojure.string.
   Two questions:

   - `addressed-to?` / `filter-messages`: does a shout belong in THIS
     reader's context?
   - `digest`: collapse a burst of per-turn :progress rows into one row.

   Delivery contract. A shout reaches the agent that SPAWNED the shouter and
   nobody else:

     :broadcast? true  -> every reader
     author = reader   -> never (a ling does not read back its own shout;
                          coordinator lanes are exempt so the wave scheduler
                          still sees the events it authors)
     :parent-id set    -> that reader alone
     :parent-id absent -> root-level, coordinator readers only

   A coordinator lane is ONE Claude window. The MCP transport spells it
   `coordinator:<session>`, optionally suffixed `-<project>`, and two lanes
   with different sessions are different readers: what one window's lings
   say never enters another window's context. A lane spelled without a
   session (`coordinator`, `coordinator-hive`: the legacy and Emacs paths)
   still matches every lane, so nothing that used to arrive stops arriving.

   Digest contract. Rows whose :e is digestible collapse, per agent, into ONE
   row carrying the burst count under :n and the LAST message under :m, sitting
   at the position of that agent's last such row. Every other event (started,
   completed, error, aborted, ask) passes through verbatim and in place."
  (:require [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Reader identity
;; =============================================================================

(def ^:const coordinator-prefix
  "Prefix every coordinator-lane reader id carries."
  "coordinator")

(defn coordinator-reader?
  "True when `reader-id` names a coordinator lane. The MCP lane derives its
   reader id as \"coordinator\" or \"coordinator-<project-id>\" (see
   hive-dsl.context.identity/make-piggyback-agent-id), so this is a prefix
   test, not equality. nil counts as a coordinator."
  [reader-id]
  (or (nil? reader-id)
      (str/starts-with? (str reader-id) coordinator-prefix)))

(defn coordinator-session
  "The session token of a coordinator-lane id, or nil when the id names the
   lane without one (\"coordinator\", \"coordinator-hive\"). The MCP lane
   spells a session `coordinator:<session>` and may suffix `-<project>`; a
   session token never contains a dash
   (hive-dsl.context.identity/session-id-shape?), so the first dash ends it."
  [id]
  (when (and (some? id) (coordinator-reader? id))
    (second (re-find #"^coordinator:([^-]+)" (str id)))))

(defn same-agent?
  "Do two ids name the same reader? Tolerates the coordinator lane's
   \"-<project>\" suffixing, so a shout whose :parent-id is
   \"coordinator:7\" reaches reader \"coordinator:7-hive\". Two coordinator
   ids carrying DIFFERENT sessions are different readers. An id that names
   the lane with no session at all still matches every lane."
  [reader-id other-id]
  (let [r (str reader-id)
        o (str other-id)]
    (or (= r o)
        (and (coordinator-reader? r)
             (coordinator-reader? o)
             (let [rs (coordinator-session r)
                   os (coordinator-session o)]
               (or (nil? rs) (nil? os) (= rs os)))))))

;; =============================================================================
;; Audience
;; =============================================================================

(defn addressed-to?
  "Is `msg` part of `reader-id`'s audience? First rule that matches wins; see
   the namespace docstring for the contract."
  [reader-id {:keys [agent-id parent-id broadcast?]}]
  (let [coord? (coordinator-reader? reader-id)]
    (cond
      broadcast? true
      (and (not coord?) (same-agent? reader-id agent-id)) false
      (some? parent-id) (same-agent? reader-id parent-id)
      :else coord?)))

(defn filter-messages
  "Keep only the messages addressed to `reader-id`, in order. Returns a vector."
  [reader-id msgs]
  (filterv #(addressed-to? reader-id %) msgs))

;; =============================================================================
;; Digest
;; =============================================================================

(def digestible-events
  "Event names whose bursts collapse into a rollup row."
  #{"progress"})

(defn- digestible?
  "A row the digest may fold into its agent's rollup: a digestible event the
   agent did NOT shout deliberately. A :deliberate? row is what the agent
   chose to say; folding it into the runtime's per-turn telemetry is how a
   reader loses the one message that mattered."
  [row]
  (and (contains? digestible-events (some-> (:e row) name))
       (not (:deliberate? row))))

(defn digest
  "Collapse per-agent bursts of digestible rows into one row each.

   Rows are the formatted piggyback shape {:a agent :e event :m message
   :t task}; a collapsed row gains :n, the number of rows it stands for. A
   burst of one is left untouched — :n only appears where something was
   actually dropped."
  [rows]
  (let [rows (vec rows)
        indexed (map-indexed vector rows)
        last-idx (reduce (fn [acc [i row]]
                           (if (digestible? row) (assoc acc (:a row) i) acc))
                         {} indexed)
        counts (frequencies (keep #(when (digestible? %) (:a %)) rows))]
    (into []
          (keep-indexed
           (fn [i row]
             (cond
               (not (digestible? row)) row
               (= i (get last-idx (:a row)))
               (let [n (get counts (:a row) 1)]
                 (cond-> row (> n 1) (assoc :n n)))
               :else nil)))
          rows)))

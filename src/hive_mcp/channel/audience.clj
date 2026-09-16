(ns hive-mcp.channel.audience
  "Audience routing and progress digest for the HIVEMIND piggyback channel.

   Pure calculations: no I/O, no state, no requires beyond clojure.string.
   Three questions:

   - `addressed-to?` / `filter-messages`: does a shout belong in THIS
     reader's context?
   - `peer-traffic-digest`: what does a coordinator learn about the directed
     traffic it is deliberately not shown?
   - `digest`: collapse a burst of per-turn :progress rows into one row.

   Delivery contract. First rule that matches wins:

     :to set           -> that agent ALONE. Not the coordinator, not the
                          sender's spawner. Naming a recipient is an act of
                          address, and the value of naming one is that nobody
                          else pays for the message.
     :broadcast? true  -> every reader. Reaching this rule at all means
                          hive-mcp.channel.broadcast-policy admitted an
                          argument for it; an unargued broadcast was already
                          downgraded before delivery.
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

   Supervision contract. Directed messages not reaching the coordinator is the
   saving; a coordinator that cannot see THAT its peers are talking has lost
   oversight, which is too high a price. `peer-traffic-digest` gives it one
   capped row per conversation naming the turn count, the peers, and the
   contextId to fetch the exchange by.

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
   the lane with no session at all still matches every lane.

   `scoped-id`, when given, is a 1-arg fn composing a bare agent name onto
   this read's project scope. EVERY reader id carries that suffix, not only a
   coordinator's, so without it a ling cannot match its own name. The scope is
   COMPOSED onto `other-id` rather than stripped off `reader-id`: composition
   is total, while stripping cannot tell a ling named \"worker\" in project
   \"p\" from one named \"worker-p\"."
  ([reader-id other-id] (same-agent? reader-id other-id nil))
  ([reader-id other-id scoped-id]
   (let [r (str reader-id)
         o (str other-id)]
     (or (= r o)
         (and scoped-id (= r (str (scoped-id o))))
         (and (coordinator-reader? r)
              (coordinator-reader? o)
              (let [rs (coordinator-session r)
                    os (coordinator-session o)]
                (or (nil? rs) (nil? os) (= rs os))))))))

;; =============================================================================
;; Audience
;; =============================================================================

(defn addressed-to?
  "Is `msg` part of `reader-id`'s audience? First rule that matches wins; see
   the namespace docstring for the contract.

   `scoped-id` composes a bare agent name onto this read's project scope, and
   is consulted for the DIRECTED rule alone. Spawner and self-echo matching
   already resolve without it, and widening them changes who reads whose
   turns; addressing is the one rule a project-suffixed reader could not
   satisfy for itself."
  ([reader-id msg] (addressed-to? reader-id msg nil))
  ([reader-id {:keys [agent-id parent-id broadcast? to]} scoped-id]
   (let [coord? (coordinator-reader? reader-id)]
     (cond
       ;; DIRECTED beats every other rule. Naming a recipient is an act of
       ;; address, and the whole value of naming one is that nobody else pays
       ;; for the message -- not the coordinator, not the sender's spawner.
       (some? to) (same-agent? reader-id to scoped-id)
       broadcast? true
       (and (not coord?) (same-agent? reader-id agent-id)) false
       (some? parent-id) (same-agent? reader-id parent-id)
       :else coord?))))

(defn filter-messages
  "Keep only the messages addressed to `reader-id`, in order. Returns a vector.
   `scoped-id` composes a bare agent name onto this read's project scope; see
   `same-agent?`."
  ([reader-id msgs] (filter-messages reader-id msgs nil))
  ([reader-id msgs scoped-id]
   (filterv #(addressed-to? reader-id % scoped-id) msgs)))

(defn directed?
  "Does this message name a recipient?"
  [msg]
  (some? (:to msg)))

(def max-peer-traffic-rows
  "How many conversations the coordinator's peer-traffic summary names before it
   starts folding the rest into a single overflow row.

   Without a cap the summary costs one row per CONVERSATION, so a swarm that
   opens a fresh context per message hands the coordinator a row per message —
   which is the linear cost directed addressing exists to remove, reintroduced
   one level up. Measured 2026-09-15: 48 single-message contexts produced 48
   rows and ate most of the saving. The cap makes the coordinator's share O(1)
   in the number of conversations."
  5)

(defn peer-traffic-digest
  "The coordinator's view of directed traffic it is not addressed by.

   A directed message never enters the coordinator's context; that omission IS
   the saving. But a coordinator that cannot see THAT its peers are talking has
   lost supervision, which is too high a price. So it gets one row per
   conversation instead of the conversation: how many turns, between whom, and
   the contextId to fetch the exchange by if it wants to.

   At most `max-peer-traffic-rows` conversations are named; the remainder
   collapse into one overflow row carrying the totals, so the summary's cost
   does not grow with the swarm's chattiness. The busiest conversations are the
   ones named — a conversation with more turns is the one more likely to want
   looking at.

   Returns [] for a non-coordinator reader, and for a coordinator with no such
   traffic."
  [reader-id msgs]
  (if-not (coordinator-reader? reader-id)
    []
    (let [unseen (remove #(addressed-to? reader-id %) (filter directed? msgs))
          groups (->> unseen
                      (group-by #(or (:context-id %) ::unscoped))
                      (sort-by (fn [[k rows]] [(- (count rows)) (str k)])))
          named (take max-peer-traffic-rows groups)
          overflow (drop max-peer-traffic-rows groups)
          row (fn [[ctx rows]]
                (let [n (count rows)
                      peers (->> rows
                                 (mapcat (juxt :agent-id :to))
                                 (remove nil?)
                                 distinct
                                 sort
                                 vec)]
                  (cond-> {:a "swarm"
                           :e "peer-traffic"
                           :m (str n " directed " (if (= 1 n) "message" "messages")
                                   " among " (str/join ", " peers))
                           :n n}
                    (not= ::unscoped ctx) (assoc :ctx ctx))))]
      (cond-> (mapv row named)
        (seq overflow)
        (conj (let [convs (count overflow)
                    n (reduce + 0 (map (comp count second) overflow))]
                {:a "swarm"
                 :e "peer-traffic"
                 :m (str n " more directed messages across " convs " other conversations")
                 :n n}))))))

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

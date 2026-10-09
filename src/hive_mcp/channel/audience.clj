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
     :parent-id absent -> root-level, coordinator readers only, and only
                          when the reader owns it (`root-visible?`): its
                          author is that same session, it is scoped to the
                          reader's project, the reader lane names no
                          session, or the operator opted into unowned
                          shouts. An unowned (\"global\" or nil project)
                          root shout from outside the reader's lineage
                          reaches NOBODY.

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
   this read's project scope; it is applied to `other-id`."
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

(def unowned-project
  "Project id a shout carries when nothing scoped it: no explicit project, no
   registered vessel cwd, no directory (hive-mcp.hivemind.messaging/shout!*).
   A nil :project-id reads the same."
  "global")

(defn unowned?
  "Does `msg` belong to no project? Pure. nil and \"global\" both count."
  [msg]
  (contains? #{nil unowned-project} (:project-id msg)))

(defn root-visible?
  "May a ROOT-level shout (no :to, no :broadcast?, no :parent-id) reach
   coordinator reader `reader-id`? Pure. First rule that matches wins:

     reader names no session  -> yes. The legacy and Emacs lanes
                                 (\"coordinator\", \"coordinator-hive\") have
                                 no session to scope by, so they keep the old
                                 behaviour.
     author is this session   -> yes. A coordinator still sees what it shouted
                                 itself (the wave scheduler).
     shout is project-scoped  -> yes. piggyback/get-messages already admitted
                                 it ONLY because its project is the reader's
                                 project or an HCR descendant of it.
     global-opt-in?           -> yes. The operator asked for unowned shouts
                                 ([:hivemind :unowned-global] :deliver).
     otherwise                -> no. An unowned shout from an agent outside
                                 this session's lineage reaches nobody. This
                                 is the HIVEMIND-PIGGYBACK-LEAK fix: before it,
                                 a \"team:<id>\" runner's shout or a parentless
                                 ling's shout in project \"global\" reached
                                 every coordinator window."
  [reader-id {:keys [agent-id] :as msg} global-opt-in?]
  (boolean
   (or (nil? (coordinator-session reader-id))
       (and (coordinator-reader? agent-id) (same-agent? reader-id agent-id))
       (not (unowned? msg))
       global-opt-in?)))

(defn addressed-to?
  "Is `msg` part of `reader-id`'s audience? First rule that matches wins; see
   the namespace docstring for the contract.

   `scoped-id` composes a bare agent name onto this read's project scope, and
   is consulted for the DIRECTED rule only. `opts` may carry
   :global-opt-in? (see `root-visible?`)."
  ([reader-id msg] (addressed-to? reader-id msg nil nil))
  ([reader-id msg scoped-id] (addressed-to? reader-id msg scoped-id nil))
  ([reader-id {:keys [agent-id parent-id broadcast? to] :as msg} scoped-id opts]
   (let [coord? (coordinator-reader? reader-id)]
     (cond
       ;; DIRECTED beats every other rule. Naming a recipient is an act of
       ;; address, and the whole value of naming one is that nobody else pays
       ;; for the message -- not the coordinator, not the sender's spawner.
       (some? to) (same-agent? reader-id to scoped-id)
       broadcast? true
       (and (not coord?) (same-agent? reader-id agent-id)) false
       (some? parent-id) (same-agent? reader-id parent-id)
       :else (and coord? (root-visible? reader-id msg (:global-opt-in? opts)))))))

(defn filter-messages
  "Keep only the messages addressed to `reader-id`, in order. Returns a vector.
   `scoped-id` composes a bare agent name onto this read's project scope; see
   `same-agent?`. `opts` is passed to `addressed-to?`."
  ([reader-id msgs] (filter-messages reader-id msgs nil nil))
  ([reader-id msgs scoped-id] (filter-messages reader-id msgs scoped-id nil))
  ([reader-id msgs scoped-id opts]
   (filterv #(addressed-to? reader-id % scoped-id opts) msgs)))

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

(defn in-lineage?
  "Was `msg` sent by `reader-id` or by an agent `reader-id` spawned? Pure.
   A shout carries its author's spawner as :parent-id
   (hive-mcp.hivemind.messaging/shout!*), so one hop is visible on the row.
   A reader that names no coordinator session (\"coordinator\",
   \"coordinator-hive\") cannot be told apart from any other lane, so it owns
   every lineage, as before."
  [reader-id {:keys [agent-id parent-id]}]
  (boolean
   (or (nil? (coordinator-session reader-id))
       (and (some? agent-id) (same-agent? reader-id agent-id))
       (and (some? parent-id) (same-agent? reader-id parent-id)))))

(defn supervised-conversations
  "Directed messages `reader-id` is NOT addressed by, grouped by conversation,
   kept only for conversations at least one of whose messages is in the
   reader's lineage (`in-lineage?`). Another coordinator's peers talking among
   themselves is not this reader's to supervise. Pure.
   -> seq of [context-id-or-::unscoped rows], busiest first."
  [reader-id msgs]
  (->> (filter directed? msgs)
       (remove #(addressed-to? reader-id %))
       (group-by #(or (:context-id %) ::unscoped))
       (filter (fn [[_ rows]] (some #(in-lineage? reader-id %) rows)))
       (sort-by (fn [[k rows]] [(- (count rows)) (str k)]))))

(defn peer-traffic-digest
  "The coordinator's view of directed traffic it is not addressed by.

   A directed message never enters the coordinator's context; that omission IS
   the saving. But a coordinator that cannot see THAT its peers are talking has
   lost supervision, which is too high a price. So it gets one row per
   conversation instead of the conversation: how many turns, between whom, and
   the contextId to fetch the exchange by if it wants to.

   Only conversations in the reader's lineage are named
   (`supervised-conversations`): a conversation between another coordinator's
   lings is not summarised to this one.

   At most `max-peer-traffic-rows` conversations are named; the remainder
   collapse into one overflow row carrying the totals, so the summary's cost
   does not grow with the swarm's chattiness. The busiest conversations are the
   ones named \u2014 a conversation with more turns is the one more likely to want
   looking at.

   Returns [] for a non-coordinator reader, and for a coordinator with no such
   traffic."
  [reader-id msgs]
  (if-not (coordinator-reader? reader-id)
    []
    (let [groups (supervised-conversations reader-id msgs)
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
   actually dropped.

   When the rows carry :ts, a collapsed row keeps the EARLIEST :ts of its
   burst rather than its own, so it says how far back the history it stands
   for reaches."
  [rows]
  (let [rows (vec rows)
        indexed (map-indexed vector rows)
        last-idx (reduce (fn [acc [i row]]
                           (if (digestible? row) (assoc acc (:a row) i) acc))
                         {} indexed)
        counts (frequencies (keep #(when (digestible? %) (:a %)) rows))
        min-ts (reduce (fn [acc row]
                         (if-let [ts (and (digestible? row) (:ts row))]
                           (update acc (:a row) (fnil min ts) ts)
                           acc))
                       {} rows)]
    (into []
          (keep-indexed
           (fn [i row]
             (cond
               (not (digestible? row)) row
               (= i (get last-idx (:a row)))
               (let [n (get counts (:a row) 1)
                     ts (get min-ts (:a row))]
                 (cond-> row
                   (> n 1) (assoc :n n)
                   (and (> n 1) ts) (assoc :ts ts)))
               :else nil)))
          rows)))

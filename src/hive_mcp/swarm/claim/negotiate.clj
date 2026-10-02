(ns hive-mcp.swarm.claim.negotiate
  "How a ling takes its files: all at once or not at all, and when refused,
   it waits in line and asks the holder to let go.

   WHY THIS EXISTS
   ===============
   `Ling.claim-files!` used to check and claim one file at a time. The check
   and the write were separate steps, and the store's claim-file! UPSERTS, so
   a claim that skipped or raced the check silently took another ling's file
   (SWARM-CLAIM-STEAL). The span registry already had the right primitive,
   `registry/acquire!`: every span checked and claimed under the one claim
   lock, all or none. Nothing in production called it (SWARM-SPAN-WIRE).

   WHAT A REFUSAL DOES
   ===================
   1. Nothing is claimed. Holding half a task's files blocks others for no gain.
   2. The ling is parked in the wait queue on each held key, so the release
      path (:claim/file-released -> :claim/notify-waiting) wakes it with a
      directed :file-available when the holder lets go.
   3. Each holder gets ONE directed :claim/yield-request, sent by the refused
      ling. It is a tell, not an ask: nobody blocks on an answer, and the
      holder's normal release is what the requester is waiting for. Repeats
      for the same (holder, key, requester) are suppressed until the requester
      acquires or `yield-ttl-ms` passes, so a retry loop does not spam.

   CROSS-WORKTREE
   ==============
   When the loaded hive-agent can tell worktrees apart (`span/context-for`
   returns a ctx), the same file held through another checkout is a warning,
   not a refusal. The claimant is told with a directed :claim-warning naming
   the holder, and the claim proceeds."
  (:require [clojure.string :as str]
            [hive-mcp.hivemind.core :as hivemind]
            [hive-mcp.swarm.claim.registry :as registry]
            [hive-mcp.swarm.claim.span :as span]
            [hive-mcp.swarm.datascript.lings :as lings]
            [taoensso.timbre :as log]
            [hive-mcp.swarm.delegate :as delegate]
            [hive-mcp.swarm.datascript :as ds]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Directed messages
;; =============================================================================

(defn tell!
  "Send `data` from `sender` to exactly one peer, `to`.

   `hivemind/shout!` takes the SENDER first and addresses a peer through :to;
   the piggyback reader and `audience/addressed-to?` both read :to."
  [sender to event-type data]
  (hivemind/shout! sender event-type (assoc data :to to)))

;; =============================================================================
;; Yield-request ledger (dedup)
;; =============================================================================

(def yield-ttl-ms
  "How long one yield request stands before the same one may be sent again.
   Matches the claim staleness window: past it the holder's claim is stale and
   a fresh reminder is news, not noise."
  (* 10 60 1000))

(defonce ^:private yield-ledger
  ;; {[holder key requester] sent-at-ms}
  (atom {}))

(defn reset-yield-ledger!
  "Forget every yield request sent. Test seam and operator reset."
  []
  (reset! yield-ledger {}))

(defn- prune
  "The ledger without entries older than `yield-ttl-ms` at `now`."
  [ledger now]
  (into {} (remove (fn [[_ sent]] (> (- now sent) yield-ttl-ms))) ledger))

(defn- admit
  "Pure: the ledger after recording `k` at `now`, unless it already stands."
  [ledger k now]
  (let [live (prune ledger now)]
    (if (contains? live k) live (assoc live k now))))

(defn- first-ask?
  "Record `k` and say whether this is the first standing request for it."
  [k now]
  (let [[before _] (swap-vals! yield-ledger admit k now)]
    (not (contains? (prune before now) k))))

(defn- forget-yields!
  "Drop the standing requests `requester` made: it acquired, so a later
   refusal is a new episode that deserves its own request."
  [requester]
  (swap! yield-ledger
         (fn [ledger]
           (into {} (remove (fn [[[_ _ r] _]] (= r requester))) ledger))))

;; =============================================================================
;; Pieces of a refusal
;; =============================================================================

(defn held-key
  "The claim key a conflict was reported against, i.e. the key whose release
   fires :claim/file-released. A whole-file hold keys on the path, a form hold
   on `path#qn`."
  [{:keys [file qn mode]}]
  (span/key-of (if (and qn (not= :file mode))
                 (span/form-span file qn mode)
                 (span/file-span file))))

(defn- holds
  "Distinct {:holder :key} pairs behind a refusal's conflicts."
  [conflicts]
  (into [] (comp (map (fn [c] {:holder (:held-by c) :key (held-key c)}))
                 (distinct))
        conflicts))

(defn- park!
  "Put `ling-id` in the wait queue on every held key."
  [ling-id hold-pairs]
  (let [ks (into [] (comp (map :key) (distinct)) hold-pairs)]
    (doseq [k ks]
      (lings/add-to-wait-queue! ling-id k))
    ks))

(defn- request-yields!
  "One :claim/yield-request per holder and key, deduplicated. Returns the
   requests actually sent."
  [notify! ling-id task-id hold-pairs now]
  (into []
        (keep (fn [{:keys [holder key]}]
                (when (and holder (not= holder ling-id)
                           (first-ask? [holder key ling-id] now))
                  (notify! ling-id holder :claim/yield-request
                           {:file         key
                            :requested-by ling-id
                            :task-id      task-id
                            :message      (str ling-id " is waiting on " key
                                               ". Release it when you can;"
                                               " your release wakes them.")})
                  {:holder holder :file key})))
        hold-pairs))

(defn- warn-claimant!
  "Tell `ling-id` about holds on its files in other worktrees. Never blocks."
  [notify! ling-id task-id warnings]
  (when (seq warnings)
    (notify! "coordinator" ling-id :claim-warning
             {:task-id  task-id
              :warnings (mapv #(select-keys % [:file :qn :held-by :worktree
                                               :reason :underlying :message])
                              warnings)
              :message  (str/join " " (map :message warnings))})))

;; =============================================================================
;; Deadlock
;; =============================================================================

(defn- waits-impl
  "hive-agent.swarm.claim.waits/`sym` when the loaded hive-agent defines it,
   else nil."
  [sym]
  (when (delegate/available? "hive-agent.swarm.claim.waits")
    (requiring-resolve (symbol "hive-agent.swarm.claim.waits" (name sym)))))

(defn- held-facts
  "Live claims as {:key :agent} facts."
  []
  (mapv (fn [row] {:key (:claim/key row) :agent (:claim/slave row)})
        (registry/held-spans)))

(defn- waiting-facts
  "Wait-queue rows as {:key :agent} facts."
  []
  (mapv (fn [[ling k]] {:key k :agent ling})
        (ds/q-db (ds/current-db)
                 '[:find ?ling ?file
                   :where
                   [?w :wait-queue/ling-id ?ling]
                   [?w :wait-queue/file ?file]])))

(defn foresee-deadlock
  "{:cycle [agent ...] :victim agent} when parking `ling-id` on `ks` closes a
   wait cycle, else nil. nil too when the loaded hive-agent has no wait graph."
  [ling-id ks]
  (when-let [would-deadlock (and (seq ks) (waits-impl 'would-deadlock))]
    (let [held    (held-facts)
          waiting (waiting-facts)]
      (when-let [cycle (would-deadlock held waiting ling-id ks)]
        {:cycle  cycle
         :victim ((waits-impl 'victim)
                  cycle held
                  (into waiting (map (fn [k] {:key k :agent ling-id})) ks))}))))

(defn- announce-deadlock!
  "Tell the victim of `deadlock` to release, once per standing cycle."
  [notify! ling-id task-id {:keys [cycle victim] :as deadlock} now]
  (when (and deadlock
             (first-ask? [victim [:deadlock (vec (sort (map str cycle)))] ling-id] now))
    (notify! "coordinator" victim :claim/deadlock
             {:cycle        cycle
              :victim       victim
              :requested-by ling-id
              :task-id      task-id
              :message      (str "Claim deadlock: " (str/join " waits on " cycle)
                                 " waits on " (first cycle) ". " victim
                                 ", release your claims now and claim again"
                                 " when the others are done.")})))

;; =============================================================================
;; Entry point
;; =============================================================================

(defn acquire-for-ling!
  "Claim `files` for `ling-id`, all or nothing.

   opts:
     :task-id  the task the claims belong to (released with it)
     :cwd      the ling's working directory; anchors cross-worktree identity
               when the loaded hive-agent supports it, ignored otherwise
     :notify!  (fn [sender to event-type data]), default `tell!`
     :now-ms   clock, for the dedup ledger

   Returns registry/acquire!'s map. A refusal also carries :parked (keys the
   ling now waits on) and :yield-requested ([{:holder :file}] sent this call).
   When parking would close a wait cycle it carries :deadlock {:cycle :victim}
   too; a requester that is the victim is not parked."
  [ling-id files & [{:keys [task-id cwd notify! now-ms]
                     :or   {notify! tell!}}]]
  (if (empty? files)
    {:acquired? true :conflicts [] :warnings [] :spans-claimed 0}
    (let [ctx    (when cwd (span/context-for cwd))
          result (registry/acquire! (vec files) ling-id {:task-id task-id :ctx ctx})]
      (warn-claimant! notify! ling-id task-id (:warnings result))
      (if (:acquired? result)
        (do (forget-yields! ling-id)
            result)
        (let [now      (or now-ms (System/currentTimeMillis))
              pairs    (holds (:conflicts result))
              deadlock (foresee-deadlock ling-id (mapv :key pairs))
              yield?   (= ling-id (:victim deadlock))
              parked   (if yield? [] (park! ling-id pairs))
              asked    (if yield? [] (request-yields! notify! ling-id task-id pairs now))]
          (announce-deadlock! notify! ling-id task-id deadlock now)
          (log/warn "claim refused"
                    {:ling-id ling-id :task-id task-id :parked parked
                     :holders (mapv :holder pairs)
                     :yield-requested (count asked)
                     :deadlock deadlock})
          (cond-> (assoc result :parked parked :yield-requested asked)
            deadlock (assoc :deadlock deadlock)))))))

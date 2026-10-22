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
            [hive-mcp.swarm.datascript :as ds]
            [malli.core :as m]
            [hive-dsl.context.identity :as identity]
            [hive-mcp.swarm.claim.graph :as graph])
(:import [java.nio.file Path]
[java.nio.file Paths]))
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

(def NotifyFn
  "The directed-message port: (fn [sender to event-type data])."
  [:=> [:cat :string :string :keyword :map] :any])

;; boundary: hivemind/shout!'s answer, true once the shout is recorded
(m/=> tell! [:=> [:cat :string :string :keyword :map] :boolean])

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

;; the emptied ledger, i.e. what reset! returns
(m/=> reset-yield-ledger! [:=> [:cat] [:map-of {:max 0} :any :any]])

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

(m/=> held-key [:=> [:cat [:map
                           [:file :string]
                           [:qn {:optional true} [:maybe :string]]
                           [:mode {:optional true} [:maybe span/Mode]]]]
                :string])

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

(defn request-yields-for!
  "Send the deduplicated :claim/yield-request to each holder behind
   `conflicts` on behalf of `requester`, WITHOUT parking it in the wait queue.

   For a caller that must return at once (the carto write gate): a tell, never
   an ask, so nothing blocks. Returns the requests actually sent
   ([{:holder :file}]); a repeat within `yield-ttl-ms` is suppressed.

   opts: :task-id, :notify! (default `tell!`), :now-ms."
  [requester conflicts & [{:keys [task-id notify! now-ms] :or {notify! tell!}}]]
  (request-yields! notify! requester task-id (holds conflicts)
                   (or now-ms (System/currentTimeMillis))))

(def YieldSent
  "One yield request actually sent: the holder asked and the key it holds."
  [:map [:holder :string] [:file :string]])

;; boundary: tells each holder through notify!; the requests actually sent
(m/=> request-yields-for!
      [:function
       [:=> [:cat :string [:sequential span/Conflict]] [:vector YieldSent]]
       [:=> [:cat :string [:sequential span/Conflict]
             [:maybe [:map
                      [:task-id {:optional true} [:maybe :string]]
                      [:notify! {:optional true} NotifyFn]
                      [:now-ms {:optional true} [:maybe :int]]]]]
        [:vector YieldSent]]])

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

(def Deadlock
  "A wait cycle parking would close, and the agent asked to yield."
  [:map [:cycle [:sequential :string]] [:victim :string]])

;; boundary: reads the claim and wait-queue rows
(m/=> foresee-deadlock [:=> [:cat :string [:maybe [:sequential :string]]] [:maybe Deadlock]])

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

(def AcquireForLing
  "registry/acquire!'s map, plus what a refusal did about it."
  [:map
   [:acquired? :boolean]
   [:conflicts [:vector span/Conflict]]
   [:warnings [:vector :map]]
   [:spans-claimed [:int {:min 0}]]
   [:parked {:optional true} [:vector :string]]
   [:yield-requested {:optional true} [:vector [:map [:holder :string] [:file :string]]]]
   [:deadlock {:optional true} Deadlock]])

;; =============================================================================
;; Write gate
;; =============================================================================
;;
;; carto asks `check-write!` before it writes a form; the answer is a claim
;; taken on the writer's behalf. hive-carto resolves
;; `hive-mcp.swarm.claim.negotiate/check-write!` by name; keep it stable.
;;
;;   (check-write! {:agent-id "ling-x:12345" :cwd "/abs/dir" :scope "hive-carto"
;;                  :op :write-form
;;                  :spans [{:file "/abs/x.clj" :qn "ns/f" :mode :body}]})
;;   => {:allowed? true  :acquired n :warnings [...] :lease-ms n}
;;    | {:allowed? false :conflicts [{:file :qn :mode :held-by :reason :message :why}] :hint "..."}
;;    | {:allowed? true  :skipped :no-agent}
;;    | {:allowed? true  :degraded "<msg>"}
;;
;; 1. The caller id is normalized with `identity/caller-id-lookup-keys` (base
;;    id), so one agent's writes from different sessions are ONE holder.
;; 2. Spans are acquired all-or-nothing through `registry/acquire!`; an own
;;    re-acquire refreshes the lease. Dead or unregistered holders
;;    (`lings/holder-live?`) never block.
;; 3. A refusal names each blocking form, its holder and the reason, adds :why
;;    from `hive-agent.swarm.claim.reason/conflicts` when loaded, sends each
;;    holder a :claim/yield-request WITHOUT parking the caller, and a :hint.
;; An unregistered agent cannot own a claim row, so for it the gate only checks.
;; The gate fails open ({:allowed? true :degraded msg}): carto's fingerprint
;; CAS protects the bytes either way.

(defonce ^:private reason-override
  ;; A conflict explainer registered explicitly; wins over name resolution.
  (atom nil))

(defn register-reason-fn!
  "Install `f` as the conflict explainer (nil restores name resolution).
   Returns the previous one. `f` has the shape of
   `hive-agent.swarm.claim.reason/conflicts`:
   (f held wanted slave-id {:callers-fn f :ctx ctx})
   => [{:file :qn :mode :held-by :reason :message :why {:fact :rules :premises}}]
   where `held` is what `registry/held-spans` returns and `wanted` is the
   vector of wanted spans."
  [f]
  (first (reset-vals! reason-override f)))

;; boundary: swaps the explainer seam; the previous one
(m/=> register-reason-fn! [:=> [:cat [:maybe ifn?]] [:maybe ifn?]])

(def ^:dynamic *yield-notify!*
  "(fn [sender to event-type data]) used for the gate's yield requests; nil
   means `tell!`. A test seam."
  nil)

(defn- reason-fn
  "The conflict explainer: the registered one, else
   `hive-agent.swarm.claim.reason/conflicts` when the loaded hive-agent has it,
   else nil."
  []
  (or @reason-override
      (when (delegate/available? "hive-agent.swarm.claim.reason")
        (requiring-resolve 'hive-agent.swarm.claim.reason/conflicts))))

(defn agent-key
  "The holder id for raw caller id `raw`: its base agent id per
   `identity/caller-id-lookup-keys` (\"ling-x:12345\" -> \"ling-x\"), or nil
   when `raw` is nil or blank. Use it to release an agent's claims:
   (lings/release-claims-for-slave! (agent-key raw))."
  [raw]
  (when (and (string? raw) (not (str/blank? raw)))
    (peek (identity/caller-id-lookup-keys (str/trim raw)))))

(m/=> agent-key [:=> [:cat :any] [:maybe :string]])

(defn- anchor
  "`file` as an absolute, normalized path; a relative one resolves against `cwd`."
  [cwd file]
  (let [^Path p (Paths/get (str file) (make-array String 0))]
    (str (.normalize (if (and (not (.isAbsolute p)) (not (str/blank? cwd)))
                       (.resolve (Paths/get (str cwd) (make-array String 0)) p)
                       p)))))

(defn- ->span
  "A registry span from a gate span {:file :qn :mode}. :mode defaults to :body
   for a form and :file without a qn; a string mode is accepted."
  [cwd {:keys [file qn mode]}]
  (when (str/blank? (str file))
    (throw (ex-info "gate span without :file" {:qn qn :mode mode})))
  (let [path (anchor cwd file)
        mode (some-> mode name keyword)]
    (if (or (str/blank? (str qn)) (= :file mode))
      (span/file-span path)
      (span/form-span path (str qn) (or mode :body)))))

(defn- conflict-id [c] [(:file c) (:qn c) (:held-by c)])

(defn- explain-why
  "`conflicts` each with :why from the conflict explainer, nil without one.
   The explainer is called ONCE with the whole `wanted` vector."
  [conflicts agent wanted scope ctx]
  (let [whys (if-let [f (reason-fn)]
               (try
                 (let [held (registry/held-spans {:drop-dead? true})]
                   (into {}
                         (for [c (f held wanted agent {:callers-fn (graph/callers-fn scope)
                                                       :ctx        ctx})
                               :when (:why c)]
                           [(conflict-id c) (:why c)])))
                 (catch Throwable explainer-failed-why-stays-nil
                   (log/warn "claim reason explainer failed:"
                             (ex-message explainer-failed-why-stays-nil))
                   {}))
               {})]
    (mapv #(assoc (select-keys % [:file :qn :mode :held-by :reason :message])
                  :why (get whys (conflict-id %)))
          conflicts)))

(defn- ask-yields!
  "Ask each holder to yield, without parking. Never throws."
  [agent conflicts]
  (try
    (request-yields-for! agent conflicts
                         (cond-> {}
                           *yield-notify!* (assoc :notify! *yield-notify!*)))
    (catch Throwable yield-request-is-best-effort
      (log/warn "claim gate: yield request failed:"
                (ex-message yield-request-is-best-effort))
      [])))

(defn- hint
  [conflicts asked lease-ms]
  (let [forms   (distinct (map #(or (:qn %) (:file %)) conflicts))
        holders (distinct (keep :held-by conflicts))]
    (str "Not written: " (str/join ", " forms) " is claimed by "
         (str/join ", " holders) ". "
         (when (seq asked)
           "A yield request was sent to the holder; its release frees the form. ")
         "Wait and retry (an idle claim lapses after "
         (quot lease-ms 60000) " min), ask the holder to yield"
         " (hivemind :claim/yield-request), or pick another form.")))

(defn- registered? [agent]
  (some? (ds/get-slave agent)))

(defn- check-only
  "registry/acquire!'s answer for an agent that cannot own a claim row:
   conflicts computed the same way, nothing claimed."
  [wanted agent scope ctx]
  (let [callers  (graph/callers-fn scope)
        held     (registry/held-spans {:drop-dead? true})
        assessed (mapv #(span/assess callers held % agent ctx) wanted)
        render   (fn [xs] (mapv #(assoc % :message (span/explain %)) xs))
        found    (vec (mapcat :conflicts assessed))]
    {:acquired?     (empty? found)
     :spans-claimed 0
     :conflicts     (render found)
     :warnings      (conj (render (distinct (mapcat :warnings assessed)))
                          {:reason  :unregistered-agent
                           :held-by agent
                           :message (str agent " is not a registered slave;"
                                         " checked for conflicts, nothing claimed.")})}))

(defn check-write!
  "Claim `spans` for the writing agent, or say who holds them. See the Write
   gate section above for the exact contract. Never throws: an internal error
   is {:allowed? true :degraded msg} (claims are advisory; carto's fingerprint
   CAS still protects the bytes)."
  [{:keys [agent-id cwd scope op spans]}]
  (try
    (if-let [agent (agent-key agent-id)]
      (let [lease  lings/default-stale-threshold-ms
            wanted (mapv #(->span cwd %) spans)
            ctx    (when-not (str/blank? cwd) (span/context-for cwd))
            result (if (registered? agent)
                     (registry/acquire! wanted agent {:scope scope :ctx ctx :drop-dead? true})
                     (check-only wanted agent scope ctx))]
        (if (:acquired? result)
          {:allowed? true
           :acquired (:spans-claimed result 0)
           :warnings (vec (:warnings result))
           :lease-ms lease}
          (let [conflicts (explain-why (:conflicts result) agent wanted scope ctx)
                asked     (ask-yields! agent conflicts)]
            (log/info "claim gate refused" {:agent agent :op op
                                            :conflicts (count conflicts)})
            {:allowed?  false
             :conflicts conflicts
             :hint      (hint conflicts asked lease)})))
      {:allowed? true :skipped :no-agent})
    (catch Throwable gate-fails-open
      (log/warn gate-fails-open "claim gate degraded; write allowed"
                {:agent-id agent-id :op op})
      {:allowed? true
       :degraded (str (.getSimpleName (class gate-fails-open)) ": "
                      (ex-message gate-fails-open))})))

(def GateSpan
  "One span a writer asks for."
  [:map
   [:file :string]
   [:qn {:optional true} [:maybe :string]]
   [:mode {:optional true} [:maybe [:or :keyword :string]]]])

(def WriteRequest
  "What carto sends before it writes."
  [:map
   [:agent-id {:optional true} [:maybe :string]]
   [:cwd {:optional true} [:maybe :string]]
   [:scope {:optional true} [:maybe :string]]
   [:op {:optional true} [:maybe :keyword]]
   [:spans {:optional true} [:maybe [:sequential GateSpan]]]])

(def WriteDecision
  "The gate's answer: allowed (claimed, skipped or degraded) or refused."
  [:or
   [:map
    [:allowed? [:= true]]
    [:acquired {:optional true} [:int {:min 0}]]
    [:warnings {:optional true} [:vector :map]]
    [:lease-ms {:optional true} [:int {:min 0}]]
    [:skipped {:optional true} :keyword]
    [:degraded {:optional true} :string]]
   [:map
    [:allowed? [:= false]]
    [:conflicts [:vector :map]]
    [:hint :string]]])

;; boundary: claims through the swarm store and may tell holders to yield
(m/=> check-write! [:=> [:cat WriteRequest] WriteDecision])

(m/=> acquire-for-ling!
      [:function
       [:=> [:cat :string [:maybe registry/Wanted]] AcquireForLing]
       [:=> [:cat :string [:maybe registry/Wanted]
             [:maybe [:map
                      [:task-id {:optional true} [:maybe :string]]
                      [:cwd {:optional true} [:maybe :string]]
                      [:notify! {:optional true} NotifyFn]
                      [:now-ms {:optional true} [:maybe :int]]]]]
        AcquireForLing]])

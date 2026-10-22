(ns hive-mcp.swarm.claim.gate
  "The write gate: carto asks here before it writes a form, and the answer is a
   claim taken on the writer's behalf.

   CONTRACT (hive-carto resolves `check-write!` by name; keep it stable)
   ======================================================================
     (check-write! {:agent-id \"ling-x:12345\" ; raw caller id, may be nil
                    :cwd      \"/abs/dir\"     ; optional
                    :scope    \"hive-carto\"   ; optional carto scope
                    :op       :write-form     ; informational
                    :spans    [{:file \"/abs/x.clj\" :qn \"ns/f\" :mode :body}]})
     => {:allowed? true  :acquired n :warnings [...] :lease-ms n}
      | {:allowed? false :conflicts [{:file :qn :mode :held-by :reason :message :why}]
         :hint \"...\"}
      | {:allowed? true  :skipped :no-agent}
      | {:allowed? true  :degraded \"<msg>\"}

   WHAT IT DOES
   ============
   1. The caller id is normalized with the canonical
      `hive-dsl.context.identity/caller-id-lookup-keys` (its base id: a
      session-shaped suffix is dropped), so one agent's successive writes from
      different sessions are ONE holder.
   2. The spans are acquired all-or-nothing through `registry/acquire!`.
      Re-acquiring one's own span refreshes its lease (last-activity, not
      creation, decides staleness). Holders that are dead or unregistered
      (`lings/holder-live?`, the rule `claim-file!` uses) never block.
   3. A refusal names each blocking form, its holder and the reason, adds
      :why from `hive-agent.swarm.claim.reason/conflicts` when that is on the
      classpath (nil otherwise), sends each holder the existing deduplicated
      :claim/yield-request WITHOUT parking the caller, and returns a :hint.

   An agent that is not a registered slave cannot own a claim row (the row
   references the slave entity), so for it the gate only CHECKS: it is refused
   when a live agent holds the form, allowed otherwise with :acquired 0 and an
   :unregistered-agent warning.

   FAILS OPEN, ON PURPOSE
   ======================
   `check-write!` never throws. An internal error returns
   {:allowed? true :degraded \"<msg>\"} and logs a warning. Claims are an
   advisory backstop that tells agents they are aimed at the same form before
   they spend tokens; the bytes are protected by carto's fingerprint CAS
   either way, so a broken claim store must not stop every write in the
   hive."
  (:require [clojure.string :as str]
            [hive-dsl.context.identity :as identity]
            [hive-mcp.swarm.claim.graph :as graph]
            [hive-mcp.swarm.claim.negotiate :as negotiate]
            [hive-mcp.swarm.claim.registry :as registry]
            [hive-mcp.swarm.claim.span :as span]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.datascript.lings :as lings]
            [hive-mcp.swarm.delegate :as delegate]
            [taoensso.timbre :as log]
            [malli.core :as m])
  (:import [java.nio.file Path Paths]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Seams
;; =============================================================================

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
  "(fn [sender to event-type data]) used for yield requests; nil means
   `negotiate/tell!`. A test seam."
  nil)

(defn- reason-fn
  "The conflict explainer: the registered one, else
   `hive-agent.swarm.claim.reason/conflicts` when the loaded hive-agent has it,
   else nil."
  []
  (or @reason-override
      (when (delegate/available? "hive-agent.swarm.claim.reason")
        (requiring-resolve 'hive-agent.swarm.claim.reason/conflicts))))

;; =============================================================================
;; Inputs
;; =============================================================================

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

;; =============================================================================
;; Refusal
;; =============================================================================

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

(defn- request-yields!
  "Ask each holder to yield, without parking. Never throws."
  [agent conflicts]
  (try
    (negotiate/request-yields-for! agent conflicts
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

;; =============================================================================
;; Acquire or check
;; =============================================================================

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
  "Claim `spans` for the writing agent, or say who holds them. See the
   namespace docstring for the exact contract. Never throws: an internal error
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
                asked     (request-yields! agent conflicts)]
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

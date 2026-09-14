(ns hive-mcp.tools.agora
  "MCP tools for Agora multi-ling dialogue system.

   Exposes Nash Equilibrium dialogue infrastructure as MCP tools:
   - agora_create_dialogue: Create a new dialogue session (ling-based)
   - agora_dispatch: Send message within dialogue (with signal parsing)
   - agora_check_consensus: Check Nash equilibrium status
   - agora_list_dialogues: List all dialogues
   - agora_join_dialogue: Add participant to dialogue
   - agora_get_history: Retrieve a dialogue transcript

   Result DSL: Internal logic returns Result maps ({:ok val} or {:error category}).
   Single try-result boundary at each handler level. Zero nested try-catch."

  (:require [hive-mcp.dns.result :as result]
            [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [hive-mcp.agora.dialogue :as dialogue]
            [hive-mcp.agora.consensus :as consensus]
            [hive-mcp.agora.schema :as schema]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Result DSL Helpers (boundary pattern — mirrors tools/kg.clj)
;; ============================================================

(defn- try-result
  "Execute thunk f returning Result; catch unexpected exceptions as error Result.
   Catches ExceptionInfo (structured) and Exception (generic) separately."
  [category f]
  (try
    (f)
    (catch clojure.lang.ExceptionInfo e
      (log/warn (name category) ":" (ex-message e) (ex-data e))
      (result/err category {:message (ex-message e)}))
    (catch Exception e
      (log/error e (str (name category) " failed"))
      (result/err category {:message (.getMessage e)}))))

(defn- result->mcp
  "Convert Result to MCP response.
   {:ok data} -> (mcp-json data), {:error ...} -> (mcp-error message)."
  [r]
  (if (result/ok? r)
    (mcp-json (:ok r))
    (mcp-error (or (:message r) (str (:error r))))))

;; ============================================================
;; Validation Helpers (return Results)
;; ============================================================

(defn- require-param
  "Validate a parameter is non-nil. Returns Result."
  [value param-name]
  (if (nil? value)
    (result/err :agora/validation-failed
                {:message (str "Missing required parameter: " param-name)})
    (result/ok value)))

(defn- require-dialogue
  "Validate dialogue_id exists and fetch dialogue. Returns Result with dialogue."
  [dialogue-id]
  (result/let-ok [_ (require-param dialogue-id "dialogue_id")]
                 (if-let [d (schema/get-dialogue dialogue-id)]
                   (result/ok d)
                   (result/err :agora/not-found
                               {:message (str "Dialogue not found: " dialogue-id)}))))

;; ============================================================
;; Conversion Helpers (pure, zero branching at call site)
;; ============================================================

(defn- safe-name
  "Keyword/string -> string name, nil -> fallback."
  ([x] (safe-name x "unknown"))
  ([x fallback]
   (cond
     (keyword? x) (name x)
     (string? x)  x
     :else         fallback)))

(defn- safe-kw
  "String -> keyword, nil -> nil."
  [x]
  (when x (keyword x)))

(defn- ensure-vec [x]
  (if (vector? x) x (vec x)))

(defn- truncate-preview
  "Truncate a string to max-len chars with ellipsis."
  [s max-len]
  (when s
    (if (> (count s) max-len)
      (str (subs s 0 max-len) "...")
      s)))

(defn- build-participant-status
  "Build per-participant equilibrium status vector (pure)."
  [dialogue-id participants]
  (mapv (fn [p]
          (let [t (consensus/last-turn-for dialogue-id p)
                s (:signal t)]
            {:participant      p
             :short-name       (consensus/extract-short-name p)
             :last-signal      (safe-name s nil)
             :in-equilibrium?  (boolean
                                (when s
                                  (contains? consensus/equilibrium-signals s)))}))
        participants))

(defn- format-turn
  "Format a single dialogue turn for output (pure)."
  [t]
  {:turn-num  (:turn-num t)
   :from      (:sender t)
   :to        (:receiver t)
   :signal    (safe-name (:signal t) nil)
   :message   (:message t)
   :timestamp (when-let [ts (:timestamp t)] (.getTime ts))})

(defn- enrich-dialogue-summary
  "Enrich a raw dialogue entry with turn count and participants (pure)."
  [d]
  (let [turns (schema/get-turns (:id d))
        dlg   (schema/get-dialogue (:id d))]
    {:id           (:id d)
     :topic        (:name d)
     :status       (safe-name (:status d))
     :participants (vec (or (:participants dlg) []))
     :turn-count   (count turns)
     :created      (:created d)}))

;; ============================================================
;; Pure Logic (Result-returning functions)
;; ============================================================

(defn- create-dialogue* [{:keys [participants topic]}]
  (let [pvec  (ensure-vec participants)
        topic (or topic "Unspecified dialogue")
        id    (dialogue/create-dialogue {:participants pvec :topic topic})]
    (log/info "Created Agora dialogue:" id "with participants:" pvec)
    (result/ok {:dialogue-id id :participants pvec :topic topic :status "active"})))

(defn- dispatch* [{:keys [dialogue_id to message from timeout_ms files signal]}]
  (let [r  (dialogue/dialogue-dispatch
            {:dialogue-id dialogue_id :from from :to to
             :message message :signal (safe-kw signal)
             :timeout_ms timeout_ms :files files})
        cs (consensus/check-consensus dialogue_id)]
    (log/info "Agora dispatch to" to "in dialogue" dialogue_id
              "signal:" (:signal r) "detection:" (:signal-detection r)
              "consensus:" cs)
    (result/ok {:dialogue-id      dialogue_id
                :turn             (:turn r)
                :signal           (safe-name (:signal r))
                :signal-detection (safe-name (:signal-detection r) nil)
                :consensus-status (safe-name cs)
                :dispatch-result  (:dispatch-result r)})))

(defn- check-consensus* [{:keys [dialogue_id]}]
  (result/let-ok [_dialogue (require-dialogue dialogue_id)]
                 (let [r            (consensus/consensus-result dialogue_id)
                       turns        (schema/get-turns dialogue_id)
                       last-turn    (last (sort-by :turn-number turns))
                       preview      (truncate-preview (:message last-turn) 100)
                       participants (consensus/get-participants dialogue_id)
                       pstatus      (build-participant-status dialogue_id participants)]
                   (log/debug "Consensus check for" dialogue_id ":" r)
                   (result/ok {:dialogue-id       dialogue_id
                               :status            (safe-name (:status r))
                               :nash-equilibrium? (boolean (:nash-equilibrium? r))
                               :approval-ratio    (or (:approval-ratio r) 0.0)
                               :participants      (or (:participants r) 0)
                               :turn-count        (or (:turn-count r) 0)
                               :progress-score    (or (:progress-score r) 0.0)
                               :mediator-needed?  (boolean (:mediator-needed? r))
                               :mediator-reason   (safe-name (:mediator-reason r) nil)
                               :participant-status pstatus
                               :last-turn         (when last-turn
                                                    {:from    (:sender last-turn)
                                                     :signal  (safe-name (:signal last-turn) nil)
                                                     :preview preview})}))))

(defn- list-dialogues* [{:keys [status]}]
  (let [skw      (safe-kw status)
        ds       (if skw (schema/list-dialogues skw) (schema/list-dialogues))
        enriched (mapv enrich-dialogue-summary ds)]
    (log/debug "Listed" (count enriched) "dialogues"
               (when skw (str "with status " skw)))
    (result/ok {:dialogues enriched
                :count     (count enriched)
                :filter    (safe-name skw nil)})))

(defn- join-dialogue* [{:keys [dialogue_id slave_id]}]
  (let [joined? (dialogue/join-dialogue dialogue_id slave_id)]
    (if joined?
      (do (log/info "Participant" slave_id "joined dialogue" dialogue_id)
          (result/ok {:success true :dialogue-id dialogue_id :participant slave_id}))
      (result/err :agora/join-failed
                  {:message (str "Dialogue not found: " dialogue_id)}))))

(defn- get-history* [{:keys [dialogue_id limit]}]
  (result/let-ok [_ (require-dialogue dialogue_id)]
                 (let [dlg       (dialogue/get-dialogue dialogue_id)
                       all-turns (dialogue/get-dialogue-turns dialogue_id)
                       turns     (if (and limit (pos? limit))
                                   (take-last limit all-turns)
                                   all-turns)
                       fmt       (mapv format-turn turns)]
                   (log/debug "Retrieved" (count fmt) "turns for dialogue" dialogue_id)
                   (result/ok {:dialogue-id  dialogue_id
                               :topic        (:topic dlg)
                               :status       (safe-name (:status dlg) nil)
                               :participants (vec (:participants dlg))
                               :turn-count   (count all-turns)
                               :turns        fmt}))))

;; ============================================================
;; Tool Handlers (thin boundary layer)
;; ============================================================

(defn handle-agora-create-dialogue
  "Create a new Agora dialogue session."
  [params]
  (result->mcp (try-result :agora/create-dialogue #(create-dialogue* params))))

(defn handle-agora-dispatch
  "Dispatch a message within an Agora dialogue."
  [params]
  (result->mcp (try-result :agora/dispatch #(dispatch* params))))

(defn handle-agora-check-consensus
  "Check Nash equilibrium status for a dialogue."
  [params]
  (result->mcp (try-result :agora/check-consensus #(check-consensus* params))))

(defn handle-agora-list-dialogues
  "List all Agora dialogues, optionally filtered by status."
  [params]
  (result->mcp (try-result :agora/list-dialogues #(list-dialogues* params))))

(defn handle-agora-join-dialogue
  "Add a participant to an existing dialogue."
  [params]
  (result->mcp (try-result :agora/join-dialogue #(join-dialogue* params))))

(defn handle-agora-get-history
  "Retrieve dialogue transcript with all turns."
  [params]
  (result->mcp (try-result :agora/get-history #(get-history* params))))

;; ============================================================
;; Tool Definitions
;; ============================================================

(def tools
  "REMOVED: Flat agora tools no longer exposed. Use consolidated `agora` tool."
  [])

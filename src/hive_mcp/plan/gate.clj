(ns hive-mcp.plan.gate
  "FSM gate for plan-type memory writes.

   Guarantees that any content stored as type=plan is parseable by
   plan-to-kanban and conforms to the Plan schema. This prevents
   malformed plans from entering the memory system where they would
   silently fail during plan-to-kanban conversion.

   The gate uses plan-fsm/draft->validated to run the plan through
   parsing + schema validation + dependency checking + cycle detection
   WITHOUT executing (no kanban tasks created, no KG edges).

   ## Integration Point

   Called from tools.memory.crud/handle-add when type='plan'.
   The gate runs BEFORE plans/index-plan! to ensure only valid plans
   are persisted.

   ## Return Contract

   validate-for-storage returns:
   - {:valid? true  :plan <normalized-plan> :metadata <enriched-metadata>}
   - {:valid? false :errors [...] :hint \"...\" :raw-error \"...\"}"

  (:require [hive-mcp.plan.fsm :as plan-fsm]
            [hive-mcp.plan.parser :as parser]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [malli.core :as m]
            [hive-dsl.result :as r]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Plan Content Detection
;; =============================================================================

(defn plan-content?
  "Heuristic: does this content look like it contains a plan?

   Returns true if the content has structured plan indicators:
   - EDN with :steps or :plan/steps
   - Markdown with ≥1 ## step header

   Used to decide whether to apply FSM validation or store as-is
   (some plan-tagged entries may be free-form notes about plans,
    not structured plans themselves).

   Threshold of 1 ## header (not 2) — matches parser/parse-markdown-plan
   which accepts ≥1 H2. Asymmetric thresholds rejected single-step plans
   at detection before the parser ran (kanban 20260429135733)."
  [content]
  (and (string? content)
       (not (str/blank? content))
       (or (parser/contains-edn-plan? content)
           ;; Markdown plan detection: ≥1 ## header (parser parity).
           (>= (count (re-seq #"(?m)^##\s+" content)) 1))))

;; =============================================================================
;; Hint Builder (Actionable Fix Guidance)
;; =============================================================================

(defn- build-hint
  "Build actionable fix guidance for a ling based on the failure phase.

   Returns a string with specific instructions on how to fix the plan."
  [phase errors]
  (case phase
    :parse
    (str "Plan content could not be parsed. Ensure it contains either:\n"
         "  1. EDN with {:id \"plan-xxx\" :title \"...\" :steps [{:id \"step-1\" :title \"...\"}]}\n"
         "  2. Markdown with ## headers for each step\n"
         "  3. EDN code blocks: ```edn\\n{:steps [...]}\\n```\n"
         "Minimum required: :steps vector with at least one {:id :title} entry.")

    :schema
    (str "Plan parsed but failed schema validation. Required fields:\n"
         "  Plan: {:id string, :title string, :steps [Step+]}\n"
         "  Step: {:id string, :title string}\n"
         "  Optional step fields: :description, :depends-on [], :priority :high|:medium|:low,\n"
         "                        :files [], :estimate :small|:medium|:large\n"
         "Errors: " (str/join "; " errors))

    :dependencies
    (str "Plan has steps that reference non-existent step IDs in :depends-on.\n"
         "Fix: ensure all :depends-on values match existing step :id values.\n"
         "Details: " (str/join "; " errors))

    :cycles
    (str "Plan has circular dependencies (A depends on B depends on A).\n"
         "Fix: break the cycle by removing one dependency direction.\n"
         "Details: " (str/join "; " errors))

    :fields
    (str "Plan has an addon-registered field with an invalid value.\n"
         "Fix: use a value accepted by the contributing addon (e.g. a registered\n"
         "workflow :method). Details: " (str/join "; " errors))

    ;; default
    (str "Plan validation failed. Check the content format.\n"
         "Details: " (str/join "; " errors))))

;; =============================================================================
;; Error Formatting (for MCP responses)
;; =============================================================================

(defn format-gate-error
  "Format a gate validation failure as a user-friendly MCP error string.

   Returns a string suitable for mcp-error that includes:
   - What went wrong
   - How to fix it
   - The plan-to-kanban contract summary"
  [{:keys [errors hint phase]}]
  (str "Plan validation failed (phase: " (name (or phase :unknown)) ")\n\n"
       "Errors:\n"
       (str/join "\n" (map #(str "  - " %) errors))
       "\n\n"
       "How to fix:\n"
       hint
       "\n\n"
       "Plan-to-kanban contract:\n"
       "  {:id    \"plan-<timestamp>-<topic>\"\n"
       "   :title \"Short descriptive title\"\n"
       "   :steps [{:id \"step-1\" :title \"Step title\" :depends-on [] :priority :high}]}"))

;; =============================================================================
;; Validation Gate
;; =============================================================================

(def ^:private Validation
  "The :validation map draft->validated reports for a failed plan."
  [:maybe [:map-of :keyword :any]])

(def ^:private Failures
  [:map [:errors [:vector :string]]
   [:phase [:enum :schema :dependencies :cycles :fields :unknown]]])

(def ^:private GateVerdict
  [:map [:valid? :boolean]])

(defn- validation-failures
  "Promote: read the failed checks of a :validation map into human-readable
   errors and the phase that names the first failing check."
  [validation]
  (let [schema-errors (when-not (get-in validation [:schema :valid])
                        (get-in validation [:schema :errors]))
        dep-errors (when-not (get-in validation [:dependencies :valid])
                     (get-in validation [:dependencies :invalid-refs]))
        cycle-errors (when-not (get-in validation [:cycles :valid])
                       [(get-in validation [:cycles :cycle])])
        field-errors (when-not (get-in validation [:fields :valid])
                       (get-in validation [:fields :errors]))
        all-errors (cond-> []
                     schema-errors
                     (conj (str "Schema: " (pr-str schema-errors)))
                     dep-errors
                     (conj (str "Dependencies: steps reference non-existent IDs: "
                                (str/join ", " (map #(str (:step-id %) " -> " (:missing-dep %))
                                                    dep-errors))))
                     cycle-errors
                     (conj (str "Cycles: " (str/join ", " (remove nil? cycle-errors))))
                     (seq field-errors)
                     (into (map #(str "Field " (:key %) "=" (pr-str (:value %))
                                      ": " (pr-str (:error %)))
                                field-errors)))
        phase (cond
                schema-errors :schema
                dep-errors :dependencies
                cycle-errors :cycles
                (seq field-errors) :fields
                :else :unknown)]
    {:errors all-errors :phase phase}))

(defn- validated-verdict
  "Pipeline: turn draft->validated's data map into the gate verdict.

   A plan whose FSM checks passed is still refused when it has no steps:
   empty steps pass the Malli schema but fail plan-to-kanban (the has-tasks?
   guard is only checked at :approved->:executing)."
  [result]
  (if (:valid? result)
    (let [plan (:plan result)
          steps (:steps plan)]
      (if (empty? steps)
        {:valid? false
         :errors ["Plan has no steps. At least one step is required for plan-to-kanban."]
         :phase :empty-steps
         :hint (build-hint :schema ["Plan :steps vector is empty"])
         :raw-error "Empty steps vector"}
        {:valid? true
         :plan plan
         :metadata {:steps-count (count steps)
                    :decision-id (:decision-id plan)
                    :has-dependencies? (boolean (some #(seq (:depends-on %)) steps))
                    :source-format (:source-format plan)}}))
    (let [validation (:validation result)
          {:keys [errors phase]} (validation-failures validation)]
      {:valid? false
       :errors errors
       :phase phase
       :hint (build-hint phase errors)
       :raw-error (pr-str validation)})))

(defn- thrown-verdict
  "Pipeline: the verdict for a validation that threw. `outcome` is the err
   the collect stage produced: :plan-gate/rejected carries the ExceptionInfo
   message and data, :plan-gate/unexpected any other exception's message."
  [{:keys [error message data]}]
  (if (= :plan-gate/rejected error)
    (let [phase (or (:phase data) :parse)]
      {:valid? false
       :errors [(str (name phase) " failed: " message)]
       :phase phase
       :hint (build-hint phase [(str message)])
       :raw-error message})
    {:valid? false
     :errors [(str "Unexpected error: " message)]
     :phase :unexpected
     :hint "The plan content caused an unexpected error. Check the format."
     :raw-error message}))

(defn- collect-verdict
  "Collect: run content through draft->validated (parse + schema + deps +
   cycles, no execution) and classify it. A throw anywhere in that becomes an
   err on the railway, never escapes."
  [content]
  (try
    (r/ok (validated-verdict (plan-fsm/draft->validated {:content content})))
    (catch clojure.lang.ExceptionInfo e
      (r/err :plan-gate/rejected {:message (.getMessage e) :data (ex-data e)}))
    (catch Exception e
      (r/err :plan-gate/unexpected {:message (.getMessage e) :cause e}))))

(defn- log-verdict!
  "Boundary: the one log line each outcome has always produced."
  [outcome verdict]
  (if (r/ok? outcome)
    (cond
      (:valid? verdict)
      (log/info "[plan-gate] Plan validated successfully"
                {:steps (get-in verdict [:metadata :steps-count])
                 :format (get-in verdict [:metadata :source-format])
                 :has-deps? (get-in verdict [:metadata :has-dependencies?])})
      (not= :empty-steps (:phase verdict))
      (log/warn "[plan-gate] Plan validation failed"
                {:phase (:phase verdict) :errors (:errors verdict)}))
    (if (= :plan-gate/rejected (:error outcome))
      (log/warn "[plan-gate] Plan parse/validation failed"
                {:error (:message outcome) :phase (:phase verdict) :data (:data outcome)})
      (log/error (:cause outcome) "[plan-gate] Unexpected error during plan validation"))))

(defn validate-for-storage
  "FSM gate: validate plan content before storage.

   Runs the plan through draft->validated (parse + schema + deps + cycles)
   without executing. Returns a result map indicating validity.

   Stages: collect-verdict (Collect, the FSM run on the Result railway),
   validation-failures (Promote), validated-verdict / thrown-verdict
   (Pipeline), log-verdict! (Boundary).

   Args:
     content - Raw plan content string (EDN or markdown)

   Returns:
     {:valid?   true
      :plan     <normalized-plan-map>
      :metadata {:steps-count N
                 :decision-id \"...\" (or nil)
                 :has-dependencies? bool
                 :source-format :edn|:markdown}}

     {:valid?   false
      :errors   [\"human-readable error 1\" ...]
      :hint     \"Actionable fix guidance for the ling\"
      :phase    :parse|:schema|:dependencies|:cycles
      :raw-error \"Original error message\"}"
  [content]
  (log/debug "[plan-gate] Validating plan content for storage"
             {:content-length (count (str content))})
  (let [outcome (collect-verdict content)
        verdict (if (r/ok? outcome) (:ok outcome) (thrown-verdict outcome))]
    (log-verdict! outcome verdict)
    verdict))

;; =============================================================================
;; Function Contracts (m16 coverage batch)
;; =============================================================================

(m/=> plan-content?
      [:=> [:cat :any]
       :any])

(m/=> format-gate-error
      [:=> [:cat :map]
       :string])

(m/=> validate-for-storage
      [:=> [:cat :any]
       [:map [:valid? :boolean]]])

(m/=> validation-failures [:=> [:cat Validation] Failures])
(m/=> validated-verdict [:=> [:cat [:maybe :map]] GateVerdict])
(m/=> thrown-verdict [:=> [:cat [:map [:error :keyword]]] GateVerdict])
(m/=> collect-verdict [:=> [:cat :any] [:map-of :keyword :any]])

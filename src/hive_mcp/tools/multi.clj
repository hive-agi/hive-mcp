(ns hive-mcp.tools.multi
  "hive-mcp-flavored wrapper around `hive-mcp.batch`. Preserves the
   pre-T13-P1 public API surface so existing callers (consolidated/multi,
   tools/multi_async, test/tools/multi_test) keep working unchanged.

   T13 Phase 1 extract — 2026-04-24:
   - Pure batch runner moved to `hive-mcp.batch`.
   - This namespace now supplies the hive-mcp-specific pieces:
       - `resolve-tool-handler` (consolidated → flat fallback)
       - FX emission (`register-fx!`, `fire-fx!`, handlers)
       - MCP response formatting (`format-results`, `handle-batch`)
   - Pure orchestration fns (`normalize-op`, `validate-ops`, etc.) are
     re-exported for back-compat via `def` aliases.

   T13 Phase 2 will introduce the `Batchable` protocol so any consolidated
   tool can opt into batch semantics; this namespace will become one of
   several consumers of `hive-mcp.batch`.

   Design decision: 20260207194224-3b674f5d."
  (:require [hive-mcp.batch :as batch]
            [hive-mcp.batch.protocol :as batch-proto]
            [hive-mcp.dns.result :as result :refer [rescue]]
            [hive-mcp.tools.core :as core :refer [mcp-error]]
            [hive-mcp.dsl.response :as compress]
            [taoensso.timbre :as log]
            [clojure.data.json :as json]
            [hive-mcp.context.request :as ctx]
            [malli.core :as m]
            [clojure.string :as str]
            [hive-mcp.dsl.verbs :as verbs]
            [hive-mcp.dsl.param-domain :as pd]
            [hive-mcp.multi.param-coerce :as param-coerce]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; FX Effect Handlers (hive-mcp-specific observability)
;; =============================================================================

(defn- resolve-agent-id
  "Resolve the current agent-id from agent context or environment."
  []
  (try
    (when-let [ctx-fn (requiring-resolve 'hive-mcp.context.request/current-agent-id)]
      (ctx-fn))
    (catch Exception _
      (System/getenv "CLAUDE_SWARM_SLAVE_ID"))))

(defn- handle-wave-complete
  "FX handler for :multi/wave-complete — log + optional hivemind shout."
  [{:keys [wave-num op-count success-count failed-count total-waves]}]
  (let [msg (str "[multi] Wave " wave-num "/" total-waves " complete: "
                 success-count "/" op-count " succeeded"
                 (when (pos? (or failed-count 0))
                   (str ", " failed-count " failed")))]
    (log/info msg)
    (when-let [agent-id (resolve-agent-id)]
      (rescue nil
              (when-let [shout-fn (requiring-resolve 'hive-mcp.hivemind.core/shout!)]
                (shout-fn agent-id :progress
                          {:task "multi-op"
                           :message msg}))))))

(defn- handle-op-error
  "FX handler for :multi/op-error — log per-op errors."
  [{:keys [op-id tool command error wave-num]}]
  (log/error "[multi] Op failed"
             {:op-id   op-id
              :tool    tool
              :command command
              :wave    wave-num
              :error   error}))

(defn register-fx!
  "Register multi FX handlers. Safe to call multiple times."
  []
  (when-let [reg-fx (requiring-resolve 'hive.events.fx/reg-fx)]
    (reg-fx :multi/wave-complete handle-wave-complete)
    (reg-fx :multi/op-error handle-op-error)
    (log/debug "[multi] FX effects (re-)registered")
    true))

(defn- fire-fx!
  "Fire a single FX via hive.events.fx. No-op if registry unavailable."
  [fx-id fx-data]
  (rescue nil
          (when-let [get-fx (requiring-resolve 'hive.events.fx/get-fx)]
            (when-let [handler (get-fx fx-id)]
              (handler fx-data)))))

;; =============================================================================
;; Tool Resolution (hive-mcp-specific)
;; =============================================================================

(defn resolve-consolidated-handler
  "Resolve a consolidated tool name to its handler fn."
  [tool-name]
  (rescue nil
          (let [get-handler (requiring-resolve 'hive-mcp.tools.consolidated.multi/get-tool-handler)]
            (get-handler tool-name))))

(defn resolve-tool-handler
  "Resolve a tool name to its handler.

   Order (T13 Phase 3 / multi-IAddon-native):
     1. multi.registry — covers :multi/core seed (existing 21 consolidated tools)
        AND any addon contributions registered via :multi/tool hook key.
     2. consolidated.multi — back-compat for callers that haven't loaded
        multi.registry yet (e.g. early bootstrap before core-seed fires).
     3. flat-tool fallback via hive-mcp.tools/get-tool-by-name.

   Decision: 20260429230453-7e7627cc"
  [tool-name]
  (or (rescue nil
              (when-let [resolver (requiring-resolve 'hive-mcp.multi.registry/resolve-tool-handler)]
                (resolver tool-name)))
      (resolve-consolidated-handler tool-name)
      (rescue nil
              (let [get-tool-fn (requiring-resolve 'hive-mcp.tools/get-tool-by-name)
                    tool-def    (get-tool-fn tool-name)]
                (:handler tool-def)))))

;; =============================================================================
;; Back-compat re-exports of pure batch-runner surface
;; =============================================================================
;;
;; Public API callers outside this ns reference `tools.multi/<name>` for
;; several pure fns. We delegate to `hive-mcp.batch` while keeping symbols
;; at their historical addresses.

(def ref-not-found
  "Sentinel for unresolvable reference (kept at this namespace for back-compat)."
  batch/ref-not-found)

(def ref? #'batch/ref?)
(def parse-ref #'batch/parse-ref)
(def extract-result-data #'batch/extract-result-data)
(def enrich-op-result #'batch/enrich-op-result)
(def resolve-ref #'batch/resolve-ref)
(def resolve-refs-in-value #'batch/resolve-refs-in-value)
(def resolve-op-refs #'batch/resolve-op-refs)
(def collect-ref-op-ids #'batch/collect-ref-op-ids)
(def normalize-op #'batch/normalize-op)
(def validate-ops #'batch/validate-ops)
(def assign-waves #'batch/assign-waves)

(defn execute-op
  "Execute a single op using the hive-mcp resolve-tool-handler.
   Kept at this ns for back-compat; delegates to batch/execute-op."
  [op]
  (batch/execute-op resolve-tool-handler op))

(m/=> validate-op [:=> [:cat batch/Op] [:vector :string]])

;; =============================================================================
;; Plan-time param validation
;; =============================================================================

(def ^:private closed-command-params
  "{[tool command] accepted-param-set} for every command whose handler's
   params are known exactly, derived from the DSL's `verbs/verb-params` so the
   `operations` surface and the `dsl` surface reject the same keys. A verb
   whose id param is remapped onto another command (m@ -> batch-get) is left
   out: its set describes the DSL spelling, not the raw command."
  (into {}
        (keep (fn [[verb accepted]]
                (when-let [{:keys [tool command]} (get verbs/verb-table verb)]
                  (when-not (contains? verbs/entity-id-remaps [tool command])
                    [[tool command] accepted]))))
        verbs/verb-params))

(def ^:private op-plumbing-params
  "Keys any op may carry beyond its handler's own params: batch plumbing,
   caller scope and the queueing flag every consolidated tool reads."
  (into (conj verbs/verb-meta-params pd/entity-id-key :id :wave :async)
        pd/meta-param-keys))

(defn- op-input-schema
  "The inputSchema TOOL-NAME advertises, with addon schema extensions merged,
   or nil when the tool has no flat definition. Resolved lazily:
   hive-mcp.tools.registry sits above this namespace."
  [tool-name]
  (rescue nil
          (when-let [tool-def ((requiring-resolve 'hive-mcp.tools.registry/get-tool-by-name)
                               tool-name)]
            (let [schema-ext ((requiring-resolve 'hive-mcp.extensions.registry/get-schema-extensions)
                              tool-name)]
              (cond-> (:inputSchema tool-def)
                (seq schema-ext) (update :properties merge schema-ext))))))

(defn- unknown-param-errors
  "Messages for params outside the closed set of OP's command, if it has one."
  [{:keys [tool command] :as op}]
  (when-let [accepted (get closed-command-params [(str tool) (str command)])]
    (let [unknown (->> (keys op)
                       (remove #(or (contains? accepted %)
                                    (contains? op-plumbing-params %)))
                       (sort-by str))]
      (when (seq unknown)
        [(str "Unknown param(s) for " tool " " command ": "
              (str/join ", " (map name unknown))
              " (accepted: " (str/join ", " (sort (map name accepted))) ")")]))))

(defn- schema-type-errors
  "Messages for a param whose string value cannot be decoded to the JSON type
   the target tool's schema declares. `$ref:` values resolve at run time and
   are not checked here."
  [{:keys [tool] :as op}]
  (when-let [schema (op-input-schema (str tool))]
    (let [params  (into {}
                        (remove (fn [[k v]]
                                  (or (contains? op-plumbing-params k)
                                      (batch/ref? v))))
                        op)
          coerced (param-coerce/coerce-params schema params)]
      (when-not (contains? coerced :ok)
        [(str "Parameter error for tool " tool ": " (:message coerced))]))))

(defn validate-op
  "Plan-time check of one normalized op's params, run on every op of a batch
   before any op executes. Returns a vector of error messages, empty when the
   op is acceptable. Checks params outside a closed command's accepted set and
   string values the target schema cannot decode. An op without a tool is left
   to the structural validation."
  [{:keys [tool] :as op}]
  (if (str/blank? (str tool))
    []
    (into (vec (unknown-param-errors op))
          (schema-type-errors op))))

;; =============================================================================
;; Top-level runner (thin wrapper over batch/run-operations)
;; =============================================================================

(def ^:private default-runner
  "Shared Batchable reference runner for this namespace. Lazy so the
   protocol record is built only on first batch call, and reused across
   requests. Proves the Batchable path works end-to-end (T13 Phase 2).

   PR4.1: `:resolve-handler` is wrapped in a thunk so the runner re-resolves
   the var on every call. Without this, the realized delay captures the
   current fn VALUE and `(with-redefs [resolve-tool-handler ...] ...)` in
   tests fails to influence dispatch. Var-lookup at call time keeps test
   substitution honest."
  (delay
    (batch/make-default-runner
     {:resolve-handler #(resolve-tool-handler %)
      :emit-fx         fire-fx!})))

(defn run-multi
  "Execute a vector of cross-tool operations with dependency ordering.

   Pipeline: normalize → validate → param check → assign-waves → execute-per-wave

   Every op's params are checked by `validate-op` before any op runs: one op
   with a bad param rejects the whole plan with a per-op error list and
   executes nothing.

   Options:
     :dry-run — validate and plan only, don't execute

   Returns:
     {:success bool
      :waves   {1 {:ops [...] :results [...]} ...}
      :summary {:total N :success M :failed F :waves W}
      :committed [...] :failed [...] :partial? bool
      :errors  [...]}

   Internally delegates through the `Batchable` protocol via the default
   reference runner (T13 Phase 2) rather than calling `batch/run-operations`
   directly."
  [ops & {:keys [dry-run]}]
  (batch-proto/batch-execute
   @default-runner
   ops
   {:dry-run?    (boolean dry-run)
    :validate-op #(validate-op %)}))

;; =============================================================================
;; Result Formatting (hive-mcp MCP-specific)
;; =============================================================================

(defn- format-op-result
  "Format a single op result for MCP response."
  [{:keys [id success result error]}]
  (cond-> {:id id :success success}
    result (assoc :result
                  (if (string? result)
                    result
                    (try (json/write-str result)
                         (catch Exception _ (pr-str result)))))
    error  (assoc :error error)))

(defn- format-execution-waves
  "Format execution waves for MCP response."
  [waves]
  (into {}
        (map (fn [[w {:keys [results]}]]
               [(str "wave_" w) (mapv format-op-result results)]))
        waves))

(defn- format-dry-run-plan
  "Format dry-run wave plan for MCP response."
  [waves]
  (into {}
        (map (fn [[w {:keys [ops]}]]
               [(str "wave_" w) ops]))
        waves))

(defn format-results
  "Format multi-execution results for MCP response.
   Returns a JSON-serializable map suitable for {:type \"text\" :text ...}.

   An executed run carries `committed` / `failed` op ids and `partial` when
   the batch was half-applied; a rejected plan carries `plan_rejected`.

   Options (keyword args):
   - :compact — apply batch envelope compression."
  [{:keys [success waves summary errors dry-run committed failed partial? plan-rejected?]
    :as _results} & {:keys [compact]}]
  (let [output (cond-> {:success success
                        :summary summary}
                 dry-run     (assoc :dry_run true
                                    :plan (format-dry-run-plan waves))
                 (not dry-run)
                 (assoc :waves (format-execution-waves waves))
                 (some? committed) (assoc :committed committed)
                 (some? failed)    (assoc :failed failed)
                 partial?          (assoc :partial true)
                 plan-rejected?    (assoc :plan_rejected true)
                 errors (assoc :errors errors))
        compressed (if compact
                     (compress/format-results-compact output)
                     output)]
    {:type "text"
     :text (json/write-str compressed)}))

;; =============================================================================
;; MCP Handler entry (called from consolidated/multi.clj)
;; =============================================================================

(defn- validate-batch-input
  "Validate batch input parameters. Returns Result."
  [{:keys [operations]}]
  (cond
    (or (nil? operations) (empty? operations))
    (result/err :multi/missing-ops
                {:message "Batch requires non-empty 'operations' array. Each op: {tool, command, ...params}"})

    (not (sequential? operations))
    (result/err :multi/invalid-ops
                {:message "operations must be an array of operation objects"})

    :else
    (result/ok operations)))

(defn- thread-caller-cwd
  "Default each op's scope directory to the request's caller cwd (HCR:
   explicit :directory > :_caller_cwd > request-ctx > server user.dir).
   Without this, ops executed by the shared-JVM server resolve project
   scope from the server's own cwd (hive-mcp) instead of the caller's
   pwd. Ops carrying their own :directory or :_caller_cwd keep theirs.
   Ops may still have string keys here (normalize-op runs later)."
  [ops params]
  (if-let [cwd (ctx/resolve-caller-directory params)]
    (mapv (fn [op]
            (if (or (get op :directory) (get op "directory")
                    (get op :_caller_cwd) (get op "_caller_cwd"))
              op
              (assoc op :_caller_cwd cwd)))
          ops)
    ops))

(defn handle-batch
  "Handle a batch of cross-tool operations from the MCP multi tool."
  [params]
  (let [input   (validate-batch-input params)
        compact (compress/resolve-compress-mode params)]
    (if (result/err? input)
      (mcp-error (:message input))
      (format-results (run-multi (thread-caller-cwd (:ok input) params)
                                 :dry-run (boolean (:dry_run params)))
                      :compact compact))))

(m/=> register-fx! [:=> [:cat] [:maybe [:= true]]])

(m/=> resolve-consolidated-handler [:=> [:cat :any] [:maybe core/ToolHandler]])

(m/=> resolve-tool-handler [:=> [:cat :any] [:maybe core/ToolHandler]])

(m/=> execute-op [:=> [:cat batch/Op] batch/OpResult])

;; Permissive in: handle-batch forwards caller ops as they arrive, string or
;; keyword keyed, after only a non-empty sequential check.
(m/=> run-multi [:=> [:cat [:sequential :any] [:* :any]] batch/RunResult])

(m/=> format-results [:=> [:cat :map [:* :any]] core/McpJsonResponse])

(m/=> handle-batch [:=> [:cat [:maybe :map]] core/McpResponse])

(ns hive-mcp.workflows.saa-workflow
  "SAA (Silence-Abstract-Act) workflow aggregate.

   Composes predicates (saa.predicates) and handlers (saa.handlers) into
   the FSM spec, handler/predicate maps, and compile/run API.

   This namespace is the public API — callers require only this ns.

   State graph:
   ```
   ::fsm/start --> ::catchup --> ::silence <--> ::silence-review
                                                    |
                                               ::abstract <--> ::validate-plan
                                                                    |
                                                               ::store-plan
                                                                /          \\
                                                    ::fsm/end          ::act-dispatch
                                                  (plan-only?)              |
                                                                       ::act-verify
                                                                            |
                                                                       ::fsm/end
   ```"

  (:require [hive.events.fsm :as fsm]
            [hive-mcp.dns.result :refer [rescue]]
            [hive-mcp.workflows.saa.predicates :as pred]
            [hive-mcp.workflows.saa.handlers :as h]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [taoensso.timbre :as log]
            [hive-mcp.dispatch.handler :as dh]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; =============================================================================
;; Re-exports for backward compatibility (tests reference sut/predicate-name)
;; =============================================================================

;; Each re-export holds the VAR, not the value it had at load (Capture-by-Var,
;; 20260817195749-0d407e9c). `(def has-plan? pred/has-plan?)` copies the fn
;; object, so reloading `saa.predicates` leaves this namespace answering with
;; the previous one, and every reader downstream of it too.
;;
;; Safe for all 112 call sites in saa_workflow_test because every one of them
;; INVOKES the alias, and a var is IFn. An alias read as a VALUE (compared with
;; `=`, or gated on `fn?`) would need `dh/current` instead.

(def has-required-fields?           #'pred/has-required-fields?)
(def has-startup-error?             #'pred/has-startup-error?)
(def context-loaded?                #'pred/context-loaded?)
(def context-not-loaded?            #'pred/context-not-loaded?)
(def has-observations?              #'pred/has-observations?)
(def has-error?                     #'pred/has-error?)
(def grounding-sufficient?          #'pred/grounding-sufficient?)
(def grounding-insufficient-retryable? #'pred/grounding-insufficient-retryable?)
(def grounding-max-iterations?      #'pred/grounding-max-iterations?)
(def has-plan?                      #'pred/has-plan?)
(def plan-valid?                    #'pred/plan-valid?)
(def plan-invalid-retryable?        #'pred/plan-invalid-retryable?)
(def plan-invalid-final?            #'pred/plan-invalid-final?)
(def plan-only?                     #'pred/plan-only?)
(def full-execution?                #'pred/full-execution?)
(def has-execution-result?          #'pred/has-execution-result?)
(def tests-passed?                  #'pred/tests-passed?)
(def tests-failed?                  #'pred/tests-failed?)
(def plan-nil-with-error?           #'pred/plan-nil-with-error?)
(def always                         #'pred/always)
(def noop-subscription              #'pred/noop-subscription)
(def trace-log-enter                #'pred/trace-log-enter)
(def trace-log-exit                 #'pred/trace-log-exit)

(def handle-start                   #'h/handle-start)
(def handle-catchup                 #'h/handle-catchup)
(def handle-silence                 #'h/handle-silence)
(def handle-silence-review          #'h/handle-silence-review)
(def handle-abstract                #'h/handle-abstract)
(def handle-validate-plan           #'h/handle-validate-plan)
(def handle-store-plan              #'h/handle-store-plan)
(def handle-act-dispatch            #'h/handle-act-dispatch)
(def handle-act-verify              #'h/handle-act-verify)
(def handle-end                     #'h/handle-end)
(def handle-error                   #'h/handle-error)


;; =============================================================================
;; Handler & Predicate Maps (for EDN spec keyword resolution)
;; =============================================================================

(def handler-map
  "Maps EDN keyword handlers to implementation functions.

   Stored as VARS so a reload of `hive-mcp.workflows.saa.handlers` reaches this
   table (20260817195749-0d407e9c). Handler vars survive all the way to
   INVOCATION: `fsm/compile` only does `(get handlers-map handler)` and rejects
   nil, `validate-state-spec` only asks `(nil? handler)`, and `normalize-handler`
   calls what it was given — a var is IFn, so nothing on that path needs to
   deref, and not derefing is what keeps the seam open past compile time."
  {:start          #'h/handle-start
   :catchup        #'h/handle-catchup
   :silence        #'h/handle-silence
   :silence-review #'h/handle-silence-review
   :abstract       #'h/handle-abstract
   :validate-plan  #'h/handle-validate-plan
   :store-plan     #'h/handle-store-plan
   :act-dispatch   #'h/handle-act-dispatch
   :act-verify     #'h/handle-act-verify
   :end            #'h/handle-end
   :error          #'h/handle-error})

(def predicate-map
  "Maps EDN keyword predicates to implementation functions.

   Stored as VARS so a reload of `hive-mcp.workflows.saa.predicates` reaches
   this table (20260817195749-0d407e9c). Unlike the handler vars, a predicate
   var does NOT survive to invocation: `resolve-dispatches` derefs it through
   `dh/current` because `fsm/compile-state-handler` would otherwise hand a
   non-`fn?` value to SCI as an unevaluated form. The var still buys the seam
   — it is read at compile, not at load — it just closes again sooner."
  {:has-required-fields?              #'pred/has-required-fields?
   :has-startup-error?                #'pred/has-startup-error?
   :context-loaded?                   #'pred/context-loaded?
   :context-not-loaded?               #'pred/context-not-loaded?
   :has-observations?                 #'pred/has-observations?
   :has-error?                        #'pred/has-error?
   :grounding-sufficient?             #'pred/grounding-sufficient?
   :grounding-insufficient-retryable? #'pred/grounding-insufficient-retryable?
   :grounding-max-iterations?         #'pred/grounding-max-iterations?
   :has-plan?                         #'pred/has-plan?
   :plan-valid?                       #'pred/plan-valid?
   :plan-invalid-retryable?           #'pred/plan-invalid-retryable?
   :plan-invalid-final?               #'pred/plan-invalid-final?
   :plan-only?                        #'pred/plan-only?
   :full-execution?                   #'pred/full-execution?
   :has-execution-result?             #'pred/has-execution-result?
   :tests-passed?                     #'pred/tests-passed?
   :tests-failed?                     #'pred/tests-failed?
   :plan-nil-with-error?              #'pred/plan-nil-with-error?
   :always                            #'pred/always})

(def spec-ref-map
  "Keyword -> fn table for every NON-`:handler` reference the EDN spec uses:
   dispatch predicates, `:subscriptions` handlers and the `:pre`/`:post` hooks.

   Contract: any compiler of `resources/fsm/saa-workflow.edn` that is not
   `compile-saa` (e.g. `hive-mcp.workflows.registry`) must resolve those
   keyword references through THIS map before calling `fsm/compile`.

   The three entries added here are subscription and hook references, not
   dispatch predicates, so their vars survive to invocation the way handler
   vars do — `run-subscriptions` calls `(handler path old new)` and `fsm/run`
   calls `(pre fsm resources)` / `(post resources)` directly."
  (merge predicate-map
         {:noop-subscription #'pred/noop-subscription
          :trace-log-enter   #'pred/trace-log-enter
          :trace-log-exit    #'pred/trace-log-exit}))


;; =============================================================================
;; In-Code FSM Spec (inline functions, fallback for EDN)
;; =============================================================================

(def saa-workflow-spec
  "Inline FSM spec with direct function references. Fallback when EDN unavailable.

   References are VARS, like the tables above, so a reload of the predicate or
   handler namespace reaches the fallback too (20260817195749-0d407e9c). That
   is only safe because `compile-saa` now routes this spec through
   `resolve-spec`: before that it went straight to `fsm/compile`, and a
   var-held dispatch predicate would have been handed to SCI. The fallback is
   taken exactly when the EDN load failed, so it is the path least likely to be
   exercised and least affordable to leave frozen."
  {:fsm
   {::fsm/start
    {:handler    #'h/handle-start
     :dispatches [[::fsm/error #'pred/has-startup-error?]
                  [::catchup   #'pred/has-required-fields?]]}

    ::catchup
    {:handler    #'h/handle-catchup
     :dispatches [[::silence   #'pred/context-loaded?]
                  [::fsm/error #'pred/context-not-loaded?]]}

    ::silence
    {:handler    #'h/handle-silence
     :dispatches [[::fsm/error      #'pred/has-error?]
                  [::silence-review #'pred/has-observations?]]}

    ::silence-review
    {:handler    #'h/handle-silence-review
     :dispatches [[::abstract  #'pred/grounding-sufficient?]
                  [::silence   #'pred/grounding-insufficient-retryable?]
                  [::abstract  #'pred/grounding-max-iterations?]]}

    ::abstract
    {:handler    #'h/handle-abstract
     :dispatches [[::fsm/error     #'pred/plan-nil-with-error?]
                  [::validate-plan #'pred/has-plan?]]}

    ::validate-plan
    {:handler    #'h/handle-validate-plan
     :dispatches [[::store-plan #'pred/plan-valid?]
                  [::abstract   #'pred/plan-invalid-retryable?]
                  [::fsm/error  #'pred/plan-invalid-final?]]}

    ::store-plan
    {:handler    #'h/handle-store-plan
     :dispatches [[::fsm/end      #'pred/plan-only?]
                  [::act-dispatch #'pred/full-execution?]]}

    ::act-dispatch
    {:handler    #'h/handle-act-dispatch
     :dispatches [[::fsm/error  #'pred/has-error?]
                  [::act-verify #'pred/has-execution-result?]]}

    ::act-verify
    {:handler    #'h/handle-act-verify
     :dispatches [[::fsm/end   #'pred/tests-passed?]
                  [::fsm/error #'pred/tests-failed?]]}

    ::fsm/end
    {:handler #'h/handle-end}

    ::fsm/error
    {:handler #'h/handle-error}}

   :opts
   {:max-trace 100

    :subscriptions
    {[:grounding-score]    {:handler #'pred/noop-subscription}
     [:plan-valid?]        {:handler #'pred/noop-subscription}
     [:tests-passed?]      {:handler #'pred/noop-subscription}
     [:silence-iterations] {:handler #'pred/noop-subscription}}

    :pre  #'pred/trace-log-enter
    :post #'pred/trace-log-exit}})


;; =============================================================================
;; EDN Spec Resolution
;; =============================================================================

(defn- resolve-keyword
  "Resolve a keyword reference to a function via handler-map or spec-ref-map.
   Unknown keywords and non-keywords pass through unchanged."
  [k]
  (if (keyword? k)
    (or (get handler-map k)
        (get spec-ref-map k)
        k)
    k))

(defn- resolve-dispatches
  "Resolve each dispatch predicate to an actual FUNCTION, not merely to
   something invocable.

   This is the ONE position in the whole spec where a var may not survive, and
   it is worth being precise about why. `hive.events.fsm/compile-state-handler`
   compiles a dispatch predicate as

     [target (if (fn? pred) pred (sci/eval-form sci-ctx pred))]

   so anything that is not `fn?` is treated as an UNEVALUATED FORM and handed
   to SCI. A var is not `fn?`, so a var-held predicate would be SCI-evaluated
   rather than called.

   Handlers, `:pre`, `:post` and subscription handlers deliberately do NOT get
   this treatment: `fsm/run` invokes each of them directly, a var is IFn, and
   leaving the var in place means a reload reaches them at INVOCATION time
   rather than merely at compile time. Derefing them here would be a
   pessimisation dressed as consistency — it would re-freeze, at compile, the
   exact seam the var was introduced to open."
  [dispatches]
  (mapv (fn [[state pred]] [state (dh/current (resolve-keyword pred))]) dispatches))

(defn- resolve-spec
  "Walk an EDN spec and resolve all keyword references to functions."
  [spec]
  (-> spec
      (update :fsm
              (fn [states]
                (reduce-kv
                 (fn [m state-key state-def]
                   (assoc m state-key
                          (cond-> state-def
                            (:handler state-def)
                            (update :handler resolve-keyword)
                            (:dispatches state-def)
                            (update :dispatches resolve-dispatches))))
                 {} states)))
      (update-in [:opts :pre] resolve-keyword)
      (update-in [:opts :post] resolve-keyword)
      (update-in [:opts :subscriptions]
                 (fn [subs]
                   (when subs
                     (reduce-kv
                      (fn [m k v] (assoc m k (update v :handler resolve-keyword)))
                      {} subs))))))

(defn load-edn-spec
  "Load the SAA workflow spec from resources/fsm/saa-workflow.edn
   and resolve keyword references to handler/predicate functions."
  []
  (-> (io/resource "fsm/saa-workflow.edn")
      slurp
      edn/read-string
      resolve-spec))


;; =============================================================================
;; Compilation & Execution API
;; =============================================================================

(defn compile-saa
  "Compile the SAA workflow FSM spec. Call once, reuse the compiled FSM.
   Loads from resources/fsm/saa-workflow.edn. Falls back to inline spec.

   Both specs go through `resolve-spec`, which the EDN one used to reach on its
   own via `load-edn-spec` while the inline fallback went straight to
   `fsm/compile`. That asymmetry was invisible while both specs held bare
   functions and becomes a defect the moment either holds a var: the fallback
   is taken exactly when the EDN load FAILED, so the path with no resolution
   step was also the path nobody exercises in the happy case.

   `resolve-spec` is idempotent — `resolve-keyword` returns a non-keyword
   unchanged and `dh/current` returns a non-var unchanged — so running it over
   an already-resolved EDN spec costs a walk and changes nothing."
  []
  (let [spec (or (rescue nil (load-edn-spec))
                 (do (log/warn "[saa-fsm] EDN spec load failed, using inline spec")
                     saa-workflow-spec))]
    (fsm/compile (resolve-spec spec))))

(defn run-saa
  "Execute a compiled SAA workflow FSM.
   Args: compiled-fsm, resources (side-effect fns), opts (must include :task, :agent-id)"
  ([compiled-fsm resources]
   (run-saa compiled-fsm resources {}))
  ([compiled-fsm resources opts]
   (fsm/run compiled-fsm
            resources
            {:data (merge {:agent-id nil
                           :directory nil
                           :task nil
                           :plan-only? false
                           :grounding-threshold 0.6
                           :silence-iterations 0
                           :abstract-retries 0}
                          opts)})))

(defn run-full-saa
  "Convenience: compile and run a full SAA cycle."
  [resources opts]
  (run-saa (compile-saa) resources opts))

(defn run-plan-only
  "Convenience: compile and run SAA without Act phase (plan-only mode)."
  [resources opts]
  (run-saa (compile-saa) resources (assoc opts :plan-only? true)))

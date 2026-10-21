(ns hive-mcp.emacs.client
  "Delegation shim — routes to hive-emacs.client.

   Full emacsclient implementation (50 forms, 3-state circuit breaker,
   daemon death detection) extracted to hive-emacs project. This shim
   preserves backward compatibility for 28 direct callers in hive-mcp core.

   Dynamic vars kept for backward compat; real vars live in hive-emacs.client."
  (:require [taoensso.timbre :as log] [hive-dsl.result :refer [rescue]]
            [hive-spi.editor.services :as svc]
            [clojure.string :as str]
            [malli.core :as m]
            [hive-mcp.emacs-ext.client :as ext-client]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ---------------------------------------------------------------------------
;; Dynamic vars — backward compat for code that binds these
;; ---------------------------------------------------------------------------

(def ^:dynamic *emacsclient-path*
  "Path to emacsclient binary."
  (or (System/getenv "EMACSCLIENT") "emacsclient"))

(def ^:dynamic *emacs-socket-name*
  "Emacs daemon socket name."
  (System/getenv "EMACS_SOCKET_NAME"))

(def ^:dynamic *default-timeout-ms*
  "Default timeout for emacsclient calls in milliseconds."
  5000)

(def ^:dynamic *max-timeout-ms*
  "Hard ceiling for any emacsclient call."
  30000)

(def ^:dynamic *circuit-breaker-cooldown-ms*
  "Minimum time (ms) the circuit stays open before recovery probe."
  5000)

;; ---------------------------------------------------------------------------
;; Delegation core
;; ---------------------------------------------------------------------------

(declare eval-elisp)

(def ^:const symbol-void-strike-limit
  "After this many `void-variable` / `void-function` failures for the same
   elisp symbol, `probe-feature!` and `probe-fboundp!` short-circuit further
   IPC round-trips and return cached failure (ENGINE-L0.2, incident 2026-05-11
   — runaway emacsclient probes on a missing feature can stall the swarm)."
  3)

(defonce ^:private *symbol-strike-counts
  ^{:doc "elisp-symbol → strike count. Reset on first successful probe."}
  (atom {}))

(defonce ^:private *symbol-disabled
  ^{:doc "Set of elisp symbols whose probe path is shorted (strike limit hit)."}
  (atom #{}))

(defn symbol-strike-counts
  "Diagnostic snapshot of the per-symbol probe miss counter."
  []
  @*symbol-strike-counts)

(m/=> symbol-strike-counts [:=> [:cat] [:map-of :any pos-int?]])

(defn disabled-symbols
  "Diagnostic snapshot of probe-disabled elisp symbols."
  []
  @*symbol-disabled)

(m/=> disabled-symbols [:=> [:cat] [:set :any]])

(defn reset-symbol-strikes!
  "Clear strike counts and the disabled-symbol set. Test/diagnostic only."
  []
  (reset! *symbol-strike-counts {})
  (reset! *symbol-disabled #{}))

(m/=> reset-symbol-strikes! [:=> [:cat] [:= #{}]])

(defn- void-symbol-error?
  "Return true if `error` is an elisp void-symbol failure for `sym`.
   Matches both `void-function` and `void-variable` shapes."
  [error sym]
  (boolean
   (and (string? error)
        (re-find (re-pattern (str "void-(?:variable|function)\\s+" sym)) error))))

(defn- record-symbol-strike!
  "Increment the strike counter for `sym`. Disables further probes once
   the count crosses `symbol-void-strike-limit`."
  [sym]
  (let [n (-> *symbol-strike-counts
              (swap! update sym (fnil inc 0))
              (get sym))]
    (when (>= n symbol-void-strike-limit)
      (swap! *symbol-disabled conj sym)
      (log/warn "[emacsclient] Disabling probe of" sym
                "— hit" n "void-symbol strikes (ENGINE-L0.2)"))
    n))

(defn- clear-symbol-strike!
  "Drop the strike counter and disabled flag for `sym` after a healthy probe."
  [sym]
  (swap! *symbol-strike-counts dissoc sym)
  (swap! *symbol-disabled disj sym))

(defn probe-feature!
  "Strike-tracked wrapper around (featurep '<feature>).
   Returns true / false / :disabled — the :disabled response means the
   3-strike limit has been hit; the caller MUST treat the feature as
   unavailable without scheduling further probes (ENGINE-L0.2)."
  [feature]
  (if (contains? @*symbol-disabled feature)
    :disabled
    (let [code (format "(featurep '%s)" (name feature))
          {:keys [success result error]} (eval-elisp code)]
      (cond
        (and success (= result "t"))
        (do (clear-symbol-strike! feature) true)

        (and success (= result "nil"))
        (do (record-symbol-strike! feature) false)

        (void-symbol-error? error feature)
        (do (record-symbol-strike! feature) false)

        :else false))))

(def ProbeAnswer
  "A strike-tracked probe's answer: true / false, or :disabled once the
   symbol hit the strike limit."
  [:or :boolean [:= :disabled]])

(m/=> probe-feature! [:=> [:cat [:or :symbol :keyword :string]] ProbeAnswer])

(defn probe-fboundp!
  "Strike-tracked wrapper around (fboundp '<sym>).
   Returns true / false / :disabled."
  [sym]
  (if (contains? @*symbol-disabled sym)
    :disabled
    (let [code (format "(fboundp '%s)" (name sym))
          {:keys [success result error]} (eval-elisp code)]
      (cond
        (and success (= result "t"))
        (do (clear-symbol-strike! sym) true)

        (and success (= result "nil"))
        (do (record-symbol-strike! sym) false)

        (void-symbol-error? error sym)
        (do (record-symbol-strike! sym) false)

        :else false))))

(m/=> probe-fboundp! [:=> [:cat [:or :symbol :keyword :string]] ProbeAnswer])

(defn- resolve-emacs-fn
  "Resolve an editor capability from the hive-spi service registry, or nil
   when no editor addon has published one.

   Evaluation IS a capability — it needs a live editor — so unlike elisp
   source construction it belongs behind the registry, and an unmounted addon
   legitimately means \"no editor\". The seam is the SPI, not the addon: this
   host names no hive-emacs namespace, so any vessel publishing the same
   capability keys serves these call sites."
  [cap]
  (rescue nil (svc/capability cap)))

(defn eval-elisp-with-timeout
  "Execute elisp code with a timeout, through the registered editor service.
   Returns a map with :success, :result or :error keys.
   On timeout, returns {:success false :error \"...\" :timed-out true}
   On circuit-open, returns {:success false :error \"...\" :circuit-open true}

   A timeout above *max-timeout-ms* routes to the ceiling-raising capability
   (long CIDER evals): the adapter owns its own cap, so the host asks for the
   exception rather than rebinding the adapter's var. Falls back to the plain
   capability when the adapter publishes no ceiling verb.

   No editor service registered -> an explicit failure result."
  ([code] (eval-elisp-with-timeout code *default-timeout-ms*))
  ([code timeout-ms]
   (let [over-cap? (> (or timeout-ms 0) *max-timeout-ms*)
         f (or (when over-cap? (resolve-emacs-fn :eval-elisp-raising-ceiling))
               (resolve-emacs-fn :eval-elisp-with-timeout))]
     (if f
       (f code timeout-ms)
       {:success false
        :error "No editor service registered — Emacs integration unavailable"
        :capability :eval-elisp-with-timeout}))))

(m/=> eval-elisp-with-timeout
      [:function
       [:=> [:cat :string] ext-client/EvalResult]
       [:=> [:cat :string [:maybe :int]] ext-client/EvalResult]])

(defn eval-elisp
  "Execute elisp code in running Emacs and return the result.
   Returns a map with :success, :result or :error keys."
  [code]
  (eval-elisp-with-timeout code *default-timeout-ms*))

(m/=> eval-elisp [:=> [:cat :string] ext-client/EvalResult])

(defn eval-elisp!
  "Execute elisp and return result string, or throw on non-timeout error.
   On timeout, returns {:error :timeout :msg \"...\"}.
   On circuit-open, returns {:error :circuit-open :msg \"...\"}."
  [code]
  (let [{:keys [success result error timed-out circuit-open]} (eval-elisp code)]
    (cond
      success      result
      timed-out    {:error :timeout :msg error}
      circuit-open {:error :circuit-open :msg error}
      :else        (throw (ex-info "Elisp evaluation failed"
                                   {:error error :code code})))))

(m/=> eval-elisp! [:=> [:cat :string] ext-client/ElispReturn])

(defn emacs-running?
  "Check if Emacs server is running. Returns false on timeout."
  []
  (:success (eval-elisp-with-timeout "t" 2000)))

(m/=> emacs-running? [:=> [:cat] [:maybe :boolean]])

;; ---------------------------------------------------------------------------
;; Convenience functions — delegate through eval-elisp!
;; Kept for any residual callers; tool handlers use eval-elisp directly.
;; ---------------------------------------------------------------------------

(defn buffer-list [] (eval-elisp! "(mapcar #'buffer-name (buffer-list))"))

(m/=> buffer-list [:=> [:cat] ext-client/ElispReturn])
(defn current-buffer [] (eval-elisp! "(buffer-name)"))

(m/=> current-buffer [:=> [:cat] ext-client/ElispReturn])
(defn current-file []
  (let [result (eval-elisp! "(buffer-file-name)")]
    (when (not= result "nil") result)))

(m/=> current-file [:=> [:cat] ext-client/ElispReturn])
(defn buffer-content [buffer-name]
  (eval-elisp! (format "(with-current-buffer \"%s\" (buffer-string))" buffer-name)))

(m/=> buffer-content [:=> [:cat :string] ext-client/ElispReturn])
(defn switch-to-buffer [buffer-name]
  (eval-elisp! (format "(switch-to-buffer \"%s\")" buffer-name)))

(m/=> switch-to-buffer [:=> [:cat :string] ext-client/ElispReturn])
(defn find-file [file-path]
  (eval-elisp! (format "(find-file \"%s\")" file-path)))

(m/=> find-file [:=> [:cat :string] ext-client/ElispReturn])
(defn save-buffer [] (eval-elisp! "(save-buffer)"))

(m/=> save-buffer [:=> [:cat] ext-client/ElispReturn])
(defn goto-line [line-number]
  (eval-elisp! (format "(goto-line %d)" line-number)))

(m/=> goto-line [:=> [:cat :int] ext-client/ElispReturn])
(defn insert-text [text]
  (eval-elisp! (format "(insert \"%s\")" (clojure.string/escape text {\" "\\\"" \\ "\\\\"}))))

(m/=> insert-text [:=> [:cat :string] ext-client/ElispReturn])
(defn project-root []
  (let [result (eval-elisp! "(project-root (project-current))")]
    (when (not= result "nil") result)))

(m/=> project-root [:=> [:cat] ext-client/ElispReturn])
(defn recent-files [] (eval-elisp! "recentf-list"))

(m/=> recent-files [:=> [:cat] ext-client/ElispReturn])
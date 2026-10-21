(ns hive-mcp.emacs-ext.client
  "Extension-registry façade for `:emacs/*` client keys.

   Replaces the legacy `hive-mcp.emacs.client` delegation shim. Each fn
   resolves a callable from `hive-mcp.extensions.registry` via the
   `:emacs/*` key contributed by hive-emacs's IAddon `(hooks)` method.
   When hive-emacs is not loaded (registry miss), returns a uniformly-
   shaped failure map — never throws.

   Decisions:
   - 20260429195812-0c5dfe8d (outphase free-form ext-keys via IAddon hooks)
   - 20260429230453-7e7627cc (Phase 2: consumers swap shim for registry lookup)"
  (:require [hive-mcp.extensions.registry :as ext]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:dynamic *default-timeout-ms*
  "Default timeout for emacsclient calls in milliseconds.
   Façade-local default — consumers no longer reach into hive-emacs.client."
  5000)

;; ---------------------------------------------------------------------------
;; Value schemas — the shapes every emacsclient path agrees on
;; ---------------------------------------------------------------------------

(def EvalResult
  "Result map of an elisp evaluation: `:success` plus `:result` on success,
   `:error` (a message, or a keyword from the editor SPI) on failure, and the
   `:timed-out` / `:circuit-open` markers of the adapter's breaker."
  [:map
   [:success :boolean]
   [:result {:optional true} [:maybe :string]]
   [:error {:optional true} [:maybe [:or :string :keyword]]]
   [:timed-out {:optional true} :boolean]
   [:circuit-open {:optional true} :boolean]
   [:capability {:optional true} :keyword]])

(def ElispFailure
  "The non-throwing failure an `eval-elisp!`-backed call returns for a
   timeout or an open circuit breaker."
  [:map
   [:error [:enum :timeout :circuit-open]]
   [:msg [:maybe [:or :string :keyword]]]])

(def ElispReturn
  "What an `eval-elisp!`-backed convenience call returns: the result string,
   an `ElispFailure`, or nil when hive-emacs is not loaded or the elisp
   answered \"nil\"."
  [:maybe [:or :string ElispFailure]])

(def FacadeReturn
  "What a registry-backed convenience call passes through. hive-emacs's
   implementation answers like `ElispReturn` (an elisp-printed string); the
   in-process stub contributors answer with data (a vector of names, or true
   for an effect). The façade forwards either unchanged, and nil when no
   contributor is registered."
  [:maybe [:or :string :boolean [:vector :string] ElispFailure]])

(defn- not-loaded
  [k]
  {:success false
   :error (str "hive-emacs not loaded — extension " k " unavailable")})

(defn eval-elisp [code]
  (if-let [f (ext/get-extension :emacs/eval-elisp)]
    (f code)
    (not-loaded :emacs/eval-elisp)))

(m/=> eval-elisp [:=> [:cat :string] EvalResult])

(defn eval-elisp-with-timeout
  ([code] (eval-elisp-with-timeout code *default-timeout-ms*))
  ([code timeout-ms]
   (if-let [f (ext/get-extension :emacs/eval-elisp-with-timeout)]
     (f code timeout-ms)
     (not-loaded :emacs/eval-elisp-with-timeout))))

(m/=> eval-elisp-with-timeout
      [:function
       [:=> [:cat :string] EvalResult]
       [:=> [:cat :string [:maybe :int]] EvalResult]])

(defn emacs-running? []
  (if-let [f (ext/get-extension :emacs/running?)] (f) false))

(m/=> emacs-running? [:=> [:cat] [:maybe :boolean]])

(defn buffer-list []
  (when-let [f (ext/get-extension :emacs/buffer-list)] (f)))

(m/=> buffer-list [:=> [:cat] FacadeReturn])

(defn current-buffer []
  (when-let [f (ext/get-extension :emacs/current-buffer)] (f)))

(m/=> current-buffer [:=> [:cat] FacadeReturn])

(defn current-file []
  (when-let [f (ext/get-extension :emacs/current-file)] (f)))

(m/=> current-file [:=> [:cat] FacadeReturn])

(defn switch-to-buffer [buffer-name]
  (when-let [f (ext/get-extension :emacs/switch-to-buffer)] (f buffer-name)))

(m/=> switch-to-buffer [:=> [:cat :string] FacadeReturn])

(defn find-file [file-path]
  (when-let [f (ext/get-extension :emacs/find-file)] (f file-path)))

(m/=> find-file [:=> [:cat :string] FacadeReturn])

(defn save-buffer []
  (when-let [f (ext/get-extension :emacs/save-buffer)] (f)))

(m/=> save-buffer [:=> [:cat] FacadeReturn])

(defn goto-line [line-number]
  (when-let [f (ext/get-extension :emacs/goto-line)] (f line-number)))

(m/=> goto-line [:=> [:cat :int] FacadeReturn])

(defn insert-text [text]
  (when-let [f (ext/get-extension :emacs/insert-text)] (f text)))

(m/=> insert-text [:=> [:cat :string] FacadeReturn])

(defn project-root []
  (when-let [f (ext/get-extension :emacs/project-root)] (f)))

(m/=> project-root [:=> [:cat] FacadeReturn])

(defn recent-files []
  (when-let [f (ext/get-extension :emacs/recent-files)] (f)))

(m/=> recent-files [:=> [:cat] FacadeReturn])

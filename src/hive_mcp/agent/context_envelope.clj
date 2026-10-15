(ns hive-mcp.agent.context-envelope
  "Context preparation for agent spawn and dispatch. Delegates to extension layer when available."
  (:require [hive-mcp.dns.result :refer [rescue]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.protocols.dispatch :as dispatch-ctx]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const max-context-chars
  "Hard cap for context output (~2.5K tokens)."
  10000)

(defn- try-ext
  "Call the implementation registered for EXT-KEY, else the one named by SYM.
   Returns nil when neither is available or the call throws."
  [ext-key sym & args]
  (rescue nil
          (when-let [f (or (ext/get-extension ext-key)
                           (requiring-resolve sym))]
            (apply f args))))

(defn- cap-output
  "Enforce hard character cap on context output."
  [s]
  (when (and s (string? s) (pos? (count s)))
    (if (> (count s) max-context-chars)
      (subs s 0 max-context-chars)
      s)))

(defn enrich-context
  "Prepare enriched context for agent dispatch via extension layer."
  ([ctx-refs kg-node-ids scope]
   (enrich-context ctx-refs kg-node-ids scope {}))
  ([ctx-refs kg-node-ids scope opts]
   (cap-output
    (try-ext :ctx/enrich
             'hive-mcp.extensions.context/enrich
             ctx-refs kg-node-ids scope opts))))

(defn prepare-spawn-context
  "Prepare context for agent spawn via extension layer."
  ([directory]
   (prepare-spawn-context directory {}))
  ([directory opts]
   (cap-output
    (try-ext :ctx/prepare-spawn
             'hive-mcp.extensions.context/prepare-spawn
             directory opts))))

(defn from-dispatch-context
  "Extract context from an IDispatchContext instance."
  ([dispatch-context]
   (from-dispatch-context dispatch-context {}))
  ([dispatch-context opts]
   (when (and dispatch-context
              (= :ref (dispatch-ctx/context-type dispatch-context)))
     (let [{:keys [ctx-refs kg-node-ids scope]} dispatch-context]
       (enrich-context ctx-refs kg-node-ids scope opts)))))

;; The extension layer may be absent or throw (try-ext rescues to nil), and the
;; output is capped at max-context-chars, so every answer is nil or a
;; non-empty string. ctx-refs / kg-node-ids / scope are forwarded untouched.
(def ^:private Opts [:maybe :map])
(def ^:private Envelope [:maybe [:string {:min 1}]])

(m/=> enrich-context
      [:function
       [:=> [:cat :any :any :any] Envelope]
       [:=> [:cat :any :any :any Opts] Envelope]])

(m/=> prepare-spawn-context
      [:function
       [:=> [:cat :any] Envelope]
       [:=> [:cat :any Opts] Envelope]])

;; The argument is nil or something implementing IDispatchContext: a plain map
;; has no context-type implementation and throws (probed).
(def ^:private DispatchContext
  [:maybe [:fn (fn [x] (satisfies? dispatch-ctx/IDispatchContext x))]])

(m/=> from-dispatch-context
      [:function
       [:=> [:cat DispatchContext] Envelope]
       [:=> [:cat DispatchContext Opts] Envelope]])

;; Backwards-compatible aliases (build-l2-envelope removed — use enrich-context directly)
(def build-spawn-envelope #'prepare-spawn-context)
(def envelope-from-dispatch-context #'from-dispatch-context)

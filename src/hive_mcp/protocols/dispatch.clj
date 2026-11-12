(ns hive-mcp.protocols.dispatch
  "Dispatch context for agent communication: a host facade over
   `hive-spi.swarm.dispatch-context`.

   The protocol and the TextContext / RefContext records are hive-spi's, not
   redefined here: two defprotocols of one name split `satisfies?`, so a
   context built on one side would not be a context on the other (the
   event-backbone defect). This namespace re-exports them and keeps only what
   is host-side:

     ->ref-context    injects the host default reconstruct-fn
                      (hive-mcp.context.reconstruction/reconstruct-context,
                      resolved by symbol at call time)
     ->graph-context  looks up the :dp/graph-ctx extension

   Protocol methods are defn delegates, not `def` aliases: an alias captures
   the method fn at load time and misses an impl registered later by `extend`."
  (:require [hive-dsl.result :refer [rescue]]
            [hive-spi.swarm.dispatch-context :as spi]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; Re-exports from hive-spi (never a second defprotocol)
;;; ============================================================================

(def IDispatchContext
  "hive-spi's IDispatchContext, the one dispatch-context protocol."
  spi/IDispatchContext)

(defn resolve-context
  "Delegates to `spi/resolve-context`: a map with at minimum {:prompt string}."
  [ctx]
  (spi/resolve-context ctx))

(defn context-type
  "Delegates to `spi/context-type`: the keyword naming the context kind."
  [ctx]
  (spi/context-type ctx))

(def ->TextContext "hive-spi's TextContext positional constructor." spi/->TextContext)
(def map->TextContext "hive-spi's TextContext map constructor." spi/map->TextContext)
(def ->RefContext "hive-spi's RefContext positional constructor." spi/->RefContext)
(def map->RefContext "hive-spi's RefContext map constructor." spi/map->RefContext)

(defn- dispatch-context?
  "Does `x` satisfy the (hive-spi) IDispatchContext protocol?"
  [x]
  (satisfies? spi/IDispatchContext x))

(def ^:private AnyContext
  "Any IDispatchContext; generates TextContexts."
  [:fn {:gen/schema :string :gen/fmap spi/->TextContext} dispatch-context?])

(m/=> resolve-context [:=> [:cat AnyContext] [:map [:prompt :any]]])
(m/=> context-type [:=> [:cat AnyContext] :keyword])

;;; ============================================================================
;;; Factory Functions
;;; ============================================================================

(defn ->text-context
  "Create a TextContext wrapping a plain text prompt."
  [prompt]
  (spi/->text-context prompt))

(def DispatchContext
  "Any value satisfying IDispatchContext; generates TextContexts."
  [:fn {:gen/schema :string :gen/fmap spi/->TextContext}
   dispatch-context?])

(def TextContextValue
  "A TextContext record."
  [:fn {:gen/schema :string :gen/fmap spi/->TextContext}
   #(instance? hive_spi.swarm.dispatch_context.TextContext %)])

(m/=> ->text-context [:=> [:cat :any] TextContextValue])

(defn- host-reconstruct
  "The host default reconstruct-fn, resolved lazily to avoid circular deps:
   hive-mcp.context.reconstruction loads at call time, not at require time."
  [refs nodes scope]
  (when-let [f (rescue nil (requiring-resolve
                            'hive-mcp.context.reconstruction/reconstruct-context))]
    (f refs nodes scope)))

(defn ->ref-context
  "Create a RefContext for pass-by-reference dispatch. Without a
   :reconstruct-fn the host default reconstruction is injected."
  [prompt {:keys [reconstruct-fn] :as opts}]
  (spi/->ref-context prompt
                     (assoc (or opts {}) :reconstruct-fn
                            (or reconstruct-fn host-reconstruct))))

(def RefContextOpts
  "The options `->ref-context` destructures; nil reads as {}."
  [:maybe [:map
           [:ctx-refs {:optional true} [:maybe [:map-of :any :any]]]
           [:kg-node-ids {:optional true} [:maybe [:or [:sequential :any] [:set :any]]]]
           [:scope {:optional true} :any]
           [:reconstruct-fn {:optional true} [:maybe ifn?]]]])

(def RefContextValue
  "A RefContext record."
  [:fn {:gen/schema :string :gen/fmap (fn [p] (->ref-context p {}))}
   #(instance? hive_spi.swarm.dispatch_context.RefContext %)])

(m/=> ->ref-context [:=> [:cat :any RefContextOpts] RefContextValue])

(defn ->graph-context
  "Create an extension-backed dispatch context."

  [task-node-id kg-store]
  (if-let [f (rescue nil
               (when-let [get-ext (requiring-resolve 'hive-mcp.extensions.registry/get-extension)]
                 (get-ext :dp/graph-ctx)))]
    (f task-node-id kg-store)
    (spi/->text-context (str "Extension context unavailable. Task node: " task-node-id))))

(m/=> ->graph-context [:=> [:cat :any :any] DispatchContext])

;;; ============================================================================
;;; Backward Compatibility
;;; ============================================================================

(defn ensure-context
  "Coerce a string or IDispatchContext into an IDispatchContext."
  [prompt-or-context]
  (spi/ensure-context prompt-or-context))

(m/=> ensure-context [:=> [:cat :any] DispatchContext])

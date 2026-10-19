;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.knowledge-graph.slots.schema
  "Malli value objects for the slot routing layer: breaker records and
   policies, recovery policies, and the three role ports as predicates.

   Port generators build the production implementation late-bound, so this
   namespace does not compile-couple to the modules whose contracts use it."
  (:require [hive-mcp.knowledge-graph.slots.protocol :as p]))

(def BreakerState
  "The breaker state machine's states."
  [:enum :closed :open :half-open])

(def Breaker
  "A breaker record. Every key is optional on input: the transition fns
   default each one."
  [:map
   [:state {:optional true} [:maybe BreakerState]]
   [:failures {:optional true} [:maybe [:int {:min 0}]]]
   [:opened-at {:optional true} [:maybe :int]]
   [:cooldown-ms {:optional true} [:maybe [:int {:min 0}]]]])

(def FreshBreaker
  "The breaker of a slot that has not failed."
  [:map
   [:state [:= :closed]]
   [:failures [:= 0]]
   [:opened-at :nil]
   [:cooldown-ms [:= 0]]])

(def FailedBreaker
  "A breaker after a failure was accounted: state, count and cooldown are
   set; opened-at is set once it trips."
  [:map
   [:state [:enum :closed :open]]
   [:failures [:int {:min 1}]]
   [:opened-at {:optional true} [:maybe :int]]
   [:cooldown-ms [:int {:min 0}]]])

(def Policy
  "Breaker tuning knobs."
  [:map
   [:max-failures [:int {:min 1}]]
   [:initial-cooldown-ms [:int {:min 0}]]
   [:max-cooldown-ms [:int {:min 0}]]])

(def BreakerAtom
  "The atom of slot -> breaker the stateful breaker fns swap."
  [:fn {:error/message "should be an atom"
        :gen/schema [:map-of :keyword FreshBreaker]
        :gen/fmap atom}
   #(instance? clojure.lang.IAtom %)])

(def Decision
  "A breaker decision."
  [:enum :pass :block])

(def RecoveryStrategy
  "A heal-and-open! strategy: one step, or a chain of them."
  [:or :keyword [:vector :keyword]])

(def RecoveryPolicy
  "A slot's heal-and-open! policy."
  [:map
   [:strategy {:optional true} RecoveryStrategy]
   [:max-attempts {:optional true} :int]])

(def PathLookup
  "A config lookup: a fn of a get-in path to the value there."
  [:fn {:error/message "should be a path lookup fn"
        :gen/elements [(constantly nil)
                       (fn [path] (when (= :recovery-policy (last path))
                                    {:strategy "audit" :max-attempts 1}))
                       (fn [path] (when (= :backend (last path)) "datascript"))]}
   ifn?])

(def SlotBackendMap
  "A slot -> backend default mapping."
  [:maybe [:map-of :keyword :keyword]])

(defn- late
  "Call the fn named by SYM with ARGS, resolved at generation time."
  [sym & args]
  (apply (requiring-resolve sym) args))

(def Resolver
  "An IBackendResolver."
  [:fn {:error/message "should satisfy IBackendResolver"
        :gen/schema :nil
        :gen/fmap (fn [_] (late 'hive-mcp.knowledge-graph.slots.config/->resolver
                                (constantly nil)))}
   #(satisfies? p/IBackendResolver %)])

(def FactoryFn
  "A backend factory fn, of (backend) or (backend opts)."
  [:fn {:error/message "should be a factory fn"
        :gen/elements [(constantly nil)]}
   ifn?])

(def Factory
  "An IBackendFactory."
  [:fn {:error/message "should satisfy IBackendFactory"
        :gen/schema :nil
        :gen/fmap (fn [_] (late 'hive-mcp.knowledge-graph.slots.factory/->factory
                                (constantly nil)))}
   #(satisfies? p/IBackendFactory %)])

(def Registry
  "An ISlotRegistry."
  [:fn {:error/message "should satisfy ISlotRegistry"
        :gen/schema :nil
        :gen/fmap (fn [_] (late 'hive-mcp.knowledge-graph.slots.registry/->registry
                                (late 'hive-mcp.knowledge-graph.slots.config/->resolver
                                      (constantly nil))
                                (late 'hive-mcp.knowledge-graph.slots.factory/->factory
                                      (constantly nil))))}
   #(satisfies? p/ISlotRegistry %)])

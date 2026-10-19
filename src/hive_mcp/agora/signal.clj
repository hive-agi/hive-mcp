(ns hive-mcp.agora.signal
  "Data-driven signal schema for Agora dialogues.

   Compat shim: moved to hive-agent.swarm.agora.signal in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.agora.signal" sym))

(def signal-types @(impl 'signal-types))

(def equilibrium-signals @(impl 'equilibrium-signals))

(def disruption-signals @(impl 'disruption-signals))

(def neutral-signals @(impl 'neutral-signals))

(def all-valid-signals @(impl 'all-valid-signals))

(def default-strength @(impl 'default-strength))

(def default-evidence @(impl 'default-evidence))

;; Value schemas for the shim contracts. They transcribe the docstrings of
;; hive-agent.swarm.agora.signal, where the behaviour lives.

(def SignalType
  "Every recognised signal keyword (`all-valid-signals`): the five turn
   signals plus the `:lgtm` alias."
  [:enum :propose :counter :no-change :approve :defer :lgtm])

(def Signal
  "A constructed signal: type, clamped strength, target, evidence, message."
  [:map
   [:type SignalType]
   [:strength [:double {:min 0.0 :max 1.0}]]
   [:target :any]
   [:evidence [:vector :any]]
   [:message :string]])

(def SignalError
  "The refusal a constructor answers instead of a signal."
  [:map [:error :string]])

(def SignalOrError
  "A signal or its refusal."
  [:or Signal SignalError])

(def SignalInput
  "The structured map `signal-from-map` reads. Every key is optional: a
   missing type or message is answered with a refusal, not a throw."
  [:maybe
   [:map
    [:type {:optional true} [:maybe [:or :keyword :string]]]
    [:signal {:optional true} [:maybe [:or :keyword :string]]]
    [:strength {:optional true} [:maybe number?]]
    [:confidence {:optional true} [:maybe number?]]
    [:target {:optional true} :any]
    [:evidence {:optional true} [:maybe [:sequential :any]]]
    [:message {:optional true} [:maybe :string]]]])

(def Contribution
  "How a signal moves a dialogue toward equilibrium."
  [:enum :positive :negative :neutral])

(def Classifiable
  "What `equilibrium-contribution` classifies: a signal map or a bare type.
   An unknown value is classified, never refused."
  [:or :map :keyword :string :nil])

(defn signal-from-map
  {:arglists '([{:keys [type signal strength confidence target evidence message], :as input}])}
  [& args]
  (apply (impl 'signal-from-map) args))

(m/=> signal-from-map [:=> [:cat SignalInput] SignalOrError])

(defn signal-from-legacy
  {:arglists '([message])}
  [& args]
  (apply (impl 'signal-from-legacy) args))

(m/=> signal-from-legacy
      [:=> [:cat :string] [:tuple Signal :string [:enum :prefix :default]]])

(defn valid-signal?
  {:arglists '([signal-map])}
  [& args]
  (apply (impl 'valid-signal?) args))

(m/=> valid-signal? [:=> [:cat :any] :boolean])

(defn signal-type
  {:arglists '([signal-map])}
  [& args]
  (apply (impl 'signal-type) args))

(m/=> signal-type
      [:=> [:cat [:maybe [:map [:type {:optional true} [:maybe :keyword]]]]]
       [:maybe :keyword]])

(defn equilibrium-contribution
  {:arglists '([signal-or-type])}
  [& args]
  (apply (impl 'equilibrium-contribution) args))

(m/=> equilibrium-contribution [:=> [:cat Classifiable] Contribution])

(defn format-signal-prefix
  {:arglists '([signal-type message])}
  [& args]
  (apply (impl 'format-signal-prefix) args))

(m/=> format-signal-prefix [:=> [:cat [:or :keyword :string :symbol] :any] :string])

(defn signal->json-schema
  {:arglists '([])}
  [& args]
  (apply (impl 'signal->json-schema) args))

(m/=> signal->json-schema
      [:=> [:cat]
       [:map
        [:type [:= "object"]]
        [:properties [:map-of :keyword :map]]
        [:required [:vector :string]]]])

(defn parse-signal
  {:arglists '([input] [input default-message])}
  [& args]
  (apply (impl 'parse-signal) args))

;; The input is open on purpose: the body dispatches on map, string and keyword
;; and refuses every other value with an {:error ...} map rather than a throw.
(m/=> parse-signal
      [:function
       [:=> [:cat :any] SignalOrError]
       [:=> [:cat :any [:maybe :string]] SignalOrError]])

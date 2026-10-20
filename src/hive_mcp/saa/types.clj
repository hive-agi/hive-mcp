(ns hive-mcp.saa.types
  "ADT closed sums for the SAA seam.

   - SaaRegistryEntry — addon contributions routed to the SAA registry by key namespace
   - PhaseMessage     — envelopes streamed on a phase out-ch

   Compile-time exhaustiveness via `hive-dsl.adt/adt-case`. raw-msg->phase-message
   lifts the bridge query! raw message shapes into PhaseMessage variants."
  (:require [hive-dsl.adt :as adt]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; SaaRegistryEntry — what addons contribute to the SAA registry
;; =============================================================================

(adt/defadt SaaRegistryEntry
  "Addon contribution routed by key namespace through the SAA registry.
   An addon returns these from `(hooks [this])` under any `:saa/*` key.

   :saa/phase-provider — registers an IPhaseProvider implementation
   :saa/scorer         — registers an IObservationScorer implementation
   :saa/planner        — registers an IPlanSynthesizer implementation
   :saa/tool-intent    — maps a neutral tool-intent keyword to a concrete tool set
   :saa/plan-store     — the store-plan port: :store is
                         (fn [plan agent-id directory])
                         => {:memory-id _ :kanban-ids _ :kg-edges _}.
                         Used only by runs that opt in with :store-plan? true.
   :saa/dispatch-mode  — one Act execution mode, keyed by :mode (open set):
                         :dispatch is (fn [plan agent-id ctx]) => {:wave-id _ :result _},
                         ctx carries :run-id, :plan-memory-id and :directory."
  [:saa/phase-provider {:provider (constantly true) :owner keyword?}]
  [:saa/scorer         {:scorer (constantly true) :owner keyword?}]
  [:saa/planner        {:planner (constantly true) :owner keyword?}]
  [:saa/tool-intent    {:intent keyword? :tools coll? :owner keyword?}]
  [:saa/plan-store     {:store ifn? :owner keyword?}]
  [:saa/dispatch-mode  {:mode keyword? :dispatch ifn? :owner keyword?}])

;; =============================================================================
;; PhaseMessage — streamed envelopes on a phase out-ch
;; =============================================================================

(adt/defadt PhaseMessage
  "An envelope streamed on a SAA phase out-ch.

   :pm/started        — phase began
   :pm/chunk          — incremental content from the provider
   :pm/observation    — a structured observation collected in Silence
   :pm/phase-complete — phase finished with its terminal payload
   :pm/error          — phase failed
   :pm/saa-complete   — the full SAA cycle finished"
  [:pm/started        {:phase keyword?}]
  [:pm/chunk          {:phase keyword? :content any?}]
  [:pm/observation    {:phase keyword? :observation any?}]
  [:pm/phase-complete {:phase keyword? :payload any?}]
  [:pm/error          {:phase keyword? :error any?}]
  [:pm/saa-complete   {:summary map?}])

;; =============================================================================
;; raw-msg->phase-message — bridge query! raw → PhaseMessage (FIX#6)
;; =============================================================================

;; =============================================================================
;; Malli shapes — the ADT values, and the child-registry state they land in
;; =============================================================================

(def PhaseMessageVariant
  "The closed set of `PhaseMessage` variants."
  [:enum :pm/started :pm/chunk :pm/observation :pm/phase-complete :pm/error
   :pm/saa-complete])

(def PhaseMessageValue
  "Any `PhaseMessage` ADT value; payload keys depend on the variant."
  [:map
   [:adt/type [:= :PhaseMessage]]
   [:adt/variant PhaseMessageVariant]])

(def RawMsg
  "A bridge query! raw message. Open map: an unrecognized `:type` (or none)
   becomes an opaque :pm/chunk."
  [:map [:type {:optional true} :any]])

(def Owner
  "A registry owner — an addon-id keyword, or :saa/core for boot seeds."
  :keyword)

(def RegisterOutcome
  "What a singleton child registry's `register!` answers."
  [:enum :ok :replaced :conflict])

(def RegistryRecord
  "One stored singleton-registry entry: the contributed entry map stamped
   with its owner and registration instant. The owner is stored as given
   (an addon id, documented as a keyword)."
  [:map [:owner :any] [:registered-at inst?]])

(def RegistryState
  "State of a singleton child registry (providers, scorers, planners,
   plan-stores, dispatch-modes): id → record, owner → set of ids."
  [:map
   [:by-id [:map-of :any RegistryRecord]]
   [:by-owner [:map-of :any [:set :any]]]])

(def RegistrySnapshot
  "A singleton child registry's `snapshot` value."
  [:map [:version :int] [:data RegistryState]])

(def RestoreArg
  "What `restore!` accepts: a prior snapshot, or nil (restore to empty)."
  [:maybe [:map [:data {:optional true} [:maybe :map]]]])

(defn raw-msg->phase-message
  "Map a bridge query! raw message into a PhaseMessage variant.

   Raw shapes (from sdk.execution / bridge):
     {:type :message  :phase _ :data _}  → :pm/chunk
     {:type :complete ...}               → :pm/phase-complete
     {:type :result   ...}               → :pm/phase-complete
     {:type :error    :error/:message _} → :pm/error
     {:type :saa-complete ...}           → :pm/saa-complete
   phase is the SAA phase keyword to stamp when the raw message omits one."
  [raw-msg phase]
  (let [ph (or (:saa-phase raw-msg) (:phase raw-msg) phase)]
    (case (:type raw-msg)
      :message
      (phase-message :pm/chunk {:phase ph :content (or (:content raw-msg) (:data raw-msg))})

      (:complete :result)
      (phase-message :pm/phase-complete
                     {:phase ph :payload (or (:content raw-msg) (:data raw-msg) raw-msg)})

      :error
      (phase-message :pm/error {:phase ph :error (or (:error raw-msg) (:message raw-msg))})

      :saa-complete
      (phase-message :pm/saa-complete {:summary (dissoc raw-msg :type)})

      ;; default: treat any unrecognized shape as an opaque chunk
      (phase-message :pm/chunk {:phase ph :content raw-msg}))))

;; phase is the stamp for a raw message without one — the adapter passes the
;; provider-options :phase, which may be absent
(m/=> raw-msg->phase-message [:=> [:cat RawMsg [:maybe :keyword]] PhaseMessageValue])

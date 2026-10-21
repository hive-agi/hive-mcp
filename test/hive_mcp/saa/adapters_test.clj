(ns hive-mcp.saa.adapters-test
  "Facets for DefaultPhaseProvider: the IPhaseProvider law suite run against
   the adapter built by ->default-phase-provider (phase-config answers the
   model's descriptor; build-options is the neutral descriptor projection
   with caller options winning), a trifecta on the constructor's contract, and
   a golden over execute-phase! driven by support's MockSession, which replays
   raw bridge messages through the agent-bridge port."
  (:require [hive-mcp.protocols.saa :as psaa]
            [hive-mcp.saa.adapters :as adapters]
            [hive-mcp.saa.model :as model]
            [hive-mcp.saa.support :as support]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private NeutralOpts
  [:map
   [:capabilities {:optional true} [:set [:enum :read :write]]]
   [:extra {:optional true} [:enum "x" "y"]]])

(deftest-port-contract phase-provider-port-laws
  {:port/methods
   {psaa/phase-config  {:in  [:cat model/SaaPhase]
                        :out [:map [:phase model/SaaPhase] [:permission-intent :keyword]]
                        :rel (fn [[phase] out] (= out (model/phase-descriptor phase)))}
    psaa/build-options {:in  [:cat model/SaaPhase NeutralOpts]
                        :out [:map [:phase model/SaaPhase] [:capabilities :any] [:prompt :string]]
                        :rel (fn [[phase opts] out]
                               (let [d (model/phase-descriptor phase)]
                                 (= out (merge {:phase phase
                                                :capabilities (:tool-intent d)
                                                :permission-intent (:permission-intent d)
                                                :prompt (:goal-prompt-fragment d)}
                                               opts))))}}}
  (adapters/->default-phase-provider))

(deftrifecta-from-schema default-phase-provider-constructor
  hive-mcp.saa.adapters/->default-phase-provider
  {:in [:cat [:or :nil [:enum "session-a" :backend]]]
   :out adapters/PhaseProvider
   :rel (fn [[backend] p] (= backend (:backend-or-session p)))
   ;; Mutation facet off: PhaseProvider's only generator is the nil-backed
   ;; record, so every schema-derived mutant equals a real output.
   :mutation false})

(golden/deftest-golden execute-phase-lifts-raw-messages-golden
  "test/golden/hive-mcp/saa/adapters-execute-phase.edn"
  (let [provider (adapters/->default-phase-provider)
        run (fn [phase raws]
              (->> (psaa/execute-phase! provider (support/->mock-session raws) "prompt"
                                        (psaa/build-options provider phase {}))
                   support/drain
                   (mapv #(select-keys % [:adt/type :adt/variant :phase]))))]
    {:silence (run :silence [{:type :message :content "hi"} {:type :result :result "done"}])
     :act (run :act [{:type :error :error "bad"}])
     :empty (run :abstract [])}))

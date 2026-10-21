(ns hive-mcp.agent.agent-definition.validate-test
  "Trifectas for the agent-definition validators, over a domain that mixes
   valid definitions with ones broken in a single field, driven by the
   contracts on `hive-mcp.agent.agent-definition.validate`."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.agent.agent-definition.spec :as spec]
            [hive-mcp.agent.agent-definition.validate :as v]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Candidate
  "A definition map whose fields are each drawn from a valid and an invalid
   value, so roughly half the cases fail validation in a known field."
  [:map
   [:agent-type [:enum "explore" "a1" 7]]
   [:description [:enum "Fast" ""]]
   [:system-prompt [:enum "You search." :nope]]
   [:max-turns {:optional true} [:enum 3 0]]
   [:source {:optional true} [:enum :user :plugin :bogus]]
   [:model {:optional true} [:enum "opus" :inherit :gpt]]])

(defn- spec-valid? [d] (m/validate spec/AgentDefinition d))

(deftrifecta-from-schema validate-trifecta
  hive-mcp.agent.agent-definition.validate/validate
  {:in          Candidate
   :out         spec/Validation
   :rel         (fn [d out]
                  (if (spec-valid? d)
                    (= {:valid true :data d} out)
                    (and (false? (:valid out)) (map? (:errors out)))))
   :num-tests   300
   :n-cases     12
   :golden-path "test/golden/agent/validate-candidates.edn"})

(deftrifecta-from-schema valid?-trifecta
  hive-mcp.agent.agent-definition.validate/valid?
  {:in        Candidate
   :out       :boolean
   :rel       (fn [d out] (= out (spec-valid? d)))
   :num-tests 300
   :n-cases   12})

(deftrifecta-from-schema explain-trifecta
  hive-mcp.agent.agent-definition.validate/explain
  {:in        Candidate
   :out       [:maybe [:map-of :keyword :any]]
   :rel       (fn [d out]
                (if (spec-valid? d)
                  (nil? out)
                  ;; every humanized key names a field of the candidate
                  (and (seq out) (every? #(contains? d %) (keys out)))))
   :num-tests 300
   :n-cases   12})

(defrecord ^:private Rec [agent-type description system-prompt model])

(deftest records-are-coerced-and-nils-stripped-test
  (let [r (->Rec "a" "d" "p" nil)]
    (is (= {:valid true :data {:agent-type "a" :description "d" :system-prompt "p"}}
           (v/validate r)))
    (is (true? (v/valid? r)))
    (is (= {:agent-type "a" :description "d" :system-prompt "p"} (v/validate-or-throw! r)))))

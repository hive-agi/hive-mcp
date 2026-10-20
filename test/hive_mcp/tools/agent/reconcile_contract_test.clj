(ns hive-mcp.tools.agent.reconcile-contract-test
  "Contract facets for elisp-row reconciliation. The probe is a KnownAgents
   record built from generated id sets (the ILivenessEvidence port), never
   the live registry. The oracle is the ns-doc table: a phantom of a
   registered agent is dropped, one only heard is :unregistered, the rest
   stay :orphan and are hidden unless stale rows are requested; a live row
   whose registry row is retired becomes :orphaned."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.agent.reconcile :as rc]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id [:enum "a" "b" "c"])

(def ^:private Row
  "An orphan row (synthetic id) or a live-looking row, over a tiny id
   universe so it collides with the probe's sets."
  [:or
   [:map {:gen/fmap (fn [{n :slave/name :as r}] (assoc r :slave/id (str "swarm-" n "-orphan")))}
    [:slave/name Id] [:slave/status [:= :orphan]]]
   [:map [:slave/id Id] [:slave/status [:enum :idle :working]]]])

(defn- probe-of
  "A KnownAgents probe over registered / heard / retired id sets."
  [[reg heard retired]]
  (rc/->known-agents reg heard (into {} (for [id retired] [id {:slave/id id :slave/status :zombie}]))))

(def ^:private Probe
  [:fn {:gen/schema [:tuple [:set Id] [:set Id] [:set Id]] :gen/fmap probe-of}
   #(satisfies? rc/ILivenessEvidence %)])

(defn- expected
  "The reconciled status of ROW under the probe's sets, or ::dropped."
  [probe row stale?]
  (if (= :orphan (:slave/status row))
    (let [n (:slave/name row)]
      (cond ((:registered probe) n) ::dropped
            ((:heard probe) n)      :unregistered
            stale?                  :orphan
            :else                   ::dropped))
    (if (contains? (:retired probe) (:slave/id row)) :orphaned (:slave/status row))))

;; No :golden-path: the generated input holds a KnownAgents record, which
;; reads back from EDN as a plain map and so never equals the live input.
;; reconcile-row-golden below pins the same table over literal rows.
(deftrifecta-from-schema reconcile-follows-the-liveness-table
  #'rc/reconcile
  {:in [:cat Probe [:vector {:max 4} Row] [:map [:include-stale? :boolean]]]
   :out [:vector rc/AgentRow]
   :rel (fn [[probe rows {stale? :include-stale?}] out]
          (= (keep #(let [e (expected probe % stale?)] (when (not= ::dropped e) e)) rows)
             (map :slave/status out)))
   :classify (fn [[_ rows] out] (cond (empty? rows) :empty
                                      (< (count out) (count rows)) :dropped
                                      :else :kept))
   :classify-domain #{:empty :dropped :kept}
   :classify-floor 10
   :num-tests 200})

(deftrifecta-from-schema phantom-name-unwraps-the-synthetic-id
  #'rc/phantom-name
  {:in [:cat [:map [:slave/id [:enum "swarm-x-orphan" "swarm--orphan" "ling-1" "swarm-y"]]
              [:slave/name {:optional true} [:enum "" "named"]]]]
   :out [:maybe :string]
   :rel (fn [[{id :slave/id nm :slave/name}] out]
          (= out (cond (seq nm) nm
                       (= id "swarm-x-orphan") "x"
                       (= id "swarm--orphan") ""
                       :else id)))
   :golden-path "test/golden/hive-mcp/tools/agent/reconcile-phantom-name.edn"
   :num-tests 60})

(golden/deftest-golden reconcile-row-golden
  "test/golden/hive-mcp/tools/agent/reconcile-rows.edn"
  (let [dead  {:slave/id "z" :slave/status :zombie :slave/cwd "/w" :slave/project-id "p"
               :slave/status-changed-at 0}
        probe (probe-of [#{"r"} #{"h"} #{"z"}])
        rows  [{:slave/id "swarm-r-orphan" :slave/status :orphan}
               {:slave/id "swarm-h-orphan" :slave/status :orphan}
               {:slave/id "swarm-q-orphan" :slave/status :orphan}
               {:slave/id "z" :slave/status :working}
               {:slave/id "ok" :slave/status :idle}]]
    {:ghost      (rc/orphan-ghost {:slave/id "z" :slave/status :working} dead)
     :retired?   (mapv rc/retired? [nil {:slave/alive? false} {:slave/status :dead} {:slave/status :idle}])
     :row?       [(rc/orphan-row? {:slave/status :orphan}) (rc/orphaned-row? {:slave/status :orphaned})]
     :fresh      (rc/reconcile probe rows {:include-stale? false})
     :stale      (rc/reconcile probe rows {:include-stale? true})}))

(deftest live-evidence-is-a-probe
  (is (m/validate rc/KnownAgentsValue (rc/live-evidence)))
  (is (= :none (rc/evidence (rc/->known-agents nil nil) "q"))))

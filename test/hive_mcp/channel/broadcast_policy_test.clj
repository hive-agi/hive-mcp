(ns hive-mcp.channel.broadcast-policy-test
  "Trifectas for the broadcast reason policy
   (`hive-mcp.channel.broadcast-policy`), driven by its contracts.

   Split out of the former `hive-mcp.channel.broadcast-trifecta-test` so the
   test namespace is named after its source namespace; the forms are
   unchanged."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.channel.broadcast-ledger :as ledger]
            [hive-mcp.channel.broadcast-policy :as policy]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (ledger/reset-ledger!) (try (f) (finally (ledger/reset-ledger!)))))

(def ^:private Reason
  [:enum :halt :membership :shared-discovery :coordinator-directive
   "halt" ":membership" "gossip" :because "" nil])

(hst/deftrifecta-from-schema admissible-reason-contract
  hive-mcp.channel.broadcast-policy/admissible-reason?
  {:in        Reason
   :out       :boolean
   :rel       (fn [r out]
                (= out (contains? #{:halt :membership :shared-discovery :coordinator-directive
                                    "halt" ":membership"} r)))
   :n-cases   16
   :golden-path "test/golden/channel/broadcast-admissible-reason.edn"})

(hst/deftrifecta-from-schema volume-exempt-contract
  hive-mcp.channel.broadcast-policy/volume-exempt?
  {:in        Reason
   :out       :boolean
   :rel       (fn [r out] (= out (contains? #{:halt "halt"} r)))
   :n-cases   16})

(def ^:private MsgIn
  [:map
   [:broadcast? {:optional true} :boolean]
   [:broadcast-reason {:optional true} Reason]
   [:to {:optional true} [:enum "ling-2"]]])

(def ^:private OptsIn
  [:maybe [:map [:recent-broadcasts {:optional true} [:enum 0 7 8 50]]
           [:budget {:optional true} [:enum 8 100]]]])

(defn- expected-decision
  "The verdict the docstring describes, written out rule by rule."
  [{:keys [broadcast? broadcast-reason to]} {:keys [recent-broadcasts budget]}]
  (let [budget   (or budget 8)
        fallback (if to {:verdict :direct} {:verdict :spawner})
        reason   (cond (keyword? broadcast-reason) broadcast-reason
                       (and (string? broadcast-reason) (seq broadcast-reason))
                       (keyword (if (= \: (first broadcast-reason)) (subs broadcast-reason 1) broadcast-reason)))
        ok?      (contains? policy/admissible-reasons reason)]
    (cond
      (not broadcast?) fallback
      (some? to) (assoc fallback :refused :addressed-to-a-peer)
      (not ok?) (assoc fallback :refused (if (nil? broadcast-reason) :no-reason-given :reason-not-admissible))
      (and (number? recent-broadcasts) (>= recent-broadcasts budget) (not= :halt reason))
      (assoc fallback :refused :budget-exhausted)
      :else {:verdict :broadcast :reason reason})))

(hst/deftrifecta-from-schema decide-contract
  hive-mcp.channel.broadcast-policy/decide
  {:in        [:cat MsgIn OptsIn]
   :out       policy/Decision
   :rel       (fn [[msg opts] out] (= out (expected-decision msg opts)))
   :num-tests 300
   :n-cases   16
   :golden-path "test/golden/channel/broadcast-decide.edn"})

(hst/deftrifecta-from-schema admitted-contract
  hive-mcp.channel.broadcast-policy/admitted?
  {:in        MsgIn
   :out       :boolean
   :rel       (fn [msg out] (= out (= :broadcast (:verdict (expected-decision msg nil)))))
   :num-tests 200})

(hst/deftrifecta-from-schema apply-policy-contract
  hive-mcp.channel.broadcast-policy/apply-policy
  {:in        [:cat [:map
                     [:message [:enum "m"]]
                     [:broadcast? {:optional true} :boolean]
                     [:broadcast-reason {:optional true} Reason]
                     [:to {:optional true} [:enum "ling-2"]]]
               OptsIn]
   :out       :map
   ;; a refused broadcast is downgraded, never dropped: the message survives
   :rel       (fn [[msg opts] out]
                (let [d (expected-decision msg opts)]
                  (and (= "m" (:message out))
                       (= (= :broadcast (:verdict d)) (true? (:broadcast? out)))
                       (= (:refused d) (:broadcast-refused out)))))
   :num-tests 300
   :n-cases   12})

(deftest decide-one-arity-is-no-volume-gate-test
  (is (= {:verdict :broadcast :reason :membership}
         (policy/decide {:broadcast? true :broadcast-reason :membership}))))

(deftest policy-schemas-generate-test
  (doseq [s [policy/RoutedMsg policy/VolumeOpts policy/Decision]]
    (is (seq (mg/sample s {:size 3})) (pr-str (m/form s)))))

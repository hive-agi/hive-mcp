(ns hive-mcp.agent.ling.terminal-registry-test
  "Facets for the terminal registry: a golden over one scripted register /
   resolve / replace / deregister session with an injected ITerminalAddon
   stub, and a trifecta on register-terminal!'s refusal of non-addons."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-addon.terminal :as terminal]
            [hive-mcp.agent.ling.strategy :as strategy]
            [hive-mcp.agent.ling.terminal-registry :as reg]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden-fn]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (reg/clear-registry!) (try (f) (finally (reg/clear-registry!)))))

(defn- stub-addon [id]
  (reify terminal/ITerminalAddon
    (terminal-id [_] id)
    (terminal-spawn! [_ ctx _] (str (name id) "-" (:id ctx)))
    (terminal-dispatch! [_ _ _] true)
    (terminal-status [_ _ ds] ds)
    (terminal-kill! [_ ctx] {:killed? true :id (:id ctx)})
    (terminal-interrupt! [_ ctx] {:success? true :ling-id (:id ctx)})))

(deftest-golden-fn terminal-registry-session-golden
  "test/golden/agent/terminal-registry-session.edn"
  (fn []
    (reg/clear-registry!)
    (let [spawn-via (fn [k] (some-> (reg/resolve-terminal-strategy k)
                                    (strategy/strategy-spawn! {:id "l1"} {})))
          steps [[:empty        (reg/registered-terminals)]
                 [:reg-alpha    (reg/register-terminal! :alpha (stub-addon :alpha))]
                 [:reg-bad      (reg/register-terminal! :bad "not-an-addon")]
                 [:reg-beta     (reg/register-terminal! :beta (stub-addon :beta))]
                 [:terminals    (reg/registered-terminals)]
                 [:spawn-alpha  (spawn-via :alpha)]
                 [:replace      (reg/register-terminal! :alpha (stub-addon :alpha2))]
                 [:spawn-alpha2 (spawn-via :alpha)]
                 [:spawn-none   (spawn-via :nope)]
                 [:raw-is-addon (satisfies? terminal/ITerminalAddon (reg/get-terminal-addon :beta))]
                 [:raw-missing  (reg/get-terminal-addon :nope)]
                 [:dereg-beta   (reg/deregister-terminal! :beta)]
                 [:dereg-again  (reg/deregister-terminal! :beta)]
                 [:after        (reg/registered-terminals)]
                 [:clear        (reg/clear-registry!)]
                 [:cleared      (reg/registered-terminals)]]]
      (into [] steps))))

(deftrifecta-from-schema register-refuses-non-addons-trifecta
  hive-mcp.agent.ling.terminal-registry/register-terminal!
  {:in        [:cat [:enum :a :b :c] [:or :nil :string :int [:map-of :keyword :int]]]
   :out       [:map [:registered? :boolean] [:terminal-id :keyword]
               [:errors {:optional true} [:vector :string]]]
   :rel       (fn [[k _] out]
                (and (= {:registered? false :terminal-id k} (dissoc out :errors))
                     (seq (:errors out))
                     (nil? (reg/resolve-terminal-strategy k))))
   :num-tests 100})

(deftest registry-starts-clean-test
  (is (= #{} (reg/registered-terminals))))

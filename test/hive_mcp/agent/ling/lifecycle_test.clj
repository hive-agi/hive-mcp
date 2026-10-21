(ns hive-mcp.agent.ling.lifecycle-test
  "Facets for the ling lifecycle helpers: a trifecta on the pure `ling-ctx`
   projection, and a golden on `resolve-strategy` with a stub terminal addon
   registered through the terminal registry (the seam it reads)."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-addon.terminal :as terminal]
            [hive-mcp.agent.ling.lifecycle :as lc]
            [hive-mcp.agent.ling.strategy :as strategy]
            [hive-mcp.agent.ling.terminal-registry :as treg]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden-fn]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (treg/clear-registry!) (try (f) (finally (treg/clear-registry!)))))

(def ^:private Ling
  "A ling record's fields, each optional, so every cond-> arm of ling-ctx is
   both taken and skipped."
  [:map
   [:id [:enum "l1" "l2"]]
   [:cwd {:optional true} [:enum "/w" nil]]
   [:presets {:optional true} [:enum ["a"] nil]]
   [:project-id {:optional true} [:enum "p" nil]]
   [:spawn-mode {:optional true} [:enum :headless :vterm nil]]
   [:model {:optional true} [:enum "opus" nil]]
   [:provider {:optional true} [:enum :anthropic nil]]
   [:kg-compress? {:optional true} [:maybe :boolean]]
   [:verbose? {:optional true} [:maybe :boolean]]
   [:llm-retries {:optional true} [:enum 3 nil]]
   [:resume {:optional true} [:enum {:run-id "r"} nil]]
   [:token-budget {:optional true} [:enum 1000 nil]]
   [:sandbox {:optional true} [:maybe :boolean]]
   [:unrelated {:optional true} [:enum :dropped]]])

(def ^:private always [:id :cwd :presets :project-id :spawn-mode :model])
(def ^:private when-truthy [:provider :llm-retries :resume :token-budget])
(def ^:private when-some [:kg-compress? :verbose? :sandbox])

(deftrifecta-from-schema ling-ctx-trifecta
  hive-mcp.agent.ling.lifecycle/ling-ctx
  {:in          Ling
   :out         [:map [:id :any]]
   :rel         (fn [l out]
                  (and (= (select-keys out always) (zipmap always (map l always)))
                       (every? #(= (contains? out %) (boolean (get l %))) when-truthy)
                       (every? #(= (contains? out %) (some? (get l %))) when-some)
                       (not (contains? out :unrelated))
                       (every? #(= (get out %) (get l %)) (keys out))))
   :num-tests   300
   :n-cases     12
   :golden-path "test/golden/agent/lifecycle-ling-ctx.edn"})

(defn- stub-addon [id]
  (reify terminal/ITerminalAddon
    (terminal-id [_] id)
    (terminal-spawn! [_ ctx _] (str (name id) ":" (:id ctx)))
    (terminal-dispatch! [_ _ _] true)
    (terminal-status [_ _ ds] ds)
    (terminal-kill! [_ _] {:killed? true})
    (terminal-interrupt! [_ _] {:success? true})))

(deftest-golden-fn resolve-strategy-golden
  "test/golden/agent/lifecycle-resolve-strategy.edn"
  (fn []
    (treg/clear-registry!)
    (treg/register-terminal! :stub-term (stub-addon :stub-term))
    (let [s (lc/resolve-strategy :stub-term)]
      {:registered (strategy/strategy-spawn! s {:id "l9"} {})
       :missing    (try (lc/resolve-strategy :no-such-mode-f02)
                        (catch clojure.lang.ExceptionInfo e
                          {:msg (ex-message e)
                           :mode (:mode (ex-data e))
                           :terminals (:registered-terminals (ex-data e))}))})))

(deftest provider-key-resolution-test
  ;; an unknown provider has no secret key mapped and short-circuits to nil
  (is (nil? (lc/resolve-api-key-for-provider :no-such-provider)))
  (is (nil? (lc/resolve-api-key-for-provider nil)))
  (is (keyword? (lc/default-spawn-mode))))

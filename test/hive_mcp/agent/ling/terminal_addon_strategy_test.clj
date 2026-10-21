(ns hive-mcp.agent.ling.terminal-addon-strategy-test
  "Golden over the ITerminalAddon -> ILingStrategy adapter: all five strategy
   methods driven through an injected happy terminal addon and an injected
   throwing one, so both the delegation and the rescue fallbacks are pinned."
  (:require [clojure.test :refer [deftest is]]
            [hive-addon.terminal :as terminal]
            [hive-mcp.agent.ling.strategy :as strategy]
            [hive-mcp.agent.ling.terminal-addon-strategy :as tas]
            [hive-test.golden :refer [deftest-golden-fn]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- stub-addon
  "An ITerminalAddon stub. `throw?` makes every method throw, the way a broken
   addon would; otherwise each method answers a value derived from its args."
  [id throw?]
  (let [boom (fn [what] (throw (ex-info (str what " failed") {:id id})))]
    (reify terminal/ITerminalAddon
      (terminal-id [_] id)
      (terminal-spawn! [_ ctx _opts] (if throw? (boom "spawn") (str "slave-" (:id ctx))))
      (terminal-dispatch! [_ _ctx task] (if throw? (boom "dispatch") {:queued (:task task)}))
      (terminal-status [_ ctx ds] (if throw? (boom "status") (merge {:slave/id (:id ctx)} ds)))
      (terminal-kill! [_ ctx] (if throw? (boom "kill") {:killed? true :id (:id ctx)}))
      (terminal-interrupt! [_ ctx] (if throw? (boom "interrupt") {:success? true :ling-id (:id ctx)})))))

(defn- drive [addon]
  (let [strat (tas/->terminal-addon-strategy addon)
        ctx   {:id "ling-3"}
        try*  (fn [f] (try (f) (catch clojure.lang.ExceptionInfo e {:thrown (ex-message e)})))]
    {:spawn      (try* #(strategy/strategy-spawn! strat ctx {}))
     :dispatch   (try* #(strategy/strategy-dispatch! strat ctx {:task "t"}))
     :status     (strategy/strategy-status strat ctx {:slave/status :busy})
     :status-nil (strategy/strategy-status strat ctx nil)
     :kill       (strategy/strategy-kill! strat ctx)
     :interrupt  (strategy/strategy-interrupt! strat ctx)}))

(deftest-golden-fn terminal-addon-strategy-golden
  "test/golden/agent/terminal-addon-strategy.edn"
  (fn [] {:happy (drive (stub-addon :gold false)) :throwing (drive (stub-addon :gold true))}))

(deftest constructor-contract-test
  (is (satisfies? strategy/ILingStrategy (tas/->terminal-addon-strategy (stub-addon :x false))))
  (is (thrown? AssertionError (tas/->terminal-addon-strategy {:not :an-addon}))))
